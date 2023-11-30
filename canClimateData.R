defineModule(sim, list(
  name = "canClimateData",
  description = paste(
    "Prepares projected and historical climate data for fitting and predicting fires,",
    "and calculating climate effects on forest growth and mortality."
  ),
  keywords = "",
  authors = c(
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "aut"),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre")),
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(canClimateData = "1.0.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "canClimateData.Rmd")),
  reqdPkgs = list("archive", "digest", "geodata", "googledrive", "purrr",
                  "R.utils", "sf", "spatialEco", "terra",
                  "PredictiveEcology/climateData@development (>= 1.0.1)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.5.9046)",
                  "PredictiveEcology/LandR@development (>= 1.1.0.9064)",
                  "PredictiveEcology/reproducible@development (>= 2.0.8.9001)",
                  "PredictiveEcology/SpaDES.tools@development (>= 2.0.4.9002)"),
  parameters = rbind(
    defineParameter("bufferDist", "numeric", 20000, NA, NA,
                    "Distance (m) to buffer `studyArea` and `rasterToMatch` when creating 'Large' versions."),
    defineParameter("climateGCM", "character", "CNRM-ESM2-1", NA, NA,
                    paste("Global Circulation Model to use for climate projections:",
                          "currently '13GCMs_ensemble', 'CanESM5', 'CNRM-ESM2-1', or 'CCSM4'.")),
    defineParameter("climateSSP", "numeric", 370, NA, NA,
                    "SSP emissions scenario for `climateGCM`: one of 245, 370, or 585.",
                    "[If using 'climateGCM = CCSM4', climateSSP must be one of 45 or 85.]"),
    defineParameter("historicalFireYears", "numeric", default = 1991:2020, NA, NA,
                    desc = "range of years captured by the historical climate data"),
    defineParameter("projectedFireYears", "numeric", default = 2011:2100, NA, NA,
                    desc = "range of years captured by the projected climate data"),
    defineParameter("studyAreaName", "character", NA_character_, NA, NA,
                    paste("User-defined label for the current stuyd area.",
                          "If `NA`, a hash of `studyArea` will be used.")),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = bindrows(
    expectsInput("rasterToMatch", "SpatRaster",
                 desc = "template raster", sourceURL = NA),
    expectsInput("rasterToMatchLarge", "SpatRaster",
                 desc = "template raster for larger area", sourceURL = NA),
    expectsInput("rasterToMatchReporting", "SpatRaster",
                 desc = "template raster for reporting area", sourceURL = NA),
    expectsInput("studyArea", "sf",
                 desc = "study area used for simulation (buffered to mitigate edge effects)",
                 sourceURL = NA),
    expectsInput("studyAreaLarge", "sf",
                 desc = "study area used for module parameterization (buffered)", sourceURL = NA),
    expectsInput("studyAreaReporting", "sf",
                 desc = "study area used for reporting/post-processing", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("ATAstack", "SpatRaster",
                  desc = "annual projected mean annual temperature anomalies, units stored as tenth of a degree"),
    createsOutput("CMIstack", "SpatRaster",
                  desc = "annual projected mean climate moisture deficit"),
    createsOutput("CMInormal", "SpatRaster",
                  desc = "Climate Moisture Index Normals from 1950-2010"),
    createsOutput("historicalClimateRasters", "list",
                  desc = "list of a single raster stack - historical MDC calculated from ClimateNA data"),
    createsOutput("projectedClimateRasters", "list",
                  desc = "list of a single raster stack - projected MDC calculated from ClimateNA data")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.canClimateData = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "canClimateData", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "canClimateData", "save")
    },
    warning(paste("Undefined event type: \"", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  dPath <- asPath(inputPath(sim), 1)

  if (is.na(P(sim)$studyAreaName)) {
    ## use unique hash as study area name
    P(sim, "studyAreaName") <- studyAreaName(sim$studyArea)
  }

  ## ensure this matches mod$targetCRS defined in .inputObjects !!
  mod$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  fn1 <- function(x) {
    x <- readRDS(x)
    if (is(x, "PackedSpatVector")) {
      x <- terra::unwrap(x)
    }
    st_as_sf(x) |>
      st_transform(mod$targetCRS)
  }

  canProvs <- Cache(
    prepInputs,
    "GADM",
    fun = fn1,
    dlFun = "geodata::gadm",
    country = "CAN", level = 1, path = dPath, version = "3.6",
    targetFile = "gadm36_CAN_1_sp.rds", ## TODO: update with 'version' above
    destinationPath = dPath,
    useCache = P(sim)$.useCache
  ) |>
    sf::st_as_sf() |>
    sf::st_transform(mod$targetCRS)

  whereAmI <- postProcessTo(from = canProvs, to = sim$studyArea) |>
    Cache(omitArgs = "from") |> # canProvs will never vary
    (function(x) x$NAME_1)() |>
    gsub("Nunavut", "Northwest Territories", x = _) |> ## NT+NU together
    unique()

  provsWithData <- data.frame(
    shortName = c("AB", "BC", "MB", "NT", "ON", "QC", "SK", "YT"),
    longName = c("Alberta", "British Columbia", "Manitoba",
                 "Northwest Territories",
                 "Ontario", "Québec", "Saskatchewan", "Yukon"),
    dirName = c("Alberta", "British Columbia", "Manitoba",
                "Northwest Territories & Nunavut",
                "Ontario", "Quebec", "Saskatchewan", "Yukon") ## no accents
  )

  stopifnot(all(whereAmI %in% provsWithData$longName))

  mod$studyAreaNameShort <- provsWithData[provsWithData$longName %in% whereAmI, ]$shortName
  mod$studyAreaNameDir <- provsWithData[provsWithData$longName %in% whereAmI, ]$dirName |>
    setNames(mod$studyAreaNameShort)

  if (!P(sim)$climateGCM %in% c("13GCMs_ensemble", "CanESM5", "CNRM-ESM2-1", "CCSM4")) {
    stop("Invalid climate model specified.\n",
         "climateGCM should be one of '13GCMs_ensemble', 'CanESM5', 'CNRM-ESM2-1', or 'CCSM4'.")
  }

  if (!P(sim)$climateSSP %in% c(45, 85, 245, 370, 585)) {
    stop("Invalid SSP scenario for climate model ", P(sim)$climateGCM, ".\n",
         "climateSSP should be one of '245', '370', or '585' (or one of RCP '45' and '85').")
  }

  ## studyArea-specific shapefiles and rasters
  dt <- data.table::fread(file = file.path(dataPath(sim), "climateDataURLs.csv"))
  dt <- dt[studyArea %in% mod$studyAreaNameShort]

  digestSA_RTM <- CacheDigest(list(sim$studyArea, sim$rasterToMatch))$outputHash

  sim$studyArea$studyAreaName <- paste0(P(sim)$studyAreaName, collapse = "_")  # makes it a data.frame

  stopifnot(getOption("reproducible.useNewDigestAlgorithm") == 2)

  if (FALSE) { # currently not used --> will be needed for climateNA
    demURL <- sapply(mod$studyAreaNameShort, switch,
                     AB = "https://drive.google.com/file/d/1g1SEU65zje6686pQXQzVVQQc44IXaznr/",
                     BC = "https://drive.google.com/file/d/1DaAYFr0z38qmbZcz452QPMP_fzwUIqLD/",
                     MB = "https://drive.google.com/file/d/1X7b2CE6QyCvik3UG9pUj6zc4b5UYZi8w/",
                     NT = "https://drive.google.com/file/d/13n8LjQJihy9kd3SniS91EuXunowbWOBa/", ## with NU
                     ON = "https://drive.google.com/file/d/1NP2toth6-c5g7dzwLh34pqZ0rdypTauS/",
                     QC = "https://drive.google.com/file/d/1xShpp_irB2AH1ak9Z1i4wXZJuzncpAbc/",
                     SK = "https://drive.google.com/file/d/1CooPdqc3SlVVU7y_BaPfZD0OXt42fjBC/",
                     YT = "https://drive.google.com/file/d/1CUMjLFGdGtwaQlErQ0Oq89ICUCcX641Q/",
                     RIA = "https://drive.google.com/file/d/13sGg1X9DEOSkedg1m0PxcdJiuBESk072/")


    ## get pre-made DEM to use with climate data
    dems <- lapply(mod$studyAreaNameShort, function(prov) {
      cacheTags <- c(prov, currentModule(sim))
      dem <- Cache(prepInputs,
                   url = demURL[[prov]],
                   destinationPath = dPath,
                   fun = "terra::rast",
                   useCache = P(sim)$.useCache,
                   userTags = c(paste0("DEM_", prov), cacheTags))
      crs(dem) <- crs("epsg:4326")
      dem
    })
    dem <- SpaDES.tools::mergeRaster(dems)
  }

  ##
  ## Five climateType-specific objects needed for each prepClimateData:
  #  climateType: climateType name
  #  climateURLs: URLs
  #  climatePath: output path; should be shared across project e.g., `inputPath(sim)`
  #  climateYears: years for historical or projected data
  #  fun: a custom "load after preProcess" function
  ##

  # Object 1 -- climateType
  climateType <- list("historical", "projected_monthly", "normals", "projected_annual_ATA", "projected_annual_CMI")
  names(climateType) <- climateType

  # Object 2 -- URLS
  ## lookup table to get climate urls  based on studyArea, GCM, and SSP
  historicalClimateURL <- dt[type == "hist_monthly", GID]
  names(historicalClimateURL) <- mod$studyAreaNameShort

  projectedClimateUrlMonthly <- dt[GCM == P(sim)$climateGCM &
                                     SSP == P(sim)$climateSSP &
                                     type == "proj_monthly", GID]
  names(projectedClimateUrlMonthly) <- mod$studyAreaNameShort

  projectedClimateUrlAnnual <- dt[GCM == P(sim)$climateGCM &
                                    SSP == P(sim)$climateSSP &
                                    type == "proj_annual", GID]
  names(projectedClimateUrlAnnual) <- mod$studyAreaNameShort

  normalsClimateUrl <- dt[type == "hist_normals", GID]
  names(normalsClimateUrl) <- mod$studyAreaNameShort
  climateURLs <- list(historicalClimateURL,
                      projectedClimateUrlMonthly,
                      normalsClimateUrl,
                      projectedClimateUrlAnnual,
                      projectedClimateUrlAnnual)

  # Object 3 -- climateYears
  climateYears <- list(P(sim)$historicalFireYears,
                    P(sim)$projectedFireYears,
                    NA,
                    P(sim)$projectedFireYears,
                    P(sim)$projectedFireYears
                    )

  # Object 4. output paths (`climatePaths`)
  historicalClimatePath <- file.path(dPath, "climate", "historic") |>
    checkPath(create = TRUE)
  projectedClimatePathRoot <- file.path(dPath, "climate", "future",
                                    paste0(P(sim)$climateGCM, "_ssp", P(sim)$climateSSP)) |>
    checkPath(create = TRUE)
  projectedClimatePathMonthly <- file.path(projectedClimatePathRoot, "monthly") |>
    checkPath(create = TRUE)
  projAnnualClimatePathAnnual <- file.path(projectedClimatePathRoot, "annual") |>
    checkPath(create = TRUE)
  normalsClimatePath <- checkPath(file.path(historicalClimatePath, "normals"), create = TRUE)

  climatePath <- list(historicalClimatePath,
                      projectedClimatePathMonthly,
                      normalsClimatePath,
                      projAnnualClimatePathAnnual,
                      projAnnualClimatePathAnnual)

  # Object 5 -- the function to call within prepClimate
  fun <- list(
    quote(makeMDC(inputPath = checkPath(file.path(climatePath, SANlong),
                                        create = TRUE),
                  years = climateYears)),
    quote(makeMDC(inputPath = checkPath(file.path(climatePath, SANlong),
                                        create = TRUE),
                  years = climateYears)),
    quote(makeLandRCS_1950_2010_normals(pathToNormalRasters = file.path(climatePath, SANlong))),
    quote(makeLandRCS_projectedATA(normalMAT = normals[["MATnormal"]],
                                         studyAreaForMask = studyAreaForMask,
                                         pathToFutureRasters = file.path(climatePath, SANlong),
                                         years = climateYears)),
    quote(makeLandRCS_projectedCMI(normalMAT = normals[["MATnormal"]],
                                         studyAreaForMask = studyAreaForMask,
                                         pathToFutureRasters = file.path(climatePath, SANlong),
                                         years = climateYears))
    )

  # Object 6 -- what variable is being calculated
  climateVar <- list("MDC", "MDC", "Many", "ATA", "CMI")

  # Transpose the lists so each climateType is top element
  climateEraArgs <- purrr::transpose(listNamed(climateType, climateYears, climatePath, climateURLs, fun, climateVar))

  commonArgs <- list(studyAreaNamesShort = mod$studyAreaNameShort,
                     studyAreaNamesLong = mod$studyAreaNameDir,
                     studyAreaName = P(sim)$studyAreaName,
                     rasterToMatch = sim$rasterToMatch, studyArea = sim$studyArea,
                     currentModuleName = currentModule(sim),
                     digestSA_RTM = digestSA_RTM)

  omitArgs <- c("rasterToMatch", "studyArea")
  quick <- c("climatePath")

  # The 4 steps below can be put into a loop, but this is perhaps clearer to see each step,
  #  particularly with the different assignments to sim$
  # historical
  eraHere <- climateType[[1]]
  allArgs <- modifyList2(climateEraArgs[[eraHere]], commonArgs)
  out <- Cache(do.call(prepClimateData, allArgs, quote = TRUE), # quote is needed to not evaluated the `fun`
                                        omitArgs = omitArgs, quick = quick,
                                        .functionName = prepClimateFunctionName(eraHere))
  sim$historicalClimateRasters <- list("MDC" = out)

  # projected_monthly
  eraHere <- climateType[[2]]
  allArgs <- modifyList2(climateEraArgs[[eraHere]], commonArgs)
  out <- Cache(do.call(prepClimateData, allArgs, quote = TRUE),
                                       omitArgs = omitArgs, quick = quick,
                                       .functionName = prepClimateFunctionName(eraHere))
  sim$projectedClimateRasters <- list("MDC" = out)

  # normals
  eraHere <- climateType[[3]]
  allArgs <- modifyList2(climateEraArgs[[eraHere]], commonArgs)
  normals <- Cache(do.call(prepClimateData, allArgs, quote = TRUE),
                   omitArgs = omitArgs, quick = quick,
                   .functionName = prepClimateFunctionName(eraHere))
  sim$CMInormal <- normals[["CMInormal"]]

  # projected_annual
  # undebug(prepInputs)
  eraHere <- climateType[[4]]
  sa <- commonArgs$studyArea
  commonArgsSubset <- commonArgs[!names(commonArgs) %in% c("studyArea", "rasterToMatch")]
  commonArgsSubset$studyAreaForMask <- sa
  commonArgsSubset$saveInner <- FALSE
  allArgs <- modifyList2(climateEraArgs[[eraHere]], commonArgsSubset, list(normals = normals)) # normals is used by the fun
  projCMIATA <- Cache(do.call(prepClimateData, allArgs, quote = TRUE),
                      omitArgs = omitArgs, quick = quick,
                      .functionName = prepClimateFunctionName(eraHere))
  browser()

  eraHere <- climateType[[5]] # very similar to climateType[[4]]
  allArgs <- modifyList2(climateEraArgs[[eraHere]], commonArgsSubset, list(normals = normals)) # normals is used by the fun
  projCMIATA <- Cache(do.call(prepClimateData, allArgs, quote = TRUE),
                      omitArgs = omitArgs, quick = quick,
                      .functionName = prepClimateFunctionName(eraHere))

  browser()
  sim$ATAstack <- Cache(SpaDES.tools::mergeRaster, projCMIATA[["projectedATA"]])
  sim$CMIstack <- Cache(SpaDES.tools::mergeRaster, projCMIATA[["projectedCMI"]])


  browser()
  ## CLIMATE DATA FOR gmcsDataPrep:
  ## 1) get and unzip normals and projected annual
  ## 2) run makeLandRCS_1950_2010normals, it returns a raster stack with two layers, normal MAT, and normal CMI
  ## 3) assign normal CMI to sim
  ## 4) run makeLandRCS_projectedCMIandATA, with normal MAT as an input arg. It returns a list of raster stacks (projected ATA and CMI). Assign both to sim
  ## 5) Profit
  # out <- Map(era = eras, climateYears = years, climatePath = climatePaths,
  #            climateURLs = climateURLs, fun = funs,
  #     function(era, climateYears, climatePath, climateURLs, fun) {
  #              out <- Cache(
  #                prepClimateData(studyAreaNamesShort = mod$studyAreaNameShort,
  #                                studyAreaNamesLong = mod$studyAreaNameDir,
  #                                studyAreaName = P(sim)$studyAreaName,
  #                                climateYears = climateYears, # not used by normals
  #                                rasterToMatch = sim$rasterToMatch, studyArea = sim$studyArea,
  #                                currentModuleName = currentModule(sim),
  #                                climatePath = climatePath, climateURLs = climateURLs,
  #                                normalsClimatePath = normalsClimatePath, # only used by normals
  #                                digestSA_RTM = digestSA_RTM,
  #                                era = era,
  #                                fun = fun
  #                ), omitArgs = c("rasterToMatch", "studyArea"), quick = c("climatePath"),
  #                .functionName = paste0("prepClimateData_", era)
  #              )
  #            })
  #
  # # Use the "grep" below in case a user uses slightly different era names
  # sim$CMInormal <- out[[grep("norm", eras, value = TRUE)]][["CMInormal"]]
  # sim$historicalClimateRasters <- out[[grep("hist", eras, value = TRUE)]]
  # sim$projectedClimateRasters <- out[[grep("proj", eras, value = TRUE)]]
  # browser()
  # sim$historicalClimateRasters <- Cache(
  #   prepClimateData(studyAreaNamesShort = mod$studyAreaNameShort, studyAreaNamesLong = mod$studyAreaNameDir,
  #                   studyAreaName = P(sim)$studyAreaName,
  #                   climateYears = P(sim)$historicalFireYears,
  #                   rasterToMatch = sim$rasterToMatch, studyArea = sim$studyArea,
  #                   currentModuleName = currentModule(sim),
  #                   climatePath = historicalClimatePath, climateURLs = historicalClimateURL,
  #                   digestSA_RTM = digestSA_RTM,
  #                   era = "historical",
  #                   fun = quote(
  #                     makeMDC(inputPath = checkPath(file.path(climatePath, SANlong),
  #                                                   create = TRUE),
  #                             years = climateYears)),
  #                   # climatePath = climatePath, # for makeMDC
  #                   # studyAreaNameDir = studyAreaNameLong[[prov]], # for makeMDC
  #                   # climateYears = climateYears
  #                   ), omitArgs = c("rasterToMatch", "studyArea"), quick = c("climatePath"),
  #   .functionName = "prepClimateData_historical"
  # )
  #
  # ## FUTURE CLIMATE DATA
  # sim$projectedClimateRasters <-
  #   prepClimateData(studyAreaNamesShort = mod$studyAreaNameShort, studyAreaNamesLong = mod$studyAreaNameDir,
  #                   studyAreaName = P(sim)$studyAreaName,
  #                   climateYears = P(sim)$projectedFireYears,
  #                   rasterToMatch = sim$rasterToMatch, studyArea = sim$studyArea,
  #                   currentModuleName = currentModule(sim),
  #                   climatePath = projectedClimatePath, climateURLs = projectedClimateUrl,
  #                   digestSA_RTM = digestSA_RTM,
  #                   era = "projected",
  #                   fun = quote(
  #                     makeMDC(inputPath = checkPath(file.path(climatePath, SANlong),
  #                                                   create = TRUE),
  #                             years = climateYears)),
  #                   # climatePath = climatePath, # for makeMDC
  #                   # studyAreaNameDir = studyAreaNameDir[[prov]], # for makeMDC
  #                   climateYears = climateYears) # for makeMDC)
  #
  # norms <- prepClimateData(studyAreaNamesShort = mod$studyAreaNameShort, studyAreaNamesLong = mod$studyAreaNameDir,
  #                 studyAreaName = P(sim)$studyAreaName,
  #                 climateYears = P(sim)$projectedFireYears,
  #                 rasterToMatch = sim$rasterToMatch, studyArea = sim$studyArea,
  #                 currentModuleName = currentModule(sim),
  #                 climatePath = normalsClimatePath, climateURLs = projectedClimateUrl,
  #                 digestSA_RTM = digestSA_RTM,
  #                 era = "projected",
  #                 fun = quote(
  #                   makeLandRCS_1950_2010_normals(
  #                   pathToNormalRasters = file.path(climatePath, SANlong))
  #                   ),
  #                 # climatePath = climatePath, # for makeMDC
  #                 studyAreaNameDir = studyAreaNameDir) # for makeMDC
  #
  #   histMDCs <- Map(prov = mod$studyAreaNameShort, function(prov) {
  #     cacheTags <- c(P(sim)$studyAreaName, currentModule(sim))
  #     historicalClimateArchive <- file.path(historicalClimatePath, paste0(mod$studyAreaNameDir[[prov]], ".zip"))
  #     historicalMDCfile <- file.path(historicalClimatePath,
  #                                  paste0("MDC_historical_",
  #                                           paste(P(sim)$studyAreaName, collapse = "_"), ".tif"))
  #
  #     browser()
  #     historicalMDC <- Cache(
  #       prepInputs(
  #         # for preProcess
  #         url = as_id(historicalClimateURL[[prov]]),
  #         destinationPath = historicalClimatePath,
  #         archive = historicalClimateArchive,
  #
  #         # For loading to R
  #         fun = quote(
  #           makeMDC(inputPath = checkPath(file.path(historicalClimatePath, studyAreaNameDir),
  #                                         create = TRUE),
  #                   years = years)),
  #         historicalClimatePath = historicalClimatePath, # for makeMDC
  #         studyAreaNameDir = mod$studyAreaNameDir[[prov]], # for makeMDC
  #         years = P(sim)$historicalFireYears, # for makeMDC
  #
  #         # for postProcessTo
  #         to = sim$rasterToMatch,
  #         maskTo = sim$studyArea,
  #         writeTo = historicalMDCfile,
  #         datatype = "INT2U",
  #         useCache = FALSE # This is the internal cache for postProcessTo
  #       ),
  #       omitArgs = c("to", "maskTo", "writeTo"), # don't digest these each time
  #       .functionName = "makeMDC_forHistoricalMDC",
  #       .cacheExtra = c(digestSA_RTM, prov),
  #       quick = c("destinationPath", "archive"), # these are outputs; don't cache on them
  #       userTags = c("historicMDC", prov, cacheTags))
  #
  #     return(historicalMDC)
  #
  #     # if (FALSE) {
  #     #   ## need to download and extract w/o prepInputs to preserve folder structure!
  #     #   if (!file.exists(historicalClimateArchive)) {
  #     #     tryCatch({
  #     #       R.utils::withTimeout({
  #     #         googledrive::drive_download(file = as_id(historicalClimateURL[[prov]]), path = historicalClimateArchive)
  #     #       }, timeout = 1800, onTimeout = "error")
  #     #     },
  #     #     error = function(e) {
  #     #       unlink(historicalClimateArchive)
  #     #       stop(paste0(
  #     #         "The download of the file '", basename(historicalClimateArchive), "' was unsuccessful, ",
  #     #         "most likely due to its size and an unstable internet connection.",
  #     #         "Please download it manually from ",
  #     #         paste0("https://drive.google.com/file/d/", historicalClimateURL[[prov]]),
  #     #         ", save it as ", historicalClimateArchive, "."
  #     #       ))
  #     #     })
  #     #     archive::archive_extract(historicalClimateArchive, historicalClimatePath)
  #     #   } else {
  #     #     if (!dir.exists(tools::file_path_sans_ext(historicalClimateArchive))) {
  #     #       archive::archive_extract(historicalClimateArchive, historicalClimatePath)
  #     #     }
  #     #   }
  #     #
  #     #   ## all downstream stuff from this one archive file should have same Cache assessment
  #     #   ##   do it once here and pass to each subsequent through .cacheExtra
  #     #   digestFiles <- digest::digest(file = historicalClimateArchive, algo = "xxhash64")
  #     #   digestYears <- CacheDigest(list(P(sim)$historicalFireYears))$outputHash
  #     #
  #     #   historicalMDC <- {
  #     #     makeMDC(inputPath = checkPath(file.path(historicalClimatePath, mod$studyAreaNameDir[[prov]]),
  #     #                                   create = TRUE),
  #     #             years = P(sim)$historicalFireYears
  #     #     ) |>
  #     #       postProcessTo(to = sim$rasterToMatch,
  #     #                     maskTo = sim$studyArea,
  #     #                     writeTo = historicalMDCfile,
  #     #                     quick = "writeTo",
  #     #                     datatype = "INT2U")
  #     #   } |>
  #     #     Cache(
  #     #       omitArgs = c("from", "to", "maskTo", "writeTo", "to"),
  #     #       .functionName = "makeMDC_forHistoricalMDC",
  #     #       .cacheExtra = c(digestFiles, digestSA_RTM, digestYears),
  #     #       userTags = c("historicMDC", cacheTags)
  #     #     )
  #     # }
  #
  #   })
  #   historicalMDC <- SpaDES.tools::mergeRaster(histMDCs)
  #
  #   ## The names need "year" at the start, because not every year will have fires (data issue in RIA),
  #   ## so fireSense matches fires + climate rasters by year.
  #   ## WARNING: names(historicalMDC) <- paste0('year', P(sim)$historicalFireYears) # Bad
  #   ##          |-> allows for index mismatching
  #   historicalMDC <- updateStackYearNames(historicalMDC, Par$historicalFireYears)
  #
  #   compareGeom(historicalMDC, sim$rasterToMatch)
  #
  #   sim$historicalClimateRasters <- list("MDC" = historicalMDC)
  #
  #   ## FUTURE CLIMATE DATA
  #   projectedClimatePath <- file.path(dPath, "climate", "future",
  #                                     paste0(P(sim)$climateGCM, "_ssp", P(sim)$climateSSP)) |>
  #     checkPath(create = TRUE)
  #
  #   projMDCs <- lapply(mod$studyAreaNameShort, function(prov) {
  #     browser()
  #
  #     cacheTags <- c(prov, currentModule(sim))
  #     projectedClimateArchive <- file.path(dirname(projectedClimatePath),
  #                                          paste0(mod$studyAreaNameDir[[prov]], "_",
  #                                                 P(sim)$climateGCM, "_ssp",
  #                                                 P(sim)$climateSSP, ".zip"))
  #     projectedMDCfile <- file.path(dirname(projectedClimatePath),
  #                                   paste0("MDC_future_", P(sim)$climateGCM,
  #                                          "_ssp", P(sim)$climateSSP, "_",
  #                                          paste(P(sim)$studyAreaName, collapse = "_"), ".tif"))
  #
  #     if (FALSE) {
  #       ## need to download and extract w/o prepInputs to preserve folder structure!
  #       if (!file.exists(projectedClimateArchive)) {
  #         tryCatch({
  #           R.utils::withTimeout({
  #             googledrive::drive_download(file = as_id(projectedClimateUrl[[prov]]), path = projectedClimateArchive)
  #           }, timeout = 1800,  onTimeout = "error")
  #         },
  #         error = function(e) {
  #           unlink(projectedClimateArchive)
  #           stop(paste0(
  #             "The download of the file '", basename(projectedClimateArchive), "' was unsuccessful, ",
  #             "most likely due to its size and an unstable internet connection.",
  #             "Please download it manually from ",
  #             paste0("https://drive.google.com/file/d/", projectedClimateUrl[[prov]]),
  #             ", save it as ", projectedClimateArchive, "."
  #           ))
  #         })
  #         archive::archive_extract(projectedClimateArchive, projectedClimatePath)
  #       } else {
  #         if (!dir.exists(file.path(dirname(projectedClimatePath),
  #                                   paste0(P(sim)$climateGCM, "_ssp",
  #                                          P(sim)$climateSSP, mod$studyAreaNameDir[[prov]])))) {
  #           archive::archive_extract(projectedClimateArchive, projectedClimatePath)
  #         }
  #       }
  #       digestFiles <- digest::digest(file = projectedClimateArchive, algo = "xxhash64")
  #       digestYears <- CacheDigest(list(P(sim)$projectedFireYears))$outputHash
  #
  #       projectedMDC <- {
  #         makeMDC(inputPath = file.path(projectedClimatePath, mod$studyAreaNameDir[[prov]]),
  #                 years = P(sim)$projectedFireYears) |>
  #           postProcessTo(to = sim$rasterToMatch,
  #                         maskTo = sim$studyArea,
  #                         writeTo = projectedMDCfile,
  #                         quick = "writeTo",
  #                         datatype = "INT2U")
  #       } |>
  #         Cache(
  #           omitArgs = c("from", "to", "maskTo"),
  #           userTags = c("projectedMDC", cacheTags),
  #           .functionName = "makeMDC_forProjectedMDC",
  #           .cacheExtra = c(digestFiles, digestSA_RTM, digestYears)
  #         )
  #     }
  #
  #     browser()
  #     prepInputs(url = as_id(projectedClimateUrl[[prov]], destin))
  #
  #
  #
  #     if (!file.exists(projectedClimateArchive)) {
  #       tryCatch({
  #         R.utils::withTimeout({
  #           googledrive::drive_download(file = as_id(projectedClimateUrl[[prov]]), path = projectedClimateArchive)
  #         }, timeout = 1800,  onTimeout = "error")
  #       },
  #       error = function(e) {
  #         unlink(projectedClimateArchive)
  #         stop(paste0(
  #           "The download of the file '", basename(projectedClimateArchive), "' was unsuccessful, ",
  #           "most likely due to its size and an unstable internet connection.",
  #           "Please download it manually from ",
  #           paste0("https://drive.google.com/file/d/", projectedClimateUrl[[prov]]),
  #           ", save it as ", projectedClimateArchive, "."
  #         ))
  #       })
  #       archive::archive_extract(projectedClimateArchive, projectedClimatePath)
  #     } else {
  #       if (!dir.exists(file.path(dirname(projectedClimatePath),
  #                                 paste0(P(sim)$climateGCM, "_ssp",
  #                                        P(sim)$climateSSP, mod$studyAreaNameDir[[prov]])))) {
  #         archive::archive_extract(projectedClimateArchive, projectedClimatePath)
  #       }
  #     }
  #     digestFiles <- digest::digest(file = projectedClimateArchive, algo = "xxhash64")
  #     digestYears <- CacheDigest(list(P(sim)$projectedFireYears))$outputHash
  #
  #     projectedMDC <- {
  #       makeMDC(inputPath = file.path(projectedClimatePath, mod$studyAreaNameDir[[prov]]),
  #               years = P(sim)$projectedFireYears) |>
  #         postProcessTo(to = sim$rasterToMatch,
  #                       maskTo = sim$studyArea,
  #                       writeTo = projectedMDCfile,
  #                       quick = "writeTo",
  #                       datatype = "INT2U")
  #     } |>
  #       Cache(
  #         omitArgs = c("from", "to", "maskTo"),
  #         userTags = c("projectedMDC", cacheTags),
  #         .functionName = "makeMDC_forProjectedMDC",
  #         .cacheExtra = c(digestFiles, digestSA_RTM, digestYears)
  #       )
  #   })
  #   projectedMDC <- SpaDES.tools::mergeRaster(projMDCs)
  #
  #   ## WARNING: names(projectedMDC) <- paste0('year', P(sim)$projectedFireYears) # Bad
  #   ##          |-> allows for index mismatching
  #   projectedMDC <- updateStackYearNames(projectedMDC, Par$projectedFireYears)
  #
  #   sim$projectedClimateRasters <- list("MDC" = projectedMDC)

  ## CLIMATE DATA FOR gmcsDataPrep:
  ## 1) get and unzip normals and projected annual
  ## 2) run makeLandRCS_1950_2010normals, it returns a raster stack with two layers, normal MAT, and normal CMI
  ## 3) assign normal CMI to sim
  ## 4) run makeLandRCS_projectedCMIandATA, with normal MAT as an input arg. It returns a list of raster stacks (projected ATA and CMI). Assign both to sim
  ## 5) Profit
#
#   norms <- lapply(mod$studyAreaNameShort, function(prov) {
#     cacheTags <- c(prov, currentModule(sim))
#     # normalsClimateUrl <- dt[studyArea == prov & type == "hist_normals", GID]
#     normalsClimatePath <- checkPath(file.path(historicalClimatePath, "normals"), create = TRUE)
#     normalsClimateArchive <- file.path(normalsClimatePath, paste0(mod$studyAreaNameDir[[prov]], "_normals.zip"))
#
#     if (!file.exists(normalsClimateArchive)) {
#       ## need to download and extract w/o prepInputs to preserve folder structure!
#       tryCatch({
#         R.utils::withTimeout({
#           googledrive::drive_download(file = as_id(normalsClimateUrl), path = normalsClimateArchive)
#         }, timeout = 1800, onTimeout = "error")
#       },
#       error = function(e) {
#         unlink(normalsClimateArchive)
#         stop(paste0(
#           "The download of the file '", basename(normalsClimateArchive), "' was unsuccessful, ",
#           "most likely due to its size and an unstable internet connection.",
#           "Please download it manually from ",
#           paste0("https://drive.google.com/file/d/", normalsClimateUrl),
#           ", save it as ", normalsClimateArchive, "."
#         ))
#       })
#       archive::archive_extract(normalsClimateArchive, normalsClimatePath)
#     }
#
#     Cache(
#       makeLandRCS_1950_2010_normals,
#       pathToNormalRasters = file.path(normalsClimatePath, mod$studyAreaNameDir[[prov]]),
#       rasterToMatch = sim$rasterToMatch,
#       userTags = c("normals", cacheTags)
#     )
#   })
#   normals <- SpaDES.tools::mergeRaster(norms)
#   sim$CMInormal <- normals[["CMInormal"]]
#
#   browser()
#   projCMIATA <- lapply(mod$studyAreaNameShort, function(prov) {
#     cacheTags <- c(prov, currentModule(sim))
#     projAnnualClimateUrl <- dt[studyArea == prov &
#                                  GCM == P(sim)$climateGCM &
#                                  SSP == P(sim)$climateSSP &
#                                  type == "proj_annual", GID]
#     projAnnualClimatePath <- file.path(projectedClimatePath, "annual") |>
#       checkPath(create = TRUE)
#     projAnnualClimateArchive <- file.path(dirname(projAnnualClimatePath),
#                                           paste0(mod$studyAreaNameDir[[prov]], "_",
#                                                  P(sim)$climateGCM, "_ssp",
#                                                  P(sim)$climateSSP, "_annual.zip"))
#
#     if (!file.exists(projAnnualClimateArchive)) {
#       ## need to download and extract w/o prepInputs to preserve folder structure!
#       googledrive::drive_download(file = googledrive::as_id(projAnnualClimateUrl), path = projAnnualClimateArchive)
#       archive::archive_extract(projAnnualClimateArchive, projAnnualClimatePath)
#     }
#
#     Cache(makeLandRCS_projectedCMIandATA,
#           normalMAT = normals[["MATnormal"]],
#           pathToFutureRasters = file.path(projAnnualClimatePath, mod$studyAreaNameDir[[prov]]),
#           years = P(sim)$projectedFireYears,
#           useCache = TRUE,
#           userTags = c("projectedCMIandATA", cacheTags))
#   }) |>
#     purrr::transpose()
#
#   sim$ATAstack <- Cache(SpaDES.tools::mergeRaster, projCMIATA[["projectedATA"]])
#   sim$CMIstack <- Cache(SpaDES.tools::mergeRaster, projCMIATA[["projectedCMI"]])

  ## TODO: lighten these are a heavy tests; only check a few of the layers for now
  # yearsToCheck <- sample.int(nlayers(sim$ATAstack), 10)
  # stopifnot(all(unlist(lapply(sim$ATAstack[[yearsToCheck]], function(x) all(!is.na(x[]))))))
  #
  # yearsToCheck <- sample.int(nlayers(sim$CMIstack), 10)
  # stopifnot(all(unlist(lapply(sim$CMIstack[[yearsToCheck]], function(x) all(!is.na(x[]))))))

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  ## TODO: ensure defaults work using terra/sf
  mod$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  if (!suppliedElsewhere("studyArea", sim)) {
    ## random study area spanning portions of NE AB and NW SK
    sim$studyArea <- terra::vect(cbind(-110, 58), crs = "epsg:4326") |>
      SpaDES.tools::randomStudyArea(size = 5e10) |>
      sf::st_as_sf() |>
      sf::st_transform(mod$targetCRS)
  }

  if (!suppliedElsewhere("studyAreaReporting", sim)) {
    sim$studyAreaReporting <- sim$studyArea
  }

  if (!suppliedElsewhere("studyArea", sim)) {
    ## NOTE: studyArea and studyAreaLarge are the same [buffered] area
    sim$studyArea <- sf::st_buffer(sim$studyArea, P(sim)$bufferDist)
  }

  if (!suppliedElsewhere("studyAreaLarge", sim)) {
    sim$studyAreaLarge <- sim$studyArea
  }

  if (is.na(P(sim)$studyAreaName)) {
    ## use unique hash as study area name
    P(sim, "studyAreaName") <- studyAreaName(sim$studyArea)
  }

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- Cache(LandR::prepInputsLCC,
                               year = 2005,
                               studyArea = sim$studyArea,
                               destinationPath = dPath,
                               useCache = P(sim)$.useCache,
                               filename2 = NULL)
    writeRaster(sim$rasterToMatch, file.path(dPath, paste0(P(sim)$studyAreaName, "_rtm.tif")),
                datatype = "INT1U", overwrite = TRUE)
  }

  if (!suppliedElsewhere("rasterToMatchLarge", sim)) {
    sim$rasterToMatchLarge <- Cache(LandR::prepInputsLCC,
                                    year = 2005,
                                    studyArea = sim$studyAreaLarge,
                                    destinationPath = dPath,
                                    useCache = P(sim)$.useCache,
                                    filename2 = NULL)
    writeRaster(sim$rasterToMatchLarge,  file.path(dPath, paste0(P(sim)$studyAreaName, "_rtml.tif")),
                datatype = "INT1U", overwrite = TRUE)
  }

  if (!suppliedElsewhere("rasterToMatchReporting", sim)) {
    sim$rasterToMatchReporting <- Cache(maskTo, sim$rasterToMatch, maskTo = sim$studyAreaReporting)
    writeRaster(sim$rasterToMatchReporting,  file.path(dPath, paste0(P(sim)$studyAreaName, "_rtmr.tif")),
                datatype = "INT1U", overwrite = TRUE)
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


#' prepInputs for climate data
#'
#' @param studyAreaNamesShort A character vector of short names for the "regions", e.g., c("AB", "BC")
#' @param studyAreaNamesLong A character vector of long names for the "regions" e.g., c("Alberta", "British Columbia")
#' @param climateURLs A character vector of urls to pass -- one at a time -- to `prepInputs(url = ...)`
#' @param studyAreaName A character string that represents the studyArea for the project e.g., "Edehzhie"
#' @param climateYears A numeric vector of years to extract climate data for e.g., 1991:2020 or 2025:2100
#' @param rasterToMatch A `SpatRaster` to pass to `prepInputs` --> which will pass to `postProcessTo`.
#'   To skip `postProcessTo`, user must set this to `NULL` and `studyArea = NULL, saveInner = FALSE`.
#' @param studyArea A `SpatVector` to pass to `prepInputs` --> which will pass to `postProcessTo`.
#'   To skip `postProcessTo`, user must set this to `NULL` and `studyArea = NULL, saveInner = FALSE`.
#' @param currentModuleName A character string e.g., extracted from `currentModule(sim)`, defaults to `"NoModule"`
#' @param climatePath A character string that is passed to `prepInputs(..., destinationPath = climatePath)`.
#'   If `saveInner = TRUE` (the default), this will also be used to build the filename passed to
#'   `postProcessTo(..., writeTo = XXX)`.
#' @param digestSA_RTM A charater string that represents the a unique identifier of the `studyArea` and
#'   `rasterToMatch` objects, e.g., from `CacheDigest(list(studyArea, rasterToMatch))$outputHash`. This is
#'   used to save time internally so the many `prepInputs` calls don't have to keep digesting these two
#'   spatial objects.
#' @param climateType A character string that is used as a label for this call. Intended to be used
#'   to indicate e.g., "historical" or "projected" or more nuance, like "projected_annual_ATA".
#'
#' @param fun A quoted function call that is passed to `fun` argument in `prepInputs`.
#'   This can use `SANshort` (each of `studyAreaNamesShort`),
#'   `SANlong` (each of `studyAreaNamesLong`), `climatePath`, `climateURL` (each of `climateURL`),
#'   `climateYears`, or anything passed to `...`
#' @param climateVar A character string used as a label (for filenames and other labels for messaging)
#'   that indicates the type of climate variable
#'   being calculated/downloaded/derived e.g., `"MDC"` (which is the default).
#' @param saveInner Length 1 logical. Should `postProcessTo(...)` be passed a `writeTo` for each
#'   individual `studyAreaShort`
#' @param saveOuter Length 1 logical. After `mergeRaster` of all the `studyAreaShort`, should
#'   the final `SpatRaster` be saved to disk.
#' @param ... Any optional named argument needed for `fun`
#' @export
#'
prepClimateData <- function(studyAreaNamesShort,
                            studyAreaNamesLong, climateURLs, # these are vectorized on studyAreaNamesShort
                            studyAreaName, climateYears,
                            rasterToMatch = NULL, studyArea = NULL, currentModuleName = "NoModule",
                            climatePath, digestSA_RTM, climateType = c("historical", "projected"),
                            fun, climateVar = "MDC", saveInner = TRUE, saveOuter = TRUE, ...) {

  objsForDigest1 <- c("studyAreaName", "climateYears", "fun", "climatePath", "climateType", "currentModuleName")
  objs <- mget(objsForDigest1, envir = environment())
  dig1 <- CacheDigest(objsToDigest = objs)$outputHash
  climDatAll <-  Map(SANshort = studyAreaNamesShort,
                     SANlong = studyAreaNamesLong,
                     climateURL = climateURLs,
                     function(SANshort, SANlong, climateURL) {

                       cacheTags <- c(studyAreaName, currentModuleName)
                       # climateArchive <- file.path(climatePath, paste0(studyAreaNameDir[[SANshort]], ".zip"))
                       filenameForSaving <- NULL
                       if (isTRUE(saveInner))
                         filenameForSaving <- file.path(climatePath,
                                                        paste0(climateVar, "_", climateType[1], "_", SANshort, "_",
                                                               paste(studyAreaName, collapse = "_"), ".tif"))
                       climData <- Cache(
                         prepInputs(
                           # for preProcess
                           targetFile = NA_character_,
                           url = as_id(climateURL),
                           destinationPath = climatePath,
                           # archive = climateArchive,

                           # For loading to R
                           fun = fun,
                           SANshort = SANshort,
                           SANlong = SANlong,
                           climatePath = climatePath,
                           climateURL = climateURL,
                           climateYears = climateYears,
                           ...,

                           # for postProcessTo
                           to = rasterToMatch,
                           maskTo = studyArea,
                           writeTo = filenameForSaving,
                           datatype = "INT2U",
                           useCache = FALSE, # This is the internal cache for postProcessTo

                           overwrite = TRUE # If it exists, overwrite so it doesn't require manual intervention
                         ),
                         omitArgs = c("to", "maskTo", "overwrite", "fun"), # don't digest these each time
                         .functionName = paste0("prepInputs_", format(fun[[1]]), "_", climateType[1], "_", SANshort),
                         .cacheExtra = c(digestSA_RTM, dig1),
                         quick = c("destinationPath", "archive", "writeTo", "climatePath"), # these are outputs; don't cache on them
                         userTags = c(paste0(climateType[1]), SANshort, cacheTags))

                       return(climData)
                     })

  if (is.null(rasterToMatch))
    if (!is.null(list(...)$normals))
      rasterToMatch <- list(...)$normals[[1]]

  climDatAllMerged <- Cache(
    transposeMergeWrite(climDatAll, climateType, climateYears, rasterToMatch,
                        climatePath, studyAreaName, saveOuter, climateVar),
    omitArgs = c("climDatAll", "climateType", "climateYears", "rasterToMatch", "climatePath", "studyAreaName"),
    .cacheExtra = c(digestSA_RTM, dig1, studyAreaNamesLong, studyAreaNamesShort, climateURLs)
  )

  climDatAllMerged

}


prepClimateFunctionName <- function(climateType) {
  paste0("prepClimateData_", climateType)
}


transposeMergeWrite <- function(climDatAll, climateType, climateYears, rasterToMatch, climatePath, studyAreaName,
                                saveOuter = TRUE, climateVar = "MDC") {
  nlyrs <- reproducible::nlayers2(climDatAll[[1]])
  message("merging spatial layers by year for ", climateType, " data :")
  climDatAllMerged <- Map(nam = names(climDatAll[[1]]), function(nam) {
    message("layer ", names(climDatAll[[1]][[nam]]), "; ", match(nam, names(climDatAll[[1]])), " of ", nlyrs)
    ll <- unname(lapply(climDatAll, function(x) {
      x[[nam]]
    } ))
    SpaDES.tools::mergeRaster(ll)
  })
  climDatAllMerged <- if (is(climDatAllMerged[[1]], "SpatRaster"))
    terra::rast(climDatAllMerged) else raster::stack(climDatAllMerged)

  # varnames(climDatAllMerged) <- ""

  ## The names need "year" at the start, because not every year will have fires (data issue in RIA),
  ## so fireSense matches fires + climate rasters by year.
  ## WARNING: names(climDatAllMerged) <- paste0('year', climateYears) # Bad
  ##          |-> allows for index mismatching

  if (!grepl("normal", climateType)) # be loose with "normal" or "normals" because they should be equivalent
    climDatAllMerged <- updateStackYearNames(climDatAllMerged, climateYears)
  compareGeom(climDatAllMerged, rasterToMatch)

  filenameForSaving <- NULL
  if (isTRUE(saveOuter)) {
    filenameForSaving <- file.path(climatePath,
                                   paste0(climateVar, "_", climateType[1], "_",
                                          paste(studyAreaName, collapse = "_"), ".tif"))
    climDatAllMerged <- writeTo(climDatAllMerged, writeTo = filenameForSaving,
                                overwrite = TRUE)
  }
  climDatAllMerged
}
