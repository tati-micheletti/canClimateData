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

  whereAmI <- reproducible::postProcessTo(canProvs, to = sim$studyArea)$NAME_1 |>
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

  ## lookup table to get climate urls  based on studyArea, GCM, and SSP
  historicalClimateURL <- dt[studyArea %in% mod$studyAreaNameShort & type == "hist_monthly", GID]
  names(historicalClimateURL) <- mod$studyAreaNameShort

  projectedClimateUrl <- dt[studyArea %in% mod$studyAreaNameShort &
                              GCM == P(sim)$climateGCM &
                              SSP == P(sim)$climateSSP &
                              type == "proj_monthly", GID]
  names(projectedClimateUrl) <- mod$studyAreaNameShort

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

  sim$studyArea$studyAreaName <- paste0(P(sim)$studyAreaName, collapse = "_")  # makes it a data.frame

  stopifnot(getOption("reproducible.useNewDigestAlgorithm") == 2)

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

  ## HISTORIC CLIMATE DATA
  digestSA_RTM <- CacheDigest(list(sim$studyArea, sim$rasterToMatch))$outputHash

  historicalClimatePath <- file.path(dPath, "climate", "historic") |>
    checkPath(create = TRUE)
  histMDCs <- lapply(mod$studyAreaNameShort, function(prov) {
    cacheTags <- c(P(sim)$studyAreaName, currentModule(sim))
    historicalClimateArchive <- file.path(historicalClimatePath, paste0(mod$studyAreaNameDir[[prov]], ".zip"))
    historicalMDCfile <- file.path(historicalClimatePath,
                                   paste0("MDC_historical_",
                                          paste(P(sim)$studyAreaName, collapse = "_"), ".tif"))

    ## need to download and extract w/o prepInputs to preserve folder structure!
    if (!file.exists(historicalClimateArchive)) {
      tryCatch({
        R.utils::withTimeout({
          googledrive::drive_download(file = as_id(historicalClimateURL[[prov]]), path = historicalClimateArchive)
        }, timeout = 1800, onTimeout = "error")
      },
      error = function(e) {
        unlink(historicalClimateArchive)
        stop(paste0(
          "The download of the file '", basename(historicalClimateArchive), "' was unsuccessful, ",
          "most likely due to its size and an unstable internet connection.",
          "Please download it manually from ",
          paste0("https://drive.google.com/file/d/", historicalClimateURL[[prov]]),
          ", save it as ", historicalClimateArchive, "."
        ))
      })
      archive::archive_extract(historicalClimateArchive, historicalClimatePath)
    } else {
      if (!dir.exists(file.path(dirname(historicalClimateArchive), mod$studyAreaNameDir[[prov]]))) {
        archive::archive_extract(historicalClimateArchive, historicalClimatePath)
      }
    }

    ## all downstream stuff from this one archive file should have same Cache assessment
    ##   do it once here and pass to each subsequent through .cacheExtra
    digestFiles <- digest::digest(file = historicalClimateArchive, algo = "xxhash64")
    digestYears <- CacheDigest(list(P(sim)$historicalFireYears))$outputHash

    historicalMDC <- {
      makeMDC(inputPath = checkPath(file.path(historicalClimatePath, mod$studyAreaNameDir[[prov]]),
                                    create = TRUE),
              years = P(sim)$historicalFireYears
      ) |>
        postProcessTo(to = sim$rasterToMatch,
                      maskTo = sim$studyArea,
                      writeTo = historicalMDCfile,
                      quick = "writeTo",
                      datatype = "INT2U")
    } |>
      Cache(
        omitArgs = c("from", "to", "maskTo", "writeTo", "to"),
        .functionName = "makeMDC_forHistoricalMDC",
        .cacheExtra = c(digestFiles, digestSA_RTM, digestYears),
        userTags = c("historicMDC", cacheTags)
      )

    return(historicalMDC)
  })
  historicalMDC <- SpaDES.tools::mergeRaster(histMDCs)

  ## The names need "year" at the start, because not every year will have fires (data issue in RIA),
  ## so fireSense matches fires + climate rasters by year.
  ## WARNING: names(historicalMDC) <- paste0('year', P(sim)$historicalFireYears) # Bad
  ##          |-> allows for index mismatching
  historicalMDC <- updateStackYearNames(historicalMDC, Par$historicalFireYears)

  compareGeom(historicalMDC, sim$rasterToMatch)

  sim$historicalClimateRasters <- list("MDC" = historicalMDC)

  ## FUTURE CLIMATE DATA
  projectedClimatePath <- file.path(dPath, "climate", "future",
                                    paste0(P(sim)$climateGCM, "_ssp", P(sim)$climateSSP)) |>
    checkPath(create = TRUE)

  projMDCs <- lapply(mod$studyAreaNameShort, function(prov) {
    cacheTags <- c(prov, currentModule(sim))
    projectedClimateArchive <- file.path(dirname(projectedClimatePath),
                                         paste0(mod$studyAreaNameDir[[prov]], "_",
                                                P(sim)$climateGCM, "_ssp",
                                                P(sim)$climateSSP, ".zip"))
    projectedMDCfile <- file.path(dirname(projectedClimatePath),
                                  paste0("MDC_future_", P(sim)$climateGCM,
                                         "_ssp", P(sim)$climateSSP, "_",
                                         paste(P(sim)$studyAreaName, collapse = "_"), ".tif"))

    ## need to download and extract w/o prepInputs to preserve folder structure!
    if (!file.exists(projectedClimateArchive)) {
      tryCatch({
        R.utils::withTimeout({
          googledrive::drive_download(file = as_id(projectedClimateUrl[[prov]]), path = projectedClimateArchive)
        }, timeout = 1800,  onTimeout = "error")
      },
      error = function(e) {
        unlink(projectedClimateArchive)
        stop(paste0(
          "The download of the file '", basename(projectedClimateArchive), "' was unsuccessful, ",
          "most likely due to its size and an unstable internet connection.",
          "Please download it manually from ",
          paste0("https://drive.google.com/file/d/", projectedClimateUrl[[prov]]),
          ", save it as ", projectedClimateArchive, "."
        ))
      })
      archive::archive_extract(projectedClimateArchive, projectedClimatePath)
    } else {
      if (!dir.exists(file.path(dirname(projectedClimateArchive), mod$studyAreaNameDir[[prov]]))) {
        archive::archive_extract(projectedClimateArchive, projectedClimatePath)
      }
    }
    digestFiles <- digest::digest(file = projectedClimateArchive, algo = "xxhash64")
    digestYears <- CacheDigest(list(P(sim)$projectedFireYears))$outputHash

    projectedMDC <- {
      makeMDC(inputPath = file.path(projectedClimatePath, mod$studyAreaNameDir[[prov]]),
              years = P(sim)$projectedFireYears) |>
        postProcessTo(to = sim$rasterToMatch,
                      maskTo = sim$studyArea,
                      writeTo = projectedMDCfile,
                      quick = "writeTo",
                      datatype = "INT2U")
    } |>
      Cache(
        omitArgs = c("from", "to", "maskTo"),
        userTags = c("projectedMDC", cacheTags),
        .functionName = "makeMDC_forProjectedMDC",
        .cacheExtra = c(digestFiles, digestSA_RTM, digestYears)
      )
  })
  projectedMDC <- SpaDES.tools::mergeRaster(projMDCs)

  ## WARNING: names(projectedMDC) <- paste0('year', P(sim)$projectedFireYears) # Bad
  ##          |-> allows for index mismatching
  projectedMDC <- updateStackYearNames(projectedMDC, Par$projectedFireYears)

  sim$projectedClimateRasters <- list("MDC" = projectedMDC)

  ## CLIMATE DATA FOR gmcsDataPrep:
  ## 1) get and unzip normals and projected annual
  ## 2) run makeLandRCS_1950_2010normals, it returns a raster stack with two layers, normal MAT, and normal CMI
  ## 3) assign normal CMI to sim
  ## 4) run makeLandRCS_projectedCMIandATA, with normal MAT as an input arg. It returns a list of raster stacks (projected ATA and CMI). Assign both to sim
  ## 5) Profit

  norms <- lapply(mod$studyAreaNameShort, function(prov) {
    cacheTags <- c(prov, currentModule(sim))
    normalsClimateUrl <- dt[studyArea == prov & type == "hist_normals", GID]
    normalsClimatePath <- checkPath(file.path(historicalClimatePath, "normals"), create = TRUE)
    normalsClimateArchive <- file.path(normalsClimatePath, paste0(mod$studyAreaNameDir[[prov]], "_normals.zip"))

    if (!file.exists(normalsClimateArchive)) {
      ## need to download and extract w/o prepInputs to preserve folder structure!
      tryCatch({
        R.utils::withTimeout({
          googledrive::drive_download(file = as_id(normalsClimateUrl), path = normalsClimateArchive)
        }, timeout = 1800, onTimeout = "error")
      },
      error = function(e) {
        unlink(normalsClimateArchive)
        stop(paste0(
          "The download of the file '", basename(normalsClimateArchive), "' was unsuccessful, ",
          "most likely due to its size and an unstable internet connection.",
          "Please download it manually from ",
          paste0("https://drive.google.com/file/d/", normalsClimateUrl),
          ", save it as ", normalsClimateArchive, "."
        ))
      })
      archive::archive_extract(normalsClimateArchive, normalsClimatePath)
    } else {
      if (!dir.exists(file.path(dirname(normalsClimateArchive), mod$studyAreaNameDir[[prov]]))) {
        archive::archive_extract(normalsClimateArchive, normalsClimatePath)
      }
    }

    Cache(
      makeLandRCS_1950_2010_normals,
      pathToNormalRasters = file.path(normalsClimatePath, mod$studyAreaNameDir[[prov]]),
      rasterToMatch = sim$rasterToMatch,
      userTags = c("normals", cacheTags)
    )
  })
  normals <- SpaDES.tools::mergeRaster(norms)
  sim$CMInormal <- normals[["CMInormal"]]

  projCMIATA <- lapply(mod$studyAreaNameShort, function(prov) {
    cacheTags <- c(prov, currentModule(sim))
    projAnnualClimateUrl <- dt[studyArea == prov &
                                 GCM == P(sim)$climateGCM &
                                 SSP == P(sim)$climateSSP &
                                 type == "proj_annual", GID]
    projAnnualClimatePath <- file.path(projectedClimatePath, "annual") |>
      checkPath(create = TRUE)
    projAnnualClimateArchive <- file.path(dirname(projAnnualClimatePath),
                                          paste0(mod$studyAreaNameDir[[prov]], "_",
                                                 P(sim)$climateGCM, "_ssp",
                                                 P(sim)$climateSSP, "_annual.zip"))

    if (!file.exists(projAnnualClimateArchive)) {
      ## need to download and extract w/o prepInputs to preserve folder structure!
      googledrive::drive_download(file = as_id(projAnnualClimateUrl), path = projAnnualClimateArchive)
      archive::archive_extract(projAnnualClimateArchive, projAnnualClimatePath)
    }

    Cache(makeLandRCS_projectedCMIandATA,
          normalMAT = normals[["MATnormal"]],
          pathToFutureRasters = file.path(projAnnualClimatePath, mod$studyAreaNameDir[[prov]]),
          years = P(sim)$projectedFireYears,
          useCache = TRUE,
          userTags = c("projectedCMIandATA", cacheTags))
  }) |>
    purrr::transpose()

  sim$ATAstack <- Cache(SpaDES.tools::mergeRaster, projCMIATA[["projectedATA"]])
  sim$CMIstack <- Cache(SpaDES.tools::mergeRaster, projCMIATA[["projectedCMI"]])

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
