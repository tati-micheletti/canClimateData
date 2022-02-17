---
title: "canClimateData"
author: "Alex Chubaty"
date: "17 February 2022"
output:
  html_document:
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Prepare Canadian historic and projected climate data for use with LandR.CS and fireSense.

Currently supports study areas in AB, BC, SK, MB, ON, QR, NT, and YT, for the following climate scenarios:

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> studyArea </th>
   <th style="text-align:left;"> GCM </th>
   <th style="text-align:right;"> SSP </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> AB </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AB </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AB </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AB </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AB </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AB </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AB </td>
   <td style="text-align:left;"> CCSM4 </td>
   <td style="text-align:right;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AB </td>
   <td style="text-align:left;"> CCSM4 </td>
   <td style="text-align:right;"> 85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AB </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AB </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AB </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> BC </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> 13GCMs_ensemble </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> BC </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> 13GCMs_ensemble </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> BC </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> 13GCMs_ensemble </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> BC </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CanESM5 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> BC </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CanESM5 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> BC </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CanESM5 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> BC </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CCSM4 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> BC </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CCSM4 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 85 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> BC </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> BC </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> BC </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MB </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MB </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MB </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MB </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MB </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MB </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MB </td>
   <td style="text-align:left;"> CCSM4 </td>
   <td style="text-align:right;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MB </td>
   <td style="text-align:left;"> CCSM4 </td>
   <td style="text-align:right;"> 85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MB </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MB </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MB </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> NT </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> 13GCMs_ensemble </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> NT </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> 13GCMs_ensemble </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> NT </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> 13GCMs_ensemble </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> NT </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CanESM5 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> NT </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CanESM5 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> NT </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CanESM5 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> NT </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CCSM4 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> NT </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CCSM4 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 85 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> NT </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> NT </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> NT </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ON </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ON </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ON </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ON </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ON </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ON </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ON </td>
   <td style="text-align:left;"> CCSM4 </td>
   <td style="text-align:right;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ON </td>
   <td style="text-align:left;"> CCSM4 </td>
   <td style="text-align:right;"> 85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ON </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ON </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ON </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> QC </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CanESM5 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIA </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIA </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIA </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIA </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIA </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIA </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIA </td>
   <td style="text-align:left;"> CCSM4 </td>
   <td style="text-align:right;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIA </td>
   <td style="text-align:left;"> CCSM4 </td>
   <td style="text-align:right;"> 85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIA </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIA </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIA </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> SK </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> 13GCMs_ensemble </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> SK </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> 13GCMs_ensemble </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> SK </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> 13GCMs_ensemble </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> SK </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CanESM5 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> SK </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CanESM5 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> SK </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CanESM5 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> SK </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CCSM4 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> SK </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CCSM4 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 85 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> SK </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> SK </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> SK </td>
   <td style="text-align:left;background-color: #f0f0f0 !important;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;background-color: #f0f0f0 !important;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YT </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YT </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YT </td>
   <td style="text-align:left;"> 13GCMs_ensemble </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YT </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YT </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YT </td>
   <td style="text-align:left;"> CanESM5 </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YT </td>
   <td style="text-align:left;"> CCSM4 </td>
   <td style="text-align:right;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YT </td>
   <td style="text-align:left;"> CCSM4 </td>
   <td style="text-align:right;"> 85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YT </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 245 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YT </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YT </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:right;"> 585 </td>
  </tr>
</tbody>
</table>

# Parameters

Provide a summary of user-visible parameters.

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> .plotInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first plot event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time interval between plot events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first save event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between save events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bufferDist </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 20000 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Distance (m) to buffer studyArea and rasterToMatch when creating 'Large' versions. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateGCM </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> CNRM-ESM2-1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Global Circulation Model to use for climate projections: currently '13GCMs_ensemble', 'CanESM5', 'CNRM-ESM2-1', or 'CCSM4'. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateSSP </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 370 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> SSP emissions scenario for `climateGCM`: one of 245, 370, or 585.[If using 'climateGCM = CCSM4', climateSSP must be one of 45 or 85.] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> historicalFireYears </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1991, 19.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> range of years captured by the historical climate data </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedFireYears </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 2011, 20.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> range of years captured by the projected climate data </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> RIA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> One of 'AB', 'BC', 'MB', 'NT', 'ON', 'QC', 'SK', 'YT', or 'RIA'. </td>
  </tr>
</tbody>
</table>

# Events

This module consists of a single `init` event that performs all the data preparation.

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData("canClimateData", "..")` may be sufficient.

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> template raster </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatchLarge </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> template raster for larger area </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatchReporting </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> template raster for reporting area </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyArea </td>
   <td style="text-align:left;"> SpatialPolygonsDataFrame </td>
   <td style="text-align:left;"> study area used for simulation (buffered to mitigate edge effects) </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaLarge </td>
   <td style="text-align:left;"> SpatialPolygonsDataFrame </td>
   <td style="text-align:left;"> study area used for module parameterization (buffered) </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaReporting </td>
   <td style="text-align:left;"> SpatialPolygonsDataFrame </td>
   <td style="text-align:left;"> study area used for reporting/post-processing </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

## Output data

Description of the module outputs.

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ATAstack </td>
   <td style="text-align:left;"> RasterStack </td>
   <td style="text-align:left;"> annual projected mean annual temperature anomalies, units stored as tenth of a degree </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CMIstack </td>
   <td style="text-align:left;"> RasterStack </td>
   <td style="text-align:left;"> annual projected mean climate moisture deficit </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CMInormal </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Climate Moisture Index Normals from 1950-2010 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> historicalClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> list of a single raster stack - historical MDC calculated from ClimateNA data </td>
  </tr>
  <tr>
   <td style="text-align:left;"> projectedClimateRasters </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> list of a single raster stack - projected MDC calculated from ClimateNA data </td>
  </tr>
</tbody>
</table>

# Links to other modules

Originally developed to provide inputs to `gmcsDataPrep` (for use with climate-sensitive LandR Biomass projections; see `Biomass_core`) and the `fireSense` suite of modules.
