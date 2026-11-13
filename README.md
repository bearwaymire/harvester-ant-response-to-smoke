# Harvester Ant Response to Smoke

## Overview

This repository contains data, analysis scripts, and a video-processing guide for a study examining how smoke exposure affects foraging behavior and mound activity in harvester ants.

## Statistical_Analysis

The scripts for statistical analysis are separated into:

- `Foraging_Analysis.R`: Analyzes how foraging trails are affected 30 seconds and 5 minutes after treatment with smoke or air.
- `Mound_Analysis.R`: Analyzes how mound activity is affected 30 seconds and 5 minutes after treatment with smoke or air.

## Data

The data are separated into:

- `foraging_data.RDS`: Foraging data to be used in `Foraging_Analysis.R`.
- `mound_data.RDS`: Mound activity data to be used in `Mound_Analysis.R`.

## Video_Analysis

- `Batch_TrackMate_Analysis.groovy`: Applies saved TrackMate detection parameters to multiple sampling windows and reports the total number of detected spots.
- `Fiji_ImageJ_TrackMate_Guide.pdf`: Provides step-by-step instructions for using Fiji and TrackMate to quantify mound activity from videos.
- `Initial_TrackMate_Analysis.ijm`: Prepares image sequences, creates or loads masks, and opens TrackMate for initial spot detection.
