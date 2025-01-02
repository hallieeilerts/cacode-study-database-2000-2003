# Study database

test23

## Objective
To generate age-specific study databases for use as training data for CA-CODE (Child and Adolescent Causes of Death Estimation) model for 2000-2023.

## Folder structure

1. Raw data is stored in data
2. Codes are written in source (src) folder
3. Generated files are in the gen folder
4. The "results" folder is manually created and maintained for storing/sharing the final database with others, and ensuring that results do not get overwritten.

Example:
```
StudyDatabase/
├─ data/                    # Local copy of input files (not checked in)
│  ├─ ad-hoc                # One folder per data input group
│  │  ├─ mds.csv            # Original input file provided by the source
│  ├─ distillr              # 
│     ├─ ...                #
├─ src/                     # Source code
│  ├─ process-new-studies   # One folder for each stage of pipeline
│  │  ├─ ...                # Code files for that pipeline stage
│  │  ├─ ...
│  ├─ update-old-studies
│  ├─ util.R                # One file with shared helper functions
│  ├─ ... 
├─ gen/                     # Generated files
│  ├─ process-new-studies   # One folder per stage of piepline
│  │  ├─ output             # Outputs from that pipeline stage
│  │  │  ├─ data.csv        # Dataset with the indicator values
│  │  │  ├─ metadata.json   # Metadata for the indicator
│  │  ├─ audit              # Intermediate outputs used for checking data quality
│  │  │  ├─ plots.pdf       # For example, plots
│  │  │  ├─ comparison.csv  # For example, spreadsheets comparing values to previous database
│  ├─ update-old-studies   
│  ├─ ...                   
```

## Instructions

1. Open src/set-inputs.R
2. Un-comment a single age/sex group
3. Set local path to the CA CODE Data Warehouse
4. Walk through src/make.R
   - Some scripts involve manual inspections of data

## Format of final output

A csv file with one row per study data point age/sex/VA method strata. Columns for id variables, CODs, covariates.

