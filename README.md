# PopHIVE Insights

These projects contain analyses that highlight uses of the data found in PopHIVE

## Instructions for contributors

### Getting started

-   Ensure you have Git set up on your computer. Clone this repository to your computer, using either Github Desktop or Git within Rstudio

-   If you are working on a new set of analyses, create a new project within this repository. Make use of good practice with Git including branching and frequent commits

-   Identify the dataset(s) you want to use from the PopHIVE Ingest repository. All of the available datasets can be viewed [here](https://github.com/PopHIVE/Ingest/blob/main/status.md). You are generally going to want to access datasets either from the /standard folder within the individual dataset folder or from the /dist older in the bundle\_\* repositories

-   All of the datasets from PopHIVE/ingest are stored as either parquet files or compressed csv files. To access the file directly from Github. Navigate to the page where the data are stored. For example [here](https://github.com/PopHIVE/Ingest/blob/main/data/delphi_doctors_claims/standard/data.csv.gz). Right click either the'Raw' button or the 'View raw' text and select 'Copy link address'. Once you have that, you can read the data into R.

    compressed csv files can be read in with the vroom::vroom function:

    `r ds1 <-vroom::vroom('https://github.com/PopHIVE/Ingest/raw/refs/heads/main/data/delphi_doctors_claims/standard/data.csv.gz')`

Parquet files can be read using the arrow package:

`r ds2 <- read_parquet('https://github.com/PopHIVE/Ingest/raw/refs/heads/main/data/bundle_injury_overdose/dist/deaths_cause_age.parquet')`

-   Now you are ready to start exploring the data. You could a Quarto document to organize your code.

-   Remember to commit your code changes frequently and push changes back to Github. This is very important so that other on the team can see your work too!
