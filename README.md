# AD Trial Feasibility Dashboard

A clinical trial feasibility dashboard for Alzheimer's Disease, built in R Shiny.

## Live App

👉 https://jrkampel.shinyapps.io/ad-trial-feasibility/

## What it does

This dashboard supports site and country selection decisions for Alzheimer's Disease 
clinical trials by combining real-time trial activity data with patient prevalence estimates.

- **Overview**: World map of recruiting AD trials by country
- **Trial Landscape**: Trial count by country and phase breakdown
- **Site Scoring**: Interactive country ranking with adjustable weighting sliders
- **Data**: 500 recruiting trials pulled live from the ClinicalTrials.gov API v2

## Scoring methodology

Each country is scored on two dimensions:

- **Prevalence score**: size of the patient pool, normalised to 0-100
- **Competition score**: inverse of trial count, normalised to 0-100 (fewer trials = less competition)
- **Composite score**: user-weighted combination of the two, adjustable via sliders

## Data sources

- Trial data: [ClinicalTrials.gov API v2](https://clinicaltrials.gov/data-api/api)
- Prevalence estimates: Global Burden of Disease 2019

## Built with

- R Shiny
- shinydashboard
- plotly
- ClinicalTrials.gov API

## Author

Julia Kampel — MSc Cancer Genetics and Data Science, Queen Mary University of London, 2026
