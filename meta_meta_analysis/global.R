##### global.R script

# App opens straight away.
# Runs default model only when user clicks button. 
# Uses Goggle Sheets for user to upload data from shiny app. 
# Loads data from Google Sheets.
# Model formula includes Paper ID and Control as random intercepts

# -------------------------------------------------------------------------------------

# Load packages
library(shiny)  # Required to run any Shiny app
library(readxl) # for reading excel files
library(robustlmm) # for running robust models
library(tidyverse) # for data manipulation
library(tibble) # to convert rownames to column
library(broom) # for getting model summary
library(bslib) # For themes to customise appearance of shiny app
library(shinycssloaders) # for loading symbols (while models run)
library(shinyjs) # for enabling and disabling download button
library(googlesheets4) # for uploading files to Google Sheets
library(maps) # for mapping
library(stringr) # for checking first_name and second_name inputs only contain letters
library(tools) # for getting file extension
library(shinydisconnect) # for displaying nice error message if whole shiny app disconnects 

# -------------------------------------------------------------------------------------

# Read in Goggle Sheets authentication token, email, and sheet ID
sheet_id <- as.character(read.table("details_for_googlesheets/sheet_id.txt"))
email <- as.character(read.table("details_for_googlesheets/email.txt"))
gs4_auth(cache = ".secrets", email = email)

# -------------------------------------------------------------------------------------

# Read in table with agricultural systems definitions

agri_systems_def <- read_excel("data_and_models/agricultural_systems_definitions.xlsx")

# -------------------------------------------------------------------------------------

# Define the 'then' function, so in the upload data tab, 
# it only returns the 1 error message at a time to the user 

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

# -------------------------------------------------------------------------------------





