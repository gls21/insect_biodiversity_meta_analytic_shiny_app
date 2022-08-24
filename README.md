# Insect biodiversity meta-analytic Shiny app
## This Shiny app allows users to investigate insect biodiversity change using a meta-meta-analytic approach
## This app was built as part of my Computational Methods in Ecology and Evolution master's project at Imperial College London with the Natural History Museum

#### Running the Shiny app
Use the following web address the access the Shiny app:
https://r26dnk-grace-skinner.shinyapps.io/meta_meta_analysis/ 

#### Description
The motivation behind making the Shiny app comes from a desire to make the best use of the data we have available now and in the future to untangle the complex trends and drivers of insect biodiversity change. 
The app performs what we call 'meta-meta-analysis' defined as analysing multiple meta-analysis studies together by combining effect sizes collected for each of these.
It allows users to run custom robust linear mixed-effects models to investigate their specific interests.
When new meta-analyses are conducted, the data can be uploaded and included in future models. 

#### Main features of the app
* Introductory tab to give overview of where the data comes from and its geographic representativeness.
* Agricultural systems tab where users can either run a defualt model (on all of the data) or a custom model based on choices made by the user.
    * The app runs robust models on the effect of agricultural system on biodiveristy in terms of log response ratio. 
    * In future, we aim to incorporate new variables over which the user has choice, including a choice of which threat to investigate (e.g. land-use change, temperature), and filters for location and taxonomic groups.
    * The app produces a figure and table of the results.
    * The user is able to download their results.
* Uploading data tab where new meta-analysis data can be uploaded to Google Sheets (where the data is stored). This data can then be included in future models. 
* Reference tab for more details enabling the user to find the original papers used in the study.   

#### Directories/Scripts
* data_and_models - contains the agricultural systems definition table
* global.R - run once when app is launched. Packages, variables, and functions loaded/created here are available to both server.R and ui.R
* server.R - defines how the Shiny app works (back-end development)
* rsconnect - contains file with information on the deployed app
* ui.R - defines the way the Shiny app looks (front-end development)

#### Languages used to build the Shiny app
* R version 4.2.0

#### R packages used by the Shiny app 
* broom - for getting model summary
* bslib - for themes to customise appearance of shiny app
* googlesheets4 - for uploading files to googlesheets in Shiny app
* maps - for mapping
* readxl - for reading excel files
* robustlmm - for running robust models
* shiny version 1.7.1 - required to run any Shiny app
* shinycssloaders - for loading symbols (while models run) in Shiny app
* shinydisconnect - for displaying nice error message if whole Shiny app disconnects
* shinyjs - for enabling and disabling download button in Shiny app
* stringr - for checking first_name and second_name inputs only contain letters in Shiny app
* tibble - used to convert rownames to column
* tidyverse - for data manipulation
* tools - for getting file extension

#### Author name and contact
* Grace Skinner
* gls21@ic.ac.uk
