
# README for Shiny App Project

This project contains an interactive application displaying information on the Estimated incidence of Tuberculosis in various countries in 2019. 

The app is available at: https://menziesprf.shinyapps.io/weekend/

### App contents

#### Tab 1 

The homepage contains a histogram displayin estimated incidence of TB for the 20 countries with the highest incidence in 2019. Using the drop down selecter the user can select which of these 20 countries to include.

There is a tab/button to hover over that displays information on the data used.

There is also a link to the WHO webpage from where the data was sourced.

#### Tab 2 

The 2nd tab allows the user to filter the reported age group and sex.

#### Tab 3 

The 3rd tab allows the user to filter for the incidence in populations with an associated risk factor. Of note in this tab is that South Africa reports the highest incidence of individuals suffering from TB and HIV. 

### Repository Contents

* Raw data folder

* "prep_script.R" - Basic data cleaning/preparation script

* "app.R" - script UI and server of the shiny app

* "rsconnect" - documentation required for the shiny app

