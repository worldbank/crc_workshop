# Climate-Resilient Cities Workshop Shiny App

This repo contains the code for a Shiny app which is designed to accompany the Climate-Resilient Cities Workshops by the City Resilience Program (CRP) at the World Bank.
The app features interactive spatial visualizations of data related to cities' hazard exposures and geographical contexts.
The data inputs, which are not included in this repo, consist primarily of vector and raster outputs of a typical City Scan, though the Shiny app is mutually independent of the City Scan.

## Contents

The code is structured as follows:  
`app.R` is the main body of the Shiny app. It contains the UI and server functions, as well as style parameters for the app.  
`cities.R` reads the data inputs and sets color palettes for each spatial layer.  
`global_params.R` defines global functions and variables.  
`text.R` contains the major texts in the Shiny app.

## Using the Repo and Setting Up the File Structure

This repo should be cloned directly into the directory from which the Shiny app is to be run.
The `app.R` and `cities.R` files should be modified based on the cities to be included in the Shiny app.
The code here includes two cities: Sarajevo and Tuzla.
When adapting the code, look for lines that contain the city names or city-specific variables and change them to the new city names or corresponding variables.
If the vector and raster files having different naming structures than the ones referenced in the code, update those as well.
Additionally, `text.R` should be adjusted to suit the new Shiny app.
`global_params.R` should still function without any additional changes.  

Apart from the R scripts, the directory should also include:

- An R project file (`.Rproj`)
- For each city to be included in the Shiny app, make an eponymous subdirectory without changing any capitalization, spaces, or special characters
(e.g., the subdirectory for the city Ramallah Al-Bireh should be named `Ramallah Al-Bireh`).
Each of the subdirectory should contain the vector and raster data files for that city.
- A `www` subdirectory containing the following:
    - The World Bank logo file
    - The GFDRR logo file
    - The CRP logo file
    - Icons for fire stations, health facilities, police stations, and schools, if these are part of the layers shown in the Shiny app

## Contact

For inquiries, email rsu@worldbank.org.
