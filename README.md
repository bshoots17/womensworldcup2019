# Women's World Cup 2019 Through Data

## Description
Repository for my DSCI 304: Data Visualization final project about the 2019 Women's World Cup. 

The most current version of the website can be found here: https://biancaschutz.shinyapps.io/WWC2019/

## Contents

- **AppData.R**: this R file contains replicable code to create the .csv and .json files found in the App/data folder
- **world/**: shapefile used to create the viewership map, was simplified in AppData.R into viewership.geo.json
- **App/**: container used to deploy the Shiny Application. Includes the following subdirectories and files:
  - **www/***: images and custom css styling
  - **data/**: the data files used for the visualizations
  - **app.R**: the UI and server components of the Shiny App
  - **aesthetics.R**: the aesthetic components of the app, such as any constants, ggplot customization, and paragraph text
  - **about_page.R**: the nav_panel used to create the "About" page
