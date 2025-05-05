# ============================================== #
# File: app.R
# Description: Shiny web application built with R.
#              This file contains both the UI and server logic.
#
# Author: Bianca Schutz
# Date Created: April 24, 2025
# Last Modified: May 4, 2025

# ============================================== #
# ---- Set-Up ----
# ============================================== #

## ---- Packages ----
library(shiny)
library(shinythemes)
library(ggplot2)
library(bslib)
library(sf)
library(plotly)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggimage)
library(viridis)
library(magick)
library(grid)
library(leaflet)
library(leaflet.providers)

## ---- Set Options ----
options(ggimage.usemagick = TRUE)

## ---- Local Sources ----
source("aesthetics.R")
source("about_page.R")

## ---- Constants ----
labels <- c("Group Stage Match 1",
                       "Group Stage Match 2", 
                       "Group Stage Match 3",
                       "Round of 16",
                       "Quarterfinal", 
                       "Semifinal",
                       "Final")

icons <- iconList(
  hainaut = makeIcon(
    "hainaut.png",
    iconWidth = 50
  ),
  delaune = makeIcon(
    "delaune.png",
    iconWidth = 50
  ),
  roazhon = makeIcon(
    "roazhon.png",
    iconWidth = 50
  ),
  allianz = makeIcon(
    "allianz.png",
    iconWidth = 50
  ),
  alps = makeIcon(
    "alps.png",
    iconWidth = 50
  ),
  princes = makeIcon(
    "princes.png",
    iconWidth = 50
  ),
  oceane = makeIcon(
    "oceane.png",
    iconWidth = 50
  ),
  mosson = makeIcon(
    "mosson.png",
    iconWidth = 50
  ),
  groupama = makeIcon(
    "groupama.png",
    iconWidth = 50
  )
)

## ---- Data -----
shots <- read.csv("data/shots.csv")

top_scorers <- read.csv("data/top_scorers.csv")

uswnt_games <- read.csv("data/uswnt_games.csv")

viewership <- read_sf("data/viewership.geo.json")

wwcevents <- read.csv("data/wwcevents.csv")

prizes <- read.csv("data/prizes.csv") %>% rename("prize_money" = "Prize.Money..millions.USD.")

stadiums <- read.csv("data/stadiums.csv")

player_opts <- unique(wwcevents$player.name)

# ============================================== #
# ---- UI ----
# ============================================== #

ui <- page_navbar(
  
  ## ---- Title and Aesthetics ----
  title = app_title,
  theme = bs_theme(version = 5, bootswatch = "journal"),
  header = tagList(red_card, image_settings,
  tags$head(tags$link(rel = "stylesheet", 
                      type = "text/css", 
                      href = "custom.css"))),
  
  # ---- About Panel ----
  # see the about_page.R file
  about_panel,
  
  # ---- USWNT: By the Numbers ----
  nav_panel(title = "USWNT: By the Numbers", 
            fluidPage(
              img(
                src = "wwc2.jpeg", 
                class = "full-width-image"),
              
  ## ---- USWNT Performance Card ----
              card(
                h1("Measuring the USWNT's Performance"),  
                class = "red-card"),
              card(
                h1("A Historic Fourth Star"), 
                p_4th_star),
  
  ## ---- Shot Map ----
  card(h1(
    "How accurate were the USWNT's shots?"),
    class = "red-card"),
              layout_columns(
                card(
                  card_body(selectInput("game", 
                                        label = "Select a Game", 
                                        choices = labels, 
                                        selected = "Group Stage Match 1"),
                            plotOutput("shots"))),
                p_shots, 
                col_widths = c(7, 5)),
  
  ## ---- Top Scorers Bar Chart ----
              card(
                card_body(
                  plotOutput("scorers")
                  )),
  
  ## ---- Heat Map ----
  card(h1(
    "Understand Player Impact"),
    class = "red-card"),
              layout_columns(
                  card(selectInput("player", 
                              label = "Select a Player:", 
                              choices = player_opts, 
                              selected = "Megan Anna Rapinoe"), 
                  selectInput("heatmap", 
                              label = "Select an Action:", 
                              choices = c("All", "Pass", "Shot", "Dribble", "Carry"), 
                              selected = "All")),
                  card(plotOutput("heat")),
                  col_widths = c(3, 9)
                  ))),
  # ---- About The Tournament ----
  nav_panel(title = "The Tournament", 
            fluidPage(
                HTML('
    <iframe src="https://flo.uri.sh/visualisation/23001060/embed" 
            title="Interactive or visual content" 
            class="flourish-embed-iframe" 
            frameborder="0" 
            scrolling="no" 
            style="width:100%;height:600px;" 
            sandbox="allow-same-origin allow-forms allow-scripts allow-downloads allow-popups allow-popups-to-escape-sandbox allow-top-navigation-by-user-activation">
    </iframe>
    <div style="width:100%!;margin-top:4px!important;text-align:right!important;">
      <a class="flourish-credit" href="https://public.flourish.studio/visualisation/23001060/?utm_source=embed&utm_campaign=visualisation/23001060" target="_top" style="text-decoration:none!important">
        <img alt="Made with Flourish" src="https://public.flourish.studio/resources/made_with_flourish.svg" style="width:105px!important;height:16px!important;border:none!important;margin:0!important;">
      </a>
    </div>
  '),
              card(
              h1("The Hosts"), 
              class = "red-card"),
              card(
                leafletOutput("leaf"),
                full_screen = TRUE
              ),
  ## ---- Viewership Map ----
            card(
              h1("Visualizing the Tournament's Growth"), 
              class = "red-card"),
            layout_columns(
              card(
                plotOutput("viewmap")
              ),
              p_map),
            
            ## ---- Plotly Prize Money ----
            card(
              h1("Understanding The Need For Change"), 
              class = "red-card"), 
            layout_columns(
              card(
                plotlyOutput("plotly1")),
              card(
                p_prize),
              col_widths = c(8, 4))))
)

# ============================================== #
# ---- SERVER ----
# ============================================== #

server <- function(input, output) {
  
  ## ---- Shot Map ----
  output$shots <- renderPlot({
    req(input$game)
    match_info <- uswnt_games %>% dplyr::filter(label == input$game)
    id <- match_info %>% dplyr::pull(match_id)
    opponent <- ifelse(match_info$home_team.country.name == "United States of America", match_info$away_team.country.name, match_info$home_team.country.name)
    
    shot_data <- shots %>% dplyr::filter(match_id == id)
    
    geom_halfpitch + 
      geom_point(data = shot_data, aes(color = shot_outcome, x = location.x, y = location.y), size = 4) +
      scale_color_manual(
        values = c(
          "Goal" = "green3", 
          setNames(viridis::plasma(length(unique(shot_data$shot_outcome[shot_data$shot_outcome != "Goal"])), direction = -1),
                   unique(shot_data$shot_outcome[shot_data$shot_outcome != "Goal"]))
        )) +
      labs(color = "Shot Outcome", title = "USWNT Shot Map", subtitle = paste("vs", opponent, "in 2019 Women's World Cup", input$game)) +
      scale_shape_manual(values = c()) +
      pitch_theme
  })
  
  ## ---- Top Scorers Bar Chart ----
  
  output$scorers <- renderPlot({
    ggplot(top_scorers) +
      geom_col(aes(fill = file, 
                   x = reorder(last_name, -goals_scored), 
                   y = goals_scored)) +
      ggimage::geom_image(aes(x = last_name, image = file, y = goals_scored - .75), hjust = 1, size = .1) +
      scale_fill_manual(values = country_colors) +
      ggthemes::theme_fivethirtyeight() +
      labs(title = "Two of the Tournament's Top Goalscorers Are American",
           y = "Total Goals Scored") +
      theme(axis.title.y = element_text(),
            legend.position = "none")
  })

  ## ---- Viewership Map ----
  
  output$viewmap <- renderPlot({

    ggplot() +
      geom_sf(data = viewership, aes(fill = view_change/100)) +
      scale_fill_viridis_c(option = "plasma", direction = -1, label = scales::percent_format()) +
      labs(fill = "Percent Change", title = "Percent Change in Women's World Cup TV Ratings from 2015 to 2019", subtitle = "While viewership dropped in North America in 2019, it's clear that women's soccer is gaining traction globally.") +
      maptheme

  })

## ---- Plotly Prize Money ----
  
  output$plotly1 <- renderPlotly({
    p <- ggplot(prizes, aes(x = Tournament, y = prize_money, text = Position, text2 = Amount)) +
      geom_col(aes(fill = Position)) +
      ggthemes::scale_fill_tableau() +
      labs(title = "The World Cup Gender Prize Money Gap", y = "Total Prize Money Per Result")

    ggplotly(p, tooltip = c("text", "text2"))
  })

## ---- Heat Map ----
  output$heat <- renderPlot({
  req(input$heatmap)
  req(input$player)

  if (input$heatmap == "All") {
    player_data <- wwcevents %>% filter(player.name == input$player)

    title <- paste0(input$player, "'s Impact on ", player_data$team.name[1], " Games, All Actions")

  } else {
    player_data <- wwcevents %>%
      filter(type.name == input$heatmap,
             player.name == input$player)

    title <- paste0(input$player, "'s Impact on ", player_data$team.name[1], "Games, ", input$heatmap)

  }
    ggplot(player_data, aes(x = location.x, y = location.y)) +
      ggsoccer::annotate_pitch(dimensions = ggsoccer::pitch_statsbomb) +
      geom_density_2d_filled(h = c(7, 7), n = 300, bins = 10,
                             aes(alpha = after_stat(level))) +
      scale_alpha_manual(values = c(0, rep(1, 9))) +  
      guides(alpha = "none") +
      theme(legend.position = "none") +
      ggsoccer::theme_pitch() +
      scale_fill_viridis_d(option = "plasma") +
      labs(title = title)
  })

## ---- Leaflet map ----
  output$leaf <- renderLeaflet({
    leaflet(stadiums) %>%
      setView(lng = 2.2137, lat = 47.5, zoom = 5) %>%  
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(
        ~Longitude, ~Latitude,
        icon = ~icons[icon_key],
        label = ~Stadium
      )
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
