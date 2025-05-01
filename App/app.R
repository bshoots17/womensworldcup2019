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

options(ggimage.usemagick = TRUE)

source("aesthetics.R")
source("about_page.R")

# LOAD IN DATA

labels <- c("Group Stage Match 1",
                       "Group Stage Match 2", 
                       "Group Stage Match 3",
                       "Round of 16",
                       "Quarterfinal", 
                       "Semifinal",
                       "Final")

shots <- read.csv("data/shots.csv")

top_scorers <- read.csv("data/top_scorers.csv")

uswnt_games <- read.csv("data/uswnt_games.csv")

viewership <- read_sf("data/viewership.geo.json")

wwcevents <- read.csv("data/wwcevents.csv")

prizes <- read.csv("data/prizes.csv") %>% rename("prize_money" = "Prize.Money..millions.USD.")

player_opts <- unique(wwcevents$player.name)

############### UI ####################

ui <- page_navbar(
  title = app_title,
  theme = bs_theme(version = 5, bootswatch = "journal"),
  header = tagList(red_card, image_settings,
  tags$head(tags$link(rel = "stylesheet", 
                      type = "text/css", 
                      href = "custom.css"))),
  about_panel,
  nav_panel(title = "Beyond the Numbers", 
            fluidPage(
              img(
                src = "wwc2.jpeg", 
                class = "full-width-image"),
              card(
                h1("Measuring the USWNT's Performance"),  
                class = "red-card"),
              card(
                h1("A Historic Fourth Star"), 
                p_4th_star),
              layout_columns(
                card(
                  card_header("How accurate were the USWNT's shots?"), 
                  card_body(selectInput("game", 
                                        label = "Select a Game", 
                                        choices = labels, 
                                        selected = "Group Stage Match 1"),
                            plotOutput("shots"))),
                p_shots, 
                col_widths = c(7, 5)),
              card(
                card_body(
                  plotOutput("scorers")
                  )),
              layout_columns(
                card(
                  card_header("Understand Player Impact"), 
                  selectInput("player", 
                              label = "Select a Player", 
                              choices = player_opts, 
                              selected = "Megan Anna Rapinoe"), 
                  selectInput("heatmap", 
                              label = "Select an Action", 
                              choices = c("All", "Pass", "Shot", "Dribble", "Carry"), 
                              selected = "All"),
                  plotOutput("heat")
                  )),
              card(
                h1("Visualizing the Tournament's Growth"), 
                class = "red-card"),
              layout_columns(
                card(
                  plotOutput("viewmap")
                  ),
                p_map),
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

############### SERVER ####################
server <- function(input, output) {
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

  output$viewmap <- renderPlot({

    ggplot() +
      geom_sf(data = viewership, aes(fill = view_change/100)) +
      scale_fill_viridis_c(option = "plasma", direction = -1, label = scales::percent_format()) +
      labs(fill = "Percent Change", title = "Percent Change in Women's World Cup TV Ratings from 2015 to 2019", subtitle = "While viewership dropped in North America in 2019, it's clear that women's soccer is gaining traction globally.") +
      maptheme

  })

  output$plotly1 <- renderPlotly({
    p <- ggplot(prizes, aes(x = Tournament, y = prize_money, text = Position, text2 = Amount)) +
      geom_col(aes(fill = Position)) +
      ggthemes::scale_fill_tableau() +
      labs(title = "The World Cup Gender Prize Money Gap", y = "Total Prize Money Per Result")

    ggplotly(p, tooltip = c("text", "text2"))
  })

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
      scale_alpha_manual(values = c(0, rep(1, 9))) +  # 0 level transparent, others semi-transparent
      guides(alpha = "none") +
      theme(legend.position = "none") +
      ggsoccer::theme_pitch() +
      scale_fill_viridis_d(option = "plasma") +
      labs(title = title)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
