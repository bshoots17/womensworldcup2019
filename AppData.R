# ====================================================
# 0. SETUP
# ====================================================

## 0.1 Load Packages
library(StatsBombR)
library(tidyverse)

# ====================================================
# 1. CREATE CSV DATAFRAMES FOR SHINY APP
# ====================================================

## 1.1 Load WWC StatsBomb Data
Comps <- FreeCompetitions()

wwc = Comps %>%
  filter(competition_id == 72, season_id == 30)

wwcmatches <- FreeMatches(wwc)

wwcevents <- cleanlocations(free_allevents(wwcmatches))

### 1.2 Creating Top Scorers DataFrame for Bar Graph ###
top_scorers <- wwcevents %>% mutate(is_goal = replace_na(ifelse(shot.outcome.name == "Goal", TRUE, FALSE), FALSE)) %>% group_by(player.name) %>% summarize(goals_scored = sum(is_goal)) %>% arrange(desc(goals_scored)) %>% head(10)

# cleaning up some player names so they match the names people know them by
top_scorers$player.name <- gsub("Alexandra Morgan Carrasco", "Alex Morgan", top_scorers$player.name)

top_scorers$player.name <- gsub("Megan Anna Rapinoe", "Megan Rapinoe", top_scorers$player.name)

# adding image file names
top_scorers$country <- c("www/uswnt.png", "www/eng.png", "www/uswnt.png", "www/aus.png",
                         "www/bra.png", "www/fra.png", "www/ita.png", "www/uswnt.png",
                         "www/ita.png", "www/spa.png")

# getting their last names to avoid cluttering the plot
top_scorers <- top_scorers %>% mutate(last_name = word(player.name, -1))

top_scorers$last_name <- gsub("Fuentes", "Hermoso", top_scorers$last_name)


top_scorers$last_name <- gsub("Hollins", "Lloyd", top_scorers$last_name)

top_scorers$last_name <- gsub("Silva", "Cristiane", top_scorers$last_name)

# write.csv(top_scorers, "top_scorers.csv")

## 1.3 Creating USWNT Shots DataFrame

shots <- wwcevents %>% filter(type.name == "Shot", team.name == "United States Women's") %>% select(shot.outcome.name, match_id, team.name, location.x, location.y, shot.type.name) %>% rename("shot_outcome" = "shot.outcome.name")

shots$shot_outcome <- factor(shots$shot_outcome, 
                             levels = c("Goal", "Blocked", "Off T", "Post", 
                                        "Saved Off Target", "Saved", "Saved to Post", "Wayward"),
                             labels = c("Goal", "Blocked", "Off Target", "Off The Post", 
                                        "Saved Off Target", "Saved", "Saved to Post", "Wayward"))

# write.csv(shots, "shots.csv")

## 1.4 Creating USWNT Matches Key

uswnt_games <- wwcmatches %>% filter(home_team.home_team_id == 1214 | away_team.away_team_id == 1214) %>% select(-away_team.managers, -home_team.managers)

labels <- c("Group Stage Match 1",
            "Group Stage Match 2", 
            "Group Stage Match 3",
            "Round of 16",
            "Quarterfinal", 
            "Semifinal",
            "Final")

uswnt_games$label <- labels

# write.csv(uswnt_games, "uswnt_games.csv")


## 1.5 Creating Viewership Shapefile 

df <- data.frame(
  region = c("Africa & Middle East", "Asia", "Europe", 
             "N.C. America & Caribbean", "Oceania", 
             "South America", "Global Total"),
  view2019 = c(80.62, 486.25, 1089.31, 416.49, 7.71, 413.39, 2493.78),
  view2015 = c(31.81, 442.63, 308.53, 436.58, 7.57, 66.66, 1293.78)
)

df2 <- df %>% dplyr::mutate(view_change = ((view2019 - view2015) / view2015) * 100)

shape <- st_read("world")

subregions <- c("Micronesia", "Eastern Asia", "Western Europe", "Southern Europe", "South America",
                "Central America", "Caribbean", "Northern Africa", "Western Africa", "Northern Europe",
                "Central Asia", "Middle Africa", "Southern Asia", "Polynesia", "Western Asia",
                "Eastern Europe", "Eastern Africa", "Australia and New Zealand", "South-Eastern Asia",
                "Melanesia", "Northern America","Southern Africa")

get_region <- function(subregion) {
  if (is.na(subregion)) {
    return("No")
  } else if (subregion %in% c("Northern Africa", "Western Africa", "Middle Africa", "Eastern Africa", "Southern Africa",
                              "Western Asia")) {
    return("Africa & Middle East")
  } else if (subregion %in% c("Eastern Asia", "Southern Asia", "Central Asia", "South-Eastern Asia")) {
    return("Asia")
  } else if (subregion %in% c("Western Europe", "Southern Europe", "Northern Europe", "Eastern Europe")) {
    return("Europe")
  } else if (subregion %in% c("Central America", "Caribbean", "Northern America")) {
    return("N.C. America & Caribbean")
  } else if (subregion %in% c("Micronesia", "Polynesia", "Melanesia", "Australia and New Zealand")) {
    return("Oceania")
  } else if (subregion == "South America") {
    return("South America")
  } else {
    return("Unknown")
  }}


region_group <- sapply(subregions, get_region)

grouped_df <- data.frame(Subregion = subregions, Region = region_group)

shape3 <- shape %>% dplyr::inner_join(grouped_df, by = c("region" = "Subregion"))

# combining regions into singular shapes so that we need to store less data
shape_dissolved <- shape3 %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise()

shape_final <- shape_dissolved %>% inner_join(df2, by = c("Region" = "region"))

sf::st_write(shape_final, dsn = "viewership.geojson", layer = "wwcviewership.geojson")

## 1.6 Creating WWC Events with only certain columns to minimize amount of data
wwcevents_selected <- wwcevents %>% select(location.x, location.y, team.name, type.name, player.name) %>% filter(!is.na(location.x))

# write.csv(wwcevents_selected, "wwcevents.csv")

## 1.7 Creating Prize Money DataFrame

library(RSQLite)

# con <- RSQLite::dbConnect(RSQLite::SQLite(), "Final/uswnt_wwc_2019.db")
# prizes <- dbGetQuery(con, statement = "SELECT * FROM prizes")

# write.csv(prizes, "prizes.csv")
