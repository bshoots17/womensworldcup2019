### AESTHETICS ###

# Contains all paragraph text, images, formatting, plotting reqs, etc.
library(shiny)
library(ggplot2)
library(ggthemes)
library(bslib)

red_card <- tags$head(tags$style(HTML("
  .red-card {
      background-color: #BB2533 !important;
      color: white;
      padding: 20px;
      width: 100vw;
      margin-left: calc(-50vw + 50%);
      box-sizing: border-box;}")))

image_settings <- tags$head(
  tags$style(HTML("
      .full-width-image {
        width: 100vw;
        margin-left: calc(-50vw + 50%);
        position: relative;
        left: 0;
        margin-bottom: 30px;
      }
    "))
)

geom_halfpitch <- ggplot() + annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", linewidth = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", linewidth = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", linewidth = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "black") +
  annotate("path", colour = "black", linewidth = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  # add centre spot
  annotate("point", x = 60 , y = 40, colour = "black") +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), linewidth = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), linewidth = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  coord_flip(xlim = c(85, 121))

pitch_theme <- ggthemes::theme_fivethirtyeight() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    aspect.ratio = 65 / 100,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    legend.margin = margin(t = 5, r = 0, b = 0, l = 0),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

maptheme <- ggthemes::theme_fivethirtyeight() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5),
    legend.key.width = unit(1, "cm"),
    legend.text = element_text(margin = margin(r = 10))
  )

country_colors <- c(
  "www/bra.png" = "#12b045",
  "www/ita.png" = "#e50027",
  "www/uswnt.png" = "#bc2030",
  "www/eng.png" = "#2756ae",
  "www/aus.png" = "grey70",
  "www/fra.png" = "#1b2c4c",
  "www/spa.png" = "#ffca02"
)

p_4th_star <- p("In 2019, the US Women's National Team (USWNT) made women's soccer history by winning their fourth Women's World Cup in France and became one of few teams to win consecutive World Cups. The USWNT took on many of the favorites to win the tournament, such as England and the hosts, France, and put on dominant performances such as their 13-0 victory against Thailand. While they were one of the favorites as the defending champions, many doubted their ability to pull off a second win in a row following their defeat in the 2016 Summer Olympics.", 
                style = "font-size:20px;")

p_shots <- card(
  p("This shot map shows the result of all USWNT shots at goal in a particular match. For example, in the USWNT's first game against Thailand, they scored 13 goals. In this game, Alex Morgan scored 5 goals, tying the record for most goals by a single player in a match. The green dots mark where the USWNT took shots in which they scored goals from. ", 
    style = "font-size:20px;"),
  p("The plot is interactive, so feel free to click around to different matches and see how accurate or inaccurate (or how good a goalkeeper was) the USWNT was in a particular match!", 
    style = "font-size:20px;"))

p_map <- card(
  p("In a Global Broadcast and Audience Report released after the 2019 Women's World Cup, FIFA revealed that France 2019 was the most watched FIFA Women’s World Cup ever. Compared to Canada 2015, the tournament's viewership increased 30%, reaching a total audience of 1.12 billion people.", 
    style = "font-size:20px;"), 
  p("The map shows that this increase largely came from South America and other regions of the world. Viewership of the 2019 Women's World Cup in South America, compared to the 2015 Women's World Cup, increased from 67 million to 413 million.", 
    style = "font-size:20px;"))

p_prize <- p("Compared to the 2018 World Cup, the prize money given to the 2019 Women's World Cup was significantly less, highlighting a larger issue of a gender pay gap in soccer. While the prize money in the tournament has increased over the past few Women's World Cups, the winners, for example, earn $34 million less than the men's winners. According to the PFA, even if the women's prize money pool continued to increased 100% each year, the prize money would only become equal by 2039.", style = "font-size:20px;")