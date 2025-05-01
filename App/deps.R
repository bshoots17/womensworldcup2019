### app dependencies
library(ggplot2)
library(ggthemes)

geom_halfpitch<- ggplot() + annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", linewidth = 0.6) +
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

maptheme <- pitch_theme <- ggthemes::theme_fivethirtyeight() +
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
