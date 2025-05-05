# ============================================== #
# File: about_page.R
# Description: Shiny web application built with R.
#              This file contains the aesthetics
#              and text needed for the About Page.
#
# Author: Bianca Schutz
# Date Created: April 24, 2025
# Last Modified: May 5, 2025

# ============================================== #
# ---- Set-Up ----
# ============================================== #

## ---- Packages ----

library(bslib)

# ============================================== #
# ---- About Page ----
# ============================================== #

# contains the nav_panel used to create the static "About" page

about_panel <- nav_panel(title = "About", 
                          fluidPage(
                            layout_columns(
                              card(
                                card_header("About Me", 
                                            style = "font-size:40px;"), 
                                p("Hi! I'm Bianca, a senior at Rice University. I had the privilege of attending some of the 2019 Women's World Cup games in France. I was in France for a school trip and my family and I stayed for a few extra days to catch the USWNT take on France in Paris, then England in the semis, and finally the Netherlands in the final in Lyon.", 
                                  style = "font-size:20px;"), 
                                p("Seeing the USWNT play in a Women's World Cup was a blast and compared to other games I'd attended back home in Texas, the energy level in the stadium was much higher. The media coverage during the tournament also stood out to me as the USWNT got more attention than before.", 
                                  style = "font-size:20px;")), 
                              img(src='me_at_final.jpg', 
                                  alt = "Me at the Women's World Cup Final in 2019"),
                              col_widths = c(8, 4)),
                            layout_columns(
                              img(
                                src = "equalpay.jpg"), 
                              card(
                                card_header("About This Page", 
                                            style = "font-size:40px;"), 
                                p('This page was created to chronicle and examine the road to the US Women\'s historic fourth WWC win, and to highlight how the game has continued to grow. Another important part of this tournament was how political it was. While they travelled around France, back home President Trump and others criticized the USWNT for their fight for equal pay and their political stances. When the USWNT won the final against the Netherlands, the entire stadium chanted "Equal Pay." A fight for change and improvement in the sport is a theme echoed throughout the tournament, and this page addresses that as well.', 
                                  style = "font-size:20px;"),
                                p("This page was created using Shiny App for my DSCI 304: Data Visualization final project. The play-by-play data was obtained from the StatsBombR package, and other data came from FIFA.", style = "font-size:20px;")),
                              col_widths = c(5, 7)
                                    )))
