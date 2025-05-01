### AESTHETICS ###

app_title <- tagList(
  # Logo
  tags$img(src = "logo.png", height = "40px", style = "margin-top: -5px; margin-right: 10px;"),
  "2019 Women's World Cup"
)

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