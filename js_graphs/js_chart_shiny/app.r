library(shiny)

addResourcePath("html", getwd()) 

ui <- fluidPage(
  column(width = 3, 
         style = "min-width: 135px; margin: auto;"
  ),
  tags$head(
    tags$style(HTML("
      html, body, #shiny-app {
        height: 100%;
        margin: 0;
        padding: 0;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      #shiny-app {
        width: 100%;
        height: 100%;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      #iframe-container {
        display: grid;
        grid-template-columns: 1fr 1fr;
        grid-template-rows: 1fr 1fr;
        gap: 1px;
        width: 100%;
        height: 100%;
        box-sizing: border-box;
      }
      .iframe-item {
        width: 100%;
        height: 100%;
        border: none;
      }
      @media (max-width: 800px) {
        #iframe-container {
          grid-template-columns: 1fr;
          grid-template-rows: repeat(4, 1fr);
          gap: 1px;
        }
        .iframe-item {
          height: 20vh; /* This ensures each iframe takes up 50% of the viewport height */
        }
      }
      /* Adjust the height of the lower iframes */
      #iframe-container .iframe-item:nth-child(3),
      #iframe-container .iframe-item:nth-child(4) {
        height: 25vh; /* Adjust as needed */
      }
    "))
  ),
  div(
    id = "shiny-app",
    div(
      id = "iframe-container",
      tags$iframe(class="iframe-item", src="html/1_Anzahl_Vorlesungen.html"),
      tags$iframe(class="iframe-item", src="html/2_Anzahl_Semester.html"),
      tags$iframe(class="iframe-item", src="html/3_Donut.html"),
      tags$iframe(class="iframe-item", src="html/4_Balken.html")
    )
  )
)

server <- function(input, output) {
}

shinyApp(ui, server)
