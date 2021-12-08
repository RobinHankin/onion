library(shinybusy)

shinyUI(
  fluidPage(
    tags$head(
      tags$script(src = "three.min.js"),
      tags$script(src = "scene.js")
    ),
    add_busy_spinner(
      spin = "circle", position = "bottom-left",
      height = "100px", width = "100px"
    ),
    sidebarLayout(
      sidebarPanel(
        titlePanel("Kochanek-Bartels spline"),
        sliderInput(
          "numt", "tension", value = -1, step = 0.1, min = -2, max = 2
        ),
        sliderInput(
          "numc", "continuity", value = 5, step = 0.1, min = -5, max = 5
        ),
        sliderInput(
          "numb", "bias", value = 0, step = 0.1, min = -1, max = 1
        ),
        actionButton("run", "Go", class = "btn-info btn-block"),
        br(),
        helpText(
          "When you see the spinner at the bottom left, a spline is under",
          "calculation. Please wait"
        )
      ),
      mainPanel(
        tags$div(id = "sphere")
      )
    )
  )
)