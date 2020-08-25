library(plotly)

library(shiny)

shinyUI(fluidPage(
    titlePanel("HEIGHTMATIC-9000"),

    sidebarLayout(
        sidebarPanel(
            tags$b("Instructions"),
            tags$div(
                tags$ul(
                    tags$li("Select each parent's height"),
                    tags$li("Decide if you want to see both genders or just one"),
                    tags$li("Move around the parent's heights and see how that affects the children's")
                )
            ),
            sliderInput("slider_father",
                        "Father height (cm)",
                        min = 140,
                        max = 210,
                        value = 176),
            sliderInput("slider_mother",
                        "Mother height (cm)",
                        min = 140,
                        max = 210,
                        value = 163),
            radioButtons("radio_gender", "Child gender:",
                         choiceNames = list(
                             "Both",
                             "Male",
                             "Female"
                         ),
                         choiceValues = list(
                             "both", "male", "female"
                         )),            
        ),
        mainPanel(
            fluidRow(
                column(6, plotlyOutput("parentPlot")),
                column(6, plotlyOutput("childrenPlot"))
            )
        )
    )
))
