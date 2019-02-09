fluidPage(
  titlePanel("Parkrun Running Times Scatter Plot"),
  sidebarLayout(
    sidebarPanel(
      textInput("athleteIDInput", label = h3("Enter your Parkrun Athlete ID (usually a 6-digit number): "), value = "733446"),
      fluidRow(p("I started slower (33m) and got quicker over time.  Once I got past 25m, I occasionally recorded times that
                 were slower when running with a buggy so I set a threshold here to exclude slower runs after I first crossed 
                 that threshold.")),
      sliderInput("timeThresholdInput", label = h3("Select threshold: "), value = 25, min = 13, max = 60),
      fileInput("weightFileInput", label = h4("(Optional) CSV file containing two comma-separated columns: date and weight"))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Run times scatter plot", plotOutput("scatterPlotOutput")),
                  tabPanel("Weight vs. speed",
                           textOutput("weightFileOutput"),
                           plotOutput("weightSpeedPlotOutput")))
    )
  )
)
