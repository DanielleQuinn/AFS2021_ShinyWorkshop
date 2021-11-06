# Load packages ----
library(shiny)

# 1 ----
ui <- fluidPage()

server <- function(input, output) {}

shinyApp(ui, server)

# 2 ----
ui <- fluidPage(p("Add a static text element to your page"))

server <- function(input, output) {}

shinyApp(ui, server)

# 3 ----
ui <- fluidPage(
  h1("Header Level 1"),
  h2("Header Level 2"),
  p("Plain Text"),
  br(),
  strong("Bold Text"),
  em("Italic Text"),
  br(),
  HTML("Use HTML tags to <b>customize</b> <i>your text</i>")
)

server <- function(input, output) {}

shinyApp(ui, server)

# 4 ----
ui <- fluidPage(
  fluidRow(h1("Row 1: Header")),
  fluidRow(p("Row 2: Some text"))
)

server <- function(input, output) {}

shinyApp(ui, server)

# Exercise 1 ----

# 5 ----
ui <- fluidPage(
  titlePanel("Title"),
)

server <- function(input, output) {}

shinyApp(ui, server)

# Exercise 2 ----

# 6 ----
ui <- fluidPage(
  selectInput(inputId = "example1", label = "Pick One", choices = 1:5)
)

server <- function(input, output) {}

shinyApp(ui, server)

# 7 ----
ui <- fluidPage(
  sliderInput(inputId = "example2", label = "Pick a Value",
              min = 1, max = 100, value = 25)
)

server <- function(input, output) {}

shinyApp(ui, server)

# Exercise 3 ----

# 8 ----
ui <- fluidPage(
  p("Display the plot below this text"),
  plotOutput(outputId = "myplot")
)

server <- function(input, output) {
  output$myplot <- renderPlot({plot(1, 1)})
}

shinyApp(ui, server)

# 9 ----
ui <- fluidPage(
  numericInput(inputId = "mynumber", label = "Provide a value", value = 20),
  plotOutput(outputId = "myplot")
)

server <- function(input, output) {
  output$myplot <- renderPlot({plot(1, input$mynumber)})
}

shinyApp(ui, server)

# Exercise 4 ----
