# ---- Capstone App Activity ----
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(DT)
library(leaflet)

# ---- Step 1: Build the UI ----
ui <- fluidPage(
  titlePanel("Application Title"),
  sidebarLayout(
    sidebarPanel(
      p("Text"),
      p("Input control 1"),
      p("Input control 2"),
      p("Input control 3"),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(p("Plot here")),
      fluidRow(
        column(width = 6,
               fluidRow(p("Text")),
               fluidRow(column(width = 4, p("VB1")),
                        column(width = 4, p("VB2")),
                          column(width = 4, p("VB3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {}

shinyApp(ui, server)


# ---- Step 2: Add static elements ----
ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      p("Input control 1"),
      p("Input control 2"),
      p("Input control 3"),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(p("Plot here")),
      fluidRow(
        column(width = 6,
               fluidRow(p("Text")),
               fluidRow(column(width = 4, p("VB1")),
                        column(width = 4, p("VB2")),
                        column(width = 4, p("VB3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {}

shinyApp(ui, server)

# ---- Step 3: Import data ----
# limit data interaction to static components
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "region", label = "Region",
                  choices = unique(data$Region)),
      checkboxGroupInput(inputId = "site", label = "Site(s)",
                   choices = unique(data$Site),
                   selected = unique(data$Site)),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(p("Plot here")),
      fluidRow(
        column(width = 6,
               fluidRow(p("Text")),
               fluidRow(column(width = 4, p("VB1")),
                        column(width = 4, p("VB2")),
                        column(width = 4, p("VB3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {}

shinyApp(ui, server)


# ---- Step 4: Fix input control 2 (reactive) ----
# only show sites within the selected region
# introducing uiOutput() and renderOutput()
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot")),
      fluidRow(
        column(width = 6,
               fluidRow(p("Text")),
               fluidRow(column(width = 4, p("VB1")),
                        column(width = 4, p("VB2")),
                        column(width = 4, p("VB3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
}

shinyApp(ui, server)

# ---- Step 5: Create plot (static) ----
# simplest version of plot, not reactive to any input controls
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "region", label = "Region",
                  choices = unique(data$Region)),
      checkboxGroupInput(inputId = "site", label = "Site(s)",
                         choices = unique(data$Site),
                         selected = unique(data$Site)),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot")),
      fluidRow(
        column(width = 6,
               fluidRow(p("Text")),
               fluidRow(column(width = 4, p("VB1")),
                        column(width = 4, p("VB2")),
                        column(width = 4, p("VB3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Plot
  output$myplot <- renderPlot({
    ggplot(data) +
      geom_point(aes(x = Year, y = Adults)) +
      geom_line(aes(x = Year, y = Adults, group = Site)) +
      theme_bw()
  })
}

shinyApp(ui, server)

# ---- Step 6: Create plot (reactive) ----
# subset data to only plot selected items
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                         choices = unique(data$Site),
                         selected = unique(data$Site)),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot")),
      fluidRow(
        column(width = 6,
               fluidRow(p("Text")),
               fluidRow(column(width = 4, p("VB1")),
                        column(width = 4, p("VB2")),
                        column(width = 4, p("VB3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Plot
  output$myplot <- renderPlot({
    data_subset <- data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
    
    ggplot(data_subset) +
      geom_point(aes(x = Year, y = Adults)) +
      geom_line(aes(x = Year, y = Adults, group = Site)) +
      theme_bw()
  })
}

shinyApp(ui, server)

# ---- Step 7: Add plot reactivity to metric input ----
# make y axis represent selected metric
# introducing aes_string()
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot")),
      fluidRow(
        column(width = 6,
               fluidRow(p("Text")),
               fluidRow(column(width = 4, p("VB1")),
                        column(width = 4, p("VB2")),
                        column(width = 4, p("VB3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Plot
  output$myplot <- renderPlot({
    data_subset <- data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
    
    ggplot(data_subset) +
      geom_point(aes_string(x = "Year", y = .data[[input$yaxis]])) +
      geom_line(aes_string(x = "Year", y = .data[[input$yaxis]], group = "Site")) +
      theme_bw()
  })
}

shinyApp(ui, server)

# ---- Step 8 Alternative syntax ----
# make y axis represent selected metric
# introducing .data[[input$yaxis]]
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot")),
      fluidRow(
        column(width = 6,
               fluidRow(p("Text")),
               fluidRow(column(width = 4, p("VB1")),
                        column(width = 4, p("VB2")),
                        column(width = 4, p("VB3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Plot
  output$myplot <- renderPlot({
    data_subset <- data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
    
    ggplot(data_subset) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
}

shinyApp(ui, server)
# ---- Step 9: Add static value boxes ----
# fill in value boxes with static values
# note: valueBox() only uses basic arguments when used outside of a shinydashboard setting
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot")),
      fluidRow(
        column(width = 6,
               fluidRow(p("Text")),
               fluidRow(column(width = 4, valueBoxOutput("valuebox1")),
                        column(width = 4, valueBoxOutput("valuebox2")),
                        column(width = 4, valueBoxOutput("valuebox3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Plot
  output$myplot <- renderPlot({
    data_subset <- data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
    
    ggplot(data_subset) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
  
  # Value boxes
  output$valuebox1 <- renderValueBox({
    valueBox(1, "Adults")
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox(2, "Pups")
  })
  
  output$valuebox3 <- renderValueBox({
    valueBox(3, "Total")
  })
}

shinyApp(ui, server)

# "Before we continue, notice that we're going to need to repeat our
# data filter AGAIN!"
# Next: learn about reactive()

# ---- Step 10: Make value boxes dynamic, using reactive() to filter data ----
# introducing reactive()
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot")),
      fluidRow(
        column(width = 6,
               fluidRow(p("Text")),
               fluidRow(column(width = 4, valueBoxOutput("valuebox1")),
                        column(width = 4, valueBoxOutput("valuebox2")),
                        column(width = 4, valueBoxOutput("valuebox3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Reactive data subset
  data_subset <- reactive({
    data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
  })
  
  # Plot
  output$myplot <- renderPlot({
    ggplot(data_subset()) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
  
  # Value boxes
  output$valuebox1 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, na.rm = TRUE), "Adults")
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox(sum(data_subset()$Pups, na.rm = TRUE), "Pups")
  })
  
  output$valuebox3 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, data_subset()$Pups, na.rm = TRUE), "Total")
  })
}

shinyApp(ui, server)

# ---- Step 11: Add static text ----
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot")),
      fluidRow(
        column(width = 6,
               fluidRow(textOutput("description")),
               fluidRow(column(width = 4, valueBoxOutput("valuebox1")),
                        column(width = 4, valueBoxOutput("valuebox2")),
                        column(width = 4, valueBoxOutput("valuebox3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Reactive data subset
  data_subset <- reactive({
    data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
  })
  
  # Plot
  output$myplot <- renderPlot({
    ggplot(data_subset()) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
  
  # Value boxes
  output$valuebox1 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, na.rm = TRUE), "Adults")
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox(sum(data_subset()$Pups, na.rm = TRUE), "Pups")
  })
  
  output$valuebox3 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, data_subset()$Pups, na.rm = TRUE), "Total")
  })
  
  # Text
  output$description <- renderText({
    "Helpful information will go here!"
  })
}

shinyApp(ui, server)

# ---- Step 12: Add brushOpts to plot ----
# introducing plotOutput(..., brush = brushOpt(id))
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot", brush = "point_brush")),
      fluidRow(
        column(width = 6,
               fluidRow(textOutput("description")),
               fluidRow(column(width = 4, valueBoxOutput("valuebox1")),
                        column(width = 4, valueBoxOutput("valuebox2")),
                        column(width = 4, valueBoxOutput("valuebox3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Reactive data subset
  data_subset <- reactive({
    data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
  })
  
  # Plot
  output$myplot <- renderPlot({
    ggplot(data_subset()) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
  
  # Value boxes
  output$valuebox1 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, na.rm = TRUE), "Adults")
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox(sum(data_subset()$Pups, na.rm = TRUE), "Pups")
  })
  
  output$valuebox3 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, data_subset()$Pups, na.rm = TRUE), "Total")
  })
  
  # Text
  output$description <- renderText({
    "Helpful information will go here!"
  })
}

shinyApp(ui, server)

# ---- Step 13: Make text dynamic, based on brushed points ----
# introducing input$point_brush$ymax
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot", brush = "point_brush")),
      fluidRow(
        column(width = 6,
               fluidRow(textOutput("description")),
               fluidRow(column(width = 4, valueBoxOutput("valuebox1")),
                        column(width = 4, valueBoxOutput("valuebox2")),
                        column(width = 4, valueBoxOutput("valuebox3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Reactive data subset
  data_subset <- reactive({
    data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
  })
  
  # Plot
  output$myplot <- renderPlot({
    ggplot(data_subset()) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
  
  # Value boxes
  output$valuebox1 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, na.rm = TRUE), "Adults")
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox(sum(data_subset()$Pups, na.rm = TRUE), "Pups")
  })
  
  output$valuebox3 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, data_subset()$Pups, na.rm = TRUE), "Total")
  })
  
  # Text
  output$description <- renderText({
    paste("The ymax is", input$point_brush$ymax)
  })
}

shinyApp(ui, server)

# ---- Step 14: Make text dynamic and useful ----
# introducing .data[[input$yaxis]] and req()
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot", brush = "point_brush")),
      fluidRow(
        column(width = 6,
               fluidRow(textOutput("description")),
               fluidRow(column(width = 4, valueBoxOutput("valuebox1")),
                        column(width = 4, valueBoxOutput("valuebox2")),
                        column(width = 4, valueBoxOutput("valuebox3"))),
               fluidRow(p("Table"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Reactive data subset
  data_subset <- reactive({
    data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
  })
  
  # Plot
  output$myplot <- renderPlot({
    ggplot(data_subset()) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
  
  # Value boxes
  output$valuebox1 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, na.rm = TRUE), "Adults")
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox(sum(data_subset()$Pups, na.rm = TRUE), "Pups")
  })
  
  output$valuebox3 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, data_subset()$Pups, na.rm = TRUE), "Total")
  })
  
  # Reactive selected points
  selected_points <- reactive({
    data_subset() %>%
      filter(between(Year,
                     as.numeric(input$point_brush$xmin),
                     as.numeric(input$point_brush$xmax)),
             between(.data[[input$yaxis]],
                     as.numeric(input$point_brush$ymin),
                     as.numeric(input$point_brush$ymax)))
  })
  
  # Text
  output$description <- renderText({
    req(input$point_brush)
    paste("Your selection represents",
          sum(selected_points()[[input$yaxis]], na.rm = TRUE),
          input$yaxis,
          "between",
          round(as.numeric(input$point_brush$xmin), 0),
          "and",
          round(as.numeric(input$point_brush$xmax), 0))
  })
}

shinyApp(ui, server)

# ---- Step 15: Create a table ----
# introducing DT::dataTableOutput() and DT::renderDataTable()
data <- read.delim("sea_lions_count.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot", brush = "point_brush")),
      fluidRow(
        column(width = 6,
               fluidRow(textOutput("description")),
               fluidRow(column(width = 4, valueBoxOutput("valuebox1")),
                        column(width = 4, valueBoxOutput("valuebox2")),
                        column(width = 4, valueBoxOutput("valuebox3"))),
               fluidRow(DT::dataTableOutput("mytable"))),
        column(width = 6, p("Map"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Reactive data subset
  data_subset <- reactive({
    data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
  })
  
  # Plot
  output$myplot <- renderPlot({
    ggplot(data_subset()) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
  
  # Value boxes
  output$valuebox1 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, na.rm = TRUE), "Adults")
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox(sum(data_subset()$Pups, na.rm = TRUE), "Pups")
  })
  
  output$valuebox3 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, data_subset()$Pups, na.rm = TRUE), "Total")
  })
  
  # Reactive selected points
  selected_points <- reactive({
    data_subset() %>%
      filter(between(Year,
                     as.numeric(input$point_brush$xmin),
                     as.numeric(input$point_brush$xmax)),
             between(.data[[input$yaxis]],
                     as.numeric(input$point_brush$ymin),
                     as.numeric(input$point_brush$ymax)))
  })
  
  # Text
  output$description <- renderText({
    req(input$point_brush)
    paste("Your selection represents",
          sum(selected_points()[[input$yaxis]], na.rm = TRUE),
          input$yaxis,
          "between",
          round(as.numeric(input$point_brush$xmin), 0),
          "and",
          round(as.numeric(input$point_brush$xmax), 0))
  })
  
  # Table
  output$mytable <- DT::renderDataTable({
    data_subset() %>%
      group_by(Site) %>%
      summarise(Adults = sum(Adults, na.rm = TRUE),
                Pups = sum(Pups, na.rm = TRUE))
  })
}

shinyApp(ui, server)

# Lots of formatting options here; we can look at some of them if there's time

# ---- Step 16: Create a map ----
# introducing leaflet
data <- read.delim("sea_lions_count.txt")
coords <- read.delim("sea_lions_sites.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot", brush = "point_brush")),
      fluidRow(
        column(width = 6,
               fluidRow(textOutput("description")),
               fluidRow(column(width = 4, valueBoxOutput("valuebox1")),
                        column(width = 4, valueBoxOutput("valuebox2")),
                        column(width = 4, valueBoxOutput("valuebox3"))),
               fluidRow(DT::dataTableOutput("mytable"))),
        column(width = 6, leafletOutput("mymap"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Reactive data subset
  data_subset <- reactive({
    data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
  })
  
  # Plot
  output$myplot <- renderPlot({
    ggplot(data_subset()) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
  
  # Value boxes
  output$valuebox1 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, na.rm = TRUE), "Adults")
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox(sum(data_subset()$Pups, na.rm = TRUE), "Pups")
  })
  
  output$valuebox3 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, data_subset()$Pups, na.rm = TRUE), "Total")
  })
  
  # Reactive selected points
  selected_points <- reactive({
    data_subset() %>%
      filter(between(Year,
                     as.numeric(input$point_brush$xmin),
                     as.numeric(input$point_brush$xmax)),
             between(.data[[input$yaxis]],
                     as.numeric(input$point_brush$ymin),
                     as.numeric(input$point_brush$ymax)))
  })
  
  # Text
  output$description <- renderText({
    req(input$point_brush)
    paste("Your selection represents",
          sum(selected_points()[[input$yaxis]], na.rm = TRUE),
          input$yaxis,
          "between",
          round(as.numeric(input$point_brush$xmin), 0),
          "and",
          round(as.numeric(input$point_brush$xmax), 0))
  })
  
  # Table values
  table_values <- reactive({
    data_subset() %>%
      group_by(Site) %>%
      summarise(Adults = sum(Adults, na.rm = TRUE),
                Pups = sum(Pups, na.rm = TRUE))
  })
  
  # Table
  output$mytable <- DT::renderDataTable({
    table_values()
  })
  
  # Map data
  map_data <- reactive({
    table_values() %>%
      left_join(coords)
  })
  # Map
  output$mymap <- renderLeaflet({
    leaflet(map_data()) %>%
      addTiles() %>%
      addCircleMarkers()
  })
}

shinyApp(ui, server)

# ---- Step 17: Make the map more useful ----
# size based on relative abundance
# click on point to see Site name
# aesthetics
data <- read.delim("sea_lions_count.txt")
coords <- read.delim("sea_lions_sites.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot", brush = "point_brush")),
      fluidRow(
        column(width = 6,
               fluidRow(textOutput("description")),
               fluidRow(column(width = 4, valueBoxOutput("valuebox1")),
                        column(width = 4, valueBoxOutput("valuebox2")),
                        column(width = 4, valueBoxOutput("valuebox3"))),
               fluidRow(DT::dataTableOutput("mytable"))),
        column(width = 6, leafletOutput("mymap"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Reactive data subset
  data_subset <- reactive({
    data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
  })
  
  # Plot
  output$myplot <- renderPlot({
    ggplot(data_subset()) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
  
  # Value boxes
  output$valuebox1 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, na.rm = TRUE), "Adults")
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox(sum(data_subset()$Pups, na.rm = TRUE), "Pups")
  })
  
  output$valuebox3 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, data_subset()$Pups, na.rm = TRUE), "Total")
  })
  
  # Reactive selected points
  selected_points <- reactive({
    data_subset() %>%
      filter(between(Year,
                     as.numeric(input$point_brush$xmin),
                     as.numeric(input$point_brush$xmax)),
             between(.data[[input$yaxis]],
                     as.numeric(input$point_brush$ymin),
                     as.numeric(input$point_brush$ymax)))
  })
  
  # Text
  output$description <- renderText({
    req(input$point_brush)
    paste("Your selection represents",
          sum(selected_points()[[input$yaxis]], na.rm = TRUE),
          input$yaxis,
          "between",
          round(as.numeric(input$point_brush$xmin), 0),
          "and",
          round(as.numeric(input$point_brush$xmax), 0))
  })
  
  # Table values
  table_values <- reactive({
    data_subset() %>%
      group_by(Site) %>%
      summarise(Adults = sum(Adults, na.rm = TRUE),
                Pups = sum(Pups, na.rm = TRUE))
  })
  
  # Table
  output$mytable <- DT::renderDataTable({
    table_values()
  })
  
  # Map data
  map_data <- reactive({
    table_values() %>%
      left_join(coords) %>%
      mutate(relative_count = .data[[input$yaxis]]/sum(.data[[input$yaxis]]))
  })

  # Map
  output$mymap <- renderLeaflet({
    pal <- colorNumeric(heat.colors(10, rev = TRUE), map_data()$relative_count)
    
    leaflet(map_data()) %>%
      addTiles() %>%
      addCircleMarkers(popup = ~Site,
                       stroke = FALSE,
                       radius = ~relative_count * 50,
                       fillOpacity = 0.5,
                       fillColor = ~pal(relative_count))
  })
  
}

shinyApp(ui, server)

# ---- Step 18: Select rows from table ----
# select rows from table to highlight on map
# introducing observeEvent(), leafletProxy(), and input$mytable_selected_rows
data <- read.delim("sea_lions_count.txt")
coords <- read.delim("sea_lions_sites.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      p("Download Button"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot", brush = "point_brush")),
      fluidRow(
        column(width = 6,
               fluidRow(textOutput("description")),
               fluidRow(column(width = 4, valueBoxOutput("valuebox1")),
                        column(width = 4, valueBoxOutput("valuebox2")),
                        column(width = 4, valueBoxOutput("valuebox3"))),
               fluidRow(DT::dataTableOutput("mytable"))),
        column(width = 6, leafletOutput("mymap"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Reactive data subset
  data_subset <- reactive({
    data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
  })
  
  # Plot
  output$myplot <- renderPlot({
    ggplot(data_subset()) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
  
  # Value boxes
  output$valuebox1 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, na.rm = TRUE), "Adults")
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox(sum(data_subset()$Pups, na.rm = TRUE), "Pups")
  })
  
  output$valuebox3 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, data_subset()$Pups, na.rm = TRUE), "Total")
  })
  
  # Reactive selected points
  selected_points <- reactive({
    data_subset() %>%
      filter(between(Year,
                     as.numeric(input$point_brush$xmin),
                     as.numeric(input$point_brush$xmax)),
             between(.data[[input$yaxis]],
                     as.numeric(input$point_brush$ymin),
                     as.numeric(input$point_brush$ymax)))
  })
  
  # Text
  output$description <- renderText({
    req(input$point_brush)
    paste("Your selection represents",
          sum(selected_points()[[input$yaxis]], na.rm = TRUE),
          input$yaxis,
          "between",
          round(as.numeric(input$point_brush$xmin), 0),
          "and",
          round(as.numeric(input$point_brush$xmax), 0))
  })
  
  # Table values
  table_values <- reactive({
    data_subset() %>%
      group_by(Site) %>%
      summarise(Adults = sum(Adults, na.rm = TRUE),
                Pups = sum(Pups, na.rm = TRUE))
  })
  
  # Table
  output$mytable <- DT::renderDataTable({
    table_values()
  })
  
  # Map data
  map_data <- reactive({
    table_values() %>%
      left_join(coords) %>%
      mutate(relative_count = .data[[input$yaxis]]/sum(.data[[input$yaxis]]))
  })
  
  # Map
  output$mymap <- renderLeaflet({
    pal <- colorNumeric(heat.colors(10, rev = TRUE), map_data()$relative_count)
    
    leaflet(map_data()) %>%
      addTiles() %>%
      addCircleMarkers(popup = ~Site,
                       stroke = FALSE,
                       radius = ~relative_count * 50,
                       fillOpacity = 0.5,
                       color = ~pal(relative_count))
  })
  
  observeEvent(input$mytable_rows_selected, ignoreNULL = FALSE, {
    pal <- colorNumeric(heat.colors(10, rev = TRUE), map_data()$relative_count)
    
    ifelse(is.null(input$mytable_rows_selected),
           rows <- 0,
           rows <- input$mytable_rows_selected)
    
      leafletProxy("mymap") %>%
        clearMarkers() %>%
        addCircleMarkers(popup = ~Site,
                         stroke = FALSE,
                         radius = ~relative_count * 50,
                         fillOpacity = 0.5,
                         fillColor = ~pal(relative_count),
                         data = map_data()) %>%
        addCircleMarkers(popup = ~Site,
                         stroke = TRUE,
                         color = "black",
                         radius = ~relative_count*50,
                         fillOpacity = 0,
                         data = map_data() %>%
                           slice(rows))
  })
  
}

shinyApp(ui, server)


# ---- Step 19: Download data button ----
# introducing downloadHandler()
data <- read.delim("sea_lions_count.txt")
coords <- read.delim("sea_lions_sites.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      downloadButton("download_data", "Download Selected Data"),
      p("Download Button")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot", brush = "point_brush")),
      fluidRow(
        column(width = 6,
               fluidRow(textOutput("description")),
               fluidRow(column(width = 4, valueBoxOutput("valuebox1")),
                        column(width = 4, valueBoxOutput("valuebox2")),
                        column(width = 4, valueBoxOutput("valuebox3"))),
               fluidRow(DT::dataTableOutput("mytable"))),
        column(width = 6, leafletOutput("mymap"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Reactive data subset
  data_subset <- reactive({
    data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
  })
  
  # Plot
  output$myplot <- renderPlot({
    ggplot(data_subset()) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
  
  # Value boxes
  output$valuebox1 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, na.rm = TRUE), "Adults")
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox(sum(data_subset()$Pups, na.rm = TRUE), "Pups")
  })
  
  output$valuebox3 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, data_subset()$Pups, na.rm = TRUE), "Total")
  })
  
  # Reactive selected points
  selected_points <- reactive({
    data_subset() %>%
      filter(between(Year,
                     as.numeric(input$point_brush$xmin),
                     as.numeric(input$point_brush$xmax)),
             between(.data[[input$yaxis]],
                     as.numeric(input$point_brush$ymin),
                     as.numeric(input$point_brush$ymax)))
  })
  
  # Text
  output$description <- renderText({
    req(input$point_brush)
    paste("Your selection represents",
          sum(selected_points()[[input$yaxis]], na.rm = TRUE),
          input$yaxis,
          "between",
          round(as.numeric(input$point_brush$xmin), 0),
          "and",
          round(as.numeric(input$point_brush$xmax), 0))
  })
  
  # Table values
  table_values <- reactive({
    data_subset() %>%
      group_by(Site) %>%
      summarise(Adults = sum(Adults, na.rm = TRUE),
                Pups = sum(Pups, na.rm = TRUE))
  })
  
  # Table
  output$mytable <- DT::renderDataTable({
    table_values()
  })
  
  # Map data
  map_data <- reactive({
    table_values() %>%
      left_join(coords) %>%
      mutate(relative_count = .data[[input$yaxis]]/sum(.data[[input$yaxis]]))
  })
  
  # Map
  output$mymap <- renderLeaflet({
    pal <- colorNumeric(heat.colors(10, rev = TRUE), map_data()$relative_count)
    
    leaflet(map_data()) %>%
      addTiles() %>%
      addCircleMarkers(popup = ~Site,
                       stroke = FALSE,
                       radius = ~relative_count * 50,
                       fillOpacity = 0.5,
                       color = ~pal(relative_count))
  })
  
  observeEvent(input$mytable_rows_selected, ignoreNULL = FALSE, {
    pal <- colorNumeric(heat.colors(10, rev = TRUE), map_data()$relative_count)
    
    ifelse(is.null(input$mytable_rows_selected),
           rows <- 0,
           rows <- input$mytable_rows_selected)
    
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addCircleMarkers(popup = ~Site,
                       stroke = FALSE,
                       radius = ~relative_count * 50,
                       fillOpacity = 0.5,
                       fillColor = ~pal(relative_count),
                       data = map_data()) %>%
      addCircleMarkers(popup = ~Site,
                       stroke = TRUE,
                       color = "black",
                       radius = ~relative_count*50,
                       fillOpacity = 0,
                       data = map_data() %>%
                         slice(rows))
  })
  
  # Download data
  output$download_data <- downloadHandler(
    filename = "mydata.csv",
    content = function(file) {
      write.csv(data_subset(), file)
    }
  )

}

shinyApp(ui, server)



# ---- Step 20: Download plot ----
# Need to render plot in two steps
data <- read.delim("sea_lions_count.txt")
coords <- read.delim("sea_lions_sites.txt")

ui <- fluidPage(
  titlePanel("Sea Lion Application"),
  sidebarLayout(
    sidebarPanel(
      HTML(read_file("textblock.txt")),
      selectInput(inputId = "selected_region", label = "Region",
                  choices = unique(data$Region)),
      uiOutput("site_inputcontrol"),
      radioButtons(inputId = "yaxis", label = "Metric",
                   choices = c("Adults", "Pups")),
      downloadButton("download_data", "Download Selected Data"),
      downloadButton("download_plot", "Download Current Plot")
    ),
    mainPanel(
      fluidRow(plotOutput("myplot", brush = "point_brush")),
      fluidRow(
        column(width = 6,
               fluidRow(textOutput("description")),
               fluidRow(column(width = 4, valueBoxOutput("valuebox1")),
                        column(width = 4, valueBoxOutput("valuebox2")),
                        column(width = 4, valueBoxOutput("valuebox3"))),
               fluidRow(DT::dataTableOutput("mytable"))),
        column(width = 6, leafletOutput("mymap"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive input control
  output$site_inputcontrol <- renderUI({
    mychoices <- data %>%
      filter(Region == input$selected_region) %>%
      pull(Site)
    
    checkboxGroupInput(inputId = "selected_site", label = "Site(s)",
                       choices = unique(mychoices),
                       selected = unique(mychoices))
  })
  
  # Reactive data subset
  data_subset <- reactive({
    data %>%
      filter(Region == input$selected_region,
             Site %in% input$selected_site)
  })
  
  # Store plot
  stored_plot <- reactive({
    ggplot(data_subset()) +
      geom_point(aes(x = Year, y = .data[[input$yaxis]])) +
      geom_line(aes(x = Year, y = .data[[input$yaxis]], group = Site)) +
      theme_bw()
  })
  # Plot
  output$myplot <- renderPlot({
    stored_plot()
  })
  
  # Value boxes
  output$valuebox1 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, na.rm = TRUE), "Adults")
  })
  
  output$valuebox2 <- renderValueBox({
    valueBox(sum(data_subset()$Pups, na.rm = TRUE), "Pups")
  })
  
  output$valuebox3 <- renderValueBox({
    valueBox(sum(data_subset()$Adults, data_subset()$Pups, na.rm = TRUE), "Total")
  })
  
  # Reactive selected points
  selected_points <- reactive({
    data_subset() %>%
      filter(between(Year,
                     as.numeric(input$point_brush$xmin),
                     as.numeric(input$point_brush$xmax)),
             between(.data[[input$yaxis]],
                     as.numeric(input$point_brush$ymin),
                     as.numeric(input$point_brush$ymax)))
  })
  
  # Text
  output$description <- renderText({
    req(input$point_brush)
    paste("Your selection represents",
          sum(selected_points()[[input$yaxis]], na.rm = TRUE),
          input$yaxis,
          "between",
          round(as.numeric(input$point_brush$xmin), 0),
          "and",
          round(as.numeric(input$point_brush$xmax), 0))
  })
  
  # Table values
  table_values <- reactive({
    data_subset() %>%
      group_by(Site) %>%
      summarise(Adults = sum(Adults, na.rm = TRUE),
                Pups = sum(Pups, na.rm = TRUE))
  })
  
  # Table
  output$mytable <- DT::renderDataTable({
    table_values()
  })
  
  # Map data
  map_data <- reactive({
    table_values() %>%
      left_join(coords) %>%
      mutate(relative_count = .data[[input$yaxis]]/sum(.data[[input$yaxis]]))
  })
  
  # Map
  output$mymap <- renderLeaflet({
    pal <- colorNumeric(heat.colors(10, rev = TRUE), map_data()$relative_count)
    
    leaflet(map_data()) %>%
      addTiles() %>%
      addCircleMarkers(popup = ~Site,
                       stroke = FALSE,
                       radius = ~relative_count * 50,
                       fillOpacity = 0.5,
                       color = ~pal(relative_count))
  })
  
  observeEvent(input$mytable_rows_selected, ignoreNULL = FALSE, {
    pal <- colorNumeric(heat.colors(10, rev = TRUE), map_data()$relative_count)
    
    ifelse(is.null(input$mytable_rows_selected),
           rows <- 0,
           rows <- input$mytable_rows_selected)
    
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addCircleMarkers(popup = ~Site,
                       stroke = FALSE,
                       radius = ~relative_count * 50,
                       fillOpacity = 0.5,
                       fillColor = ~pal(relative_count),
                       data = map_data()) %>%
      addCircleMarkers(popup = ~Site,
                       stroke = TRUE,
                       color = "black",
                       radius = ~relative_count*50,
                       fillOpacity = 0,
                       data = map_data() %>%
                         slice(rows))
  })
  
  # Download data
  output$download_data <- downloadHandler(
    filename = "mydata.csv",
    content = function(file) {
      write.csv(data_subset(), file)
    }
  )
  
  # Download plot
  output$download_plot <- downloadHandler(
    filename = "myplot.png",
    content = function(file) {
      ggsave(file, stored_plot())
    }
  )
  
}

shinyApp(ui, server)
