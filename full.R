# ---- Sea Lion Application ----

# ---- Load packages ----
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(DT)
library(leaflet)

# ---- Import data ----
data <- read.delim("sea_lions_count.txt")
coords <- read.delim("sea_lions_sites.txt")

# ---- Build UI ----
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

# ---- Define Server Function ----
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

# ---- Run Application ----
shinyApp(ui, server)
