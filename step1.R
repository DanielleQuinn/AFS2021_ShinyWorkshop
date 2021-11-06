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
