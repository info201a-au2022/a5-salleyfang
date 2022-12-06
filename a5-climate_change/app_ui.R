library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(shinythemes)

# ADD SHINY WIDGETS: https://dreamrs.github.io/shinyWidgets/

text_page <- tabPanel(
  titlePanel("Selected Values"),
  htmlOutput("co2_pop_ratio"),
  htmlOutput("co2_country_lowest"),
  htmlOutput("co2_change_lowest"),
  htmlOutput("co2_country_highest"),
  htmlOutput("co2_change_highest")
)
# map_sidebar <- sidebarPanel(
#   selectInput(
#     inputId = "mapvar",
#     label = "Variable to Map",
#     choices = list(
#       "CO2 Emissions" = "co2",
#       "Cumulative CO2 Emissions" = "cumulative_co2",
#       "CO2 Emissions Per Capita" = "co2_per_capita",
#       "Share of Total CO2 Emissions Per Country (%)" = "share_global_co2",
#       "Energy Consumption Per Capita" = "energy_per_capita"
#     )
#   )
# )
# map_main <- mainPanel(
#   plotlyOutput("map")
# )

barchart_sidebar <- sidebarPanel(
  selectInput(
    inputId = "country", label = "Choose a Country",
    choices = unique(co2_type_df$country)
  )
)
barchart_main <- mainPanel(
  plotlyOutput("barchart")
)

graph_sidebar <- sidebarPanel(
  sliderInput(inputId = "minyear", label = "Min Year",
              min = 1850, max = 2020, value = 1900),
  sliderInput(inputId = "maxyear", label = "Max Year",
              min = 1851, max = 2021, value = 2000),
  pickerInput(inputId = "countries", label = "Choose a Country",
              multiple = TRUE, choices = unique(country_co2_df$country),
              selected = "Afghanistan",
              options = list(`selected-text-format`= "static",
                             title = "Select at least one discovery method"))
)
graph_main <- mainPanel(
  plotlyOutput("graph")
)

widgets_page <- tabPanel(
   titlePanel("Map & Graph"),
   sidebarLayout(barchart_sidebar, barchart_main),
   sidebarLayout(graph_sidebar, graph_main)
)

ui <- navbarPage(
  theme = shinytheme("united"),
  title = "CO2 Changes",
  text_page,
  widgets_page
)