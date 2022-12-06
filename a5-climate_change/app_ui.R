library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

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

plot_sidebar <- sidebarPanel(
  sliderInput(inputId = "min_year", label = "Min Year",
              min = 1850, max = 2020, value = 1900),
  sliderInput(inputId = "max_year", label = "Max Year",
              min = 1851, max = 2021, value = 2000),
  sliderInput(inputId = "min_co2", label = "Min CO2 Emissions",
              min = 0, max = 439999, value = 10000),
  sliderInput(inputId = "max_co2", label = "Max CO2 Emissions",
              min = 1, max = 440000, value = 400000)
  # selectInput(
  #   inputId = "emission_type",
  #   label = "Select a CO2 Emission Type",
  #   choices = list(
  #     "Cement CO2" = "cement_co2",
  #     "Coal CO2" = "coal_co2",
  #     "Flaring CO2" = "flaring_co2",
  #     "Gas CO2" = "gas_co2",
  #     "Oil CO2" = "oil_co2",
  #     "Trade CO2" = "trade_co2"
  #   )
  # )
)

plot_main <- mainPanel(
  plotlyOutput("plot")
)

# co2_df <- read.csv(
#   file = "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv",
#   stringsAsFactors = FALSE, header = TRUE
# )

co2_df <- read.csv(file = "owid-co2-data.csv")

country_co2_df <- co2_df %>% 
  filter(iso_code != "", na.rm = TRUE) %>%
  select(country, iso_code, year, co2) %>%
  replace(is.na(.), 0)

graph_sidebar <- sidebarPanel(
  selectInput(
    inputId = "country", label = "Choose a Country",
    choices = unique(country_co2_df$country)
  )
)
graph_main <- mainPanel(
  plotlyOutput("graph")
)

widgets_page <- tabPanel(
   titlePanel("Map & Graph"),
   sidebarLayout(plot_sidebar, plot_main),
#   sidebarLayout(map_sidebar, map_main),
   sidebarLayout(graph_sidebar, graph_main)
)

ui <- navbarPage(
  title = "CO2 Changes",
  text_page,
  widgets_page
)