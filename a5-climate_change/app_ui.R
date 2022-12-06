library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinythemes)
library(rsconnect)
library(reshape2)
library(shinyWidgets)

# ADD SHINY WIDGETS: https://dreamrs.github.io/shinyWidgets/
co2_df <- read_csv("owid-co2-data.csv")

# co2_type_df <- co2_df %>%
#   filter(iso_code != "", na.rm = TRUE) %>% 
#   select(country, cement_co2, coal_co2, flaring_co2, gas_co2, oil_co2,
#          trade_co2) %>% 
#   replace(is.na(.), 0.0)  %>%
#   group_by(country) %>%
#   mutate(sum_cement = sum(cement_co2)) %>% 
#   mutate(sum_coal = sum(coal_co2)) %>% 
#   mutate(sum_flaring = sum(flaring_co2)) %>% 
#   mutate(sum_gas = sum(gas_co2)) %>% 
#   mutate(sum_oil = sum(oil_co2)) %>% 
#   mutate(sum_trade = sum(trade_co2)) %>% 
#   select(country, sum_cement, sum_coal, sum_flaring, sum_gas, sum_oil,
#          sum_trade) %>% 
#   distinct() %>% 
#   pivot_longer(-country, names_to = "co2_type", values_to = "value")
# 
# country_co2_df <- co2_df %>% 
#   filter(iso_code != "", na.rm = TRUE) %>%
#   select(country, year, co2) %>%
#   replace(is.na(.), 0.0)

text_page <- tabPanel(
  titlePanel("Selected Values"),
  htmlOutput("co2_pop_ratio"),
  htmlOutput("co2_country_lowest"),
  htmlOutput("co2_change_lowest"),
  htmlOutput("co2_country_highest"),
  htmlOutput("co2_change_highest")
)

barchart_sidebar <- sidebarPanel(
  selectInput(
    inputId = "country", label = "Choose a Country",
    choices = unique(co2_df$country)
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
              multiple = TRUE, choices = unique(co2_df$country),
              selected = c("Afghanistan", "Angola"),
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
  theme = shinytheme("darkly"),
  title = "CO2 Changes",
  text_page,
  widgets_page
)