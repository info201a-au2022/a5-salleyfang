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
   titlePanel("Interactive Data Visualizations"),
   h2("Total CO2 Emissions Per Type For a Selected Country"),
   sidebarLayout(barchart_sidebar, barchart_main),
   p(""),
   p(em("Caption, Fig 1."), "This bar chart displays the total emissions
     for different CO2 emiissions types. It allows users to select a
     country and view its total cement, coal, flaring, gas, oil, and
     trade emissions as a bar chart. Seeing this data can help users
     better understand the distribution of CO2 emissions among different
     countries. This information can help uncover patterns in which
     continents/regions/economies may have more emissions of a certain type."),
   h2("Yearly CO2 Emissions For Countries and Time Period Selected"),
   sidebarLayout(graph_sidebar, graph_main),
   p(""),
   p(em("Caption, Fig 2."), "This line graph displays the CO2 emissions for
     each country selected from a drop down menu. It allows the user
     to control the years being displayed. It graphs the yearly
     CO2 emissions per selected year and allows users to select any
     number of countries. With the ability to compare multiple countries
     and their yearly CO2 emissions over time, users can get a better
     idea of the countries that have seen slower and/or more rapid growth
     and when those rises began occurring. Finding patterns within 
     different parts of the world may be an important starting point
     to discover the reasons behind why certain countries have higher
     yearly CO2 emissions.")
)

ui <- navbarPage(
  theme = shinytheme("darkly"),
  title = "CO2 Changes",
  text_page,
  widgets_page
)