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

text_page <- tabPanel(
  titlePanel("Introduction"),
  h2(strong("Intro")),
  p(""),
  h2(strong("Values of Interest")),
  p("Hi I think this is", textOutput("co2_country_lowest", inline = T))
  # htmlOutput("co2_pop_ratio"),
  # htmlOutput("co2_country_lowest"),
  # htmlOutput("co2_change_lowest"),
  # htmlOutput("co2_country_highest"),
  # htmlOutput("co2_change_highest")
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
   h2(strong("Total CO2 Emissions Per Type For a Selected Country")),
   sidebarLayout(barchart_sidebar, barchart_main),
   p(""),
   p(em("Caption, Fig 1."), "This bar chart displays the total emissions
     for different CO2 emissions types. It allows users to select a
     country and view its total cement, coal, flaring, gas, oil, and
     trade emissions as a bar chart. Seeing this data can help users
     better understand the distribution of CO2 emissions among different
     countries. This information can help uncover patterns in which
     continents/regions/economies may have more emissions of a certain type."),
   h2(strong("Yearly CO2 Emissions For Countries and Time Period Selected")),
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