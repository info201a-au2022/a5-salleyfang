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
  p(strong("Climate change"), "and", em("CO2 emission rates specifically"), "have 
    become a highly discussed topic in recent years. The effects of climate change
    are becoming more present, and one of the factors in that becoming the case is
    the rapid rise in CO2 emissions. The", strong("variables"), "I will focus on
    are", em("yearly CO2 emissions"), "for countries and", em("cumulative emissions"),
    "for industries that produce CO2 within countries (measured in millions of tons)."),
  p("Analyzing the", strong("yearly CO2 emissions rates"), "allows us to find patterns
    in the CO2 emissions changes among different countries. It is important to
    see years when countries have seen rises and/or falls in CO2 emissions 
    and identify the countries that are currently seeing fast rises in order
    analyze and find ways to slow down the growth before it becomes a 
    bigger issue. These solutions can help prevent rapid increases in other
    countries as well."),
  p(strong("Cumulative CO2 emissions"), ", seen in the different types of CO2 
    emissions I will cover (cement, coal, flaring, gas, oil, trade) is crucial in
    determining the types of industries among countries around the world that
    contribute the most to CO2 emissions. For example, countries in the
    Middle East have big gas and oil industries, seen in the much
    larger CO2 emissions rates for those two variables over others. The values
    for these cumulative variables is important to analyze because finding the
    most significant causes behind a country's CO2 emissions can signal to people
    where to specifically try and slow down the growth. We can use this information
    to better prepare when an industry in another country begins to grow and rising
    CO2 emissions becomes an issue. Preparation is one of the ways in which we can
    take part in slowing down the effects of climate change."),
  h2(strong("Values of Interest")),
  p("The first value of interest I wanted to highlight was the country with
    the", em("highest CO2 Emissions to population ratio in 2021."),
    strong(textOutput("co2_pop_ratio_country", inline = T)), "had a ratio of",
    strong(textOutput("co2_pop_ratio_val", inline = T)), "million tons (meaning for
    each person in the country, an average of 0.000036 million tons of CO2 were
    emitted. This value is crucial in seeing how different types of calculations
    can result in differing results for countries with high CO2 emissions.
    Because Qatar is a small nation, their ratios may likely be higher even if
    their overall emissions rate in 2021 was not the highest."),
  p("The second value of interest I wanted to highlight was the country with
    the", em("lowest CO2 emissions rate in 2021."),
    strong(textOutput("co2_country_lowest", inline = T)), "had the lowest CO2
    emissions rate of 0.008 million tons. I thought this value was interesting
    because Tuvalu is a country rarely mentioned in the news, yet they are
    contributing the least amount of CO2 emissions. Their", em("change from the year
    with the lowest recorded CO2 emissions to highest emissions was"),
    strong(textOutput("co2_change_lowest", inline = T)), "million tons, similar to
    their 2021 total emissions. It is likely their low emissions is due to
    their country still developing and not being in industries that produce
    high rates of CO2, but their being the lowest and having such a small growth
    in emissions should be recognized. Their growth can be seen in the following
    line chart graphing their change from the earliest recorded to latest
    recorded year."),
  plotlyOutput("tuvalu_graph"),
  p("The last value of interest I wanted to highlight was the country
    with the", em("highest CO2 emissions rate in 2021."),
    strong(textOutput("co2_country_highest", inline = T)), "had the highest CO2 
    emissions of 11472.368 million tons. This value was not surprising but is
    valuable to note as China has become a quickly growing producer of CO2
    emissions each year. Their", em("change from the year with the lowest to the
    year with the highest recorded CO2 emissions was"),
    strong(textOutput("co2_change_highest", inline = T)), "million tons. The
    country has seen rapid development and, as a result, rapid increases in CO2
    emissions. Their growth can be seen in the line graph below."),
  plotlyOutput("china_graph")
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
  title = "CO2 Data Report - Salley Fang",
  text_page,
  widgets_page
)