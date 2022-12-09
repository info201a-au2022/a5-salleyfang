library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(rsconnect)
library(reshape2)
library(shinyWidgets)

shiny_server <- function(input, output) {
  co2_df <- read_csv("owid-co2-data.csv")
  
  # VALUES OF INTEREST (Page 1)
  # co2_pop_ratio_country: Qatar has the highest co2 to population ratio in 2021
  output$co2_pop_ratio_country <- renderText({
    co2_to_population <- co2_df %>% 
      filter(year == 2021, na.rm = TRUE) %>% 
      filter(iso_code != "", na.rm = TRUE) %>% 
      select(country, population, co2) %>% 
      mutate(co2_pop_ratio = co2 / population) 
    co2_pop_max_country <- co2_to_population %>% 
      filter(co2_pop_ratio == max(co2_pop_ratio, na.rm = TRUE)) %>% 
      pull(country)
    co2_pop_max_country
  })
  # co2_pop_ratio_val: Qatar's co2 to population ratio in 2021 was
  output$co2_pop_ratio_val <- renderText({
    co2_to_population <- co2_df %>% 
      filter(year == 2021, na.rm = TRUE) %>% 
      filter(iso_code != "", na.rm = TRUE) %>% 
      select(country, population, co2) %>% 
      mutate(co2_pop_ratio = co2 / population) 
    co2_pop_max_val <- co2_to_population %>% 
      filter(co2_pop_ratio == max(co2_pop_ratio, na.rm = TRUE)) %>% 
      pull(co2_pop_ratio)
    co2_pop_max_val <- round(co2_pop_max_val, digits = 6)
    co2_pop_max_val <- format(co2_pop_max_val, scientific = FALSE)
    co2_pop_max_val
  })
  # co2_country_lowest: Tuvalu has the lowest co2 emissions rates in 2021
  output$co2_country_lowest <- renderText({
    current_co2_lowest <- co2_df %>% 
      filter(year == 2021, na.rm = TRUE) %>% 
      filter(iso_code != "", na.rm = TRUE) %>% 
      select(country, co2) %>% 
      filter(co2 == min(co2, na.rm = TRUE)) %>% 
      pull(country)
    current_co2_lowest
  })
  # co2_change_lowest: 0.007 million tons is the co2 emissions change from
  # the the year with the min co2 emissions to max co2 emissions in Tuvalu
  output$co2_change_lowest <- renderText({
    country_co2_change_lowest <- co2_df %>% 
      filter(country == "Tuvalu", na.rm = TRUE) %>% 
      select(co2) %>% 
      filter(co2 > 0.0, na.rm = TRUE) %>% 
      mutate(co2_change = max(co2) - min(co2)) %>% 
      summarise(co2_change = round(mean(co2_change), digits = 5)) %>% 
      pull(co2_change)
    country_co2_change_lowest
  })
  # tuvalu_graph: graph of tuvalu's annual co2 emissions
  output$tuvalu_graph <- renderPlotly({
    co2_tuvalu <- co2_df %>% 
      filter(country == "Tuvalu", na.rm = TRUE) %>% 
      filter(co2 > 0.0, na.rm = TRUE) %>% 
      select(year, co2)
    p <- ggplot(co2_tuvalu, aes(x = year, y = co2)) +
      geom_line() +
      labs(x = "Year", y = "CO2 Emissions (million tons)") +
      ggtitle("CO2 Emissions Change in Tuvalu")
  })
  # co2_country_highest: China has the highest co2 emissions rates in 2021
  output$co2_country_highest <- renderText({
    current_co2_highest <- co2_df %>% 
      filter(year == 2021, na.rm = TRUE) %>% 
      filter(iso_code != "", na.rm = TRUE) %>% 
      select(country, co2) %>% 
      filter(co2 == max(co2, na.rm = TRUE)) %>% 
      pull(country)
    current_co2_highest
  })
  # co2_change_highest: 11455.34 million tons is the min to max change in co2
  # emissions in China 
  output$co2_change_highest <- renderText({
    country_co2_change_highest <- co2_df %>% 
      filter(country == "China", na.rm = TRUE) %>% 
      select(co2) %>% 
      filter(co2 > 0.0, na.rm = TRUE) %>% 
      mutate(co2_change = max(co2) - min(co2)) %>% 
      summarise(co2_change = round(mean(co2_change), digits = 2)) %>% 
      pull(co2_change)
    country_co2_change_highest
  })
  # china_graph: graph of china's annual co2 emissions
  output$china_graph <- renderPlotly({
    co2_china <- co2_df %>% 
      filter(country == "China", na.rm = TRUE) %>% 
      filter(co2 > 0.0, na.rm = TRUE) %>% 
      select(year, co2)
    p <- ggplot(co2_china, aes(x = year, y = co2)) +
      geom_line() +
      labs(x = "Year", y = "CO2 Emissions (million tons)") +
      ggtitle("CO2 Emissions Change in China")
  })
  
  # CO2 Data Visualizations (Page 2)
  # DF of cumulative cement, coal, flaring, gas, oil, trade co2 emissions for
  # each country, users can pick the country to graph in bar chart
  co2_type_df <- co2_df %>%
    filter(iso_code != "", na.rm = TRUE) %>% 
    select(country, cement_co2, coal_co2, flaring_co2, gas_co2, oil_co2,
           trade_co2) %>% 
    replace(is.na(.), 0.0)  %>%
    group_by(country) %>%
    mutate(sum_cement = sum(cement_co2)) %>% 
    mutate(sum_coal = sum(coal_co2)) %>% 
    mutate(sum_flaring = sum(flaring_co2)) %>% 
    mutate(sum_gas = sum(gas_co2)) %>% 
    mutate(sum_oil = sum(oil_co2)) %>% 
    mutate(sum_trade = sum(trade_co2)) %>% 
    select(country, sum_cement, sum_coal, sum_flaring, sum_gas, sum_oil,
           sum_trade) %>% 
    distinct() %>% 
    pivot_longer(-country, names_to = "co2_type", values_to = "value")
  co2_type_df$co2_type <- str_replace_all(co2_type_df$co2_type,
                                          "sum_cement", "total cement")
  co2_type_df$co2_type <- str_replace_all(co2_type_df$co2_type,
                                          "sum_coal", "total coal")
  co2_type_df$co2_type <- str_replace_all(co2_type_df$co2_type,
                                          "sum_flaring", "total flaring")
  co2_type_df$co2_type <- str_replace_all(co2_type_df$co2_type,
                                          "sum_gas", "total gas")
  co2_type_df$co2_type <- str_replace_all(co2_type_df$co2_type,
                                          "sum_oil", "total oil")
  co2_type_df$co2_type <- str_replace_all(co2_type_df$co2_type,
                                          "sum_trade", "total trade")

  # Bar chart of emission type totals for a selected country
  output$barchart <- renderPlotly({
    p <- ggplot(co2_type_df %>%  filter(country == input$country),
                aes(x = input$country, y = value)) +
      geom_bar(aes(fill = co2_type), stat = "identity", position = "dodge") +
      labs(fill = "CO2 Emissions Type") +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Country", y = "CO2 Emissions (million tons)") +
      ggtitle("Total CO2 Emissions Per Type")
    return(p)
  })
  
  # DF of co2 emissions per year for each country, users can pick countries to
  # graph and compare trends for multiple at a time, users can select year
  country_co2_df <- co2_df %>% 
    filter(iso_code != "", na.rm = TRUE) %>%
    select(country, year, co2) %>%
    replace(is.na(.), 0.0)
  
  # Line graph of yearly co2 emission rates for countries
  output$graph <- renderPlotly({
    new_co2_df <- country_co2_df %>% 
      filter(country == input$countries) %>% 
      filter(year >= input$minyear, na.rm = TRUE) %>% 
      filter(year <= input$maxyear, na.rm = TRUE)
    p <- ggplot(data = new_co2_df, aes(x = year, y = co2)) +
         geom_line(aes(color = country)) +
         labs(x = "Year", y = "CO2 Emissions Yearly (million tons)") +
         ggtitle("Yearly CO2 Emissions By Country") 
    return(p)
  })
}
