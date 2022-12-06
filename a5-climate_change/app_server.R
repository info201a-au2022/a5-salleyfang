library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(maps)

# co2_df <- read.csv(
#                   file = "../owid-co2-data.csv",
#                   stringsAsFactors = FALSE, header = TRUE
#                   )
# co2_pop_max_co2_change <- co2_df %>% 
#   filter(country == "Qatar", na.rm = TRUE) %>% 
#   select(co2) %>% 
#   filter(co2 > 0.0, na.rm = TRUE) %>% 
#   mutate(co2_change = max(co2) - min(co2)) %>% 
#   summarise(co2_change = round(mean(co2_change), digits = 2)) %>% 
#   pull(co2_change)

# highest_co2_capita <- co2_df %>% 
#   filter(year == 2021, na.rm = TRUE) %>% 
#   filter(iso_code != "", na.rm = TRUE) %>% 
#   filter(!is.na(co2_per_capita), na.rm = TRUE) %>% 
#   select(country, co2_per_capita) %>% 
#   filter(co2_per_capita == max(co2_per_capita, na.rm = TRUE)) %>% 
#   pull(country)
#   
# lowest_co2_capita <- co2_df %>% 
#   filter(year == 2021, na.rm = TRUE) %>% 
#   filter(iso_code != "", na.rm = TRUE) %>% 
#   filter(!is.na(co2_per_capita), na.rm = TRUE) %>% 
#   filter(co2_per_capita > 0.0, na.rm = TRUE) %>% 
#   select(country, co2_per_capita) %>% 
#   filter(co2_per_capita == min(co2_per_capita, na.rm = TRUE)) %>% 
#   pull(country)
# world_data <- map_data("world") %>%
#   rename("country" = "region")

# country_df <- co2_df %>% 
#   filter(iso_code != "", na.rm = TRUE) %>% 
#   filter(year == 2021, na.rm = TRUE) %>% 
#   select(country, iso_code, co2, co2_per_capita, cumulative_co2,
#          energy_per_capita, share_global_co2) %>%
#   mutate(country = ifelse(country == "United Kingdom", "UK", country)) %>%
#   mutate(country = ifelse(country == "United States", "USA", country)) %>%
#   mutate(country = ifelse(country == "British Virgin Islands", "Virgin Islands",
#                           country)) %>%
#   mutate(country = ifelse(country == "United States Virgin Islands",
#                           "Virgin Islands", country)) %>% 
#   mutate(country = ifelse(country == "Democratic Republic of Congo",
#                           "Democratic Republic of the Congo", country))
# 
# world_country_combined <- left_join(country_df, world_data, by = "country") %>%
#   select(long, lat, country, iso_code, co2, co2_per_capita, cumulative_co2,
#          energy_per_capita, share_global_co2)
# ggplot() +
#   geom_polygon(data = world_country_combined, aes(fill = co2, x = long, y = lat, group = country)) +
#   theme_void() +
#   coord_map()
# 
# ?geom_map()
# ggplot() +
#   geom_map(aes(map_id = country, fill = co2), map = world_data, data = world_country_combined) +
#   expand_limits(x = world_data$long,
#                 y = world_data$lat)
# map('world', fill = world_data$co2, col = 1:10)
# help(package='maps')

# create_map <- function(data, var) {
#   p <- ggplot(data, aes(x = long, y = lat, group = country)) +
#        geom_polygon(aes(fill = data[,var]), color = "black", size = 0.3) +
#        scale_fill_continuous(low = "deepskyblue4", high = "brown1",
#                           na.value = "Gray") +
#        labs(fill = "Frequency") +
#        theme(legend.title = element_text(size = 9),
#              legend.key.size = unit(0.4, "cm")) +
#        labs(title = "World Map of Variable Concentrations in 2021")
#   return(p)
# } 
# co2_df <- read.csv(
#   file = "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv",
#   stringsAsFactors = FALSE, header = TRUE
# )

co2_df <- read.csv(file = "owid-co2-data.csv")

co2_type_df <- co2_df %>%
  filter(iso_code != "", na.rm = TRUE) %>% 
  select(country, year, cement_co2, coal_co2, flaring_co2, gas_co2,
         oil_co2, trade_co2, cumulative_co2) %>% 
  replace(is.na(.), 0)

country_co2_df <- co2_df %>% 
  filter(iso_code != "", na.rm = TRUE) %>%
  select(country, iso_code, year, co2) %>%
  replace(is.na(.), 0)

shiny_server <- function(input, output) {
  output$plot <- renderPlotly({
    p <- ggplot(data = co2_type_df, mapping = aes(year, cumulative_co2)) +
      geom_point(color = "blue", size = 0.5) +
      xlim(input$min_year, input$max_year) +
      ylim(input$min_co2, input$max_co2) +
      labs(x = "Year" , y = "Cumulative CO2 Emissions") +
      scale_y_continuous(labels = scales::comma) +
      ggtitle("Cumulative CO2 Emissions By Year (1850-2021)")
    return(p)
  })
  
  # output$map <- renderPlotly({
  #   # p <- ggplot() +
  #   #   geom_map(aes(map_id = country), map = world_country_combined,
  #   #            data = world_country_combined) +
  #   #   expand_limits(x = world_country_combined$long,
  #   #                 y = world_country_combined$lat)
  #   # p <- world_country_combined %>%
  #   #   ggplot(aes(fill = input$mapvar, map_id = country)) +
  #   #   geom_map(map = world_data) +
  #   #   expand_limits(x = long, y = lat) +
  #   #   theme_map()
  #   # return(p)
  #   return(create_map(world_country_combined, input$mapvar))
  # })
  
  output$graph <- renderPlotly({
    p <- ggplot(data = country_co2_df %>% filter(country == input$country),
                aes(x = year, y = co2)) +
         geom_line(aes(color = iso_code)) +
         labs(x = "Year", y = "CO2 Emissions (yearly)") +
         ggtitle("Yearly CO2 Emissions By Country")
    return(p)
  })
  
  output$co2_pop_ratio <- renderText({
    co2_to_population <- co2_df %>% 
      filter(year == 2021, na.rm = TRUE) %>% 
      filter(iso_code != "", na.rm = TRUE) %>% 
      select(country, population, co2) %>% 
      mutate(co2_pop_ratio = co2 / population) 
    co2_pop_max <- co2_to_population %>% 
      filter(co2_pop_ratio == max(co2_pop_ratio, na.rm = TRUE)) %>% 
      pull(country)
    ratio_text <- paste0("The first relevant value of interest was the country
                          with the highest CO2 to population ratio: ",
                          co2_pop_max, ". This value is useful in understanding
                          the place where there is a high (or highest) amount
                          of CO2 emissions per person in the population.")
    ratio_text
  })

  output$co2_country_lowest <- renderText({
    current_co2_lowest <- co2_df %>% 
      filter(year == 2021, na.rm = TRUE) %>% 
      filter(iso_code != "", na.rm = TRUE) %>% 
      select(country, co2) %>% 
      filter(co2 == min(co2, na.rm = TRUE)) %>% 
      pull(country)
    current_co2_lowest
  })
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
  
  output$co2_country_highest <- renderText({
    current_co2_highest <- co2_df %>% 
      filter(year == 2021, na.rm = TRUE) %>% 
      filter(iso_code != "", na.rm = TRUE) %>% 
      select(country, co2) %>% 
      filter(co2 == max(co2, na.rm = TRUE)) %>% 
      pull(country)
    current_co2_highest
  })
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
}
