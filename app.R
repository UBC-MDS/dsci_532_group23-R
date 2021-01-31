library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyverse)


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
world_df <-
  read.csv(
    "https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv"
  )

# Data pre-processing and cleaning
country_df <- read_csv("data/country_conv.csv")
population_df <- read_csv("data/world_population.csv")
names(population_df) <- sub(" ", "_", names(population_df))
energy_data <- read_csv("data/world_energy.csv")
energy_data <-
  energy_data %>% filter(energy_type != "total" &
                           energy_type != "all_renewable")
energy_data <- energy_data %>% filter(country_code != "WORL")
energy <- left_join(energy_data, country_df, by = "country_code")
energy <-
  left_join(
    energy,
    population_df,
    by = c(
      "year" = "Year",
      "country_code" = "Country_Code",
      "country" = "Country_Name"
    )
  )
energy_world_df <-
  left_join(world_df,
            energy,
            by = c("CODE" = "country_code", "COUNTRY" = "country")) %>% select(-GDP..BILLIONS.)
names(energy_world_df)[1] <- "country_name"
names(energy_world_df)[2] <- "country_code"
names(energy_world_df)[6] <- "population"



app$layout(dbcContainer(list(
  dbcRow(list(
    dbcCol(list(
      dccGraph(id = 'map_plot'),
      htmlBr(),
      dccSlider(
        id = 'select_col',
        min = 1980,
        max = 2018,
        marks = list(
          "1980" = "1980",
          "1982" = "1982",
          "1984" = "1984",
          "1986" = "1986",
          "1988" = "1988",
          "1990" = "1990",
          "1992" = "1992",
          "1994" = "1994",
          "1996" = "1996",
          "1998" = "1998",
          "2000" = "2000",
          "2002" = "2002",
          "2004" = "2004",
          "2006" = "2006",
          "2008" = "2008",
          "2010" = "2010",
          "2012" = "2012",
          "2014" = "2014",
          "2016" = "2016",
          "2018" = "2018"
        ),
        value = 1980
      )
      ), width = 8),
    
    dbcCol(list(
      dccGraph(id = 'barplot'),
      dccDropdown(
        id = 'type-select',
        options = purrr::map(t(energy_data %>% select(energy_type) %>% unique()), function(col)
          list(label = col, value = col)),
        value = 'natural_gas')
      ), width = 4
      
    )
    
    )),
  
  dbcRow(list(
    
    dbcCol(
      dccDropdown(
        id = 'col-select',
        options = purrr::map(t(energy_data %>% select(country_code) %>% unique()), function(col)
          list(label = col, value = col)),
        value = 'AFG'), width = 2
    ),
    
    dbcCol(
      dccGraph(id = 'plot-area'))
    
    ), style = list(borderRadius = 0))
)))





app$callback(output('map_plot', 'figure'),
             list(input('select_col', 'value')),
             function(time_col) {
               p <- energy_world_df %>% filter(year == time_col)
               p <- plot_ly(
                 p,
                 type = 'choropleth',
                 locations =  ~ country_code,
                 z =  ~ energy,
                 height = 6,
                 colors = c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404"),
                 text = ~ country_name
               ) %>% layout(title = paste('World Energy Consumption (', time_col,")"))
               p 
             })




app$callback(output('barplot', 'figure'),
             list(input('type-select', 'value'),
                  input('select_col', 'value')),
             function(type_col, time_col) {
               p <-  energy_world_df %>% 
                 filter(year == time_col) %>% 
                 group_by(energy_type, country_name) %>%
                 summarise(energy_consumption = sum(energy, na.rm = TRUE)) %>%
                 arrange(desc(energy_consumption)) %>%
                 filter(energy_type == type_col) %>% slice_max(energy_consumption, n = 10) %>%
                 ggplot() +
                   aes(x = energy_consumption, y = reorder(country_name, energy_consumption), fill = country_name) +
                 geom_col( show.legend = FALSE) +
                 labs(x = "Energy (Top 10)", y = " ") +
                 theme(axis.text = element_text(size = 8),
                       axis.title = element_text(size = 10),
                       legend.position = 'none')
               
               ggplotly(p) %>% layout(dragmode = 'select')
             })
 

app$callback(output('plot-area', 'figure'),
             list(input('col-select', 'value')),
             function(xcol) {
               p <- energy_data %>% filter(country_code == xcol) %>%
                 ggplot() +
                 aes(
                   x = year,
                   y = energy,
                   fill = energy_type,
                   color = energy_type
                 ) +
                 geom_line() +
                 ggthemes::scale_color_tableau() +
                 labs(x = "Year", y = "Energy Consumption (Quad BTU)") +
                 theme(axis.text = element_text(size = 9),
                       axis.title = element_text(size = 11))
               
               ggplotly(p) %>% layout(dragmode = 'select')
             })


app$run_server(host = '0.0.0.0', debug = F)
