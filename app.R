library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyverse)


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
energy_data <- read_csv("data/world_energy.csv")
energy_data <- energy_data %>% filter(energy_type != "total" & energy_type != "all_renewable")
energy_data <- energy_data %>% filter(country_code != "WORL")

#country_code_list = list(set(energy_data.country_code.unique()) - set("WORL")) 


app$layout(
  dbcContainer(
    list(
      dccGraph(id='plot-area'),
      htmlDiv(id='output-area'),
      dccDropdown(
        id='col-select',
        options = purrr::map(t(energy_data %>% select(country_code) %>% unique()), function(col) list(label = col, value = col)),
        value='AFG')
    )
  )
)
purrr::map(msleep %>% colnames, function(col) list(label = col, value = col))
app$callback(
  output('plot-area', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    p <- energy_data %>% filter(country_code == xcol) %>% 
      ggplot() +
      aes(x = year,
          y = energy,
          color = energy_type) +
      geom_line() +
      ggthemes::scale_color_tableau() +
      labs(fill = "Energy type")
    ggplotly(p) %>% layout(dragmode = 'select')
  }
)

# app$callback(
#   output('output-area', 'children'),
#   list(input('plot-area', 'selectedData')),
#   function(selected_data) {
#     toString(selected_data)
#   }
# )

app$run_server(host = '0.0.0.0', debug = F)
