library(dplyr)
library(readxl)
library(openxlsx)
library(shiny)
library(tidyr)
library(tidyverse)
library(janitor)
library(sf)
library(geosphere)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(ggplot2)
library(ggtext)
library(leaflet)
library(leafpop)
library(ggspatial)
library(ggdist)
library(scales)


#### ui section
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
           .title-panel{
           text-align: center;
           background-color: navy;
           color: white;
           }
           .custom-box {
           border: 1px solid black;
           flex: 1;
           display: flex;
           justify-content: center;
           align-items: center;
           }
           ")
    )
  ),
  div(class = "title-panel", titlePanel("World Population Analysis")),
  
  navbarPage("Dashboard",
             tabPanel("By Region",

                      fluidRow(
                        
                        column(4, plotlyOutput("net_change_plot")),
                        column(4, plotlyOutput("net_migrant")),
                        column(4, plotlyOutput("Total_pop_reg")),
                        column(4, plotlyOutput("avg_fertility")),
                        column(4, plotlyOutput("piechart_share")),
                        column(4, DT::dataTableOutput("land_area_table")),
                        column(6, plotlyOutput("map_continent")),
                        column(3, DT::dataTableOutput("ave_age")),
                        column(3, DT::dataTableOutput("Total_pop_T3"))
                      )
                      )
    
  )
)


### server
server <- function(input, output, session){
  
  
  #####Loading data 
 world_count_stat <- read_excel("world_country_stats.xlsx")
 world_Pop_2023 <- read_excel("world_population_by_country_2023.xlsx")
 continent_map <- read_sf("World_Continents.shp")

  
  
  #### Joining data frames on the country basis
  world_pop_analysis <-  full_join(
    world_Pop_2023,
    world_count_stat |> transmute(country, region),
    by = "country"
  )
  
  
  ### Including Australia in the continents
  world_pop_analysis <- world_pop_analysis |>
    mutate(region = case_when(country=="Australia" ~ "Australia", TRUE~as.character(region)))
  
  ### Changing Latin America into South America
  world_pop_analysis <- world_pop_analysis |>
    mutate(region = case_when(region == "Latin America" ~ "South America", TRUE~as.character(region)))
  
  #### Cleaning by swapping the NA's with 0's
  world_pop_analysis <- world_pop_analysis |>
    mutate(fertility_rate = case_when(is.na(fertility_rate)~0, TRUE~as.double(fertility_rate)))
  
  
  world_pop_analysis <- world_pop_analysis |>
    mutate(median_age = case_when(is.na(median_age) ~0, TRUE ~ as.double(median_age)))
  
  
  
  ##### Analysis ############
  ### sum of net change by region
  output$net_change_plot <- renderPlotly({
    B1 = world_pop_analysis %>%
      group_by(region) %>%
      summarise(total_net_change = sum(net_change)) %>%
      ggplot(aes(x = total_net_change, y = reorder(region, total_net_change))) +
      geom_bar(stat = "identity", fill = "#070db5") +
      labs(title = "Net change by region") +
      geom_text(aes(label = format(total_net_change, big.mark = ",")),
                vjust = -0.5, size = 2.75, color = "black") +
      theme(axis.text.y = element_text(hjust = 0.5),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            plot.title.position = "plot",
            plot.title = element_text(hjust = 0.5, color = "#070db5"))
  })
 
  ### sum of net_migrants by region
  output$net_migrant <- renderPlotly({
    B2 = world_pop_analysis |>
      group_by(region) |>
      summarise(total_net_migrants = sum(net_migrants)) |>
      ggplot(aes(x = total_net_migrants, y = reorder(region, total_net_migrants))) +
      geom_bar(stat = "identity", fill = "#070db5") +
      geom_text(aes(label = format(total_net_migrants, big.mark = ",")),
                vjust = -0.5, size = 2.75, color = "black") +
      labs(title = "sum of net_migrants by region") +
      theme(axis.text.y = element_text(hjust = 0.5),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            plot.title.position = "plot",
            plot.title = element_text(hjust = 0.5, color = "#070db5"))
    
  })
  
  
  
  ### average fertility rate by region
  output$avg_fertility <- renderPlotly({
    C1 = world_pop_analysis |>
      group_by(region) |>
      summarise(ave_fertility = mean(fertility_rate)) |>
      ggplot(aes(x = reorder(region, ave_fertility), y = ave_fertility)) +
      geom_col(fill = "#070db5")+
      geom_text(aes(label = format(ave_fertility, big.mark = ",")),
                vjust = -0.5, size = 2.75) +
      labs(title = "Average fertility rate by region") +
      theme(axis.text.x = element_text(hjust = 0.5, angle = 45),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            plot.title.position = "plot",
            plot.title = element_text(hjust = 0.5, color = "#070db5"))
  })
  
  
  ### Building the map
  joined_data <- world_pop_analysis |>
    group_by(region) |>
    summarise(Total_pop = sum(population))
  
  ### Joining the shapefile with the dataframe
  map_joined <- continent_map |>
    full_join(joined_data, by = c("CONTINENT"="region"))
  #### Map plotting
  output$map_continent <- renderPlotly({
    M1 = ### plot the map
      ggplot(map_joined) +
      geom_sf(aes(fill = Total_pop)) +
      geom_sf_text(aes(label = CONTINENT), color = "white", size = 3, nudge_y = 0.1) +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Total population") +
      labs(title = "Total population by Region") +
      ## Add a scale bar
      annotation_scale(location = "bl", width_hint = 0.2, text_cex = 1) +
      theme(plot.title.position = "plot",
            plot.title = element_text(hjust = 0.5, color = "#070db5"))
  })
  
  #### Total population by region
  output$Total_pop_reg <- renderPlotly({
    C2 = world_pop_analysis |>
      group_by(region) |>
      summarise(Total_pop = sum(population)) |>
      ggplot(aes(x = region, y = Total_pop)) +
      geom_col(fill = "#070db5") +
      geom_text(aes(label = format(Total_pop, big.mark = ",")),
                vjust = -0.5, size = 2.75) +
      labs(title = "Total population by region") +
      theme(axis.text.x = element_text(hjust = 0.5, angle = 45),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            plot.title.position = "plot",
            plot.title = element_text(hjust = 0.5, color = "#070db5"))
  })
  
  
  #### Piechart - worldshare by region
  pie_data_summary <- world_pop_analysis %>%
    group_by(region) %>%
    summarise(Total_worldshare = sum(world_share))
  
  region_colors <- c("navy", "blue", "skyblue", "orange", "purple", "#430266", "brown")
  
  output$piechart_share <- renderPlotly({
    pie_chart <- plot_ly(pie_data_summary, labels = ~region, values = ~Total_worldshare, type = "pie", marker = list(colors = region_colors)) %>%
      layout(title = list(text = "Worldshare by region", font = list(hjust = 0.5 , color = "#070db5")), showlegend = TRUE)
    
    pie_chart
  })
  
  
  ##### Total land Area table
  output$land_area_table <- DT::renderDataTable({
    T1 <- world_pop_analysis %>%
      group_by(region) %>%
      summarise(Total_Land_Area = sum(land_area))
    
    datatable(T1, options = list(paging = FALSE))
  })
  
  
  #### Average age by region
  output$ave_age <- renderDataTable({
    T2 = world_pop_analysis |>
      group_by(region) |>
      summarise(Average_age = mean(median_age))
    
    DT::datatable(T2, options = list(paging = FALSE))
  })
  
  
  ### Sum of population by region
  output$Total_pop_T3 <- renderDataTable({
    T3 = world_pop_analysis |>
      group_by(region) |>
      summarise(Total_population = sum(population))
    
    DT::datatable(T3, options = list(paging = FALSE))
  })
  
  
  ### Data Manipulation for the Card boxes
  #infPop <-  sum(world_pop_analysis$population,  na.rm = TRUE, na.rm = TRUE)
  
  ### Card box for the population
  #output$info1 <- valueBoxOutput({
  #  infPop <- world_pop_analysis |>
    #  summarise(infPop = sum(population)) |>
    #  pull(infPop)
    
     # valueBox(
     #   formatC(infPop,  big.mark = ','),
      #  'Total Population',
      #  icon = icon("users"),
     #   color = "navy"
     # )
    
 # })
  
  
  
  
}



### combining the ui and the server
shinyApp(ui, server)