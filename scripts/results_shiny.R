library(shiny)
library(tidyverse)
library(flexdashboard)
library(plotly)
library(DT)
library(crosstalk)
library(hrbrthemes)
setwd("C:/Users/Sydney Robinson/Desktop/NFL/Big_Data_Bowl/2021/oh_snap/")

tpoe <- read.csv("./data/target_prob/small/tpoe_player_rankings.csv")
dpoe <- read.csv("./data/catch_prob/dpoe_arrival_player_rankings.csv")

tpoe_dpoe_df <- tpoe %>% 
  full_join(dpoe, by = c("nfl_id"="nfl_id", "grp"="grp")) %>% 
  select(nfl_id, display_name.x, grp, tpoe, rnk_grp, dpoe_arr, group_rank) %>% 
  filter(grp == "CB" | grp == "S") %>% 
  mutate(tpoe = round(tpoe, digits = 2)) %>% 
  mutate(dpoe_arr = round(dpoe_arr, digits = 3)) %>% 
  na.omit()

ui <- fluidPage(
  fluidRow(
    column(6, plotlyOutput("dpoe_tpoe_graph")),
    column(6, DT::dataTableOutput("plot_data"))
  )
)

server <- function(input, output, session) {
  jittered_plot_data <- reactive({
    tpoe_dpoe_df
  })
  
  shared_data <- SharedData$new(jittered_plot_data)
  
  output$dpoe_tpoe_graph <- renderPlotly({
    p <- shared_data %>% 
      ggplot(aes(x = tpoe, y = dpoe_arr, color = grp, label = display_name.x)) +
      geom_point() + 
      scale_color_manual(values=c("#ffa3af", "#132f3c")) +
      xlab("dTPOE") +
      ylab("dCPOE")+
      theme_ipsum() +
      ggtitle("2018 Defensive Back's Coverage Ability", subtitle = "Click, hover, or drag to look more specifically on any part of the graph or take a subset") +
      theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
      
    ggplotly(p) %>% 
      highlight("plotly_selected") %>% 
      layout(legend=list(title=list(text='<b> Position </b>')))
  })
  
  output$plot_data <- DT::renderDataTable({
    DT::datatable(shared_data, options = list(dom = "Blrftip", columnDefs = list(list(visible=F, targets=c(1)))),
                style = "bootstrap", class = "compact", width = "100%", 
                colnames = c('Player' = 'display_name.x', 'Position'='grp', 'dCPOE'='dpoe_arr', 'dTPOE' = 'tpoe', 'dTPOE Position Rank' = 'rnk_grp',  'dCPOE Position Rank' = 'group_rank'))
  }, server = FALSE)
}


shinyApp(ui, server)

