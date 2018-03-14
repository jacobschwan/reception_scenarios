#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(googlesheets)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Reception Food"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("dessert_display",
                     "Dessert Displays:",
                     min = 0,
                     max = 20,
                     value = 5),
         sliderInput("chips",
                     "Tortilla Chips:",
                     min = 0,
                     max = 10,
                     value = 5),
         sliderInput("nuts",
                     "Mixed Nuts:",
                     min = 0,
                     max = 10,
                     value = 5),
         sliderInput("t_mix",
                     "Trail Mix:",
                     min = 0,
                     max = 10,
                     value = 5),
         sliderInput("donuts",
                     "Donuts:",
                     min = 0,
                     max = 30,
                     value = 20),
         sliderInput("pizza",
                     "Pizza:",
                     min = 0,
                     max = 50,
                     value = 20),
         sliderInput("tenders",
                     "Chicken Tenders:",
                     min = 0,
                     max = 300,
                     value = 150),
         sliderInput("tots",
                     "Tater Tots:",
                     min = 0,
                     max = 300,
                     value = 150),
         sliderInput("mac",
                     "Mac & Cheese:",
                     min = 0,
                     max = 300,
                     value = 150)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("item_list"),
         tableOutput("scenario_summary")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    reception <- gs_title("Reception Food/Bev")
    food <- gs_read(reception, ws = "food")
    
    output$item_list <- renderTable({
        units_vec <- c(input$dessert_display,
                       input$chips,
                       input$nuts,
                       input$t_mix,
                       input$donuts,
                       input$pizza,
                       input$tenders,
                       input$tots,
                       input$mac)
        
        food %>%
            mutate(units = units_vec) %>%
            mutate(base_cost = units * unit_price) %>%
            mutate(sc_cost = (base_cost * .22) * 1.0753,
                   base_tax = base_cost * 1.0753) %>%
            mutate(total_cost = base_cost + sc_cost + base_tax,
                   people_served = people * units) %>%
        select(item, units, total_cost, people_served)})
    
    output$scenario_summary <- renderTable({
        units_vec <- c(input$dessert_display,
                       input$chips,
                       input$nuts,
                       input$t_mix,
                       input$donuts,
                       input$pizza,
                       input$tenders,
                       input$tots,
                       input$mac)
        
        food %>%
            mutate(units = units_vec) %>%
            mutate(base_cost = units * unit_price) %>%
            mutate(sc_cost = (base_cost * .22) * 1.0753,
                   base_tax = base_cost * 1.0753) %>%
            mutate(total_cost = base_cost + sc_cost + base_tax,
                   people_served = people * units) %>%
        group_by(category) %>%
        summarise(total_cost = sum(total_cost),
                  people_served = sum(people_served, na.rm = T)) %>%
        janitor::add_totals_row()})
 
}

# Run the application 
shinyApp(ui = ui, server = server)

