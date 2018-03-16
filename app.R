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

reception <- gs_title("Reception Food/Bev")
mixed <- gs_read(reception, ws = "mixed")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Reception Food"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         mixed %>%
             select(inputId, label, min, max, value) %>%
             pmap(sliderInput)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          tableOutput("scenario_summary"),
          tableOutput("item_list")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    item_list <- reactive({
        units_vec <- map_dbl(mixed$inputId, function(x) {input[[x]]})
        
        mixed %>%
            mutate(units = units_vec,
                   people_served = 300) %>%
            mutate(base_cost = case_when(unit == "hours" ~ 
                                             people_served*(unit_price_1 + (units -1 ) * unit_price_2),
                                         TRUE ~ units * unit_price_1)) %>%
            mutate(sc_cost = (base_cost * .22) * 1.0753,
                   base_tax = base_cost * tax) %>%
            mutate(total_cost = base_cost + sc_cost + base_tax,
                   people_served = people * units) %>%
        select(item = label, units, unit, total_cost, people_served, category)})
    
    output$item_list <- renderTable({item_list() %>%
            select(-category)})
    
    output$scenario_summary <- renderTable({
        item_list() %>%
        group_by(category) %>%
        summarise(total_cost = sum(total_cost),
                  people_served = sum(people_served, na.rm = T)) %>%
        janitor::adorn_totals("row")})
 
}

# Run the application 
shinyApp(ui = ui, server = server)

