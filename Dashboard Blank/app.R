# Class 2
# Shiny Dashboard Example

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

starwars <- starwars
starwars$films <- NULL
starwars$vehicles <- NULL
starwars$starships <- NULL
meltwars <- melt(starwars, id = "name")
meltwars$name <- as.factor(meltwars$name)

pdf(NULL)

header <- dashboardHeader(title = "Star Wars Dashboard",

                        )

sidebar <- dashboardSidebar(
  sidebarMenu(
   id = "tabs",

  )
)

body <- dashboardBody(
  tabItems(
    
  )
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  output$plot_mass <- renderPlotly({
    dat <- subset(meltwars, name %in% input$char_select & variable == "mass")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  output$plot_height <- renderPlotly({
    dat <- subset(meltwars, name %in% input$char_select & variable == "height")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  output$table <- DT::renderDataTable({
    subset(starwars, name %in% input$char_select, select = c(name, height, mass, birth_year, homeworld, species))
  })
  output$mass <- renderInfoBox({
    sw <- subset(starwars, name %in% input$char_select)
    num <- round(mean(sw$mass, na.rm = T), 2)
    
    infoBox("Avg Mass", value = num, subtitle = paste(nrow(sw), "characters"), icon = icon("balance-scale"), color = "purple")
  })
  output$height <- renderValueBox({
    sw <- subset(starwars, name %in% input$char_select)
    num <- round(mean(sw$height, na.rm = T), 2)
    
    valueBox(subtitle = "Avg Height", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)