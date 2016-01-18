# load libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)

# user interface ---------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "UK Energy Mix"),
  dashboardSidebar(),
  dashboardBody()
)

# server -----------------------------------------------------------------------
server <- function(input, output) { }

# invoke app -------------------------------------------------------------------
shinyApp(ui, server)
