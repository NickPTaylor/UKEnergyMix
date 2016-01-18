# load libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(decctools)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)

# list of energy types ---------------------------------------------------------
energy_type_choice <- list(
  "Gas",
  "Coal",
  "Nuclear",
  "Hydro",
  "Net Pumped",
  "Wind",
  "OCGT",
  "Oil",
  "Biomass",
  "French Int",
  "Dutch Int",
  "NI Int",
  "Eire Int"
)

# list of time increments to choose from ---------------------------------------
time_increment_choice <- list(
  "30 minutes" = 30 * 60,
  "1 hour" = 60 * 60,
  "12 hours" = 12 * 60 * 60,
  "1 day" = 24 * 60 * 60)

# user interface ---------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "UK Energy Mix"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(width = 8,
        box(
          title = "Background",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          p(
            "This application is a simple front end to the ", code("decctools"),
            " R package.  This package provides a variety of wrapper functions
            for accessing various data sources relating to  UK energy supply.
            A function is provided to access BM reports data.  The  BM report
            data is sourced from ",
            tags$a(href = "http://www.ref.org.uk/fuel/",
                   "Renewable Energy Foundation"),
            " which is somewhat more convenient than the API provided by the
            BM report.  The data source is free to access.  The user should
            refer to the", code('dectools'),
            tags$a(
              href = paste0("https://cran.r-project.org/",
                            "web/packages/decctools/decctools.pdf"),
              "documentation"),
            "for full details.  The objective of this R Shiny application is to
            provide an interface for non-R users to make use of the
            aforementioned BM report wrapper function."
          )
        ),
        box(
          title = "Instructions",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          collapsed = FALSE,
          p(
            "The user interacts with the app using the 'Controls'.  The user
            can select a date range for the data.  Note that the user ",
            strong("must"), " click the 'Reload Data' button after modifying
            this range.  Upon doing this, the data will be refreshed.
            Subsequently, the user can select a time increment of the data
            records and filter so as to only include specified energy sources.
            The plot takes some time to update when data is loading; it will
            be 'greyed out' during this time.  After the data has loaded and
            the 'controls' are set, the user may download the filtered data
            in CSV format (to Excel)."
          ),
          p(em(
            "Please note that this application is provided simply to
            demonstrate proof of concept.  Currently, a testing framework is
            not implemented and the output should be treated with caution."
          ))
        ),
        box(
          title = "Plot",
          width = NULL, solidHeader = TRUE,
          status = "primary",
          plotOutput(
            outputId = "plot"
          )
        )
      ),
      column(width = 4,
        box(
          title = "Controls",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          dateRangeInput(
            inputId = "date_range",
            label = "Select date range:",
            start = Sys.Date() - 7,
            end = Sys.Date(),
            min = "2009-01-01",
            max = Sys.Date()
          ),
          actionButton(
            inputId = "get_data",
            label = "Reload Data"
          ),
          p(),
          selectInput(
            inputId = "time_increment",
            label = "Select time increment:",
            choices = time_increment_choice
          ),
          checkboxGroupInput(
            inputId = "type_select",
            label = "Select energry sources:",
            choices = energy_type_choice,
            selected = c("Gas", "Coal", "Nuclear", "Wind", "Oil")
          ),
          downloadButton(
            outputId = "download",
            label = "Download Filtered Data"
          )
        )
      )
    )
  )
)

# server -----------------------------------------------------------------------
server <- function(input, output) {

  # load data based on user specified date -------------------------------------
  range_data <- reactive({

    # range data ONLY changes if user requests reload --------------------------
    input$get_data

    # isolate from reacting to other changes in UI -----------------------------
    isolate({
      data = get_grid_mix(input$date_range[1], input$date_range[2])
      init_timestamp = data %>% .$datetime
      timestamp_range = init_timestamp %>% range
      list(
        data=data,
        timestamp_range = timestamp_range
      )
    })
  })

  # data is filtered by type and time increment --------------------------------
  filtered_data <- reactive({

    # filter by type -----------------------------------------------------------
    by_type_data <- range_data()$data %>%
      select(one_of("datetime", input$type_select)) %>%
      rename(Time = datetime)

    # filter by timestamp ------------------------------------------------------
    reduced_timestamp <- seq(
      range_data()$timestamp_range[1],
      range_data()$timestamp_range[2],
      by = as.numeric(input$time_increment)
    )
    dt1 <- data.table(by_type_data, key = "Time")
    dt2 <- data.table(Time = reduced_timestamp, key = "Time")

    # return filtered data
    dt1[dt2, roll = "nearest"]
  })

  # download data --------------------------------------------------------------
  output$download <- downloadHandler(
    filename = "output.csv",
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )

  # display plot ---------------------------------------------------------------
  output$plot <- renderPlot({
    ggplot(filtered_data() %>% gather(Type, Output, -Time),
      aes(x = Time, y = Output, color = Type)) +
      geom_line() +
      scale_x_datetime(
      breaks = date_breaks("12 hours"), labels = date_format("%d-%m %H:%M")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("Electricity Output by Fuel Type") + ylab("Output (MW)")
  })
}

# invoke app -------------------------------------------------------------------
shinyApp(ui, server)
