#libraries to include

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(scales)
library(DT)
library(tidyr)


# assume all of the tsv files in this directory are data of the same kind that I want to visualize
allData <- do.call(rbind, lapply(list.files(pattern = "*.tsv"), read.delim))

# convert the dates to the internal format
allData$fullDate <- allData$date
allData$newDate <- as.Date(allData$fullDate, "%m/%d/%Y")
allData$Date <- NULL

# add year day month column
allData <- separate(data = allData, col = date, into = c("month", "date", "year"), sep = "/")

# convert the columns to numeric
allData[ c("month", "date", "year")] <- sapply(allData[ c("month", "date", "year")],as.numeric)

# add new column for month that contains char
allData$monthChar[allData$month == 1] <- "Jan"
allData$monthChar[allData$month == 2] <- "Feb"
allData$monthChar[allData$month == 3] <- "Mar"
allData$monthChar[allData$month == 4] <- "Apr"
allData$monthChar[allData$month == 5] <- "May"
allData$monthChar[allData$month == 6] <- "Jun"
allData$monthChar[allData$month == 7] <- "Jul"
allData$monthChar[allData$month == 8] <- "Aug"
allData$monthChar[allData$month == 9] <- "Sep"
allData$monthChar[allData$month == 10] <- "Oct"
allData$monthChar[allData$month == 11] <- "Nov"
allData$monthChar[allData$month == 12] <- "Dec"

allData <- within(allData, {
  names <- reorder(month, seq_along(newDate), order = TRUE)
})

# parse to days in the week
allData$dayChar <- weekdays(allData$newDate)

# add new column for week that contains int
allData$day[allData$dayChar == "Monday"] <- 1
allData$day[allData$dayChar == "Tuesday"] <- 2
allData$day[allData$dayChar == "Wednesday"] <- 3
allData$day[allData$dayChar == "Thursday"] <- 4
allData$day[allData$dayChar == "Friday"] <- 5
allData$day[allData$dayChar == "Saturday"] <- 6
allData$day[allData$dayChar == "Sunday"] <- 7

# add new column for char entries that contains comma in the number
allData$ridesChar <- formatC(allData$rides, format = "d", big.mark = ",")

# turn to data frame
all_data_df <- data.frame(allData)
all_data_df$stationname[all_data_df$stationname == "OHare Airport"] <- "O'Hare Airport"

# Create the shiny application
ui <- dashboardPage(
  dashboardHeader(title = "CTA Data Visualization"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("About", tabName = "About"),
                     menuItem("Compare with table", tabName = "compare_table", selected = T)
                   )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "compare_table",
              sidebarLayout(position = "left",
                            sidebarPanel(
                              style = "margin-top:70%;",
                              fluidRow(
                                column(6, 
                                       div(checkboxGroupInput("time_frame",
                                                              "Time Frame",
                                                              choices = c("Year", "Month", "Week"),
                                                              selected = c("Year", "Month", "Week")
                                       )
                                       
                                       )
                                ),
                                column(6,
                                       div(selectInput("select_year",
                                                       "Year",
                                                       choices = c("Every", 2021:2001),
                                                       selected = c(2021)
                                                       )
                                           )
                                       )
                                ),
                              div(selectInput("select_station",
                                              "Station",
                                              choices = c("Every", "UIC-Halsted", "O'Hare Airport", "Rosemont"),
                                              selected = c("UIC-Halsted")
                                              )
                                  ),
                              width = 2
                            ),
                            mainPanel(
                              uiOutput("plots"),
                              width = 10
                              )
                            )
              ),
      tabItem(tabName = "About",
              h2("US Data")
              )
      )
    )
  )

server <- function(input, output) {
  
  # get column values reactive
  get_col_reactive <- reactive({
    year_col <- 0
    month_col <- 0
    week_col <- 0
    
    # decide how much space is required for each graph based on what is visible
    if (all(c("Year", "Month", "Week") %in% input$time_frame)) {
      year_col <- 4
      month_col <- 4
      week_col <- 4
    } else if (all(c("Month", "Week") %in% input$time_frame)) {
      year_col <- 0
      month_col <- 6
      week_col <- 6
      
    } else if (all(c("Year", "Week") %in% input$time_frame)) {
      year_col <- 6
      month_col <- 0
      week_col <- 6
      
    } else if (all(c("Year", "Month") %in% input$time_frame)) {
      year_col <- 6
      month_col <- 6
      week_col <- 0
      
    } else if (all(c("Year") %in% input$time_frame)) {
      year_col <- 12
      month_col <- 0
      week_col <- 0
      
    } else if (all(c("Month") %in% input$time_frame)) {
      year_col <- 0
      month_col <- 12
      week_col <- 0
      
    } else if (all(c("Week") %in% input$time_frame)) {
      year_col <- 0
      month_col <- 0
      week_col <- 12
    }
    
    return(list(year_col, month_col, week_col))
  })
  
  # create reactive dataframe for month and week
  df_Reactive_month_week <- reactive({
    # create dataframe for a specific year and station
    if (input$select_year != "Every") {
      if (input$select_station != "Every") {
        subset(all_data_df, all_data_df$year == input$select_year & all_data_df$stationname == input$select_station)
      } else {
        subset(all_data_df, all_data_df$year == input$select_year)
      }
      
    } else {
      if (input$select_station != "Every") {
        subset(all_data_df, all_data_df$stationname == input$select_station)
      } else {
        all_data_df
      }
    }
  })
  
  # create reactive dataframe for month and week
  df_Reactive_year <- reactive({
    if (input$select_station != "Every") {
      subset(all_data_df, all_data_df$stationname == input$select_station)
    } else {
      all_data_df
    }
  })
  
  # create graph to show yearly data
  output$entries_year_graph <- renderPlot({
      ggplot(data = df_Reactive_year(), aes(x = year, y = rides)) + 
      geom_bar(stat = "identity") +
      scale_x_continuous(breaks = seq(2001, 2021, by = 2)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Year",
           y = "Entries") +
      ggtitle(paste("Yearly Entries at", input$select_station, "CTA Station"))
  })
  
  # create graph to show monthly data
  output$entries_month_graph <- renderPlot({
    ggplot(data = df_Reactive_month_week(), aes(x = reorder(monthChar, month), y = rides)) + 
      geom_bar(stat = "identity") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Month",
           y = "Entries") +
      ggtitle(paste("Monthly Entries at", input$select_station, "CTA Station"))
  })
  
  # create graph to show weekly data
  output$entries_week <- renderPlot({
    ggplot(data = df_Reactive_month_week(), aes(x = reorder(dayChar, day), y = rides)) +
      geom_bar(stat = "identity") +
      labs(x = "Day",
             y = "Entries") +
      ggtitle(paste("Day of the week Entries at", input$select_station, "CTA Station"))
  })
  
  # create new Data frame to show yearly data
  entries_year_table <- reactive({
    # keep only following columns
    keep <- c("stationname", "year", "ridesChar")
    temp_df <- df_Reactive_year()[keep]
    
    # rename
    names(temp_df)[1] <- "Station"
    names(temp_df)[2] <- "Year"
    names(temp_df)[3] <- "Entries"
    
    temp_df
  })
  
  # create new Data frame to show monthly data
  entries_year_table <- reactive({
    # keep only following columns
    keep <- c("stationname", "year", "ridesChar")
    temp_df <- df_Reactive_month_week()[keep]
    
    # rename
    names(temp_df)[1] <- "Station"
    names(temp_df)[2] <- "Year"
    names(temp_df)[3] <- "Entries"
    
    temp_df
  })
  
  # create new Data frame to show monthly data
  entries_month_table <- reactive({
    # keep only following columns
    keep <- c("stationname", "monthChar", "ridesChar")
    temp_df <- df_Reactive_month_week()[keep]
    
    # rename
    names(temp_df)[1] <- "Station"
    names(temp_df)[2] <- "Month"
    names(temp_df)[3] <- "Entries"
    
    temp_df
  })
  
  # create new Data frame to show weekly data
  entries_week_table <- reactive({
    # keep only following columns
    keep <- c("stationname", "dayChar", "ridesChar")
    temp_df <- df_Reactive_month_week()[keep]
    
    # rename
    names(temp_df)[1] <- "Station"
    names(temp_df)[2] <- "Day of Week"
    names(temp_df)[3] <- "Entries"
    
    temp_df
  })
  
  # create a data table to show yearly data
  output$entries_year_table <- renderUI({
    # format the table layout
    div(
    tags$head(
      tags$style(
        HTML('
          .datatables {
            height: unset !important;
            width: inherit !important;
          }
           ')
      )
    ),
    
    datatable(
      entries_year_table(),
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        dom = 'tp'
      ),
      rownames = FALSE
      )
    )
  })
  
  # create a data table to show monthly data
  output$entries_month_table <- renderUI({
    # format the table layout
    div(
      tags$head(
        tags$style(
          HTML('
          .datatables {
            height: unset !important;
            width: inherit !important;
          }
           ')
        )
      ),
      
      datatable(
        entries_month_table(),
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          dom = 'tp'
        ),
        rownames = FALSE
        )
      )
    })
  
  # create a data table to show weekly data
  output$entries_week_table <- renderUI({
    # format the table layout
    div(
      tags$head(
        tags$style(
          HTML('
          .datatables {
            height: unset !important;
            width: inherit !important;
          }
           ')
        )
      ),
      
      datatable(
        entries_week_table(),
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          dom = 'tp'
        ),
        rownames = FALSE
      )
    )
  })
  
  # render UI
  output$plots <- renderUI({
    validate(
      need(input$time_frame, 'Check at least one Time Frame!')
    )
    
    col_val_reactive <- get_col_reactive()
    
    # put three plots in a row
    fluidRow(
      if (as.integer(col_val_reactive[1]) != 0) {
        column(as.integer(col_val_reactive[1]), 
               div(plotOutput("entries_year_graph")),
               uiOutput("entries_year_table")
               )
      },
      
      if (as.integer(col_val_reactive[2]) != 0) {
        column(as.integer(col_val_reactive[2]), 
               div(plotOutput("entries_month_graph")),
               uiOutput("entries_month_table")
               )
      },
      
      if (as.integer(col_val_reactive[3]) != 0) {
        column(as.integer(col_val_reactive[3]), 
               div(plotOutput("entries_week")),
               uiOutput("entries_week_table")
               )
      }
    )
  })
}

shinyApp(ui = ui, server = server)
