#libraries to include

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(scales)
library(DT)
library(tidyr)
library(tidyverse)


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

add_monthChar <- function(df) {
  # add new column for month that contains char
  df$monthChar[df$month == 1] <- "Jan"
  df$monthChar[df$month == 2] <- "Feb"
  df$monthChar[df$month == 3] <- "Mar"
  df$monthChar[df$month == 4] <- "Apr"
  df$monthChar[df$month == 5] <- "May"
  df$monthChar[df$month == 6] <- "Jun"
  df$monthChar[df$month == 7] <- "Jul"
  df$monthChar[df$month == 8] <- "Aug"
  df$monthChar[df$month == 9] <- "Sep"
  df$monthChar[df$month == 10] <- "Oct"
  df$monthChar[df$month == 11] <- "Nov"
  df$monthChar[df$month == 12] <- "Dec"
  
  df
}

allData <- add_monthChar(allData)


allData <- within(allData, {
  names <- reorder(month, seq_along(newDate), order = TRUE)
})

# parse to days in the week
allData$dayChar <- weekdays(allData$newDate)

# add new column for week that contains int
add_dayToDayChar <- function(df) {
  df$dayChar[df$day == 1] <- "Monday"
  df$dayChar[df$day == 2] <- "Tuesday"
  df$dayChar[df$day == 3] <- "Wednesday"
  df$dayChar[df$day == 4] <- "Thursday"
  df$dayChar[df$day == 5] <- "Friday"
  df$dayChar[df$day == 6] <- "Saturday"
  df$dayChar[df$day == 7] <- "Sunday"
  
  df
}

add_dayCharToDay <- function(df) {
  df$day[df$dayChar == "Monday"] <- 1
  df$day[df$dayChar == "Tuesday"] <- 2
  df$day[df$dayChar == "Wednesday"] <- 3
  df$day[df$dayChar == "Thursday"] <- 4
  df$day[df$dayChar == "Friday"] <- 5
  df$day[df$dayChar == "Saturday"] <- 6
  df$day[df$dayChar == "Sunday"] <- 7
  
  df
}
allData <- add_dayCharToDay(allData)


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
                     menuItem("Graph and Table", tabName = "compare_table", selected = T),
                     menuItem("Two Graphs", tabName = "compare_graph")
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
                                       div(checkboxGroupInput("time_frame_1",
                                                              "Time Frame",
                                                              choices = c("Year", "Month", "Week"),
                                                              selected = c("Year", "Month", "Week")
                                       )
                                       
                                       )
                                ),
                                column(6,
                                       div(selectInput("select_year_1",
                                                       "Year",
                                                       choices = c("Every", 2021:2001),
                                                       selected = c(2021)
                                                       )
                                           )
                                       )
                                ),
                              div(selectInput("select_station_1",
                                              "Station",
                                              choices = c("Every", "UIC-Halsted", "O'Hare Airport", "Rosemont"),
                                              selected = c("UIC-Halsted")
                                              )
                                  ),
                              width = 2
                            ),
                            mainPanel(
                              uiOutput("plot_and_table"),
                              width = 10
                              )
                            )
              ),
      tabItem(tabName = "compare_graph",
              sidebarLayout(position = "left",
                            sidebarPanel(
                              style = "margin-top:70%;",
                              h2("Top Plot(s)"),
                              fluidRow(
                                column(6, 
                                       div(checkboxGroupInput("time_frame_2",
                                                              "Time Frame",
                                                              choices = c("Year", "Month", "Week"),
                                                              selected = c("Year", "Month", "Week")
                                                              )
                                           )
                                       ),
                                column(6,
                                       div(selectInput("select_year_2",
                                                       "Year",
                                                       choices = c("Every", 2021:2001),
                                                       selected = c(2021)
                                                       )
                                           )
                                       )
                                ),
                              div(selectInput("select_station_2",
                                              "Station",
                                              choices = c("Every", "UIC-Halsted", "O'Hare Airport", "Rosemont"),
                                              selected = c("UIC-Halsted")
                                              )
                                  ),
                              h2("Bottom Plot(s)"),
                              fluidRow(
                                column(6, 
                                       div(checkboxGroupInput("time_frame_3",
                                                              "Time Frame",
                                                              choices = c("Year", "Month", "Week"),
                                                              selected = c("Year", "Month", "Week")
                                                              )
                                           )
                                       ),
                                column(6,
                                       div(selectInput("select_year_3",
                                                       "Year",
                                                       choices = c("Every", 2021:2001),
                                                       selected = c(2021)
                                                       )
                                           )
                                       )
                                ),
                              div(selectInput("select_station_3",
                                              "Station",
                                              choices = c("Every", "UIC-Halsted", "O'Hare Airport", "Rosemont"),
                                              selected = c("UIC-Halsted")
                                              )
                                  ),
                              width = 2
                              ),
                            mainPanel(
                              uiOutput("plot_and_plot_1"),
                              uiOutput("plot_and_plot_2"),
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
  
  #################################################################
  
  getGradientCol <- function(x) {
    gradientCol <- "black"
    
    if (x == "UIC-Halsted") {
      gradientCol <- "#e41a1c"
    } else if (x == "O'Hare Airport") {
      gradientCol <- "#377eb8"
    } else if (x == "Rosemont") {
      gradientCol <- "#4daf4a"
    } else {
      gradientCol <- "#984ea3"
    }
  }  
  
  #################################################################
  
  sum_of_year_1 <- function(year) {
    if (input$select_station_1 != "Every") {
      sum(all_data_df[all_data_df$year == year & all_data_df$stationname == input$select_station_1,]$rides)
    } else {
      sum(all_data_df[all_data_df$year == year,]$rides)
    }
  }
  
  sum_of_year_2 <- function(year) {
    if (input$select_station_2 != "Every") {
      sum(all_data_df[all_data_df$year == year & all_data_df$stationname == input$select_station_2,]$rides)
    } else {
      sum(all_data_df[all_data_df$year == year,]$rides)
    }
  }
  
  sum_of_year_3 <- function(year) {
    if (input$select_station_3 != "Every") {
      sum(all_data_df[all_data_df$year == year & all_data_df$stationname == input$select_station_3,]$rides)
    } else {
      sum(all_data_df[all_data_df$year == year,]$rides)
    }
  }
  
  #################################################################
  
  sum_of_month_1 <- function(month) {
    if (input$select_year_1 != "Every") {
      if (input$select_station_1 != "Every") {
        sum(all_data_df[all_data_df$month == month & all_data_df$year == input$select_year_1 & all_data_df$stationname == input$select_station_1,]$rides)
      } else {
        sum(all_data_df[all_data_df$month == month & all_data_df$year == input$select_year_1,]$rides)
      }
    } else {
      sum(all_data_df[all_data_df$month == month,]$rides)
    }
  }
  
  sum_of_month_2 <- function(month) {
    if (input$select_year_2 != "Every") {
      if (input$select_station_2 != "Every") {
        sum(all_data_df[all_data_df$month == month & all_data_df$year == input$select_year_2 & all_data_df$stationname == input$select_station_2,]$rides)
      } else {
        sum(all_data_df[all_data_df$month == month & all_data_df$year == input$select_year_2,]$rides)
      }
    } else {
      sum(all_data_df[all_data_df$month == month,]$rides)
    }
  }
  
  sum_of_month_3 <- function(month) {
    if (input$select_year_3 != "Every") {
      if (input$select_station_3 != "Every") {
        sum(all_data_df[all_data_df$month == month & all_data_df$year == input$select_year_3 & all_data_df$stationname == input$select_station_3,]$rides)
      } else {
        sum(all_data_df[all_data_df$month == month & all_data_df$year == input$select_year_3,]$rides)
      }
    } else {
      sum(all_data_df[all_data_df$month == month,]$rides)
    }
  }
  
  #################################################################
  
  sum_of_week_1 <- function(day) {
    if (input$select_year_1 != "Every") {
      if (input$select_station_1 != "Every") {
        sum(all_data_df[all_data_df$day == day & all_data_df$year == input$select_year_1 & all_data_df$stationname == input$select_station_1,]$rides)
      } else {
        sum(all_data_df[all_data_df$day == day & all_data_df$year == input$select_year_1,]$rides)
      }
    } else {
      sum(all_data_df[all_data_df$day == day,]$rides)
    }
  }
  
  sum_of_week_2 <- function(day) {
    if (input$select_year_2 != "Every") {
      if (input$select_station_2 != "Every") {
        sum(all_data_df[all_data_df$day == day & all_data_df$year == input$select_year_2 & all_data_df$stationname == input$select_station_2,]$rides)
      } else {
        sum(all_data_df[all_data_df$day == day & all_data_df$year == input$select_year_2,]$rides)
      }
    } else {
      sum(all_data_df[all_data_df$day == day,]$rides)
    }
  }
  
  sum_of_week_3 <- function(day) {
    if (input$select_year_3 != "Every") {
      if (input$select_station_3 != "Every") {
        sum(all_data_df[all_data_df$day == day & all_data_df$year == input$select_year_3 & all_data_df$stationname == input$select_station_3,]$rides)
      } else {
        sum(all_data_df[all_data_df$day == day & all_data_df$year == input$select_year_3,]$rides)
      }
    } else {
      sum(all_data_df[all_data_df$day == day,]$rides)
    }
  }
  
  #################################################################
  
  # get column values reactive
  get_col <- function(df) {
    year_col <- 0
    month_col <- 0
    week_col <- 0
    
    # decide how much space is required for each graph based on what is visible
    if (all(c("Year", "Month", "Week") %in% df)) {
      year_col <- 4
      month_col <- 4
      week_col <- 4
    } else if (all(c("Month", "Week") %in% df)) {
      year_col <- 0
      month_col <- 6
      week_col <- 6
      
    } else if (all(c("Year", "Week") %in% df)) {
      year_col <- 6
      month_col <- 0
      week_col <- 6
      
    } else if (all(c("Year", "Month") %in% df)) {
      year_col <- 6
      month_col <- 6
      week_col <- 0
      
    } else if (all(c("Year") %in% df)) {
      year_col <- 12
      month_col <- 0
      week_col <- 0
      
    } else if (all(c("Month") %in% df)) {
      year_col <- 0
      month_col <- 12
      week_col <- 0
      
    } else if (all(c("Week") %in% df)) {
      year_col <- 0
      month_col <- 0
      week_col <- 12
    }
    
    return(list(year_col, month_col, week_col))
  }
  
  #################################################################
  # create sum of each month of the year
  
  sum_of_month_df_1 <- reactive({
    month <- 1:12
    rides <- array(unlist(
      lapply(1:12, 
             sum_of_month_1)
    )
    )
    
    toReturn <- data.frame(month, rides)
    toReturn <- add_monthChar(toReturn)
    toReturn
  })
  
  sum_of_month_df_2 <- reactive({
    month <- 1:12
    rides <- array(unlist(
      lapply(1:12, 
             sum_of_month_2)
    )
    )
    
    toReturn <- data.frame(month, rides)
    toReturn <- add_monthChar(toReturn)
    toReturn
  })
  
  sum_of_month_df_3 <- reactive({
    month <- 1:12
    rides <- array(unlist(
      lapply(1:12, 
             sum_of_month_3)
    )
    )
    
    toReturn <- data.frame(month, rides)
    toReturn <- add_monthChar(toReturn)
    toReturn
  })
  
  #################################################################
  # create sum of each day of week
  
  sum_of_week_df_1 <- reactive({
    day <- 1:7
    rides <- array(unlist(
      lapply(1:7, 
             sum_of_week_1)
    )
    )
    
    toReturn <- data.frame(day, rides)
    toReturn <- add_dayToDayChar(toReturn)
    toReturn
  })
  
  sum_of_week_df_2 <- reactive({
    day <- 1:7
    rides <- array(unlist(
      lapply(1:7, 
             sum_of_week_2)
    )
    )
    
    toReturn <- data.frame(day, rides)
    toReturn <- add_dayToDayChar(toReturn)
    toReturn
  })
  
  sum_of_week_df_3 <- reactive({
    day <- 1:7
    rides <- array(unlist(
      lapply(1:7, 
             sum_of_week_3)
    )
    )
    
    toReturn <- data.frame(day, rides)
    toReturn <- add_dayToDayChar(toReturn)
    toReturn
  })
  
  #################################################################
  # create reactive dataframe for month and week
  
  df_Reactive_month_week_1 <- reactive({
    # create dataframe for a specific year and station
    if (input$select_year_1 != "Every") {
      if (input$select_station_1 != "Every") {
        subset(all_data_df, all_data_df$year == input$select_year_1 & all_data_df$stationname == input$select_station_1)
      } else {
        subset(all_data_df, all_data_df$year == input$select_year_1)
      }
      
    } else {
      if (input$select_station_1 != "Every") {
        subset(all_data_df, all_data_df$stationname == input$select_station_1)
      } else {
        all_data_df
      }
    }
  })
  
  df_Reactive_month_week_2 <- reactive({
    # create dataframe for a specific year and station
    
    temp_df <- NULL
    
    if (input$select_year_2 != "Every") {
      if (input$select_station_2 != "Every") {
        temp_df <- subset(all_data_df, all_data_df$year == input$select_year_2 & all_data_df$stationname == input$select_station_2)
      } else {
        temp_df <- subset(all_data_df, all_data_df$year == input$select_year_2)
      }
      
    } else {
      if (input$select_station_2 != "Every") {
        temp_df <- subset(all_data_df, all_data_df$stationname == input$select_station_2)
      } else {
        temp_df <- all_data_df
      }
    }
    
    month <- 1:12
    rides <- array(unlist(
      lapply(2001:2021, 
             sum(temp_df[temp_df$month == month,]$rides))
    )
    )
    
    data.frame(year, month)
  })
  
  df_Reactive_month_week_3 <- reactive({
    # create dataframe for a specific year and station
    
    temp_df <- NULL
    
    if (input$select_year_3 != "Every") {
      if (input$select_station_3 != "Every") {
        temp_df <- subset(all_data_df, all_data_df$year == input$select_year_3 & all_data_df$stationname == input$select_station_3)
      } else {
        temp_df <- subset(all_data_df, all_data_df$year == input$select_year_3)
      }
      
    } else {
      if (input$select_station_3 != "Every") {
        temp_df <- subset(all_data_df, all_data_df$stationname == input$select_station_3)
      } else {
        temp_df <- all_data_df
      }
    }
    
    month <- 1:12
    rides <- array(unlist(
      lapply(2001:2021, 
             sum(temp_df[temp_df$month == month,]$rides))
    )
    )
    
    data.frame(year, month)
  })
  
  #################################################################
  
  # create reactive dataframe for year
  sum_of_year_df_1 <- reactive({
    year <- 2001:2021
    rides <- array(unlist(
      lapply(2001:2021, 
             sum_of_year_1)
    )
    )
    
    data.frame(year, rides)
  })
  
  sum_of_year_df_2 <- reactive({
    year <- 2001:2021
    rides <- array(unlist(
      lapply(2001:2021, 
             sum_of_year_2)
    )
    )
    
    data.frame(year, rides)
  })
  
  sum_of_year_df_3 <- reactive({
    year <- 2001:2021
    rides <- array(unlist(
      lapply(2001:2021, 
             sum_of_year_3)
    )
    )
    
    data.frame(year, rides)
  })
  
  #################################################################
  
  df_Reactive_year_1 <- reactive({
    if (input$select_station_1 != "Every") {
      subset(all_data_df, all_data_df$stationname == input$select_station_1)
    } else {
      all_data_df
    }
  })
  
  df_Reactive_year_2 <- reactive({
    if (input$select_station_2 != "Every") {
      subset(all_data_df, all_data_df$stationname == input$select_station_2)
    } else {
      all_data_df
    }
  })
  
  df_Reactive_year_3 <- reactive({
    if (input$select_station_3 != "Every") {
      subset(all_data_df, all_data_df$stationname == input$select_station_3)
    } else {
      all_data_df
    }
  })
  
  #################################################################
  
  # create graph to show yearly data
  output$entries_year_graph_1 <- renderPlot({
    ggplot(data = sum_of_year_df_1(), aes(x = year, y = rides)) + 
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_x_continuous(breaks = seq(2001, 2021, by = 2)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Year",
           y = "Entries") +
      ggtitle(paste("Yearly Entries at", input$select_station_1, "CTA Station")) +
      scale_fill_gradient2(low = "white", 
                           high = getGradientCol(input$select_station_1), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  output$entries_year_graph_2 <- renderPlot({
    ggplot(data = sum_of_year_df_2(), aes(x = year, y = rides)) + 
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_x_continuous(breaks = seq(2001, 2021, by = 2)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Year",
           y = "Entries") +
      ggtitle(paste("Yearly Entries at", input$select_station_2, "CTA Station")) +
      scale_fill_gradient2(low = "white", 
                           high = getGradientCol(input$select_station_2), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  output$entries_year_graph_3 <- renderPlot({
    ggplot(data = sum_of_year_df_3(), aes(x = year, y = rides)) + 
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_x_continuous(breaks = seq(2001, 2021, by = 2)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Year",
           y = "Entries") +
      ggtitle(paste("Yearly Entries at", input$select_station_3, "CTA Station")) +
      scale_fill_gradient2(low = "white", 
                           high = getGradientCol(input$select_station_3), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  #################################################################
  
  # create graph to show monthly data
  output$entries_month_graph_1 <- renderPlot({
    ggplot(data = sum_of_month_df_1(), aes(x = reorder(monthChar, month), y = rides)) + 
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Month",
           y = "Entries") +
      ggtitle(paste("Monthly Entries at", input$select_station_1, "CTA Station")) +
      scale_fill_gradient2(low = "white", 
                           high = getGradientCol(input$select_station_1), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  output$entries_month_graph_2 <- renderPlot({
    ggplot(data = sum_of_month_df_2(), aes(x = reorder(monthChar, month), y = rides)) + 
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Month",
           y = "Entries") +
      ggtitle(paste("Monthly Entries at", input$select_station_2, "CTA Station")) +
      scale_fill_gradient2(low = "white", 
                           high = getGradientCol(input$select_station_2), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  output$entries_month_graph_3 <- renderPlot({
    ggplot(data = sum_of_month_df_3(), aes(x = reorder(monthChar, month), y = rides)) + 
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Month",
           y = "Entries") +
      ggtitle(paste("Monthly Entries at", input$select_station_3, "CTA Station")) +
      scale_fill_gradient2(low = "white", 
                           high = getGradientCol(input$select_station_3), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  #################################################################
  
  # create graph to show weekly data
  output$entries_week_graph_1 <- renderPlot({
    ggplot(data = sum_of_week_df_1(), aes(x = reorder(dayChar, day), y = rides)) +
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_y_continuous(labels = comma) +
      labs(x = "Day",
             y = "Entries") +
      ggtitle(paste("Day of the week Entries at", input$select_station_1, "CTA Station")) +
      scale_fill_gradient2(low = "white", 
                           high = getGradientCol(input$select_station_1), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  output$entries_week_graph_2 <- renderPlot({
    ggplot(data = sum_of_week_df_2(), aes(x = reorder(dayChar, day), y = rides)) +
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_y_continuous(labels = comma) +
      labs(x = "Day",
           y = "Entries") +
      ggtitle(paste("Day of the week Entries at", input$select_station_2, "CTA Station")) +
      scale_fill_gradient2(low = "white", 
                           high = getGradientCol(input$select_station_2), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  output$entries_week_graph_3 <- renderPlot({
    ggplot(data = sum_of_week_df_3(), aes(x = reorder(dayChar, day), y = rides)) +
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_y_continuous(labels = comma) +
      labs(x = "Day",
           y = "Entries") +
      ggtitle(paste("Day of the week Entries at", input$select_station_3, "CTA Station")) +
      scale_fill_gradient2(low = "white", 
                           high = getGradientCol(input$select_station_3), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  #################################################################
  
  # create new Data frame to show yearly data
  entries_year_table <- reactive({
    toReturn <- sum_of_year_df_1()
    
    # rename
    names(toReturn)[1] <- "Year"
    names(toReturn)[2] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  # create new Data frame to show monthly data
  entries_month_table <- reactive({
    toReturn <- sum_of_month_df_1()
    keep <- c("monthChar", "rides")
    toReturn <- toReturn[keep]
    
    # rename
    names(toReturn)[1] <- "Month"
    names(toReturn)[2] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  # create new Data frame to show weekly data
  entries_week_table <- reactive({
    toReturn <- sum_of_week_df_1()
    keep <- c("dayChar", "rides")
    toReturn <- toReturn[keep]
    
    # rename
    names(toReturn)[1] <- "Day"
    names(toReturn)[2] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  #################################################################
  
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
  
  #################################################################
  
  # render plot and table
  output$plot_and_table <- renderUI({
    validate(
      need(input$time_frame_1, 'Check at least one Time Frame!')
    )
    
    # put three plots in a row
    fluidRow(
      if (as.integer(get_col(input$time_frame_1)[1]) != 0) {
        column(as.integer(get_col(input$time_frame_1)[1]), 
               div(plotOutput("entries_year_graph_1")),
               uiOutput("entries_year_table")
        )
      },
      
      if (as.integer(get_col(input$time_frame_1)[2]) != 0) {
        column(as.integer(get_col(input$time_frame_1)[2]), 
               div(plotOutput("entries_month_graph_1")),
               uiOutput("entries_month_table")
        )
      },
      
      if (as.integer(get_col(input$time_frame_1)[3]) != 0) {
        column(as.integer(get_col(input$time_frame_1)[3]), 
               div(plotOutput("entries_week_graph_1")),
               uiOutput("entries_week_table")
        )
      }
    )
  })
  
  # render two plots
  output$plot_and_plot_1 <- renderUI({
    validate(
      need(input$time_frame_2, 'Check at least one Time Frame!')
    )
    
    # put three plots in a row
    fluidRow(
      if (as.integer(get_col(input$time_frame_2)[1]) != 0) {
        column(as.integer(get_col(input$time_frame_2)[1]), 
               div(plotOutput("entries_year_graph_2"))
        )
      },
      
      if (as.integer(get_col(input$time_frame_2)[2]) != 0) {
        column(as.integer(get_col(input$time_frame_2)[2]), 
               div(plotOutput("entries_month_graph_2"))
        )
      },
      
      if (as.integer(get_col(input$time_frame_2)[3]) != 0) {
        column(as.integer(get_col(input$time_frame_2)[3]), 
               div(plotOutput("entries_week_graph_2"))
        )
      }
    )
  })
  
  output$plot_and_plot_2 <- renderUI({
    validate(
      need(input$time_frame_3, 'Check at least one Time Frame!')
    )
    
    # put three plots in a row
    fluidRow(
      if (as.integer(get_col(input$time_frame_3)[1]) != 0) {
        column(as.integer(get_col(input$time_frame_3)[1]), 
               div(plotOutput("entries_year_graph_3"))
        )
      },
      
      if (as.integer(get_col(input$time_frame_3)[2]) != 0) {
        column(as.integer(get_col(input$time_frame_3)[2]), 
               div(plotOutput("entries_month_graph_3"))
        )
      },
      
      if (as.integer(get_col(input$time_frame_3)[3]) != 0) {
        column(as.integer(get_col(input$time_frame_3)[3]), 
               div(plotOutput("entries_week_graph_3"))
        )
      }
    )
  })
}

shinyApp(ui = ui, server = server)
