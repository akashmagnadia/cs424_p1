#libraries to include

library(shiny)
library(shinyWidgets)
library(shinyjs)
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

# parse to days in the week
allData$dayChar <- weekdays(allData$newDate)

# add new column for week that contains int
add_dayToDayChar <- function(df) {
  df$dayChar[df$day == 1] <- "Mon"
  df$dayChar[df$day == 2] <- "Tues"
  df$dayChar[df$day == 3] <- "Wed"
  df$dayChar[df$day == 4] <- "Thur"
  df$dayChar[df$day == 5] <- "Fri"
  df$dayChar[df$day == 6] <- "Sat"
  df$dayChar[df$day == 7] <- "Sun"
  
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

# set all values I won't be using to NULL
all_data_df$station_id <- NULL
all_data_df$dayType <- NULL
all_data_df$fullDate <- NULL

all_data_df$stationname[all_data_df$stationname == "OHare Airport"] <- "O'Hare Airport"

all_data_df_uic <- subset(all_data_df, all_data_df$stationname == "UIC-Halsted")
all_data_df_ohare <- subset(all_data_df, all_data_df$stationname == "O'Hare Airport")
all_data_df_rose <- subset(all_data_df, all_data_df$stationname == "Rosemont")

hideAllDesc <- function() {
  shinyjs::hide(id = "insight1")
  shinyjs::hide(id = "insight2")
  shinyjs::hide(id = "insight3")
  shinyjs::hide(id = "insight4")
  shinyjs::hide(id = "insight5")
  shinyjs::hide(id = "insight6")
  shinyjs::hide(id = "insight7")
  shinyjs::hide(id = "insight8")
  shinyjs::hide(id = "insight9")
  shinyjs::hide(id = "insight10")
}

# Create the shiny application
ui <- dashboardPage(
  dashboardHeader(title = "CTA Subway Data"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     id = "tabs",
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("About", tabName = "About"),
                     menuItem("Graphs and Tables", tabName = "compare_graph", selected = T),
                     menuItem("Interesting Insights",
                              menuSubItem("Insight 1", tabName = "insight1"),
                              menuSubItem("Insight 2", tabName = "insight2"),
                              menuSubItem("Insight 3", tabName = "insight3"),
                              menuSubItem("Insight 4", tabName = "insight4"),
                              menuSubItem("Insight 5", tabName = "insight5"),
                              menuSubItem("Insight 6", tabName = "insight6"),
                              menuSubItem("Insight 7", tabName = "insight7"),
                              menuSubItem("Insight 8", tabName = "insight8"),
                              menuSubItem("Insight 9", tabName = "insight9"),
                              menuSubItem("Insight 10", tabName = "insight10")
                     )
                   ),
                   hr(),
                   useShinyjs(),
                   div(
                     id = "insight1",
                     fluidRow(
                       column(1),
                       column(9,
                              h5("When we look at weekly data in 2021, we can see that there is increased activity at UIC-Halsted during weekdays compared to the weekend. This is because most classes are held from Monday to Friday.")
                       ),
                     )
                   ),
                   div(
                     id = "insight2",
                     fluidRow(
                       column(1),
                       column(9,
                              h5("When we look at monthly data in 2021, we can see a significant increase in activity at UIC-Halsted around August and September compared to April and May. This is because we were in remote learning the last semester, while this semester we are in-person learning.")
                       ),
                     )
                   ),
                   div(
                     id = "insight3",
                     fluidRow(
                       column(1),
                       column(9,
                              h5("When we look at monthly data in 2020, we can see that after February, there is a significant decrease in activity at UIC-Halsted. This is because the entire school went in remote learning due to COVID-19.")
                       ),
                     )
                   ),
                   div(
                     id = "insight4",
                     fluidRow(
                       column(1),
                       column(9,
                              h5("When we look at monthly data for UIC-Halsted during the years 2020 and 2021, we can see that we were in remote learning for half of the Spring 2020 semester and the next full year.")
                       ),
                     )
                   ),
                   div(
                     id = "insight5",
                     fluidRow(
                       column(1),
                       column(9,
                              h5("When we look at monthly data for UIC-Halsted and O'Hare airport for 2020, we see a similar trend of decrease in activity starting February. This is because CDC came with new guidelines for institutions to follow to slow the spread of coronavirus.")
                       ),
                     )
                   ),
                   div(
                     id = "insight6",
                     fluidRow(
                       column(1),
                       column(9,
                              h5("When we look at historic ridership data at Rosemont station, we can see a steady increase in passengers year over year.")
                       ),
                     )
                   ),
                   div(
                     id = "insight7",
                     fluidRow(
                       column(1),
                       column(9,
                              h5("When we look at ridership data from all stations during 2016, we can see a spike in ridership on November 4, 2016. This is the same date when Cub's parade was held. Five days before the parade (October 29) was the day of the game that earned them world series.")
                       ),
                     )
                   ),
                   div(
                     id = "insight8",
                     fluidRow(
                       column(1),
                       column(9,
                              h5("When we look at ridership data from O'Hare station during 2001, we see that around September there is a dip in activity. This is because there were attacks on Twin Towers in New York and there was a fear of flying for the next few months. Furthermore, during 2002 there was a dip in entries at O'Hare during September due to fear of another attack on a plane.")
                       ),
                     )
                   ),
                   div(
                     id = "insight9",
                     fluidRow(
                       column(1),
                       column(9,
                              h5("Here we look at monthly data from 2018 at UIC-Halsted. We notice that there is a dip in ridership during May, June, July, and August because that's when UIC has summer vacation.")
                       ),
                     )
                   ),
                   div(
                     id = "insight10",
                     fluidRow(
                       column(1),
                       column(9,
                              h5("Here when we look at November data for 2008, there is an overall increased activity at all CTA stations. This is because Obama held his presidential election acceptance speech at Grant Park in Chicago.")
                       ),
                     )
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
                                                              choices = c("Yearly", "Daily", "Monthly", "Weekly"),
                                                              selected = c("Yearly")
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
                                                              choices = c("Yearly", "Daily", "Monthly", "Weekly"),
                                                              selected = c("Yearly")
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
                                              selected = c("O'Hare Airport")
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
              h1("About This Visualization"),
              h3("Project created by Akash Magnadia for CS 424"),
              h3("Data source: https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
              h3("Created February 5th, 2022"),
              h3("The intention for creating this visualization is to display ridership data in an easy-to-understand fashion. In this visualization, you can set the station location to one or every station in the CTA train network. The data also be filtered by a specific year between 2001 to 2021 or every year. The data can be viewed in different panels such as Daily, Yearly, Monthly, or Weekly views. The main body of the visualization can be two portions, each portion consisting of a graph and a corresponding data for the plot in a table to display CTA ridership data."),
              h3("This visualization is designed to run on a Touch-Screen wall at UIC with a ratio of 5,760 by 3,240 as per the assignment requirement.")
      )
    )
  )
)

server <- function(input, output, session) {
  
  #################################################################
  
  getLowGradientCol <- function(x) {
    gradientCol <- "grey"
    
    if (x == "UIC-Halsted") {
      gradientCol <- "#fbb4ae"
    } else if (x == "O'Hare Airport") {
      gradientCol <- "#b3cde3"
    } else if (x == "Rosemont") {
      gradientCol <- "#ccebc5"
    } else {
      gradientCol <- "#decbe4"
    }
  }
  
  getHighGradientCol <- function(x) {
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
  
  sum_of_year_2 <- function(year) {
    if (input$select_station_2 != "Every") {
      if (input$select_station_2 == "UIC-Halsted") {
        sum(all_data_df_uic[all_data_df_uic$year == year,]$rides)
      } else if (input$select_station_2 == "Rosemont") {
        sum(all_data_df_rose[all_data_df_rose$year == year,]$rides)
      } else if (input$select_station_2 == "Ohare Airport") {
        sum(all_data_df_ohare[all_data_df_ohare$year == year,]$rides)
      } else {
        sum(all_data_df[all_data_df$year == year & all_data_df$stationname == input$select_station_2,]$rides)
      }
    } else {
      sum(all_data_df[all_data_df$year == year,]$rides)
    }
  }
  
  sum_of_year_3 <- function(year) {
    if (input$select_station_3 != "Every") {
      if (input$select_station_3 == "UIC-Halsted") {
        sum(all_data_df_uic[all_data_df_uic$year == year,]$rides)
      } else if (input$select_station_3 == "Rosemont") {
        sum(all_data_df_rose[all_data_df_rose$year == year,]$rides)
      } else if (input$select_station_3 == "Ohare Airport") {
        sum(all_data_df_ohare[all_data_df_ohare$year == year,]$rides)
      } else {
        sum(all_data_df[all_data_df$year == year & all_data_df$stationname == input$select_station_3,]$rides)
      }
    } else {
      sum(all_data_df[all_data_df$year == year,]$rides)
    }
  }
  
  #################################################################
  
  sum_of_month_2 <- function(month) {
    if (input$select_year_2 != "Every") {
      if (input$select_station_2 != "Every") {
        
        if (input$select_station_2 == "UIC-Halsted") {
          sum(all_data_df_uic[all_data_df_uic$month == month & all_data_df_uic$year == input$select_year_2,]$rides)
        } else if (input$select_station_2 == "Rosemont") {
          sum(all_data_df_rose[all_data_df_rose$month == month & all_data_df_rose$year == input$select_year_2,]$rides)
        } else if (input$select_station_2 == "Ohare Airport") {
          sum(all_data_df_ohare[all_data_df_ohare$month == month & all_data_df_ohare$year == input$select_year_2,]$rides)
        } else {
          sum(all_data_df[all_data_df$month == month & all_data_df$year == input$select_year_2 & all_data_df$stationname == input$select_station_2,]$rides)
        }
        
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
        
        if (input$select_station_3 == "UIC-Halsted") {
          sum(all_data_df_uic[all_data_df_uic$month == month & all_data_df_uic$year == input$select_year_3,]$rides)
        } else if (input$select_station_3 == "Rosemont") {
          sum(all_data_df_rose[all_data_df_rose$month == month & all_data_df_rose$year == input$select_year_3,]$rides)
        } else if (input$select_station_3 == "Ohare Airport") {
          sum(all_data_df_ohare[all_data_df_ohare$month == month & all_data_df_ohare$year == input$select_year_3,]$rides)
        } else {
          sum(all_data_df[all_data_df$month == month & all_data_df$year == input$select_year_3 & all_data_df$stationname == input$select_station_3,]$rides)
        }
        
      } else {
        sum(all_data_df[all_data_df$month == month & all_data_df$year == input$select_year_3,]$rides)
      }
    } else {
      sum(all_data_df[all_data_df$month == month,]$rides)
    }
  }
  
  #################################################################
  
  sum_of_week_2 <- function(day) {
    if (input$select_year_2 != "Every") {
      if (input$select_station_2 != "Every") {
        
        if (input$select_station_2 == "UIC-Halsted") {
          sum(all_data_df_uic[all_data_df_uic$day == day & all_data_df_uic$year == input$select_year_2,]$rides)
        } else if (input$select_station_2 == "Rosemont") {
          sum(all_data_df_rose[all_data_df_rose$day == day & all_data_df_rose$year == input$select_year_2,]$rides)
        } else if (input$select_station_2 == "Ohare Airport") {
          sum(all_data_df_ohare[all_data_df_ohare$day == day & all_data_df_ohare$year == input$select_year_2,]$rides)
        } else {
          sum(all_data_df[all_data_df$day == day & all_data_df$year == input$select_year_2 & all_data_df$stationname == input$select_station_2,]$rides)
        }
        
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
        
        if (input$select_station_3 == "UIC-Halsted") {
          sum(all_data_df_uic[all_data_df_uic$day == day & all_data_df_uic$year == input$select_year_3,]$rides)
        } else if (input$select_station_3 == "Rosemont") {
          sum(all_data_df_rose[all_data_df_rose$day == day & all_data_df_rose$year == input$select_year_3,]$rides)
        } else if (input$select_station_3 == "Ohare Airport") {
          sum(all_data_df_ohare[all_data_df_ohare$day == day & all_data_df_ohare$year == input$select_year_3,]$rides)
        } else {
          sum(all_data_df[all_data_df$day == day & all_data_df$year == input$select_year_3 & all_data_df$stationname == input$select_station_3,]$rides)
        }
        
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
    yearly_col <- 0
    daily_col <- 0
    monthly_col <- 0
    weekly_col <- 0
    
    # decide how much space is required for each graph based on what is visible
    if (all(c("Yearly", "Daily", "Monthly", "Weekly") %in% df)) {
      yearly_col <- 3
      daily_col <- 3
      monthly_col <- 3
      weekly_col <- 3
    } else if (all(c("Yearly", "Daily", "Monthly") %in% df)) {
      yearly_col <- 4
      daily_col <- 4
      monthly_col <- 4
      weekly_col <- 0
    } else if (all(c("Yearly", "Daily", "Weekly") %in% df)) {
      yearly_col <- 4
      daily_col <- 4
      monthly_col <- 0
      weekly_col <- 4
    } else if (all(c("Yearly", "Monthly", "Weekly") %in% df)) {
      yearly_col <- 4
      daily_col <- 0
      monthly_col <- 4
      weekly_col <- 4
    } else if (all(c("Daily", "Monthly", "Weekly") %in% df)) {
      yearly_col <- 0
      daily_col <- 4
      monthly_col <- 4
      weekly_col <- 4
    } else if (all(c("Yearly", "Daily") %in% df)) {
      yearly_col <- 6
      daily_col <- 6
      monthly_col <- 0
      weekly_col <- 0
    } else if (all(c("Yearly", "Weekly") %in% df)) {
      yearly_col <- 6
      daily_col <- 0
      monthly_col <- 0
      weekly_col <- 6
    } else if (all(c("Yearly", "Monthly") %in% df)) {
      yearly_col <- 6
      daily_col <- 0
      monthly_col <- 6
      weekly_col <- 0
    } else if (all(c("Daily", "Monthly") %in% df)) {
      yearly_col <- 0
      daily_col <- 6
      monthly_col <- 6
      weekly_col <- 0
    } else if (all(c("Daily", "Weekly") %in% df)) {
      yearly_col <- 0
      daily_col <- 6
      monthly_col <- 0
      weekly_col <- 6
    } else if (all(c("Monthly", "Weekly") %in% df)) {
      yearly_col <- 0
      daily_col <- 0
      monthly_col <- 6
      weekly_col <- 6
    } else if (all(c("Yearly") %in% df)) {
      yearly_col <- 12
      daily_col <- 0
      monthly_col <- 0
      weekly_col <- 0
    } else if (all(c("Daily") %in% df)) {
      yearly_col <- 0
      daily_col <- 12
      monthly_col <- 0
      weekly_col <- 0
    } else if (all(c("Monthly") %in% df)) {
      yearly_col <- 0
      daily_col <- 0
      monthly_col <- 12
      weekly_col <- 0
    } else if (all(c("Weekly") %in% df)) {
      yearly_col <- 0
      daily_col <- 0
      monthly_col <- 0
      weekly_col <- 12
    }
    
    return(list(yearly_col, daily_col, monthly_col, weekly_col))
  }
  
  #################################################################
  # create sum of each month of the year
  
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
  
  # create reactive dataframe for year
  
  year_df_2 <- reactive({
    if (input$select_station_2 != "Every") {
      if (input$select_year_2 != "Every") {
        
        if (input$select_station_2 == "UIC-Halsted") {
          subset(all_data_df_uic, all_data_df_uic$year == input$select_year_2)
        } else if (input$select_station_2 == "Rosemont") {
          subset(all_data_df_rose, all_data_df_rose$year == input$select_year_2)
        } else if (input$select_station_2 == "Ohare Airport") {
          subset(all_data_df_ohare, all_data_df_ohare$year == input$select_year_2)
        } else {
          subset(all_data_df, all_data_df$stationname == input$select_station_2 & all_data_df$year == input$select_year_2)
        }
        
      } else {
        
        if (input$select_station_2 == "UIC-Halsted") {
          all_data_df_uic
        } else if (input$select_station_2 == "Rosemont") {
          all_data_df_rose
        } else if (input$select_station_2 == "Ohare Airport") {
          all_data_df_ohare
        } else {
          subset(all_data_df, all_data_df$stationname == input$select_station_2)
        }
        
      }
    } else {
      if (input$select_year_2 != "Every") {
        subset(all_data_df, all_data_df$year == input$select_year_2)
      } else {
        all_data_df
      }
    }
  })
  
  year_df_3 <- reactive({
    if (input$select_station_3 != "Every") {
      if (input$select_year_3 != "Every") {
        
        if (input$select_station_3 == "UIC-Halsted") {
          subset(all_data_df_uic, all_data_df_uic$year == input$select_year_3)
        } else if (input$select_station_3 == "Rosemont") {
          subset(all_data_df_rose, all_data_df_rose$year == input$select_year_3)
        } else if (input$select_station_3 == "Ohare Airport") {
          subset(all_data_df_ohare, all_data_df_ohare$year == input$select_year_3)
        } else {
          subset(all_data_df, all_data_df$stationname == input$select_station_3 & all_data_df$year == input$select_year_3)
        }
        
      } else {
        
        if (input$select_station_3 == "UIC-Halsted") {
          all_data_df_uic
        } else if (input$select_station_3 == "Rosemont") {
          all_data_df_rose
        } else if (input$select_station_3 == "Ohare Airport") {
          all_data_df_ohare
        } else {
          subset(all_data_df, all_data_df$stationname == input$select_station_3)
        }
        
      }
    } else {
      if (input$select_year_3 != "Every") {
        subset(all_data_df, all_data_df$year == input$select_year_3)
      } else {
        all_data_df
      }
    }
  })
  
  #################################################################
  
  # create reactive dataframe for sum of year
  
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
  
  # create graph to show yearly data for all the years
  
  output$entries_year_sum_graph_2 <- renderPlot({
    ggplot(data = sum_of_year_df_2(), aes(x = year, y = rides)) + 
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_x_continuous(breaks = seq(2001, 2021, by = 3)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Year",
           y = "Entries") +
      ggtitle(paste("Yearly Entries at", input$select_station_2, "Station")) +
      scale_fill_gradient2(low = getLowGradientCol(input$select_station_2), 
                           high = getHighGradientCol(input$select_station_2), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  output$entries_year_sum_graph_3 <- renderPlot({
    ggplot(data = sum_of_year_df_3(), aes(x = year, y = rides)) + 
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_x_continuous(breaks = seq(2001, 2021, by = 3)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Year",
           y = "Entries") +
      ggtitle(paste("Yearly Entries at", input$select_station_3, "Station")) +
      scale_fill_gradient2(low = getLowGradientCol(input$select_station_3), 
                           high = getHighGradientCol(input$select_station_3), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  #################################################################
  
  # create graph to show daily data for the year
  
  output$entries_year_graph_2 <- renderPlot({
    ggplot(data = year_df_2(), aes(x = newDate, y = rides)) + 
      geom_bar(stat = 'identity', aes(fill = rides), fill = getHighGradientCol(input$select_station_2)) +
      scale_x_date(breaks = scales::pretty_breaks(n = 5)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Year",
           y = "Entries") +
      ggtitle(paste("Daily Entries at", input$select_station_2, "Station")) +
      theme(legend.position = "none")
  })
  
  output$entries_year_graph_3 <- renderPlot({
    ggplot(data = year_df_3(), aes(x = newDate, y = rides)) + 
      geom_bar(stat = 'identity', aes(fill = rides), fill = getHighGradientCol(input$select_station_3)) +
      scale_x_date(breaks = scales::pretty_breaks(n = 5)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Year",
           y = "Entries") +
      ggtitle(paste("Daily Entries at", input$select_station_3, "Station")) +
      theme(legend.position = "none")
  })
  
  #################################################################
  
  # create graph to show monthly data
  
  output$entries_month_graph_2 <- renderPlot({
    ggplot(data = sum_of_month_df_2(), aes(x = reorder(monthChar, month), y = rides)) + 
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Month",
           y = "Entries") +
      ggtitle(paste("Monthly Entries at", input$select_station_2, "Station")) +
      scale_fill_gradient2(low = getLowGradientCol(input$select_station_2), 
                           high = getHighGradientCol(input$select_station_2), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  output$entries_month_graph_3 <- renderPlot({
    ggplot(data = sum_of_month_df_3(), aes(x = reorder(monthChar, month), y = rides)) + 
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Month",
           y = "Entries") +
      ggtitle(paste("Monthly Entries at", input$select_station_3, "Station")) +
      scale_fill_gradient2(low = getLowGradientCol(input$select_station_3), 
                           high = getHighGradientCol(input$select_station_3), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  #################################################################
  
  # create graph to show weekly data
  
  output$entries_week_graph_2 <- renderPlot({
    ggplot(data = sum_of_week_df_2(), aes(x = reorder(dayChar, day), y = rides)) +
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_y_continuous(labels = comma) +
      labs(x = "Day",
           y = "Entries") +
      ggtitle(paste("Day of week Entries at", input$select_station_2, "Station")) +
      scale_fill_gradient2(low = getLowGradientCol(input$select_station_2), 
                           high = getHighGradientCol(input$select_station_2), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  output$entries_week_graph_3 <- renderPlot({
    ggplot(data = sum_of_week_df_3(), aes(x = reorder(dayChar, day), y = rides)) +
      geom_bar(stat = 'identity', aes(fill = rides)) +
      scale_y_continuous(labels = comma) +
      labs(x = "Day",
           y = "Entries") +
      ggtitle(paste("Day of week Entries at", input$select_station_3, "Station")) +
      scale_fill_gradient2(low = getLowGradientCol(input$select_station_3), 
                           high = getHighGradientCol(input$select_station_3), 
                           midpoint = median(0)) +
      theme(legend.position = "none")
  })
  
  #################################################################
  
  # create new Data frame to show yearly data
  
  entries_year_table_2 <- reactive({
    toReturn <- year_df_2()
    keep <- c("stationname", "newDate", "ridesChar")
    toReturn <- toReturn[keep]
    
    # rename
    names(toReturn)[1] <- "Station"
    names(toReturn)[2] <- "Date"
    names(toReturn)[3] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  entries_year_table_3 <- reactive({
    toReturn <- year_df_3()
    keep <- c("stationname", "newDate", "ridesChar")
    toReturn <- toReturn[keep]
    
    # rename
    names(toReturn)[1] <- "Station"
    names(toReturn)[2] <- "Date"
    names(toReturn)[3] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  #################################################################
  
  # create new Data frame to show monthly data
  
  entries_month_table_2 <- reactive({
    toReturn <- sum_of_month_df_2()
    keep <- c("monthChar", "rides")
    toReturn <- toReturn[keep]
    
    # rename
    names(toReturn)[1] <- "Month"
    names(toReturn)[2] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  entries_month_table_3 <- reactive({
    toReturn <- sum_of_month_df_3()
    keep <- c("monthChar", "rides")
    toReturn <- toReturn[keep]
    
    # rename
    names(toReturn)[1] <- "Month"
    names(toReturn)[2] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  #################################################################
  
  # create new Data frame to show weekly data
  
  entries_week_table_2 <- reactive({
    toReturn <- sum_of_week_df_2()
    keep <- c("dayChar", "rides")
    toReturn <- toReturn[keep]
    
    # rename
    names(toReturn)[1] <- "Day"
    names(toReturn)[2] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  entries_week_table_3 <- reactive({
    toReturn <- sum_of_week_df_3()
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
  
  # create a data frame to show yearly data for the station(s) selected
  
  entries_year_sum_table_2 <- reactive({
    toReturn <- sum_of_year_df_2()
    
    # rename
    names(toReturn)[1] <- "Year"
    names(toReturn)[2] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  entries_year_sum_table_3 <- reactive({
    toReturn <- sum_of_year_df_3()
    
    # rename
    names(toReturn)[1] <- "Year"
    names(toReturn)[2] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  #################################################################
  
  # create a data table to show yearly data for the 
  
  output$entries_year_sum_table_2 <- renderUI({
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
        entries_year_sum_table_2(),
        options = list(
          pageLength = 6,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  output$entries_year_sum_table_3 <- renderUI({
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
        entries_year_sum_table_3(),
        options = list(
          pageLength = 6,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  #################################################################
  
  # create a data table to show daily data for the year
  
  output$entries_year_table_2 <- renderUI({
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
        entries_year_table_2(),
        options = list(
          pageLength = 6,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  output$entries_year_table_3 <- renderUI({
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
        entries_year_table_3(),
        options = list(
          pageLength = 6,
          lengthChange = FALSE,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  #################################################################
  
  # create a data table to show monthly data
  
  output$entries_month_table_2 <- renderUI({
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
        entries_month_table_2(),
        options = list(
          pageLength = 6,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  output$entries_month_table_3 <- renderUI({
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
        entries_month_table_3(),
        options = list(
          pageLength = 6,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  #################################################################
  
  # create a data table to show weekly data
  
  output$entries_week_table_2 <- renderUI({
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
        entries_week_table_2(),
        options = list(
          pageLength = 6,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  output$entries_week_table_3 <- renderUI({
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
        entries_week_table_3(),
        options = list(
          pageLength = 6,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  #################################################################
  
  # render two plots
  output$plot_and_plot_1 <- renderUI({
    validate(
      need(input$time_frame_2, 'Check at least one Time Frame!')
    )
    
    # put three plots in a row
    fluidRow(
      if (as.integer(get_col(input$time_frame_2)[1]) != 0) {
        column(as.integer(get_col(input$time_frame_2)[1]), 
               div(plotOutput("entries_year_sum_graph_2")),
               uiOutput("entries_year_sum_table_2")
        )
      },
      
      if (as.integer(get_col(input$time_frame_2)[2]) != 0) {
        column(as.integer(get_col(input$time_frame_2)[2]), 
               div(plotOutput("entries_year_graph_2")),
               uiOutput("entries_year_table_2")
        )
      },
      
      if (as.integer(get_col(input$time_frame_2)[3]) != 0) {
        column(as.integer(get_col(input$time_frame_2)[3]), 
               div(plotOutput("entries_month_graph_2")),
               uiOutput("entries_month_table_2")
        )
      },
      
      if (as.integer(get_col(input$time_frame_2)[4]) != 0) {
        column(as.integer(get_col(input$time_frame_2)[4]), 
               div(plotOutput("entries_week_graph_2")),
               uiOutput("entries_week_table_2")
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
               div(plotOutput("entries_year_sum_graph_3")),
               uiOutput("entries_year_sum_table_3")
        )
      },
      
      if (as.integer(get_col(input$time_frame_3)[2]) != 0) {
        column(as.integer(get_col(input$time_frame_3)[2]), 
               div(plotOutput("entries_year_graph_3")),
               uiOutput("entries_year_table_3")
        )
      },
      
      if (as.integer(get_col(input$time_frame_3)[3]) != 0) {
        column(as.integer(get_col(input$time_frame_3)[3]), 
               div(plotOutput("entries_month_graph_3")),
               uiOutput("entries_month_table_3")
        )
      },
      
      if (as.integer(get_col(input$time_frame_3)[4]) != 0) {
        column(as.integer(get_col(input$time_frame_3)[4]),
               div(plotOutput("entries_week_graph_3")),
               uiOutput("entries_week_table_3")
        )
      }
    )
  })
  
  observeEvent(input$tabs, {
    
    if (input$tabs == "insight1") {
      updateTabItems(session, 'tabs', 'compare_graph')
      updateSelectInput(session, 'select_year_2', selected = 2021)
      updateSelectInput(session, 'select_year_3', selected = "Every")
      updateSelectInput(session, 'select_station_2', selected = "UIC-Halsted")
      updateSelectInput(session, 'select_station_3', selected = "UIC-Halsted")
      updateCheckboxGroupInput(session, 'time_frame_2', selected = c("Weekly"))
      updateCheckboxGroupInput(session, 'time_frame_3', selected = c("Weekly"))
      
      hideAllDesc()
      shinyjs::show(id = input$tabs)
    }
    
    if (input$tabs == "insight2") {
      updateTabItems(session, 'tabs', 'compare_graph')
      updateSelectInput(session, 'select_year_2', selected = 2021)
      updateSelectInput(session, 'select_year_3', selected = 2021)
      updateSelectInput(session, 'select_station_2', selected = "UIC-Halsted")
      updateSelectInput(session, 'select_station_3', selected = "UIC-Halsted")
      updateCheckboxGroupInput(session, 'time_frame_2', selected = c("Monthly"))
      updateCheckboxGroupInput(session, 'time_frame_3', selected = c("Daily"))
      
      hideAllDesc()
      shinyjs::show(id = input$tabs)
    }
    
    if (input$tabs == "insight3") {
      updateTabItems(session, 'tabs', 'compare_graph')
      updateSelectInput(session, 'select_year_2', selected = 2020)
      updateSelectInput(session, 'select_year_3', selected = 2020)
      updateSelectInput(session, 'select_station_2', selected = "UIC-Halsted")
      updateSelectInput(session, 'select_station_3', selected = "UIC-Halsted")
      updateCheckboxGroupInput(session, 'time_frame_2', selected = c("Monthly"))
      updateCheckboxGroupInput(session, 'time_frame_3', selected = c("Daily"))
      
      hideAllDesc()
      shinyjs::show(id = input$tabs)
    }
    
    if (input$tabs == "insight4") {
      updateTabItems(session, 'tabs', 'compare_graph')
      updateSelectInput(session, 'select_year_2', selected = 2021)
      updateSelectInput(session, 'select_year_3', selected = 2020)
      updateSelectInput(session, 'select_station_2', selected = "UIC-Halsted")
      updateSelectInput(session, 'select_station_3', selected = "UIC-Halsted")
      updateCheckboxGroupInput(session, 'time_frame_2', selected = c("Monthly"))
      updateCheckboxGroupInput(session, 'time_frame_3', selected = c("Monthly"))
      
      hideAllDesc()
      shinyjs::show(id = input$tabs)
    }
    
    if (input$tabs == "insight5") {
      updateTabItems(session, 'tabs', 'compare_graph')
      updateSelectInput(session, 'select_year_2', selected = 2020)
      updateSelectInput(session, 'select_year_3', selected = 2020)
      updateSelectInput(session, 'select_station_2', selected = "UIC-Halsted")
      updateSelectInput(session, 'select_station_3', selected = "O'Hare Airport")
      updateCheckboxGroupInput(session, 'time_frame_2', selected = c("Monthly"))
      updateCheckboxGroupInput(session, 'time_frame_3', selected = c("Monthly"))
      
      hideAllDesc()
      shinyjs::show(id = input$tabs)
    }
    
    if (input$tabs == "insight6") {
      updateTabItems(session, 'tabs', 'compare_graph')
      updateSelectInput(session, 'select_year_2', selected = "Every")
      updateSelectInput(session, 'select_year_3', selected = "Every")
      updateSelectInput(session, 'select_station_2', selected = "Rosemont")
      updateSelectInput(session, 'select_station_3', selected = "Rosemont")
      updateCheckboxGroupInput(session, 'time_frame_2', selected = c("Yearly"))
      updateCheckboxGroupInput(session, 'time_frame_3', selected = c("Daily"))
      
      hideAllDesc()
      shinyjs::show(id = input$tabs)
    }
    
    if (input$tabs == "insight7") {
      updateTabItems(session, 'tabs', 'compare_graph')
      updateSelectInput(session, 'select_year_2', selected = 2016)
      updateSelectInput(session, 'select_year_3', selected = 2016)
      updateSelectInput(session, 'select_station_2', selected = "Every")
      updateSelectInput(session, 'select_station_3', selected = "Every")
      updateCheckboxGroupInput(session, 'time_frame_2', selected = c("Daily"))
      updateCheckboxGroupInput(session, 'time_frame_3', selected = c("Monthly"))
      
      hideAllDesc()
      shinyjs::show(id = input$tabs)
    }
    
    if (input$tabs == "insight8") {
      updateTabItems(session, 'tabs', 'compare_graph')
      updateSelectInput(session, 'select_year_2', selected = 2001)
      updateSelectInput(session, 'select_year_3', selected = 2002)
      updateSelectInput(session, 'select_station_2', selected = "O'Hare Airport")
      updateSelectInput(session, 'select_station_3', selected = "O'Hare Airport")
      updateCheckboxGroupInput(session, 'time_frame_2', selected = c("Daily"))
      updateCheckboxGroupInput(session, 'time_frame_3', selected = c("Daily"))
      
      hideAllDesc()
      shinyjs::show(id = input$tabs)
    }
    
    if (input$tabs == "insight9") {
      updateTabItems(session, 'tabs', 'compare_graph')
      updateSelectInput(session, 'select_year_2', selected = 2018)
      updateSelectInput(session, 'select_year_3', selected = 2018)
      updateSelectInput(session, 'select_station_2', selected = "UIC-Halsted")
      updateSelectInput(session, 'select_station_3', selected = "UIC-Halsted")
      updateCheckboxGroupInput(session, 'time_frame_2', selected = c("Monthly"))
      updateCheckboxGroupInput(session, 'time_frame_3', selected = c("Daily"))
      
      hideAllDesc()
      shinyjs::show(id = input$tabs)
    }
    
    if (input$tabs == "insight10") {
      updateTabItems(session, 'tabs', 'compare_graph')
      updateSelectInput(session, 'select_year_2', selected = 2008)
      updateSelectInput(session, 'select_year_3', selected = 2008)
      updateSelectInput(session, 'select_station_2', selected = "Every")
      updateSelectInput(session, 'select_station_3', selected = "Every")
      updateCheckboxGroupInput(session, 'time_frame_2', selected = c("Daily"))
      updateCheckboxGroupInput(session, 'time_frame_3', selected = c("Monthly"))
      
      hideAllDesc()
      shinyjs::show(id = input$tabs)
    }
    
    if (input$tabs == "About") {
      hideAllDesc()
    }
  })
  
  hideAllDesc()
}

shinyApp(ui = ui, server = server)