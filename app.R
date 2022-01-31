#libraries to include

library(ggplot2)
library(lubridate)
library(scales)
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
uic_data <- data.frame(allData)

# parse data only for UIC-Halsted stop
uic_data_df <- uic_data[uic_data$stationname == "UIC-Halsted",]


# Create the shiny application
ui <- fluidPage(
  titlePanel("CTA Data Visualization"),
  sidebarLayout(position = "left",
    sidebarPanel(
      style = "margin-top:150px;",
      fluidRow(
        column(6, 
               div(checkboxGroupInput("uic_stop_checkbox",
                                      "Time Frame",
                                      choices = c("Year", "Month", "Week"),
                                      selected = c("Year", "Month", "Week")
                                      )
                   
                   )
               ),
        column(6, 
               div(selectInput("select_year",
                               "Year",
                               choices = c("All", 2021:2001),
                               selected = c(2021)
                               )
                   )
               )
      ),
       width = 2
    ),
    mainPanel(
      uiOutput("UIC_plots"),
      width = 10
    )
  )
)

server <- function(input, output) {
  
  # create reactive variable
  uic_df_Reactive <- reactive({
    # create dataframe for a specific year
    if (input$select_year != "All") {
      uic_data[uic_data$year == input$select_year,]
    } else {
      uic_data
    }
  })
  
  # create graph to show yearly data
  output$UIC_entries_year <- renderPlot({
    ggplot(data = uic_data_df, aes(x = year, y = rides)) + 
      geom_bar(stat = "identity") +
      scale_x_continuous(breaks = seq(2001, 2021, by = 2)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Year",
           y = "Entries") +
      ggtitle("Yearly Entries at UIC-Halsted CTA Station")
  })
  
  # create graph to show monthly data
  output$UIC_entries_month <- renderPlot({
    ggplot(data = uic_df_Reactive(), aes(x = reorder(monthChar, month), y = rides)) + 
      geom_bar(stat = "identity") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      labs(x = "Month",
           y = "Entries") +
      ggtitle("Monthly Entries at UIC-Halsted CTA Station")
  })
  
  # create graph to show weekly data
  output$UIC_entries_week <- renderPlot({
    ggplot(data = uic_df_Reactive(), aes(x = reorder(dayChar, day), y = rides)) + 
        geom_bar(stat = "identity") +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
        labs(x = "Day",
             y = "Entries") +
        ggtitle("Day of the week Entries at UIC-Halsted CTA Station")
  })
  
  # render UI
  output$UIC_plots <- renderUI({
    validate(
      need(input$uic_stop_checkbox, 'Check at least one Time Frame!')
    )
    
    year_col <- 0
    month_col <- 0
    week_col <- 0
    
    # decide how much space is required for each graph based on what is visible
    if (all(c("Year", "Month", "Week") %in% input$uic_stop_checkbox)) {
      year_col <- 4
      month_col <- 4
      week_col <- 4
    } else if (all(c("Month", "Week") %in% input$uic_stop_checkbox)) {
      year_col <- 0
      month_col <- 6
      week_col <- 6
      
    } else if (all(c("Year", "Week") %in% input$uic_stop_checkbox)) {
      year_col <- 6
      month_col <- 0
      week_col <- 6
      
    } else if (all(c("Year", "Month") %in% input$uic_stop_checkbox)) {
      year_col <- 6
      month_col <- 6
      week_col <- 0
      
    } else if (all(c("Year") %in% input$uic_stop_checkbox)) {
      year_col <- 12
      month_col <- 0
      week_col <- 0
      
    } else if (all(c("Month") %in% input$uic_stop_checkbox)) {
      year_col <- 0
      month_col <- 12
      week_col <- 0
      
    } else if (all(c("Week") %in% input$uic_stop_checkbox)) {
      year_col <- 0
      month_col <- 0
      week_col <- 12
    }
    
    # put three plots in a row
    fluidRow(
      if (year_col != 0) {
        column(year_col, div(plotOutput("UIC_entries_year")))
      },
      
      if (month_col != 0) {
        column(month_col, div(plotOutput("UIC_entries_month")))
      },
      
      if (week_col != 0) {
        column(week_col, div(plotOutput("UIC_entries_week")))
      }
    )
  })
}

shinyApp(ui = ui, server = server)
