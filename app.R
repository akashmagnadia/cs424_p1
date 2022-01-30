#libraries to include

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(tidyr)


# assume all of the tsv files in this directory are data of the same kind that I want to visualize

temp = list.files(pattern = "*.tsv")
allData2 <- lapply(temp, read.delim)
allData3 <- do.call(rbind, allData2)
allData <- allData3

# convert the dates to the internal format
allData$fullDate <- allData$date
allData$newDate <- as.Date(allData$fullDate, "%m/%d/%Y")
allData$Date <- NULL

# add year day month coloumn
allData <- separate(data = allData, col = date, into = c("month", "date", "year"), sep = "/")
# allData[ c("month", "date", "year")] <- sapply(allData[ c("month", "date", "year")],as.numeric)

# turn to data frame
uic_data <- data.frame(allData)

# parse data only for UIC-Halsted stop
uic_data <- uic_data[uic_data$stationname == "UIC-Halsted",]


# Create the shiny application
ui <- fluidPage(
    plotOutput("UIC_entries")
)

server <- function(input, output) {
  output$UIC_entries <- renderPlot({
    ggplot(data = uic_data, aes(x = year, y = rides)) + geom_bar(stat="identity")
  })
}

shinyApp(ui = ui, server = server)
