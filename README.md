# CS 424 Project 1

This is the first project in CS 424: Data Analysis and Visualization class. The primary intention for creating this visualization is to display ridership data for the CTA Train network for each station and years ranging from 2001 to 2021 in an easy-to-understand fashion. This project uses R as the main language, the following packages (shiny, shinydashboard, lubridate, scales, ggplot2, DT, tidyr) to preprocess, manipulate, and plot the data, RStudio as the primary development environment, and ShinyApps.io as the preferred location for the deployment of the app.

# Cloning this repository and getting this to work on a local machine
Step 1) install Anaconda via https://www.anaconda.com/products/individual. Select the correct download for you local machine's Operating System (i.e., Linux, macOS, Windows, etc.) and follow the installation turorial to successfully install Anaconda.

Step 2) open Anaconda and click on the RStudio app box, then RStudio should open.

Step 3) To actually clone the this repository, you can either do it by downloading the repo as a .zip file then unzipping it or through your local machine's terminal by first clicking on the green code button and then copy/paste the html or ssh clone repo link into the terminal app. 

Step 4) Go back to RStudio and in the file navigation pane (bottom right) navigate to the location your un-zipped folder of this repo is. Navigate inside this folder and open the app.R file. You should be able to run it without errors, but if errors are present use the following commands:

Do this first before trying the conda option, by adding the following line above the library(...) line of code.
<br>install.packages("shiny")
<br>install.packages("shinyWidgets")
<br>install.packages("shinyjs")
<br>install.packages("shinydashboard")
<br>install.packages("ggplot2")
<br>install.packages("lubridate")
<br>install.packages("scales")
<br>install.packages("DT")
<br>install.packages("tidyr")
<br>install.packages("tidyverse")

If the above doesn't work then use your local machine's terminal and type the following commands:

<br>conda install -c conda-forge r-ggplot2
<br>conda install -c conda-forge r-lubridate
<br>conda install -c conda-forge r-scales
<br>conda install -c conda-forge r-tidyr

Step 5) If you didn't have any errors or you solved the errors you had before, then you can run the app.R file by pressing the green run button near the top-right of the left half of the RStudio window.

This reposotory should be running locally on your machine and a Shiny App should be popping up in a new window after RStudio compiles it!
