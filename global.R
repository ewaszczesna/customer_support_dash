library(shiny)
library(shinydashboard)
library(shiny.info)
library(shinyWidgets)
library(data.table)
library(lubridate)
library(plotly)
library(dplyr)
library(DT)
library(scales)
library(shinycssloaders)

path <- '../dataset Data Analyst Tidio.csv'
data <- setDT(read.csv(path))

# add columns
data$TicketCreatedDate <- ymd(data$TicketCreatedDate)
data$TicketCreatedMonth <- format(as.Date(data$TicketCreatedDate), "%Y-%m")
Sys.setlocale("LC_TIME", "C")
data$TicketCreatedWeekday <-  weekdays(data$TicketCreatedDate)
data$is_solved <- !is.na(data$FullResolutionTime)
data$filled_customer_satisfaction <-!is.na(data$CustomerSatisfaction)
