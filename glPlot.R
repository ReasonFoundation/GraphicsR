###glPlot
##Data: Manually Collected
##By: Anil, Jordan, Swaroop

### Clean Global Environment ###
rm(list = ls())

#devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
### Load Packages ###
#install.packages('devtools')
#library(devtools)
#devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
#devtools::install_github("ReasonFoundation/pensionviewr", force = TRUE)
#install.packages("data.table")
library(pensionviewr)
library(tseries)
library(data.table)
library(openxlsx)
library(readr)
library(rsconnect)
library(ggplot2)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(plotly)
library(plyr)
library(dplyr)
library(orca)
library(png)
urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/APERS_GL.csv"
APERSData <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
APERSData <- as.data.table(APERSData)# converted to data.table
APERSData$year <- as.numeric(APERSData$year)

#[2] Reactive expression that filters G/L per user-chosen start year
glPlot <- function(data, title = "", 
                   font = "Calibri", str = 60,
                   lab1 = "Investment<br>Returns", lab2 = "Benefit<br>Changes<br> & Other",
                   lab3 =  "Changes to<br>Actuarial<br>Methods &<br>Assumptions", lab4 = "Negative<br>Amortization",
                   lab5 = "Deviations from<br>Demographic<br>Assumptions",
                   lab6 = "Gains From<br>Pay<br>Increases<br>Not Given",
                   lab7 = "Net Change to<br>Unfunded<br>Liability"){
  
  data <- data.table(data)
  data$year <- as.numeric(data$year)
  min <- min(data$year)
  max <- max(data$year)
  y = data[,lapply(.SD,sum),.SDcols=colnames(data)]*1e-6# sum values by each column
  y = t(y[,2:8])#Saving needed columns and transposing table for graphics
  
  
  #[3] Combine gain/loss data & categories to create interactive Waterfall chart w/ plotly
  
  x <- list(if (!is_null(lab1)) {
    paste(lab1)
  }, if (!is_null(lab2)) {
    paste(lab2)
  }, if (!is_null(lab3)) {
    paste(lab3)
  }, if (!is_null(lab4)) {
    paste(lab4)
  }, if (!is_null(lab5)) {
    paste(lab5)
  }, if (!is_null(lab5)) {
    paste(lab6)
  }, if (!is_null(lab7)) {
    paste(lab7)
  })
  
  x <- as.character(x)
  measure= c(if(!is_null(lab1) & y[1,]>0){"relative"}else{"negative"},
             if(!is_null(lab2) & y[2,]>0){"relative"}else{"negative"},
             if(!is_null(lab3) & y[3,]>0){"relative"}else{"negative"},
             if(!is_null(lab4) & y[4,]>0){"relative"}else{"negative"},
             if(!is_null(lab5) & y[5,]>0){"relative"}else{"negative"},
             if(!is_null(lab6) & y[6,]>0){"relative"}else{"negative"},
             if(lab7 == "Net Change to<br>Unfunded<br>Liability"){"total"
             }else if(!is_null(lab7) & y[7,]>0){"relative"}else{"negative"}
  )
  
  data <- data.frame(x = str_wrap(factor(x, level = x),str), measure, y)
  
  fig <- plot_ly( data,
                  type = "waterfall",
                  measure = ~measure,
                  x = ~x,
                  textposition = "outside",
                  y= ~y,
                  decreasing = list(marker = list(color = palette_reason$Green)),
                  increasing = list(marker = list(color = palette_reason$Red)),
                  totals = list(marker = list(color = palette_reason$Orange)),
                  connector = list(line = list(color= palette_reason$SpaceGrey, width = 1))) 
  
  fig <- fig %>%
    layout(title = paste0("<b>", title, "<b>"," (", min, "-",  max,")"), 
           font = if(!is_null(font)){paste(font)}else{paste("Arial")},
           xaxis = list(title = "",tickfont = list(size = 11, face = "bold")),
           yaxis = list(title = "<b>Change in Unfunded Liability (in $Billions)<b>",
                        titlefont = list(size = 12), range = c(0,5),
                        showgrid = FALSE,
                        tick0 = 0,
                        dtick = 0.5,
                        ticklen = 2,
                        linecolor = '#636363',
                        linewidth = 0.75),
           barmode = 'stack',
           autosize = T,
           showlegend = F) %>% 
    layout(annotations = list(yref = 'paper', xref = "x", showarrow = F, 
                              y = 0, x = 4.5, text = "reason.org/pensions",
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=9, color="black")))
  fig
}



glPlot(APERSData, title = "", 
       font = "Calibri", str = 60,
       lab1 = "Investment<br>Returns", lab2 = "Benefit<br>Changes<br> & Other",
       lab3 =  "Changes to<br>Actuarial<br>Methods &<br>Assumptions", lab4 = "Negative<br>Amortization",
       lab5 = "Deviations from<br>Demographic<br>Assumptions",
       lab6 = "Gains From<br>Pay<br>Increases<br>Not Given",
       lab7 = "Net Change to<br>Unfunded<br>Liability")