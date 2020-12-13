### linePlot ###
## Data: Database + Manual(AVA returns)
## Base: linePlot() function in `pensionviewr`
# By: Anil, Swaroop, and Jen

###################
rm(list=ls())
###Load/install packages
#R.Version()
#https://github.com/ReasonFoundation/pensionviewr
#Create token -> usethis::edit_r_environ() -> restart -> Sys.getenv("GITHUB_PAT")
#install.packages('devtools')
#library(devtools)
#devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
#devtools::install_github("ReasonFoundation/pensionviewr", force = TRUE)
library(reasontheme)
library(pensionviewr)
#library(janitor)
library(grid)#https://bookdown.org/rdpeng/RProgDA/the-grid-package.html
library(tidyverse)
#library(openxlsx)
library(tseries)
library(plyr)
#library(ggplot2)
library(data.table)
library(openxlsx)
#library(readr)
library(rsconnect)
library(base64enc)
#Shiny-----------
library(shiny)
library(shinyWidgets)
library(shinyjs)
#library(shinyFiles)
library(DT)
library(plotly)
library(httr)
library(jsonlite)

PERSI.data <- pullStateData(2001)
PERSI.data <- filterData(PERSI.data, 2001)
PERSI.data<- PERSI.data %>% filter (plan_name == "Idaho Public Employee Retirement System")
#View(PERSI.data)
PERSI.data$year <- as.numeric(PERSI.data$year)
#Set to data.frame for visualization
PERSI.data <- data.frame(PERSI.data)
#View(PERSI.data)
# UAL4 <- data.table(UAL4[, Tr30 := tr30[(n-UAL4[!is.na(Actual_Return),.N]):last]])
###############
#Adding AVA returns (Arkansas ERS example*)
ava_returns <- matrix(0, 19,1)
ava_returns[,1] <- c(-0.064, -0.0736, 0.0332, 0.1763, 4.70, 9.00, 12.40, 8.00, -5.90, 2.00, 3.10, 4.50, 11.40, 13.80, 8.80, 8.20, 7.70, 5.80, 6.50)
#View(ava_returns)
ava_returns <- data.table(ava_returns/100)

PERSI.data <- data.table(PERSI.data)
PERSI.data <- PERSI.data[, ava_return := ava_returns]
PERSI.data$year <- as.numeric(PERSI.data$year)
PERSI.data <- data.frame(PERSI.data)
PERSI.data <- PERSI.data %>% select(year, return_1yr, ava_return, arr)
#View(PERSI.data)
#install.packages(c("httr", "jsonlite"))
#devtools::install_github("ropensci/plotly")
 
graph <- linePlot(PERSI.data, yaxisMin = -21, yaxisMax = 21, yaxisSeq = 3,
                  yaxisScale = 100, format = "%", str = 60,
                  labelY = "", lab1 = "Market Valued Returns (Actual)", 
                  lab2 = "Actuarially Valued Investment Return (Smoothed by Plan)", 
                  lab3 = "Assumed Rate of Return", 
                  lab4 = "10-Year Geometric Rolling Average")
graph
    

linePlot <- function (data, yaxisMin = 0, yaxisMax = NULL, yaxisSeq = 5, 
                      yaxisScale = 100, format = NULL, str = 20, labelY = NULL, 
                      lab1 = NULL, lab2 = NULL, lab3 = NULL, lab4 = NULL, lab5 = NULL) 
{
  reasontheme::set_reason_theme(style = "slide")
  geomean <- function(x) {
    x <- as.vector(na.omit(x))
    x <- x + 1
    exp(mean(log(x))) - 1
  }
  if (sum(!is.na(data$return_1yr)) > 0) {
    returns <- as.numeric(data$return_1yr)
    nyear <- 10
    rolling <- geomean(returns[1:nyear])
    n <- length(na.omit(returns)) - nyear
    for (i in 1:n) {
      rolling <- rbind(rolling, geomean(returns[(i + 1):(i + 
                                                           nyear)]))
    }
    data <- data.frame(data) %>% dplyr::mutate_all(dplyr::funs(as.numeric))
    rolling <- data.table(rolling)
    data <- data.table(rbind.fill(rolling, data))
    x <- data[!is.na(return_1yr), .N]
    data[(x + 1):(x + rolling[, .N])]$V1 <- data[(1:rolling[, 
                                                            .N])]$V1
    data <- data[!(1:rolling[, .N])]
    data$year <- as.numeric(data$year)
    data <- data %>% select(year, return_1yr, ava_return, 
                            arr, V1)
  }
  else {
    data <- data.frame(data) %>% dplyr::mutate_all(dplyr::funs(as.numeric))
  }
  colnames(data) <- c("year", if (!is_null(lab1)) {
    paste(lab1)
  }, if (!is_null(lab2)) {
    paste(lab2)
  }, if (!is_null(lab3)) {
    paste(lab3)
  }, if (!is_null(lab4)) {
    paste(lab4)
  }, if (!is_null(lab5)) {
    paste(lab5)
  })
  graph <- data.table(melt(data, id.vars = "year"))
  lineColors <- c(palette_reason$Orange, palette_reason$Yellow, 
                  palette_reason$SatBlue, palette_reason$LightGrey)
  options(repr.plot.width = 1, repr.plot.height = 0.75)
  ggplot2::ggplot(graph, ggplot2::aes(x = year, y = yaxisScale * 
                                        value, group = variable)) + ggplot2::geom_line(ggplot2::aes(colour = str_wrap(factor(variable), 
                                                                                                                      str)), size = 1.5) + ggplot2::geom_hline(yintercept = 0, 
                                                                                                                                                               color = "black") + ggplot2::scale_colour_manual(values = lineColors) + 
    ggplot2::scale_y_continuous(breaks = seq(yaxisMin, if (!is.null(yaxisMax)) {
      yaxisMax
    }
    else {
      max(graph$value) * yaxisScale * 1.2
    }, by = yaxisSeq), limits = c(yaxisMin, if (!is.null(yaxisMax)) {
      yaxisMax
    } else {
      max(graph$value) * yaxisScale * 1.2
    }), labels = function(b) {
      if (format == "%") {
        paste0(round(b, 0), "%")
      }
      else if (format == "$") {
        paste0("$", round(b, 0))
      }
      else {
        paste0(format, round(b, 0))
      }
    }, expand = c(0, 0)) + ggplot2::scale_x_continuous(breaks = seq(min(graph$year), 
                                                                    max(graph$year), by = 2), expand = c(0, 0)) + labs(x = element_blank(), 
                                                                                                                       y = labelY) + theme(legend.text = element_text(size = 13)) + 
    theme(legend.direction = "vertical", legend.box = "horizontal", 
          legend.position = c(0.33, 0.09))
}


graph <- linePlot(PERSI.data, yaxisMin = -21, yaxisMax = 21, yaxisSeq = 3,
                  yaxisScale = 100, format = "%", str = 60,
                  labelY = "", lab1 = "Market Valued Returns (Actual)", 
                  lab2 = "Actuarially Valued Investment Return (Smoothed by Plan)", 
                  lab3 = "Assumed Rate of Return", 
                  lab4 = "10-Year Geometric Rolling Average", lab5 = NULL)
  
# graph + theme(legend.position= c(0.6, 0.09))

#Example w/ AVA vs. AAL
#PERSI.data <- PERSI.data %>% select(year, ava, aal)
#graph <- linePlot(PERSI.data, yaxisMin = 0, yaxisMax = 21, yaxisSeq = 3,
#                  yaxisScale = 1/1000000000, format = "$", str = 60,
#                  labelY = "AVA vs. AAL", lab1 = "Actuarial Value of Assets", 
#                  lab2 = "Actuarial Accrued Liabilities")


graph