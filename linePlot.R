### linePlot ###
## Data: Database + Manual(AVA returns)
## Base: linePlot() function in `pensionviewr`
# By: Anil, Swaroop, and Jen

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
library(ggplot2)
library(tidyverse)
library(tseries)
library(data.table)
library(readr)
library(rsconnect)
library(dplyr)
library(plyr)

########
##linePlot() function parameters:

#data = data in data.frame, tibble, or data.table format,
#yaxisMin & yaxisMax = Y-axis mininum and maximum values
#yaxisSeq = set sequency of Major breaks
#yaxisScale = 100 for percentages
#format = set Y-axis format to either "%", "$", or empty
#str = set number of srings at which to cut legend text (default is 20),
#labelY = title of Y-axis,
#lab1 - lab5 = character string of line titles/names (default is 0 Lines)

###################
#Using 2 New Functions to download and filter datat from datatabase
<<<<<<< HEAD
PERSI.data <- pullStateData(2001)
PERSI.data <- filterData(PERSI.data, 2001)
=======
reason.data <- pullStateData(2001)
reason.data <- filterData(reason.data, 2001)
>>>>>>> 6a733d618b94f683b4f30eb4ed40b46014fc4890

pl <- planList()
#filter for PERSI
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

#####Modified linePlot

linePlot <- function(data, yaxisMin = 0, yaxisMax = NULL, yaxisSeq = 5,
                     yaxisScale = 100, format = NULL, str = 20,
                     labelY = NULL, lab1 = NULL, 
                     lab2 = NULL, lab3 = NULL, 
                     lab4 = NULL, lab5 = NULL) {
  
  reasontheme::set_reason_theme(style = "slide")
  
  data <- data.table(data) %>% dplyr::mutate_all(dplyr::funs(as.numeric))
  
  
  if(sum(data$return_1yr)>0){  
    
    #####Adding GEOMEAN FUNCTION + CALCULATION if data contains "return_1yr" column
    geomean <- function(x) {
      x <- as.vector(na.omit(x))
      x <- x +1
      exp(mean(log(x)))-1 
    }
    returns <- as.numeric(data$return_1yr)
    nyear <- 10
    rolling <- geomean(returns[1:nyear])
    n <- length(na.omit(returns))-nyear
    #Geomean function
    for(i in 1:n){
      rolling <- rbind(rolling, geomean(returns[(i+1):(i+nyear)]))
    }
    rolling <- data.table(rolling)
    
    data <- data.table(rbind.fill(rolling, data))
    data[(data[!is.na(return_1yr),.N]+1):(data[!is.na(return_1yr),.N]+rolling[,.N])]$V1<- data[(1:rolling[,.N])]$V1
    data <- data[!(1:rolling[,.N])]
    data$year <- as.numeric(data$year)
  }else{NULL}
  
  #Reshuffling column order per gemean calculated column named V1
  if(sum(data$return_1yr)>0){ data <- data %>% select(year, return_1yr, ava_return, arr, V1)}
  else{NULL}
  
  colnames(data) <- c("year", if(!is_null(lab1)){paste(lab1)},
                      if(!is_null(lab2)){paste(lab2)},
                      if(!is_null(lab3)){paste(lab3)},
                      if(!is_null(lab4)){paste(lab4)},
                      if(!is_null(lab5)){paste(lab5)})
  
  #Wide-to-Long format transformation for graphing
  graph <- data.table(melt(data, id.vars="year"))
  
  lineColors <- c(palette_reason$Orange,palette_reason$Yellow, palette_reason$SatBlue,    palette_reason$LightGrey) #Updated palette to reason one
  options(repr.plot.width = 1, repr.plot.height = 0.75)
  
  ggplot2::ggplot(graph, ggplot2::aes(x = year, y = yaxisScale * value, group = variable)) +
    ggplot2::geom_line(ggplot2::aes(colour = str_wrap(factor(variable), str)), size = 1.5) + #Added str_wrap(to cut legend text)
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    ggplot2::scale_colour_manual(values = lineColors) +
    ggplot2::scale_y_continuous(
      breaks = seq(yaxisMin, if(!is.null(yaxisMax)){yaxisMax} #added automatic Y max calcualation
                   else{max(graph$value)*yaxisScale*1.2}, by=yaxisSeq), 
      limits = c(yaxisMin,if(!is.null(yaxisMax)){yaxisMax}
                 else{max(graph$value)*yaxisScale*1.2}), #added limits and expanded break scale
      labels = function(b) {
        if(format == "%"){
          paste0(round(b, 0), "%")
        }else if(format == "$"){#Adding "$" format
          paste0("$", round(b, 0))
        }else{
          paste0(round(b, 0))}
      },
      expand = c(0, 0)
    ) +
    
    ggplot2::scale_x_continuous(breaks = seq(min(graph$year), max(graph$year), by = 2), #added blank years
                                expand = c(0, 0)
    ) +
    
    labs(x = element_blank(), y = labelY)+
    theme(legend.text=element_text(size=13))+ #Added element to control legend font size
    theme(legend.direction = "vertical", #changing legend direction to "vertical"
          legend.box = "horizontal",
          legend.position= c(0.33, 0.09)) #Moved legend to the bottom
}

graph <- linePlot(PERSI.data, yaxisMin = -21, yaxisMax = 21, yaxisSeq = 3,
                  yaxisScale = 100, format = "%", str = 60,
                  labelY = "", lab1 = "Market Valued Returns (Actual)", 
                  lab2 = "Actuarially Valued Investment Return (Smoothed by Plan)", 
                  lab3 = "Assumed Rate of Return", 
                  lab4 = "10-Year Geometric Rolling Average", lab5 = NULL)

#Example w/ AVA vs. AAL
#PERSI.data <- PERSI.data %>% select(year, ava, aal)
#graph <- linePlot(PERSI.data, yaxisMin = 0, yaxisMax = 21, yaxisSeq = 3,
#                  yaxisScale = 1/1000000000, format = "$", str = 60,
#                  labelY = "AVA vs. AAL", lab1 = "Actuarial Value of Assets", 
#                  lab2 = "Actuarial Accrued Liabilities")


graph