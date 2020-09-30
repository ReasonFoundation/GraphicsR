
library(reasontheme)
library(grid)#https://bookdown.org/rdpeng/RProgDA/the-grid-package.html
library(tidyverse)
library(openxlsx)
library(tseries)
library(plyr)
library(ggplot2)
library(data.table)
library(openxlsx)
library(readr)
library(rsconnect)
library(base64enc)
#Shiny-----------
#library(shinyFiles)
library(DT)
library(plotly)

# file paths and names of files to read and save
readname <- "perctest.csv"
data_perc <- read_csv2("/Users/jensidorova/Downloads/perctest (1).csv")
savename <- "testbar1.pdf"
# filesave <- file.path("D:", "explica??es", "Genia", savename)
# data needs to be loaded with the following columns: 
# year, 25-50, median, not_important1, not_important2, 50-75
data_perc <- read_csv2(fileread)
# get it into a dataframe format
data_perc <- data.frame(data_perc)
# get all the data in numeric format
data_perc[, c(1:6)] <- sapply(data_perc[, c(1:6)], as.numeric)

#check if the mode of each column in the dataframe is numeric 
sapply(data_perc, mode)

# theme plot parameters
plotTheme <- ggplot2::theme(   
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), 
  axis.line = element_line(colour = "black"),
  plot.margin = margin(0, 0,0,0, "cm"),
  axis.text.y = element_text(size=11, color = "black"),
  axis.text.x = element_text(size=11, color = "black", angle = 90, hjust = 0.5, vjust = 0.5),
  legend.title = element_text(size = 11, colour = "white", face = "bold"))

# width of the bars in years: 0.5 will give no spaces between bars 
widthx <- 0.2
# vector with the coordinates for the geom_rect: Xmin, xmax, ymin and ymax for each year
x <- data.frame(data_perc[, c(1)]-widthx,data_perc[, c(1)]+widthx, data_perc[, c(3)]-data_perc[, c(6)],data_perc[, c(3)]+data_perc[, c(2)]  )
ymax <- ceiling(max(data_perc[, c(3)]+data_perc[, c(2)])*10)/10
# added inside the scale_y_continuous the limits=c(ymin, ymax) to control the y limits
graph <- ggplot() + 
  scale_x_continuous(name="year", breaks = seq(min(data_perc$year), max(data_perc$year), by = 2)) + 
  scale_y_continuous(name="%", breaks = seq(0, ymax, by = ymax/8), limits = c(0,ymax)) +
  # this is used to build the bars alpha is opacity of the bars
  geom_rect(data=x, mapping=aes(xmin=x[,1], xmax=x[,2], ymin=x[,3], ymax=x[,4]), fill="blue", alpha=0.5) +
  # be careful because wit the names of the columns in the data_perc, they must match
  geom_line(data = data_perc, aes(year, median), size=2, color="orange")+
  theme_bw()+
  plotTheme


graph
ggsave("/Users/jensidorova/Downloads/testbar1.pdf")
