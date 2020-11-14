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
#####
urlfile <- "https://raw.githubusercontent.com/ReasonFoundation/GraphicsR/master/Idaho%20Amo%20Data.csv"
NegAmoData <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
NegAmoData <- NegAmoData[2:nrow(NegAmoData),]
year <- NegAmoData[,1]
NegAmo <- NegAmoData[,2]
NegAmoDef <- NegAmoData[,3]
NegAmoExcess <- NegAmoData[,4]
PlotDataAmo <- data.frame(year,'Contributions',NegAmo)
PlotDataDef <- data.frame(year,'Deficiency',NegAmoDef)
PlotDataExcess <- data.frame(year,'Excess',NegAmoExcess)

colnames(PlotDataAmo) <- c('year','type','value')
colnames(PlotDataDef) <- c('year','type','value')
colnames(PlotDataExcess) <- c('year','type','value')

PlotData <- rbind(PlotDataAmo,PlotDataDef,PlotDataExcess)
names <- data.frame(PlotData[,2])
value <- PlotData[,3]

savename <- "contributions2.pdf"
#filesave <- file.path("D:", "explica??es", "Genia", savename)
# data needs to be loaded with the following columns: 
# year, 25-50, median, not_important1, not_important2, 50-75
# get it into a dataframe format

data_perc <- data.frame(NegAmoData)
# get all the data in numeric format
data_perc[, c(1:8)] <- sapply(data_perc[, c(1:8)],function(x){as.numeric(gsub(",", "", x))})
data_perc[, c(2:8)] <- sapply(data_perc[, c(2:8)],function(x){x/1000000})
data_perc[is.na(data_perc)] = 0

#check if the mode of each column in the dataframe is numeric 
sapply(data_perc, mode)
View(data_perc)
# theme plot parameters
plotTheme <- ggplot2::theme(   
  panel.grid.major = element_blank(),
  panel.grid.major.y = element_line(colour = "gray"),
  panel.grid.minor = element_blank(), 
  axis.line = element_line(colour = "black"),
  plot.margin = margin(0, 0,0,0, "cm"),
  axis.text.y = element_text(size=11, color = "black"),
  axis.text.x = element_text(size=11, color = "black", angle = 0, hjust = 0.5, vjust = 0.5),
  legend.title = element_text(size = 11, colour = "white", face = "bold"))

# width of the bars in years: 0.5 will give no spaces between bars 
widthx <- 0.2
alphalevel <- 0.7
# vector with the coordinates for the geom_rect: Xmin, xmax, ymin and ymax for each year
x <- data.frame(data_perc[, c(1)]-widthx, data_perc[, c(1)]+widthx, 0,data_perc[, c(2)] )
x2 <- data.frame(data_perc[, c(1)]-widthx, data_perc[, c(1)]+widthx, -1*data_perc[, c(3)], 0 )
x3 <- data.frame(data_perc[, c(1)]-widthx, data_perc[, c(1)]+widthx, data_perc[, c(2)],data_perc[, c(2)]+data_perc[, c(4)] )
ymax <- ceiling(max(data_perc[, c(2)]+data_perc[, c(4)]))
ymin <- ceiling(min(x2[, c(3)]))
# added inside the scale_y_continuous the limits=c(ymin, ymax) to control the y limits
graph <- ggplot() + 
  scale_x_continuous(name="year", breaks = seq(min(data_perc$X1), max(data_perc$X1), by = 2)) + 
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5), 
    name="(Millions)",
    # removes the extra space so the fill is at the origin
    #expand = c(0, 0)
    ) +
  # this is used to build the bars alpha is opacity of the bars
  geom_rect(data=x2, mapping=aes(xmin=x2[,1], xmax=x2[,2], ymin=x2[,3], ymax=x2[,4]), fill="red", alpha=alphalevel) +
  geom_rect(data=x2, mapping=aes(xmin=x3[,1], xmax=x3[,2], ymin=x3[,3], ymax=x3[,4]), fill="green", alpha=alphalevel) +
  geom_rect(data=x, mapping=aes(xmin=x[,1], xmax=x[,2], ymin=x[,3], ymax=x[,4]), fill="gray", alpha=alphalevel) +
  # be careful because wit the names of the columns in the data_perc, they must match
  ##geom_line(data = data_perc, aes(X1, X2), size=2, color="orange")+
  theme_bw()+
  plotTheme 

graph
#ggsave(filesave)
