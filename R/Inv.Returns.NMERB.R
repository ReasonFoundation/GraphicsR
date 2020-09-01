### PERSI Investment Returns ###
## Data: Datatbase + Manual(AVA returns)
## Base: linePlot() function in `pensionviewr`
# By: Anil

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
#library(shinyFiles)
library(DT)
library(plotly)


urlfile2="https://raw.githubusercontent.com/ReasonFoundation/GraphicsR/master/R/Inv.Returns.NMERB.csv"
ERB.returns <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = cols(.default = "n")))

#View(PERSI.data)

palette_reason <- data.table(
  Orange = "#FF6633", 
  LightOrange = "#FF9900",
  DarkGrey = "#333333", 
  LightGrey = "#CCCCCC",
  SpaceGrey = "#A69FA1",
  DarkBlue = "#0066CC",
  GreyBlue = "#6699CC", 
  Yellow = "#FFCC33", 
  LightBlue = "#66B2FF", 
  SatBlue = "#3366CC", 
  Green = "#669900",
  LightGreen = "#00CC66",
  Red = "#CC0000",
  LightRed = "#FF0000")


#####GEOMEAN FUNCTION
geomean <- function(x) {
  x <- as.vector(na.omit(x))
  x <- x +1
  exp(mean(log(x)))-1 
}
returns <- as.numeric(ERB.returns$mva)/100

returns <- as.vector(na.omit(returns))
nyear <- 10
rolling <- geomean(returns[(21-nyear):20])
#Geomean function
for(i in 1:nyear){
  rolling <- rbind(geomean(returns[((21-i)-nyear):(20-i)]),rolling)
}
rolling <- data.table(rolling*100)
ERB.returns <- data.table(rbind.fill(rolling, ERB.returns))
ERB.returns[(ERB.returns[!is.na(mva),.N]+1):(ERB.returns[!is.na(mva),.N]+rolling[,.N])]$V1<- ERB.returns[(1:rolling[,.N])]$V1
ERB.returns <- ERB.returns[!(1:rolling[,.N])]
# UAL4 <- data.table(UAL4[, Tr30 := tr30[(n-UAL4[!is.na(Actual_Return),.N]):last]])
#View(ERB.returns)
###############
#Adding AVA returns (Arkansas ERS example*)

ERB.returns <- data.frame(ERB.returns)
View(ERB.returns)


##Modified Lineplot
linePlot <- function(data,
                     .var1 = "adec_contribution_rates",
                     .var2 = "actual_contribution_rates",
                     .var3 = "geomean",
                     .var4 = "ava_return",
                     labelY = "Employer Contribution (% of Payroll)",
                     label1 = "ADEC Contribution Rate",
                     label2 = "Actual Contribution Rate",
                     label3 = "10-Year Geometric Rolling Average",
                     label4 = "Actuarially Valued Investment Returns",title = NULL, caption = FALSE) {
  
  #Added 3rd variable
  var1 <- rlang::sym(.var1)
  var2 <- rlang::sym(.var2)
  var3 <- rlang::sym(.var3)
  var4 <- rlang::sym(.var4)
  lab1 <- rlang::sym(label1)
  lab2 <- rlang::sym(label2)
  lab3 <- rlang::sym(label3)
  lab4 <- rlang::sym(label4)
  
  reasontheme::set_reason_theme(style = "slide")
  
  graph <- data %>%
    dplyr::select(
      .data$year,
      !!label1 := !!var1,
      !!label2 := !!var2,
      !!label3 := !!var3,
      !!label4 := !!var4
    ) %>%
    dplyr::mutate_all(dplyr::funs(as.numeric)) %>%
    tidyr::gather(key = "keys", value = "amount", -.data$year)
  
  lineColors <- c(palette_reason$Orange,palette_reason$Yellow, palette_reason$SatBlue, palette_reason$LightGrey) #Updated palette to reason one
  options(repr.plot.width = 1, repr.plot.height = 0.75)
  ggplot2::ggplot(graph, ggplot2::aes(x = graph$year, y = graph$amount)) +
    ggplot2::geom_line(ggplot2::aes(colour = str_wrap(factor(graph$keys), 15)), size = 1.5) + #Added str_wrap(to cut legend text)
    ggplot2::scale_colour_manual(values = lineColors) +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    
    ggplot2::scale_y_continuous(
      breaks = seq(-27, 21, by=3), limits = c(-27, 21), #added limits and expanded break scale
      labels = function(b) {
        paste0(round(b, 0), "%")
      },
      expand = c(0, 0)
    ) +
    
    ggplot2::scale_x_continuous(breaks = seq(min(graph$year), max(graph$year), by = 2), #added blank years
                                expand = c(0, 0)
    ) +
    
    labs(title = paste(title), x = element_blank(), y = labelY, caption = ifelse(isTRUE(caption),paste("reason.org/pensions"),paste("")))+
    theme(legend.text=element_text(size=12),
          axis.text.y = element_text(size=11, color = "black"),
          axis.text.x = element_text(size=11, color = "black"))+#Added element to control legend font size
    theme(legend.position= c(0.45, 0.12)) #Moved legend to the bottom
}

#Line Plot -- Inv.Returns
graph <- linePlot(ERB.returns,.var1 = "mva",.var2 = "arr", .var3 = "V1", .var4 = "ava",
                  labelY = "",
                  label1 = "Market Valued Returns (Actual)",
                  label2 = "Assumed Rate of Return",
                  label3 = "10-Year Geometric Rolling Average",
                  label4 = "Actuarially Valued Investment Returns", title = "NM ERB Investment Returns (2001-20)", caption = TRUE)
graph

#savePlot(graph, source = "", save_filepath = "/Users/anilniraula/Downloads/Inv.Returns.PERSI.png",
#         width_pixels = 600, height_pixels = 400)
