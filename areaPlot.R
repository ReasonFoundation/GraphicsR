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

###
##Pull PERSI data
##Load list of plans
pl <- planList()

#PULL DATA
Data <- pullStateData(2001)
Data <- filterData(Data,2001)
#View(Data)
PERSI.debt <- Data %>% filter(plan_name == "Idaho Public Employee Retirement System")
PERSI.debt$year <- as.numeric(PERSI.debt$year)
PERSI.debt$ava <- as.numeric(PERSI.debt$ava)
PERSI.debt$aal <- as.numeric(PERSI.debt$aal)

graph <- areaPlot(data, title = NULL, caption = FALSE, grid = FALSE, ticks = TRUE, sp500 = FALSE, font) {
  


#Set to data.frame for visualization
PERSI.debt <- data.table(PERSI.debt)
#Create ual column
PERSI.debt[,uaal := aal-ava]
###########
#set reasontheme
set_reason_theme(style = "slide")

####Edit areaPlot() manually
############
areaPlot <- function(data, title = NULL, caption = FALSE, grid = FALSE, ticks = TRUE, sp500 = FALSE, font) {
  
  if(isTRUE(sp500)) {
    urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/files/S%26P500.csv"
    SP500 <- read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
    SP500 <- SP500 %>% filter(year >= min(data$year))
    data <- cbind(data, SP500[,2])
    
    graph <-
      data.frame(data %>% select(year, SP500 = sp_500, funded_ratio) %>%
                   tidyr::drop_na())
    
  } else {
    
    data <- data %>%
      dplyr::filter(data$uaal != 0)
    # extrapolate between years linearly
    extrapo <- stats::approx(data$year, data$uaal,  n = 10000)
    extrapo2 <- stats::approx(data$year, data$funded_ratio, n = 10000)
    graph <- data.frame(year = extrapo$x,
                        uaal = extrapo$y,
                        funded_ratio = extrapo2$y) %>%
      tidyr::drop_na()
    graph <- graph %>%
      dplyr::mutate(sign = dplyr::case_when(.data$uaal >= 0 ~ "positive",
                                            .data$uaal < 0 ~ "negative"))
  }
  
  graph$funded_ratio <- as.numeric(graph$funded_ratio)
  if(!isTRUE(sp500)){graph$uaal <- as.numeric(graph$uaal)}
  y_minimum <- as.numeric(min(if(!isTRUE(sp500)){graph$uaal}else{0}))
  y_maximum <- as.numeric(max(if(!isTRUE(sp500)){graph$uaal}else{graph$SP500}))
  
  ggplot2::ggplot(graph,ggplot2::aes(x = graph$year)) +
    ggplot2::geom_area(ggplot2::aes(y = if(!isTRUE(sp500)){graph$uaal}else{graph$SP500}, fill = if(!isTRUE(sp500))   {graph$sign}else{palette_reason$Orange})) +#Removed "color" parameter
    ggplot2::geom_line(ggplot2::aes(y = graph$funded_ratio * (y_maximum)),
                       color = palette_reason$GreyBlue,#Referenced Color Palette
                       size = 1.5) +#Increased Size 1.
    #ggtitle(title)+
    # axis labels
    ggplot2::labs(y = if(!isTRUE(sp500)) {"Unfunded Accrued Actuarial Liabilities (Millions)"}else{"S&P500 Index"}, x = NULL) +
    # colors assigned to pos, neg
    ggplot2::scale_fill_manual(
      values = 
        if(!isTRUE(sp500)){c("negative" = paste(palette_reason$Green),#Referenced Color Palette
                             "positive" = paste(palette_reason$Red))}else{c(paste(palette_reason$Yellow))})+
    # sets the y-axis scale
    ggplot2::scale_y_continuous(
      # creates 10 break points for labels
      breaks = scales::pretty_breaks(n = 10),
      # changes the format to be dollars, without cents, scaled to be in billions
      labels = scales::dollar_format(
        prefix = if(!isTRUE(sp500)){"$"}else{""},
        scale = if(!isTRUE(sp500)){(1e-6)}else{1},
        largest_with_cents = if(!isTRUE(sp500)){1}else{0.001}),
      limits = c(y_minimum, y_maximum*1.2),
      # defines the right side y-axis as a transformation of the left side axis, maximum UAAL = 100%, sets the breaks, labels
      sec.axis = ggplot2::sec_axis(
        ~ . / (y_maximum / 100),
        breaks = scales::pretty_breaks(n = 10),
        name = "Funded Ratio",
        #set limits
        labels = function(b) {
          paste0(round(b, 0), "%")
        }
      ),
      # removes the extra space so the fill is at the origin
      expand = c(0, 0)
    )+
    geom_hline(yintercept=0, linetype="solid", color = "black", size = 0.5)+
    ##Adding titles & caption
    labs(title = paste(title), 
         caption = ifelse(isTRUE(caption),paste("reason.org/pensions"),paste(""))
    )+
    ggplot2::theme(axis.ticks = if(isFALSE(ticks)){ggplot2::element_blank()}else{ggplot2::element_line()}
    )+
    ggplot2::theme(axis.ticks.x = element_line(size = 0.5, color="black"))+
    ggplot2::theme(axis.ticks.y = element_line(size = 0.5, color="black"))+
    ggplot2::theme(axis.text=element_text(size=12),
                   axis.title=element_text(size=12,face="bold"))+
    # coord_cartesian(ylim=(c(y_minimum, y_maximum*1.2)))+##Added limits
    coord_cartesian(expand = FALSE, #turn off axis expansion (padding)
                    xlim = c(2001, 2019), ylim = c(y_minimum, y_maximum*1.2))+ #manually set limits
    # sets the x-axis scale
    ggplot2::scale_x_continuous(breaks = round(seq(min(graph$year), max(graph$year), by = 2), 1),
                                expand = c(0, 0)) +#Added blanck ticks to x-axis
    
    ggplot2::theme(legend.position = "none")+
    ggplot2::theme(text = element_text(family = paste(font), size = 9))+ 
    ##Adding Gridlines
    ggplot2::theme(panel.grid.major.y = element_line(colour= ifelse(isTRUE(grid), 
                                                                    paste(palette_reason$SpaceGrey),"white"),size = (1))) 
  
}
##Plot graph
#debt.plot  <- debtPlot(PERSI.debt,font = "Calibri")
#savePlot(debt.plot, source = "Source: PIP", save_filepath = "/Users/anilniraula/Downloads/test.png",
#width_pixels = 600, height_pixels = 400)
#With Title, caption and grid
#debtPlot(PERSI.debt, "Idaho PERS Pension Debt", caption = TRUE, grid = TRUE, ticks = TRUE, font = "Verdana")

debt <- areaPlot (PERSI.debt, caption = F, grid = F, ticks = F, sp500 = T, font = "Calibri")

debt
