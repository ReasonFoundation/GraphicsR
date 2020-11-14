### PERSI Investment Returns ###
## Data: Datatbase + Manual(AVA returns)
## Base: linePlot() function in `pensionviewr`
# By: Anil

rm(list=ls())
###Load/install packages
R.Version()
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

###################
#Using 2 New Functions to download and filter datat from datatabase
PERSI.data <- pullStateData(2001)
PERSI.data <- filterData(PERSI.data, 2001)

pl <- planList()
#filter for PERSI
PERSI.data<- PERSI.data %>% filter (plan_name == "Idaho Public Employee Retirement System")
#View(PERSI.data)

PERSI.data$year <- as.numeric(PERSI.data$year)
#Set to data.frame for visualization
PERSI.data <- data.frame(PERSI.data)
#View(PERSI.data)


#####GEOMEAN FUNCTION
geomean <- function(x) {
  x <- as.vector(na.omit(x))
  x <- x +1
  exp(mean(log(x)))-1 
}
returns <- as.numeric(PERSI.data$return_1yr)
nyear <- 10
rolling <- geomean(returns[1:nyear])
n <- length(na.omit(returns))-nyear
#Geomean function
for(i in 1:n){
  rolling <- rbind(rolling, geomean(returns[(i+1):(i+nyear)]))
}
rolling <- data.table(rolling)

PERSI.data <- data.table(rbind.fill(rolling, PERSI.data))
PERSI.data[(PERSI.data[!is.na(return_1yr),.N]+1):(PERSI.data[!is.na(return_1yr),.N]+rolling[,.N])]$V1<- PERSI.data[(1:rolling[,.N])]$V1
PERSI.data <- PERSI.data[!(1:rolling[,.N])]
# UAL4 <- data.table(UAL4[, Tr30 := tr30[(n-UAL4[!is.na(Actual_Return),.N]):last]])
#View(PERSI.data)

###############
#Adding AVA returns (Arkansas ERS example*)
ava_returns <- matrix(0, 19,1)
ava_returns[,1] <- c(NA, NA, NA, NA, 4.70, 9.00, 12.40, 8.00, -5.90, 2.00, 3.10, 4.50, 11.40, 13.80, 8.80, 8.20, 7.70, 5.80, 6.50)
#View(ava_returns)
ava_returns <- data.table(ava_returns/100)

PERSI.data <- data.table(PERSI.data)
PERSI.data <- PERSI.data[, ava_return := ava_returns]

PERSI.data$year <- as.numeric(PERSI.data$year)

PERSI.data <- data.frame(PERSI.data)


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
                     label4 = "Actuarially Valued Investment Returns") {
  
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
  ggplot2::ggplot(graph, ggplot2::aes(x = graph$year, y = graph$amount * 100)) +
    ggplot2::geom_line(ggplot2::aes(colour = str_wrap(factor(graph$keys), 15)), size = 1.5) + #Added str_wrap(to cut legend text)
    ggplot2::scale_colour_manual(values = lineColors) +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    
    ggplot2::scale_y_continuous(
      breaks = seq(-28, 28, by=4), limits = c(-28, 28), #added limits and expanded break scale
      labels = function(b) {
        paste0(round(b, 0), "%")
      },
      expand = c(0, 0)
    ) +
    
    ggplot2::scale_x_continuous(breaks = seq(min(graph$year), max(graph$year), by = 2), #added blank years
                                expand = c(0, 0)
    ) +
    
    labs(x = element_blank(), y = labelY)+
    theme(legend.text=element_text(size=12))+ #Added element to control legend font size
    theme(legend.position= c(0.51, 0.1)) #Moved legend to the bottom
}

#Line Plot -- Inv.Returns
graph <- linePlot(PERSI.data,.var1 = "return_1yr",.var2 = "arr", .var3 = "V1", .var4 = "ava_return",
                  labelY = "",
                  label1 = "Market Valued Returns (Actual)",
                  label2 = "Assumed Rate of Return",
                  label3 = "10-Year Geometric Rolling Average",
                  label4 = "Actuarially Valued Investment Returns")
graph


######
###### DEBT PLOT #####
######

###########
#set reasontheme
set_reason_theme(style = "slide")
tick <- c("axis.ticks = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()")

####Edit detPlot() manually
############
debtPlot <- function(data, title = NULL, caption = FALSE, grid = FALSE, ticks = TRUE, font) {
  
  data <- data %>%
    dplyr::filter(data$unfunded_actuarially_accrued_liabilities_dollar != 0)
  # extrapolate between years linearly
  extrapo <- stats::approx(data$year, data$unfunded_actuarially_accrued_liabilities_dollar,  n = 10000)
  extrapo2 <- stats::approx(data$year, data$funded_ratio, n = 10000)
  graph <-
    data.frame(year = extrapo$x,
               unfunded_actuarially_accrued_liabilities_dollar = extrapo$y,
               funded_ratio = extrapo2$y) %>%
    tidyr::drop_na()
    graph <- graph %>%
    dplyr::mutate(sign = dplyr::case_when(.data$unfunded_actuarially_accrued_liabilities_dollar >= 0 ~ "positive",
                                          .data$unfunded_actuarially_accrued_liabilities_dollar < 0 ~ "negative"))
  
  y_minimum <- min(graph$unfunded_actuarially_accrued_liabilities_dollar)
  y_maximum <- max(graph$unfunded_actuarially_accrued_liabilities_dollar)
  ggplot2::ggplot(graph,
                  ggplot2::aes(x = graph$year)) +
    ggplot2::geom_area(ggplot2::aes(y = graph$unfunded_actuarially_accrued_liabilities_dollar, fill = graph$sign)) +#Removed "color" paramater
    ggplot2::geom_line(ggplot2::aes(y = graph$funded_ratio * (y_maximum)),
                       color = palette_reason$GreyBlue,#Referenced Color Palette
                       size = 1.7) +#Increased Size 1.
    #ggtitle(title)+
    # axis labels
    ggplot2::labs(y = "Unfunded Accrued Actuarial Liabilities (Millions)", x = NULL) +
    
    # colors assigned to pos, neg
    ggplot2::scale_fill_manual(
      values = c("negative" = paste(palette_reason$Green),#Referenced Color Palette
                 "positive" = paste(palette_reason$Red)),#Referenced Color Palette
      aesthetics = c("colour", "fill")
    ) +
    # sets the y-axis scale
    ggplot2::scale_y_continuous(
      # creates 10 break points for labels
      breaks = scales::pretty_breaks(n = 10),
      # changes the format to be dollars, without cents, scaled to be in billions
      labels = scales::dollar_format(
        prefix = "$",
        scale = (1e-6),
        largest_with_cents = 1,
      ), 
      limits = c(y_minimum, y_maximum*1.2),
      # defines the right side y-axis as a transformation of the left side axis, maximum unfunded_actuarially_accrued_liabilities_dollar = 100%, sets the breaks, labels
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
#         width_pixels = 600, height_pixels = 400)
#With Title, caption and grid
#debtPlot(PERSI.debt, "Idaho PERS Pension Debt", caption = TRUE, grid = TRUE, ticks = TRUE, font = "Verdana")
debt.plot <- debtPlot(PERSI.data, caption = F, grid = F, ticks = T,font = "Calibri")


### ASSET ALLOCATION ###

plotTheme <- ggplot2::theme(   panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                               plot.margin = margin(0, 0,0,0, "cm"),
                               axis.text.y = element_text(size=11, color = "black"),
                               axis.text.x = element_text(size=11, color = "black", angle = 0, hjust = 0.5, vjust = 0),
                               legend.title = element_text(size = 11, colour = "white", face = "bold"))



urlfile2="https://raw.githubusercontent.com/ReasonFoundation/GraphicsR/master/R/PERSI.Asset.Alloc.csv"
PERSI.assets <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = cols(.default = "n")))
PERSI.assets <- data.table(PERSI.assets)
#Adding Othe to Alternatives
PERSI.assets$alternatives <- PERSI.assets$alternatives+ PERSI.assets$other
PERSI.assets <- PERSI.assets %>% select(year, real.estate, alternatives, equity, fixed.income, cash)

#View(PERSI.assets)
#Transpose datat from long ot wide format (each year contains Variable & Value)
PERSI.assets <- melt(PERSI.assets, id.vars="year")

asset.alloc <- (ggplot(PERSI.assets) + geom_area(aes(x = year, y = value, fill = variable, group = variable), position="fill")+
  scale_fill_manual(values=c(palette_reason$Yellow,palette_reason$Orange, 
                             palette_reason$DarkGrey, palette_reason$LightBlue, palette_reason$SatBlue))+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"), name = "",
                     breaks = seq(0, 1, by = 0.1), limits = c(0, 1))+
  scale_x_continuous(labels = function(x) paste0(x, ""), name = "",
                     breaks = seq(2001, 2019, by = 2), limits = c(2001, 2019))+
  theme_bw()+
  plotTheme+
  theme(legend.position="bottom"))


#asset.alloc
##Combine Graphs]
#library(ggpubr)
#ggarrange(debt.plot, graph, asset.alloc, ncol = 1, nrow = 3)

#savePlot(graph, source = "", save_filepath = "/Users/anilniraula/Downloads/Inv.Returns.PERSI.png",
#         width_pixels = 600, height_pixels = 400)
