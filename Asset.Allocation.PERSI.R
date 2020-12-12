#### PERSI Asset Allocation ####
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
#devtools::insta

plotTheme <- ggplot2::theme(panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            plot.margin = margin(1, 1,0,0, "cm"),
                            axis.text.y = element_text(size=9, color = "black"),
                            axis.text.x = element_text(size=9, color = "black", angle = 0, hjust = 0.5, vjust = 0),
                            legend.title = element_text(size = 9, colour = "white", face = "bold")
                            )

#Load G/L data for PERSI
urlfile2="https://raw.githubusercontent.com/ReasonFoundation/GraphicsR/master/R/PERSI.Asset.Alloc.csv"
PERSI.assets <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = cols(.default = "n")))
PERSI.assets <- data.table(PERSI.assets)
#str(PERSI.assets)
#Adding Othe to Alternatives
PERSI.assets$alternatives <- PERSI.assets$alternatives+ PERSI.assets$other
PERSI.assets <- PERSI.assets %>% select(year, real.estate, alternatives, equity, fixed.income, cash)
colnames(PERSI.assets) <- c("year", "Real Rstate", "Alternatives", "Equities", "Fixed Income", "Cash Equivalents")

#View(PERSI.assets)
#Transpose data from long ot wide format (each year contains Variable & Value)
PERSI.assets <- melt(PERSI.assets, id.vars="year")

ggplot(PERSI.assets) + 
  geom_area(aes(x = year, y = value, fill = variable, group = variable), position="fill") +
  scale_fill_manual(values=c(palette_reason$Yellow,
                             palette_reason$Orange, 
                             palette_reason$DarkGrey, 
                             palette_reason$LightBlue, 
                             palette_reason$SatBlue)
                                                   ) +
  
  scale_y_continuous(labels = function(x) paste0(x*100,"%"), name = "% of Investment Portfolio",
                     breaks = seq(0, 1, by = 0.1), limits = c(0, 1), expand=c(0,0)) +
  scale_x_continuous(labels = function(x) paste0(x, ""), name = "",
                     breaks = seq(2001, 2019, by = 2), limits = c(2001, 2019), expand=c(0,0)) +
  theme_bw() +
  plotTheme +
  theme(legend.position="bottom")