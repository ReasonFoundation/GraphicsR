## barPlot + Net Amortization
  
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
  
  
  ###
  
  #Issues with:
  # Interest_on_debt data (year gaps for TxERS)
  # payroll/salary Gain/Loss sometimes is empty (even if it is ddded to Other experience we need this category)
  
  #View(PERSI.debt)
  #"Idaho Public Employee Retirement System"

  barPlot <- function(plan = "New Mexico Educational Retirement Board"){
  
  pl <- planList()
  ## PUll plan data
  Data <- pullData(pl,plan) %>% 
    filter(year >= 2001)
  
  Data <- data.table(Data)
  
  Data$fiscal_year_of_contribution <- as.numeric(Data$fiscal_year_of_contribution)
  Data$year <- as.numeric(Data$year)
  
  ## Save Contribution FY Payroll + add year(s) to account for ContrFY-FY difference
    diff <- as.numeric(Data[year == 2002]$fiscal_year_of_contribution-Data[year == 2002]$year)
    x <- matrix(NA,diff,1)
    x <- as.data.frame(x)
    y <-data.frame(Data[year %in% fiscal_year_of_contribution]$covered_payroll_dollar)
    payroll2 <- t(cbind(t(y), x))
  
  ## ADD ne column w/ tries first "amortization_payment_total_amount"
  ## ELSE takes  "total_amortization_payment_percentage" * ContrFY Payroll
  Data <- Data[,net_amo := (interest_on_debt_dollar + 
                              if(sum(colnames(Data) %in% "amortization_payment_total_amount")!=0){
                                amortization_payment_total_amount}else{
                                  total_amortization_payment_percentage * 
                                    payroll2})]
  
  Data <- Data %>% select(year, state, plan_name, net_amo)
  Data
  }
  
######
View(barPlot(plan = "New Mexico Educational Retirement Board"))
#######
## TRY Actual ER Contribution - (Total NC - EE NC)

#"amortization_payment_total_amount",
#"total_amortization_payment_percentage",
#"covered_payroll_dollar",
#"fiscal_year_of_contribution",
#"unfunded_actuarially_accrued_liabilities_dollar"