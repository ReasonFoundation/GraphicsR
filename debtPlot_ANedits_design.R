value <- rnorm(19, 5, 15)
year <- seq(2001, 2019, by = 1)
DF <- data.frame(t(rbind(value,year)))
colnames(DF) <- c("value", "year")
ggplot(DF, aes(x = year, y = value))+geom_line(color = "red")+
geom_hline(yintercept = 0, linetype = "solid", color = "black")+
theme(
  axis.text.x = element_text(vjust = 35))+
scale_x_continuous(labels = function(x) paste0(x, ""), name = "year",
                   breaks = seq(2001, 2019, by = 2), limits = c(2001, 2019))

theme(
  axis.text.x = element_text(vjust = 20))
 
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
#devtools::install_github("ropensci/plotly")

#DF <- data.table(Fiscal_Year = seq(2001, 2019, by =1))
#DF <- DF[,var1 := data.table(rnorm(19, 30, 5))]
#DF <- DF[,var2 := data.table(rnorm(19, 60, 10))]
#DF <- data.frame(DF)

#ggplot() +
#  geom_col(data=DF %>% pivot_longer(starts_with("var")),
#           mapping = aes(x=Fiscal_Year, y=value,group = 1, fill = "orangered1"),
#           color = "black", position = "dodge2")+
#  theme_bw()

pl <- planList()
states <- as.character(unique(pl[,3]))
plans <- as.character(unique(pl[,2]))
#View(pullData(pl[state=="New Mexico"], pl[state=="New Mexico"]$display_name))
#View(pl)

#palette_reason$categorical[[3]]
#reason_color_pal("categorical")
#palette

set_reason_theme(style = "slide")

pullSourceData <- function(plan_name){
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "d629vjn37pbl3l",
    host = "ec2-3-209-200-73.compute-1.amazonaws.com",
    port = 5432,
    user = "reason_readonly",
    password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
    sslmode = "require")
  # define the query to retrieve the plan data
  
  if(str_count(plan_name)<6){
    query <- paste("select * from pull_data_state_only()
where year > '2001'
and attribute_name in ('1 Year Investment Return Percentage',
'1 Year Investment Return Percentage',
'Investment Return Assumption for GASB Reporting',
'Actuarially Accrued Liabilities Dollar',
'Total Normal Cost Percentage',
'Covered Payroll Dollar',
'Payroll Growth Assumption',
'Total Benefits Paid Dollar')")}else{
  
  plan_id <- pl$id[pl$display_name == plan_name]
  query <- paste("select * from pull_plan_data(",plan_id,")")
  #paste0("select * from pull_plan_data('", str_replace(plan_name,"'", "''"), "')")
}
  
  ###################
  
  result <- RPostgres::dbSendQuery(con, query)
  #RPostgres::dbBind(result, list(1))
  all_data <- RPostgres::dbFetch(result) %>%
    janitor::clean_names()
  RPostgres::dbClearResult(result)
  RPostgres::dbDisconnect(con)
  
  all_data %>%
    dplyr::group_by_at(dplyr::vars(-.data$attribute_value)) %>%  # group by everything other than the value column.
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%  # build group index
    tidyr::spread(.data$attribute_name, .data$attribute_value, convert = TRUE) %>%    # spread
    dplyr::select(-.data$row_id) %>%  # drop the index
    janitor::clean_names()
}

##Pull state Data only

pullStateData <- function(FY){
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "d629vjn37pbl3l",
    host = "ec2-3-209-200-73.compute-1.amazonaws.com",
    port = 5432,
    user = "reason_readonly",
    password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
    sslmode = "require")
  
  
  query <- paste("select * from pull_data_state_only()
where year > '", paste(FY-1), "'
and attribute_name in ('1 Year Investment Return Percentage',
'Investment Return Assumption for GASB Reporting',
'Market Value of Assets Dollar',
'Actuarial Value of Assets GASB Dollar',
'Actuarially Accrued Liabilities Dollar',
'Actuarial Funded Ratio Percentage',
'Unfunded Actuarially Accrued Liabilities Dollar',
'Employee Contribution Dollar',
'Employee Normal Cost Percentage',
'Employer Normal Cost Dollar',
'Employer Contribution Regular Dollar',
'Total Contribution Dollar',
'Total Normal Cost Percentage',
'Total Amortization Payment Percentage',
'Covered Payroll Dollar',
'Actuarially Required Contribution Dollar',
'Actuarially Required Contribution Paid Percentage',
'Employers Projected Actuarial Required Contribution Percentage of Payroll',
'Payroll Growth Assumption',
'Type of Employees Covered',
'Total Pension Liability Dollar',
'Amortizaton Method',
'Actuarial Cost Method in GASB Reporting',
'Number of Years Remaining on Amortization Schedule',
'Actuarial Cost Method in GASB Reporting',
'Wage Inflation',
'Total Benefits Paid Dollar')")
  
  result <- RPostgres::dbSendQuery(con, query)
  #RPostgres::dbBind(result, list(1))
  all_data <- RPostgres::dbFetch(result) %>%
    janitor::clean_names()
  RPostgres::dbClearResult(result)
  RPostgres::dbDisconnect(con)
  
  all_data %>%
    dplyr::group_by_at(dplyr::vars(-.data$attribute_value)) %>%  # group by everything other than the value column.
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%  # build group index
    tidyr::pivot_wider(names_from = attribute_name, values_from = attribute_value) %>%# CHANGED to pivot
    dplyr::select(-.data$row_id) %>%  # drop the index
    dplyr::arrange(display_name, year) %>%
    janitor::clean_names()
  
}

#pl <- planList()
#View(pullStateData(2010))
#str_count(paste0("\"",state,"\""))<6
##Add columns
##Convert to Wide format
##Why 112 state plans (which 2 are missing?)
#View(unique(all_data$display_name))

#NMPERA.wide <- pullSourceData("New Mexico Educational Retirement Board")
###Columns were some plans have no data for
columns <- c("total_pension_liability_dollar", "wage_inflation",
             "payroll_growth_assumption", "other_contribution_dollar",
             "other_additions_dollar", "x1_year_investment_return_percentage",
             "amortizaton_method", "number_of_years_remaining_on_amortization_schedule",
             "fiscal_year_of_contribution", "statutory_payment_dollar",
             "statutory_payment_percentage", "discount_rate_assumption")

#Custom Function to filter for number of variables we commonly use in pension analysis (state plans*)
filteredData <- function(plan, y, fy){
  Plan <- data.table(pullData(plan, y))
  ##Create missing columns for plans with no data for st 7 variable
  for (i in (1:length(columns))){
    if(sum((colnames(Plan) == columns[i]))==0) {
      Plan[,columns[i] := NA]}
  }
  
  if(is.na(Plan$discount_rate_assumption)){ 
    Plan$discount_rate_assumption <- Plan$investment_return_assumption_for_gasb_reporting}
  ####
  Plan <- Plan %>%
    filter(year > fy-1)
  Plan <- Plan %>%
    select(
      year,
      plan_name = display_name,
      state,
      return_1yr = x1_year_investment_return_percentage,
      actuarial_cost_method_in_gasb_reporting,
      funded_ratio = actuarial_funded_ratio_percentage,
      actuarial_valuation_report_date,
      ava = actuarial_value_of_assets_gasb_dollar,
      mva = market_value_of_assets_dollar,
      mva_smooth = market_assets_reported_for_asset_smoothing,#added
      aal = actuarially_accrued_liabilities_dollar,
      tpl = total_pension_liability_dollar,
      adec = actuarially_required_contribution_dollar,
      adec_paid_pct = actuarially_required_contribution_paid_percentage,
      statutory = statutory_payment_dollar,#NEW
      statutory_pct = statutory_payment_percentage,#NEW
      amortizaton_method,
      asset_valuation_method_for_gasb_reporting,
      total_benefit_payments = total_benefits_paid_dollar,#added
      benefit_payments = benefit_payments_dollar,
      refunds = refunds_dollar,#added
      admin_exp = administrative_expense_dollar,
      cost_structure,
      payroll = covered_payroll_dollar,
      ee_contribution = employee_contribution_dollar,
      ee_nc_pct = employee_normal_cost_percentage,
      er_contribution = employer_contribution_regular_dollar,
      er_nc_pct = employer_normal_cost_percentage,
      er_state_contribution = employer_state_contribution_dollar,
      er_proj_adec_pct = employers_projected_actuarial_required_contribution_percentage_of_payroll,
      other_contribution = other_contribution_dollar,#added
      other_additions = other_additions_dollar,#added
      fy_contribution = fiscal_year_of_contribution,
      inflation_assum = inflation_rate_assumption_for_gasb_reporting,
      arr = investment_return_assumption_for_gasb_reporting,
      dr = discount_rate_assumption,#NEW
      number_of_years_remaining_on_amortization_schedule,
      payroll_growth_assumption,
      total_amortization_payment_pct = total_amortization_payment_percentage,
      total_contribution = total_contribution_dollar,
      total_nc_pct = total_normal_cost_percentage,
      total_number_of_members,
      total_proj_adec_pct = total_projected_actuarial_required_contribution_percentage_of_payroll,
      type_of_employees_covered,
      unfunded_actuarially_accrued_liabilities_dollar,
      wage_inflation
    )
}
#x <- filteredData(pl, "California State Teachers Retirement System", 2001)
#View(x)
#y <- filteredData(pl, "Employee Retirement System of Hawaii", 2001)

filteredSourceData <- function(plan_name, fy){
  Data <- data.table(
    pullSourceData(plan_name))##Moved pullSourceData() inside this function
  Data <- data.table(Data %>%
                       filter(year > fy-1))
  ##Create columns that don't have any data
  for (i in (1:length(columns))){
    if(sum((colnames(Data) == columns[i]))==0) {
      Data[,columns[i] := NA]}
  }
  if(is.na(Data$discount_rate_assumption)){ 
    Data$discount_rate_assumption <- Data$investment_return_assumption_for_gasb_reporting}
  ####
  Data <- Data %>%
    select(
      year,
      plan_name = display_name,
      state,
      data_source_name,#Added
      return_1yr = x1_year_investment_return_percentage,
      actuarial_cost_method_in_gasb_reporting,
      funded_ratio = actuarial_funded_ratio_percentage,
      actuarial_valuation_report_date,
      ava = actuarial_value_of_assets_gasb_dollar,
      mva = market_value_of_assets_dollar,
      mva_smooth = market_assets_reported_for_asset_smoothing,#added
      aal = actuarially_accrued_liabilities_dollar,
      tpl = total_pension_liability_dollar,
      adec = actuarially_required_contribution_dollar,
      adec_paid_pct = actuarially_required_contribution_paid_percentage,
      statutory = statutory_payment_dollar,#NEW
      statutory_pct = statutory_payment_percentage,#NEW
      amortizaton_method,
      asset_valuation_method_for_gasb_reporting,
      total_benefit_payments = total_benefits_paid_dollar,#added
      benefit_payments = benefit_payments_dollar,
      refunds = refunds_dollar,#added
      admin_exp = administrative_expense_dollar,
      cost_structure,
      payroll = covered_payroll_dollar,
      ee_contribution = employee_contribution_dollar,
      ee_nc_pct = employee_normal_cost_percentage,
      er_contribution = employer_contribution_regular_dollar,
      er_nc_pct = employer_normal_cost_percentage,
      er_state_contribution = employer_state_contribution_dollar,
      er_proj_adec_pct = employers_projected_actuarial_required_contribution_percentage_of_payroll,
      other_contribution = other_contribution_dollar,#added
      other_additions = other_additions_dollar,#added
      fy_contribution = fiscal_year_of_contribution,
      inflation_assum = inflation_rate_assumption_for_gasb_reporting,
      arr = investment_return_assumption_for_gasb_reporting,
      dr = discount_rate_assumption,#NEW
      number_of_years_remaining_on_amortization_schedule,
      payroll_growth_assumption,
      total_amortization_payment_pct = total_amortization_payment_percentage,
      total_contribution = total_contribution_dollar,
      total_nc_pct = total_normal_cost_percentage,
      total_number_of_members,
      total_proj_adec_pct = total_projected_actuarial_required_contribution_percentage_of_payroll,
      type_of_employees_covered,
      unfunded_actuarially_accrued_liabilities_dollar,
      wage_inflation
    )
}

#NMPERA.wide <- pullSourceData("New Mexico Educational Retirement Board")
#NMPERA.wide <- filteredSourceData(NMPERA.wide, 2001)
#View(NMPERA.wide)
#View(filteredData(pl, "CalPERS - California Public Employees Retirement Fund", 2001))
#x <- data.table(filteredData(pl, "New Mexico Public Employees Retirement Association", 2001))
#View(x)

###For plan-by-plan data shiny app connects directly to the database
###For aggregate (state/US level analysis) data it uses imported csv file.
##Download Reason Data ----------------------------------------------------

##State-level data from ReasonGitHub

urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/reason.data.state.csv"
reason.data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
reason.data <- data.table(reason.data)

reason.data <- reason.data %>%
  #########
select(
  year,
  plan_name = display_name,
  state,
  return_1yr = x1_year_investment_return_percentage,
  actuarial_cost_method_in_gasb_reporting,
  funded_ratio = actuarial_funded_ratio_percentage,
  actuarial_valuation_report_date,
  ava = actuarial_value_of_assets_gasb_dollar,
  mva = market_value_of_assets_dollar,
  mva_smooth = market_assets_reported_for_asset_smoothing,#added
  aal = actuarially_accrued_liabilities_dollar,
  tpl = total_pension_liability_dollar,
  adec = actuarially_required_contribution_dollar,
  adec_paid_pct = actuarially_required_contribution_paid_percentage,
  amortizaton_method,
  asset_valuation_method_for_gasb_reporting,
  total_benefit_payments = total_benefits_paid_dollar,#added
  benefit_payments = benefit_payments_dollar,
  refunds = refunds_dollar,#added
  admin_exp = administrative_expense_dollar,
  cost_structure,
  payroll = covered_payroll_dollar,
  ee_contribution = employee_contribution_dollar,
  ee_nc_pct = employee_normal_cost_percentage,
  er_contribution = employer_contribution_regular_dollar,
  er_nc_pct = employer_normal_cost_percentage,
  er_state_contribution = employer_state_contribution_dollar,
  er_proj_adec_pct = employers_projected_actuarial_required_contribution_percentage_of_payroll,
  other_contribution = other_contribution_dollar,#added
  other_additions = other_additions_dollar,#added
  fy_contribution = fiscal_year_of_contribution,
  inflation_assum = inflation_rate_assumption_for_gasb_reporting,
  arr = investment_return_assumption_for_gasb_reporting,
  number_of_years_remaining_on_amortization_schedule,
  payroll_growth_assumption,
  total_amortization_payment_pct = total_amortization_payment_percentage,
  total_contribution = total_contribution_dollar,
  total_nc_pct = total_normal_cost_percentage,
  total_number_of_members,
  total_proj_adec_pct = total_projected_actuarial_required_contribution_percentage_of_payroll,
  type_of_employees_covered,
  unfunded_actuarially_accrued_liabilities_dollar,
  wage_inflation)

reason.data$arr <- as.numeric(reason.data$arr)
reason.data$aal <- as.numeric(reason.data$aal)
reason.data$return_1yr <- as.numeric(reason.data$return_1yr)
reason.data$year <- as.numeric(reason.data$year)
reason.data$mva <- as.numeric(reason.data$mva)
reason.data$return_yr <- as.numeric(reason.data$return_1yr)
reason.data$aal <- as.numeric(reason.data$aal)
reason.data$arr <- as.numeric(reason.data$arr)
reason.data$payroll <- as.numeric(reason.data$payroll)
reason.data$payroll_growth_assumption <- as.numeric(reason.data$payroll_growth_assumption)
reason.data$total_nc_pct <- as.numeric(reason.data$total_nc_pct)
reason.data$benefit_payments <- as.numeric(reason.data$benefit_payments)
reason.data$refunds <- as.numeric(reason.data$refunds)
reason.data$total_proj_adec_pct <- as.numeric(reason.data$total_proj_adec_pct)

##Load R scrip from GitHub
#https://www.r-bloggers.com/reading-an-r-file-from-github/
library(devtools)
library(roxygen2)
pullSourceData.test <- source_url("https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/PullSourceData.R")
#View(pullSourceData.test$value("New Mexico Educational Retirement Board"))
#NMPERA.wide <- pullSourceData("New Mexico Educational Retirement Board")

############Table by Aggregate pension column
arrUS <- reason.data[, median(na.omit(arr)), by=list(state, year)] %>% arrange(year)
arrUS_18 <- arrUS %>% filter(year==2001)
rownames(arrUS_18) <- c(arrUS_18$state)
#View(arrUS_18)

breaks <- seq(0.05, 0.09, length = 10)
arr_group <- cut(arrUS_18$V1, breaks = breaks, right = TRUE, include.lowest = TRUE)

x <- data.table(with(arrUS_18, table(arr_group, state)))
y <- data.table(pivot_wider(x, names_from = N, values_from = state))
z <- data.table(pivot_wider(x, names_from = N, values_from = state, values_fn = length))
y <- y[, feq := z[,3] ]
y <- y[,!2]
y[,2] <- data.frame(y[,2])

View(y)
##################
