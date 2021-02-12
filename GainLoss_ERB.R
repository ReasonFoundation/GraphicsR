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
library(orca)
library(png)


glPlot <- function(
  url = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/APERS_GL.csv", 
  interactive = FALSE, fileName = "GainLoss.png",
  title = "<b>Causes of New Mexico ERB Pension Debt (2001-2020)<b>",
  caption = FALSE,
  Ymax = 10,
  Ytick = 1,
  YearStart = 2001,
  lab1 = "Investment<br>Returns", 
  lab2 = "Benefit<br>Changes<br> & Other",
  lab3 = "Changes to<br>Actuarial<br>Methods &<br>Assumptions", 
  lab4 = "Negative<br>Amortization",
  lab5 = "Deviations from<br>Demographic<br>Assumptions",
  lab6 = "Gains From<br>Pay<br>Increases<br>Not Given",
  lab7 = "Net Change to<br>Unfunded<br>Liability"){
  
  #[1] Load Gain/Loss data from the provided url
  urlfile=url
  data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types =       NULL)
  data <- data.table(data) %>% filter(year >= YearStart)# convert to data.table
  
  data$year <- as.numeric(data$year)
  data$investments<- as.numeric(data$investments)
  data$benefit_changes<- as.numeric(data$benefit_changes)
  data$changes_methods_assumptions<- as.numeric(data$changes_methods_assumptions)
  data$negative_amortization<- as.numeric(data$negative_amortization)
  data$demographic_assumptions<- as.numeric(data$demographic_assumptions)
  data$pay_increase_assumption<- as.numeric(data$pay_increase_assumption)
  data$net_change_uaal<- as.numeric(data$net_change_uaal)

  
    #[2] Calculate Total Gain/Loss for each column (i.e. Net Change to UAL over the years)
    y = data[,lapply(.SD,sum),.SDcols=colnames(data)]/1000
    y = y[,!1]# sum values by each column
    y = t(y)#Saving needed columns and transposing table for graphics
    
    #[3] Combine gain/loss data & categories to create interactive Waterfall chart w/ plotly
    x = list(lab1, lab2, lab3, lab4, lab5, lab6, lab7)
    #View(y)
    measure= c("relative",
               "relative",
               "relative",
               "relative",
               "negative",
               "negative",
               "total")
    
    data <- data.frame(x = factor(x, levels = x), measure, y)
    #View(data)
    #m <- list(
    #  l = 50,
    #  r = 0,
    #  b = 0,
    #  t = 50,
    #  pad = 4
    #)
    data <- data.frame(data)
    
    #[4] Visualizing with Plotly
    fig <- plot_ly( data,
                    type = "waterfall",
                    measure = ~measure,
                    x = ~x,
                    textposition = "outside",
                    y= ~y,
                    decreasing = list(marker = list(color = palette_reason$Green)),
                    increasing = list(marker = list(color = palette_reason$Red)),
                    totals = list(marker = list(color = palette_reason$Orange)),
                    connector = list(line = list(color= palette_reason$SpaceGrey, width = 1))) 
    
    fig <- fig %>%
      layout(title = paste0(title),
             xaxis = list(title = "",tickfont = list(size = 11, face = "bold")),
             yaxis = list(title = "<b>Change in Unfunded Liability (in $Billions)<b>",
                          titlefont = list(size = 12), range = c(0,Ymax),
                          showgrid = FALSE,
                          tick0 = 0,
                          dtick = Ytick,
                          ticklen = 2,
                          linecolor = '#636363',
                          linewidth = 0.75),
             barmode = 'stack',
             autosize = F,
             #width = 700, height = 450, margin = m,
             showlegend = F)
    
    #ADD caption
    if(isTRUE(caption)){
      fig <- fig %>% 
        layout(annotations = list(yref = 'paper', xref = "x", showarrow = F, 
                                  y = 0, x = 4.5, text = "reason.org/pensions",
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=9, color="black")))}
    
    #`Save` as a `static` png file 
    #OR `show` as a `interactive` Plotly chart
    if(!isTRUE(interactive)){
      #Save Plotly as a Static PNG on a local computer (to reference in rmarkdown later)
      #https://github.com/plotly/orca
      orca(fig, paste0(fileName))}else{fig}
  }
  
### Arkansas PERS Data ###

ui <-fluidPage(
  titlePanel(
    title = ""),
  
  theme = shinythemes::shinytheme("spacelab"),
  sidebarLayout(
    sidebarPanel(img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/reason_logo.png"), width = 200, height = 50),
                 sliderInput('year',
                             'Select Starting Year',
                             min = 2001,
                             max = 2020,
                             value = 2001,
                             step = 1,
                             sep = "")),
    
    mainPanel(
      ###Remove error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      plotly::plotlyOutput("plot_waterfall"),
      tags$div(htmlOutput("text1"))
      
    )
  )
)
######Shiny app[server]
##########################
server <- function(input, output, session){
  
  output$text1 <- renderText({
    paste(HTML(
      "Source:"), tags$a(href="https://reason.org/topics/pension-reform/", "Pension Integrity Project at Reason Foundation"),"<br>", 
      "analysis of NM ERB CAFRs and valuation reports.", "<br>", 
      "Data represents cumulative unfunded liability by gain/loss category.",
      sep="\n")
  })

#Code to save Static graph
output$plot_waterfall <- plotly::renderPlotly({
  
  fig <- glPlot(url = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/NMERB_GL.csv",       
       interactive = TRUE,
       fileName = "GainLoss_NMERB.png",
       title = "<b>Causes of New Mexico ERB Pension Debt (2001-2020)<b>",
       caption = FALSE,
       Ymax = 10,
       Ytick = 1,
       YearStart = input$year,
       lab1 = "Investment<br>Returns", 
       lab2 = "Negative<br>Amortization",
       lab3 = "Changes to<br>Actuarial<br>Methods &<br>Assumptions", 
       lab4 = "Plan<br>Changes<br>",
       lab5 = "Deviations from<br>Demographic<br>Assumptions",
       lab6 = "Gains From<br>Pay<br>Increases<br>Not Given",
       lab7 = "Net Change to<br>Unfunded<br>Liability")
  
  fig
})
}

shinyApp(ui = ui, server = server)
#####
