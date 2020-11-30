########## Arkansas PERS Dashboard ##########
##Data: Manually Collected
##By: Anil, Jordan, Swaroop

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

####



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
library(ggplot2)

#reason_color_pal()
#library(extrafont)
#font_import(pattern="Roboto")
#loadfonts(device = "win", quiet = TRUE)
##Palette
#https://www.rapidtables.com/web/color/Web_Safe.html
#https://www.colorbook.io/hexcolors/view/A69FA1

#rgb1 <- col2rgb(palette_reason$Yellow)/255
#rownames(rgb1) <- c("red", "green", "blue")
#rgb1
#Custom color code
#ColorName <- rgb(0.2,0.2,0.4)
#ColorName  

###
##Pull PERSI data
##Load list of plans
pl <- planList()

#PULL DATA
#Data <- pullStateData(2001)
#Data <- filterData(Data,2001)
#Data <- data.table(Data)
#Data <- Data %>% select(year, plan_name, state, mva, type_of_employees_covered)
#Data <- Data %>% filter(year > 2015)
#View(Data)
#Data$mva <- Data[,median(na.omit(mva)), by = list(year, type_of_employees_covered)]

#ggplot(data = Data) +``
#  geom_point(aes(x = year, y = mva)) +
#  facet_wrap(~type_of_employees_covered, nrow = 2)


### Arkansas Data ###
# Original
urlfile = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/APERS_numeric.csv"
APERS.data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
#View(APERS.data)

### US Data from PensionViewr ###

urlfile2="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/Reason_State_Names_Mod.csv"
plan.names <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL))

### Geometric Mean Function ###
geomean <- function(x) {
  x <- as.vector(na.omit(x))
  x <- x +1
  exp(mean(log(x)))-1 
}

first.nan <- function(x) {
  first(x[which(!is.na(x))])
}

### Create a theme() for ggplot ###

plotTheme <- ggplot2::theme(   panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                               plot.margin = margin(0, 0,0,0, "cm"),
                               axis.text.y = element_text(size=8, color = "black"),
                               axis.text.x = element_text(size=8, color = "black", angle = 90, hjust = 1, vjust = 0.5),
                               axis.title.y = element_text(size=9, color = "black"),
                               axis.title.x = element_text(size=9, color = "black"),
                               legend.title = element_text(size = 8, colour = "white", face = "bold"))

###### Shiny App/Dashboard  

ui <-fluidPage(
  titlePanel(
        title = ""),
                    
  theme = shinythemes::shinytheme("spacelab"),
  sidebarLayout(
    sidebarPanel(img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/reason_logo.png"), width = 200, height = 50),
          sliderInput('year',
          'Select Starting Year',
           min = 2001,
           max = max(APERS.data$year),
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
      "analysis of APERS CAFRs and valuation reports.", "<br>", 
      "Data represents cumulative unfunded liability by gain/loss category.",
      sep="\n")
     })
  
  #[1]Pulling scv file from GitHub containing APERS gain/loss data
  
  urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/APERS_GL.csv"
  APERSData <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
  APERSData <- as.data.table(APERSData)# coverted to data.table
  
  #[2] Reactive expression that filters G/L per user-chosen start year
  
  gain_loss <- reactive({
    
    APERSData <- data.table(APERSData %>% filter(year >= input$year))
    #View(APERSData)
    y = APERSData[,lapply(.SD,sum),.SDcols=colnames(APERSData)]*1e-6# sum values by each column
    y = t(y[,2:8])#Saving needed columns and transposing table for graphics
  })
  
  #[3] Combine gain/loss data & categories to create interactive Waterfall chart w/ plotly
  
  output$plot_waterfall <- plotly::renderPlotly({
    
    y <- gain_loss()
    x = list(paste0("<b>", "Investment<br>Returns","</b>"),
             paste0("<b>", "Benefit<br>Changes<br> & Other","</b>"),
                    paste0("<b>", "Changes to<br>Actuarial<br>Methods &<br>Assumptions","</b>"),
                           paste0("<b>", "Negative<br>Amortization","</b>"),
                                  paste0("<b>", "Deviations from<br>Demographic<br>Assumptions","</b>"),
                                         paste0("<b>", "Gains From<br>Pay Increases<br>Not Given","</b>"),
                                                paste0("<b>", "Net Change to<br>Unfunded<br>Liability","</b>"))
    
    measure= c("relative",
               "relative",
               "relative",
               "relative",
               "relative",
               "relative",
               "total")
    
    data <- data.frame(x = factor(x, levels = x), measure, y)
    #View(data)
    
    fig <- plot_ly( data,
                    type = "waterfall",
                    measure = ~measure,
                    x = ~x,
                    textposition = "outside",
                    y= ~y,
                    text = "",
                    hoverinfo = 'text',
                    hovertemplate = paste0("Pension Debt Gain/(Loss): ", "<br>", "$", round(y, 2), "B", "<br><extra></extra>"),
                    decreasing = list(marker = list(color = palette_reason$Green)),
                    increasing = list(marker = list(color = palette_reason$Red)),
                    totals = list(marker = list(color = palette_reason$Orange)),
                    connector = list(line = list(color= palette_reason$SpaceGrey, width = 1))) 
    
    fig <- fig %>%
      layout(title = paste0("<b>Causes of Arkansas ERS Pension Debt<b>"," (",input$year,"-2019)"),
             xaxis = list(title = "",tickfont = list(size = 11, face = "bold")),
             yaxis = list(title = "<b>Change in Unfunded Liability (in $Billions)<b>",
                          titlefont = list(size = 12), range = c(0,5),
                          showgrid = FALSE,
                          tick0 = 0,
                          dtick = 0.5,
                          ticklen = 2,
                          linecolor = '#636363',
                          linewidth = 0.75),
             barmode = 'stack',
             autosize = T,
             showlegend = F) %>% 
      layout(annotations = list(yref = 'paper', xref = "x", showarrow = F, 
                    y = 0, x = 4.5, text = "reason.org/pensions",
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=9, color="black")))
    
    fig
    
  })
}


shinyApp(ui = ui, server = server)