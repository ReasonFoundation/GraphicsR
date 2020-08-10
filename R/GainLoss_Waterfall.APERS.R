library(plotly)
library(tidyverse)
urlfile="https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/APERS_GL.csv"
APERSData <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)

APERSData <- as.data.table(APERSData)
y = APERSData[,lapply(.SD,sum),.SDcols=colnames(APERSData)]*1e-6
y = t(y[,2:8])

x= c("Underperforming Investments", "Benefit Changes & Other", "Changes in Methods and Assumption", "Negative Amortization", "Actual Demographic Performance", "Pay Increase not given","Net Change to Unfunded Liability")
measure= c("relative", "relative", "relative", "relative", "relative","relative", "total")

data = data.frame(x=factor(x,levels=x),measure,y)

fig <- plot_ly(
  data, name = "Data", type = "waterfall", measure = ~measure,
  x = ~x, textposition = "outside", y= ~y, text =~"",
  connector = list(line = list(color= "rgb(63, 63, 63)"))) 
fig <- fig %>%
  layout(title = "Causes of Pension Debt",
         xaxis = list(title = ""),
         yaxis = list(title = "Change in Unfunded Liability ($Billions)"),
         autosize = TRUE,
         showlegend = FALSE)

fig

