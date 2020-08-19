library(plotly)
library(tidyverse)
urlfile="https://raw.githubusercontent.com/ReasonFoundation/GraphicsR/master/Idaho_GainLoss.csv"
IdahoPERS <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)

#Creating labels and measures
x= c("Underperforming Investments", "Negative Amortization", "Changes in Methods and Assumption", "Deviations from Demographic Assumptions", "Other", "Changes to Benefits","New Members", "Gains from Pay Increases Not Given", "Net Change to Unfunded Liability")
measure= c("relative", "relative", "relative", "relative", "relative","relative", "relative","relative", "total")
y = c(-sum(IdahoPERS[,2]), -sum(IdahoPERS[,3]), -sum(IdahoPERS[,4]), -sum(IdahoPERS[,5]), -sum(IdahoPERS[,6]), -sum(IdahoPERS[,7]), -sum(IdahoPERS[,8]), -sum(IdahoPERS[,9]), 0)

#Seting labels to factors and saving as data.frame
data = data.frame(x=factor(x,levels=x),measure,y)

fig <- plot_ly( data,
                type = "waterfall",
                measure = ~measure,
                x = ~x,
                textposition = "outside",
                y= ~y,
                text = "",
                
                hoverinfo = 'text',
                hovertemplate = paste('%{y:$,.0f}<extra></extra>'),
                decreasing = list(marker = list(color = "#70AD47")),
                increasing = list(marker = list(color = "#D81E00")),
                totals = list(marker = list(color = "#ED9213")),
                
                connector = list(line = list(color= "#333333"))) 

fig <- fig %>%
  layout(title = "",
         xaxis = list(title = ""),
         yaxis = list(title = "Change in Unfunded Liability (Billions)"),
         autosize = T,
         showlegend = F)

fig

