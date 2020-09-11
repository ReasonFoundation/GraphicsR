library(pensionviewr)
library(reasontheme)
library(tidyverse)
setwd(getwd())
urlfile <- "https://raw.githubusercontent.com/ReasonFoundation/GraphicsR/master/Idaho_DR_Treasury.csv"
IdahoDR <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)

##Modified Lineplot
linePlotTemp <- function(data, labelY, varList, labelList,
                         yaxisMin = NULL, yaxisMax = NULL, yaxisSeq = NULL, yaxisScale = 100, percentageTRUE = FALSE,
                         xaxisMin = min(data$year), xaxisMax = max(data$year), xaxisScale = 2) {
  
  data <- filter(data, data$year >= xaxisMin & data$year <= xaxisMax)
  if(length(varList) != length(labelList)){
    newLength <- min(length(varList),length(labelList))
    varList <- varList[1:newLength]
    labelList <- labelList[1:newLength]
  }
  if(length(varList) > 4){varList <- varList[1:4]}
  if(length(labelList) > 4){labelList <- labelList[1:4]}
  
  reasontheme::set_reason_theme(style = "slide")
  graph <- data %>%
    dplyr::select(
      .data$year,
      if(length(varList) > 0){as.character(varList[1])},
      if(length(varList) > 1){as.character(varList[2])},
      if(length(varList) > 2){as.character(varList[3])},
      if(length(varList) > 3){as.character(varList[4])}
    ) %>%
    dplyr::mutate_all(dplyr::funs(as.numeric)) %>%
    tidyr::gather(key = "keys", value = "amount", -.data$year)
  
  lineColors <- c("#FF6633","#FFCC33", "#3366CC", "#A69FA1") #Updated palette to reason one
  lineColors <- lineColors[1:length(varList)]
  options(repr.plot.width = 1, repr.plot.height = 0.75)
  ggplot2::ggplot(graph, ggplot2::aes(x = graph$year, y = graph$amount * yaxisScale)) +
    ggplot2::geom_line(ggplot2::aes(colour = str_wrap(factor(graph$keys), 15)), size = 1.5) + #Added str_wrap(to cut legend text)
    ggplot2::scale_colour_manual(values = lineColors) +
    #ggplot2::theme(legend.text = "center") +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    
    ggplot2::scale_y_continuous(
      #if(!is.null(yaxisMin) & !is.null(yaxisMax) & !is.null(yaxisSeq)){
      #  breaks = seq(yaxisMin, yaxisMax, by=yaxisSeq) #added limits and expanded break scale
      #}
      
      labels = function(b) {
        if(percentageTRUE){
          paste0(round(b, 0), "%")
        }else{
          paste0(round(b, 0))
        }  
        },
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(breaks = seq(xaxisMin, xaxisMax, by = xaxisScale), #added blank years
                                expand = c(0, 0)
    ) +
    
    labs(x = element_blank(), y = labelY)+
    theme(legend.text=element_text(size=12))+ #Added element to control legend font size
    theme(legend.position= c(0.51, 0.1)) #Moved legend to the bottom
}

#Line Plot -- Inv.Returns
linePlotTemp(IdahoDR, labelY = "DR vs Treasury", list("30YTBondRate","DR"),
             list('30-Year Treasury Bond Yield Rate','Discount Rate'), percentageTRUE = TRUE)
