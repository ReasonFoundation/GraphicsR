library(pensionviewr)
library(reasontheme)
library(tidyverse)
setwd(getwd())
urlfile <- "https://raw.githubusercontent.com/ReasonFoundation/GraphicsR/master/Idaho_DR_Treasury.csv"
IdahoDR <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
#file <- "https://raw.githubusercontent.com/ReasonFoundation/GraphicsR/master/Idaho_DR_Treasury.csv"
#IdahoDR <- read_csv('Idaho_DR_Treasury.csv', col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)

##Modified Lineplot
linePlot <- function(data,
                     .var1 = FALSE,
                     .var2 = FALSE,
                     .var3 = FALSE,
                     .var4 = FALSE,
                     labelY = FALSE,
                     label1 = FALSE,
                     label2 = FALSE,
                     label3 = FALSE,
                     label4 = FALSE) {
  
  #Added 3rd variable
  if(!isFALSE(.var1)){var1 <- rlang::sym(.var1)}
  if(!isFALSE(.var2)){var2 <- rlang::sym(.var2)}
  if(!isFALSE(.var3)){var3 <- rlang::sym(.var3)}
  if(!isFALSE(.var4)){var4 <- rlang::sym(.var4)}
  if(!isFALSE(label1)){lab1 <- rlang::sym(label1)}
  if(!isFALSE(label2)){lab2 <- rlang::sym(label2)}
  if(!isFALSE(label3)){lab3 <- rlang::sym(label3)}
  if(!isFALSE(label4)){lab4 <- rlang::sym(label4)}
  
  reasontheme::set_reason_theme(style = "slide")
  
  graph <- data %>%
    dplyr::select(
      .data$year,
      if(!isFALSE(.var1)){!!label1 := !!var1},
      if(!isFALSE(.var2)){!!label2 := !!var2},
      if(!isFALSE(.var3)){!!label3 := !!var3},
      if(!isFALSE(.var4)){!!label4 := !!var4}
    ) %>%
    dplyr::mutate_all(dplyr::funs(as.numeric)) %>%
    tidyr::gather(key = "keys", value = "amount", -.data$year)
  
  varList <- c(.var1,.var2,.var3,.var4)
  TrueList <- 4 - table(varList)["FALSE"]
  lineColors <- c(palette_reason$Orange,palette_reason$Yellow, palette_reason$SatBlue, palette_reason$LightGrey) #Updated palette to reason one
  lineColors <- lineColors[1:TrueList]
  
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
graph <- linePlot(IdahoDR,.var1 = "30YTBondRate",.var2 = "DR", .var3 = FALSE, .var4 = FALSE,
                  labelY = "Test",
                  label1 = "30-Year Treasury Bond Yield Rate",
                  label2 = "Discount Rate",
                  label3 = FALSE,
                  label4 = FALSE)
graph
