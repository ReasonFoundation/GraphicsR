PERSI.data <- pullStateData(2001)
PERSI.data <- filterData(PERSI.data, 2001)

pl <- planList()
IPERS <- filteredData(pl, "Idaho Public Employee Retirement System", 2001)

IPERS$year <- as.numeric(IPERS$year)
#Set to data.frame for visualization
IPERS <- data.frame(IPERS)
#View(IPERS)

palette_reason <- data.table(
  Orange = "#FF6633", 
  LightOrange = "#FF9900",
  DarkGrey = "#333333", 
  SpaceGrey = "#A69FA1",
  DarkBlue = "#0066CC",
  GreyBlue = "#6699CC", 
  Yellow = "#FFCC33", 
  LightBlue = "#66B2FF", 
  SatBlue = "#3366CC", 
  Green = "#669900",
  LightGreen = "#00CC66",
  Red = "#CC0000",
  LightRed = "#FF0000")


#####GEOMEAN FUNCTION
geomean <- function(x) {
  x <- as.vector(na.omit(x))
  x <- x +1
  exp(mean(log(x)))-1 
}
returns <- as.numeric(IPERS$return_1yr)
nyear <- 10
rolling <- geomean(returns[1:nyear])
n <- length(na.omit(returns))-nyear
#Geomean function
for(i in 1:n){
  rolling <- rbind(rolling, geomean(returns[(i+1):(i+nyear)]))
}
rolling <- data.table(rolling)

IPERS <- data.table(rbind.fill(rolling, IPERS))
IPERS[(IPERS[!is.na(return_1yr),.N]+1):(IPERS[!is.na(return_1yr),.N]+rolling[,.N])]$V1<- IPERS[(1:rolling[,.N])]$V1
IPERS <- IPERS[!(1:rolling[,.N])]
# UAL4 <- data.table(UAL4[, Tr30 := tr30[(n-UAL4[!is.na(Actual_Return),.N]):last]])
#View(IPERS)
###############
#Adding AVA returns

ava_returns <- matrix(0, 19,1)
ava_returns[,1] <- c(NA, NA, NA, NA, 4.70, 9.00, 12.40, 8.00, -5.90, 2.00, 3.10, 4.50, 11.40, 13.80, 8.80, 8.20, 7.70, 5.80, 6.50)
#View(ava_returns)
ava_returns <- data.table(ava_returns/100)

IPERS <- data.table(IPERS)
IPERS <- IPERS[, ava_return := ava_returns]

IPERS$year <- as.numeric(IPERS$year)

IPERS <- data.frame(IPERS)


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
  
  lineColors <- c(palette_reason$Orange,palette_reason$Yellow, palette_reason$SatBlue, palette_reason$SpaceGrey) #Updated palette to reason one
  options(repr.plot.width = 1, repr.plot.height = 0.75)
  ggplot2::ggplot(graph, ggplot2::aes(x = graph$year, y = graph$amount * 100)) +
    ggplot2::geom_line(ggplot2::aes(colour = str_wrap(factor(graph$keys), 15)), size = 1.5) + #Added str_wrap(to cut legends)
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
    theme(legend.text=element_text(size=10))+ #Added element to control legend font size
    theme(legend.position="bottom")
}

#Line Plot -- Inv.Returns


linePlot(IPERS,.var1 = "return_1yr",.var2 = "arr", .var3 = "V1", .var4 = "ava_return",
         labelY = "",
         label1 = "Market Valued Returns (Actual)",
         label2 = "Assumed Rate of Return",
         label3 = "10-Year Geometric Rolling Average",
         label4 = "Actuarially Valued Investment Returns")
