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

linePlot <- function(data,
                     .var1 = "adec_contribution_rates",
                     .var2 = "actual_contribution_rates",
                     labelY = "Employer Contribution (% of Payroll)",
                     label1 = "ADEC Contribution Rate",
                     label2 = "Actual Contribution Rate") {
  
  var1 <- rlang::sym(.var1)
  var2 <- rlang::sym(.var2)
  lab1 <- rlang::sym(label1)
  lab2 <- rlang::sym(label2)
  
  reasontheme::set_reason_theme(style = "slide")
  
  graph <- data %>%
    dplyr::select(
      .data$year,
      !!label1 := !!var1,
      !!label2 := !!var2
    ) %>%
    dplyr::mutate_all(dplyr::funs(as.numeric)) %>%
    tidyr::gather(key = "keys", value = "amount", -.data$year)
  
  lineColors <- c(palette_reason$SatBlue,palette_reason$SpaceGrey)
  
  ggplot2::ggplot(graph, ggplot2::aes(x = graph$year, y = graph$amount * 100)) +
    ggplot2::geom_line(ggplot2::aes(colour = factor(graph$keys)), size = 2) +
    ggplot2::scale_colour_manual(values = lineColors) +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(10),
      labels = function(b) {
        paste0(round(b, 0), "%")
      },
      expand = c(0, 0)
    ) +
    
    ggplot2::scale_x_continuous(breaks = seq(min(graph$year), max(graph$year), by = 2),
                                expand = c(0, 0)
    ) +
    
    labs(x = element_blank(), y = labelY)
  
}

#Line Plot -- Inv.Returns
linePlot(IPERS,.var1 = "return_1yr",.var2 = "arr",
         labelY = "",
         label1 = "Market Valued Returns (Actual)",
         label2 = "Assumed Rate of Return")
