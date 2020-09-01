#### PERSI Asset Allocation ####
library(tidyr)
library(data.table)

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


#Load G/L data for PERSI
urlfile2="https://raw.githubusercontent.com/ReasonFoundation/GraphicsR/master/R/PERSI.Asset.Alloc.csv"
PERSI.assets <- data.table(read_csv(url(urlfile2), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = cols(.default = "n")))
PERSI.assets <- data.table(PERSI.assets)
#Adding Othe to Alternatives
PERSI.assets$alternatives <- PERSI.assets$alternatives+ PERSI.assets$other
PERSI.assets <- PERSI.assets %>% select(year, real.estate, alternatives, equity, fixed.income, cash)

#View(PERSI.assets)
#Transpose datat from long ot wide format (each year contains Variable & Value)
df.mlt <- melt(PERSI.assets, id.vars="year")

ggplot(df.mlt) + geom_area(aes(x = year, y = value, fill = variable, group = variable), position="fill")+
  scale_fill_manual(values=c(palette_reason$Yellow,palette_reason$Orange, palette_reason$DarkGrey, palette_reason$LightBlue, palette_reason$SatBlue))