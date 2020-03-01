library("pxR")
library(ggplot2)
library(gganimate)
theme_set(theme_bw())
library(zoo)
library(directlabels)
install.packages("gifski")
install.packages("png")

rentspx <- read.px(choose.files())
rents <- as.data.frame(rentspx)

# Format the date column as a date
rents$Quarter <- as.yearqtr(rents$Quarter, format = "%YQ%q")
  # rents$Year <- as.Date(rents$Year, format = "%Y")

rents$Quarter <- as.Date(rents$Quarter, format = "%YQ%q")
all_rents$Quarter <- as.Date(all_rents$Quarter, format = "%YQ%q")

str(rents[1:3,])
View(rents)
View(all_rents)

# Make a vector to help pick out the county-level data.
counties <- c("Carlow","Cavan","Clare","Cork","Donegal",
              "Dublin","Galway","Kerry","Kildare","Kilkenny",
              "Laois","Leitrim","Limerick","Longford","Louth",
              "Mayo","Meath","Monaghan","Offaly","Roscommon",
              "Sligo","Tipperary","Waterford","Westmeath",
              "Wexford","Wicklow")

all_rents <- rents[which(rents$Location %in% counties
                & rents$Property.Type == "All property types"
                & rents$Number.of.Bedrooms == "All bedrooms"),]

all_rents$Qtr_rank <- ave(all_rents$value, all_rents$Quarter, FUN=rank)

# Line chart; x = time, y = rent. lines for rent in 26 counties, different colours. 
counties_plot <- 
  ggplot(data = all_rents, aes(x = all_rents$Quarter, y = all_rents$value, colour = all_rents$Location)) +
  geom_line() + scale_colour_discrete(guide = 'none') +
  geom_dl(aes(label = all_rents$Location), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.6)) +
  geom_dl(aes(label = all_rents$Location), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.6)) + 
  ggtitle("Average value of rent recorded for each county") +
  xlab("Q4 2007 to Q2 2018") +
  ylab("Rent") + 
  geom_point() +
  expand_limits(y = 0)

counties_plot

# These three lines produce a 26-line chart of the ranking column...
# with a different color line for each Location & labels at start and end of lines
ggplot(data = all_rents, aes(x = all_rents$Quarter, y = all_rents$Qtr_rank, colour = all_rents$Location)) +
  geom_line() + scale_colour_discrete(guide = 'none') +
  geom_dl(aes(label = all_rents$Location), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(aes(label = all_rents$Location), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8))
  
# A list of the Dublin postal areas as they appear in the data.
# Dublin 22 is just represented as 'Clondalking, Dublin 22' in the dataset.
dublin_pcodes <- c("Dublin 1", "Dublin 2", "Dublin 3", "Dublin 4", "Dublin 5", "Dublin 6", "Dublin 6W", 
                   "Dublin 7", "Dublin 8", "Dublin 9", "Dublin 10", "Dublin 11", "Dublin 12", "Dublin 13",
                   "Dublin 14","Dublin 15", "Dublin 16","Dublin 17", "Dublin 18", "Dublin 20",
                   "Clondalkin, Dublin 22","Dublin 24")

dublin_rents <- rents[which(rents$Location %in% dublin_pcodes
                         & rents$Property.Type == "All property types"
                         & rents$Number.of.Bedrooms == "All bedrooms"),]

# Replacing Dublin 1 with D1, etc, for readability on the charts.
dublin_rents$Location <- str_replace(dublin_rents$Location, fixed("ublin "),"")
dublin_rents$Location <- str_replace(dublin_rents$Location, fixed("Clondalkin, "),"")

dublin_plot <- 
  ggplot(data = dublin_rents, aes(x = dublin_rents$Quarter, y = dublin_rents$value, colour = dublin_rents$Location)) +
  geom_line() + scale_colour_discrete(guide = 'none') +
  geom_dl(aes(label = dublin_rents$Location), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.6)) +
  ggtitle("Average monthly rent of tenancies registered each quarter") +
  xlab("Q4 2007 to Q2 2018") +
  ylab("Rent in Euros") + 
  geom_point(alpha = .3, size = 3) +
  expand_limits(y = 0)

dublin_plot 

+ transition_reveal(dublin_rents$Quarter)
