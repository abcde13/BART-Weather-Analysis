
setwd("~/Documents/Dev/Data/Rproj/BartTwitter")

#source("TwitterApiHandling.R")

library(readr)

year1 <- read_csv("sfo_weather_1.csv")
year2 <- read_csv("sfo_weather_2.csv")
year3 <- read_csv("sfo_weather_3.csv")

weather$PST <- as.Date(weather$PST)
weather$Events <- factor(weather$Events)

