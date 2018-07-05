library(dplyr)
library(ggplot2)
setwd("C:/Users/Doug/Documents/CoD/cwl-data/data")
# read in the csv
filenames <- list.files( pattern="*.csv", full.names=TRUE)

output <- data.frame()

for (i in (length(filenames)-1):(length(filenames))) {
  data <- read.csv(filenames[i])
  data <- subset(data, data$mode == 'Hardpoint') # sort by mode
  output <- rbind(output, data)
}

output %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  group_by(team) %>%
  summarise(wins = mean(win), W = sum(win), L = n()-W, n = n()) %>%
  filter(team == "Team Kaliber") %>%
  arrange(desc(wins))
