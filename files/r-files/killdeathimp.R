library(dplyr)
library(ggplot2)
setwd("~/Documents/CoD/full-game-data")
# read in the csv
filenames <- list.files( pattern="*.csv", full.names=TRUE)

output <- data.frame()

for (i in 1:(length(filenames)-2)) {
  data <- read.csv(filenames[i])
  data <- subset(data, data$mode == 'Search & Destroy')
  output <- rbind(output, data)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Hp winning impact
output %>%
  group_by(team,player) %>%
  mutate(sub = ifelse(fave.weapon == 'PPSh-41',1,0)) %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  summarise(kill.imp = exp(glm(as.factor(win) ~ kills + deaths, family = 'binomial')$coefficients[2]),
            death.imp = exp(glm(as.factor(win) ~ kills + deaths, family = 'binomial')$coefficients[3]),
             sub = ifelse(mean(sub)>0.8,'sub', 'AR/flex'), n = n()) %>%
  ungroup() %>%
  # group_by(team, sub) %>%
  # mutate(kr = kill.imp-mean(kill.imp), dr = death.imp-mean(death.imp)) %>%
  mutate(kpd = (kill.imp-1) + abs(death.imp-1)) %>%
  filter(n > 20) %>%
  # filter(sub == 'sub') %>%
  # filter(team == "Enigma6") %>%
  # group_by(team) %>%
  # summarise(kpd = max(kpd)-mean(kpd)) %>%
  arrange(desc(kpd))
  
# Search & Destroy
output %>%
  group_by(team,player) %>%
  mutate(sub = ifelse(fave.weapon == 'PPSh-41',1,0)) %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  summarise(kill.imp = lm(score ~ kills + deaths)$coefficients[2],
            death.imp = lm(score ~ kills + deaths)$coefficients[3],
            sub = ifelse(mean(sub)>0.8,'sub', 'AR/flex'), n = n()) %>%
  ungroup() %>%
  # group_by(team, sub) %>%
  # mutate(kr = kill.imp-mean(kill.imp), dr = death.imp-mean(death.imp)) %>%
  mutate(kpd = (kill.imp-1) + abs(death.imp-1)) %>%
  filter(n > 20) %>%
  # filter(sub == 'sub') %>%
  # filter(team == "Enigma6") %>%
  # group_by(team) %>%
  # summarise(kpd = max(kpd)-mean(kpd)) %>%
  arrange(desc(kpd))


# Look at some players
output %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  group_by(player, win) %>%
  summarise(kdr = sum(kills.per.10min)/sum(deaths.per.10min), n = n()) %>%
  group_by(player) %>%
  summarise(minmax = max(kdr)-min(kdr)) %>%
  arrange(desc(minmax))


# Make table by game
first <- output %>%
  group_by(match.id, team) %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  summarise(score = mean(score), kills = sum(kills), deaths = sum(deaths), ksa = sum(kills..stayed.alive.), win = mean(win)) %>%
  filter(score != 250) %>%
  ungroup() %>% group_by(team) %>%
  summarise(score = mean(score), n = n())

second <- output %>%
  group_by(team) %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  summarise(wins = mean(win))

first <- merge(first, second, by = 'team')
first <- subset(first, first$n >5 & first$wins > 0.5)