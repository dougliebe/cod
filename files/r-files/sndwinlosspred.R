library(dplyr)
library(ggplot2)
setwd("C:/Users/Doug/Documents/CoD/cwl-data/data")
# read in the csv
filenames <- list.files( pattern="*.csv", full.names=TRUE)

output <- data.frame()

for (i in 1:(length(filenames))) {
  data <- read.csv(filenames[i])
  data <- subset(data, data$mode == 'Search & Destroy') # sort by mode
  output <- rbind(output, data)
}

# Rating 0.1 snd
rating <- output %>%
  mutate(kpr = kills/snd.rounds) %>%
  mutate(ksapr = kills..stayed.alive./snd.rounds) %>%
  mutate(dpr = deaths/snd.rounds) %>%
  mutate(apr = assists/snd.rounds) %>%
  mutate(fpr = snd.firstbloods/snd.rounds) %>%
  mutate(mkpr = (snd.2.kill.round+snd.4.kill.round+snd.3.kill.round)/snd.rounds) %>%
  mutate(kas = (kills+assists+snd.survives)/snd.rounds) %>%
  mutate(sepr = scorestreaks.earned/snd.rounds) %>%
  # mutate(dpk = hits/(kills+1E-9)) %>%
  mutate(win = ifelse(win. == "W",1,0))

m <- glm(win ~ ksapr + apr + fpr + dpr, rating, family = 'binomial')
summary(m)
exp(coefficients(m))
rating$rating  <- round(exp(predict(m, rating))/(1+exp(predict(m, rating))),2)+1
rating %>%
  group_by(player) %>%
  # summarise(rating_avg = mean(rating, na.rm = T), n = n()) %>%
  summarise(rating_avg = sum(hits)/sum(kills), n = sum(kills)) %>%
  filter(n > 200) %>%
  arrange((rating_avg)) %>% head()

# Rating 0.1 hp
rating <- output %>%
  mutate(kpr = kills/duration..s.*600) %>%
  mutate(ksapr = kills..stayed.alive./duration..s.*600) %>%
  mutate(dpr = deaths/duration..s.*600) %>%
  mutate(apr = assists/duration..s.*600) %>%
  mutate(mkpr = (snd.2.kill.round+snd.4.kill.round+snd.3.kill.round)/duration..s.*600) %>%
  mutate(sepr = scorestreaks.earned/duration..s.*600) %>%
  mutate(win = ifelse(win. == "W",1,0))

m <- glm(win ~ ksapr + apr + dpr, rating, family = 'binomial')
summary(m)
exp(coefficients(m))
rating$rating  <- round(predict(m, rating),2)+1
rating %>%
  group_by(player) %>%
  summarise(rating_avg = mean(rating, na.rm = T), n = n()) %>%
  filter(n > 50) %>%
  arrange(desc(rating_avg)) %>% head()

# Rating 0.1 ctf
rating <- output %>%
  mutate(kpr = kills/duration..s.*600) %>%
  mutate(ksapr = kills..stayed.alive./duration..s.*600) %>%
  mutate(dpr = deaths/duration..s.*600) %>%
  mutate(apr = assists/duration..s.*600) %>%
  mutate(mkpr = (snd.2.kill.round+snd.4.kill.round+snd.3.kill.round)/duration..s.*600) %>%
  mutate(sepr = scorestreaks.earned/duration..s.*600) %>%
  mutate(win = ifelse(win. == "W",1,0))

m <- glm(win ~  ksapr + dpr + sepr, rating, family = 'binomial')
summary(m)
rating$rating  <- round(exp(predict(m, rating))/(1+exp(predict(m, rating))),2)+1

rating %>%
  arrange(desc(rating)) %>% head()


#sort output
output <- output %>%
  mutate(date = as.Date(substr(end.time, start = 1, stop = 10))) %>%
  mutate(win = ifelse(win. == "W",1,0)) %>%
  group_by(match.id, date,team, map) %>%
  summarise(wins = sum(snd.firstbloods)/mean(num.lives), w = ifelse(sum(win>0),1,0)) %>%
  arrange((date))

output %>%
  group_by(team, map) %>%
  summarise(win = mean(wins))



i = 1
results <- data.frame()
for (i in 1:nrow(output)) {
start_output <- output[i,]
win <- start_output$w
opp <- (output[output$match.id == start_output$match.id &
                         output$team != start_output$team, 'team'])[[1]]
team <- start_output$team
# win_diff <- team_win_pct - opp_win_pct
date = as.numeric(as.Date(start_output$date))
map = start_output$map
# scoreboard <- output[1:i,] %>%
#   group_by(date,team) %>%
#   summarise(wins = ifelse(sum(win)> 0,1,0)) %>%
#   ungroup() %>%
#   group_by(team) %>%
#   summarise(win = min(0.8,max(0.20,mean(wins))))

results <- rbind(results, data.frame(win, opp, team, date, map))

}

# get win % on dates

winpcts <- data.frame()
dates <- as.numeric(as.Date(unique(output$date)))
teams <- unique(output$team)
for(i in 5:length(dates)) {
  out <- output[output$date < dates[i],] %>%
    group_by(team, map) %>%
    summarise(winp = mean(wins), n = n())
  winpcts <- rbind(winpcts, data.frame(out, date = dates[i]))
  }

results <- merge(results, winpcts, by = c('team','map','date'))
results <- merge(results, winpcts, by.x = c('opp','map','date'),by.y = c('team','map','date'))
results <- subset(results, results$n.x > 15 & results$n.y > 15)

results$winp.x <- pmin(pmax(results$winp.x,0.2),0.8)*100
results$winp.y <- pmin(pmax(results$winp.y,0.2),0.8)*100
results$diff <- (results$winp.x - results$winp.y)

m <- glm(win~winp.y+winp.x, data = results, family = 'binomial')
summary(m)
x <- 