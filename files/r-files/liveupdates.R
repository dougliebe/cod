library(httr)
library(jsonlite)
library(lubridate)
library(RCurl)

url  <- "https://stats.gammaray.io/api/v1/report/cwl-champs-2018/playermatches/"

raw <- getURL(url = url)
data <- read.csv (text = raw)

hppred <- readRDS('hprating.rds')
sndpred <- readRDS('sndrating.rds')
ctfpred <- readRDS('ctfrating.rds')


# Rating 0.1 snd
rating <- data %>%
  filter(mode == 'Search & Destroy') %>%
  mutate(kpr = kills/snd.rounds) %>%
  mutate(ksapr = kills..stayed.alive./snd.rounds) %>%
  mutate(dpr = deaths/snd.rounds) %>%
  mutate(apr = assists/snd.rounds) %>%
  mutate(fpr = snd.firstbloods/snd.rounds) %>%
  mutate(mkpr = (snd.2.kill.round+snd.4.kill.round+snd.3.kill.round)/snd.rounds) %>%
  mutate(sepr = scorestreaks.earned/snd.rounds) %>%
  # mutate(dpk = hits/(kills+1E-9)) %>%
  mutate(win = ifelse(win. == "W",1,0))
rating$rating  = round(exp(predict(sndpred,rating))/(1+exp(predict(sndpred, rating))),2)

# Rating 0.1 hp
ratinghp <- data %>%
  filter(mode == 'Hardpoint') %>%
  mutate(kpr = kills/duration..s.*600) %>%
  mutate(ksapr = kills..stayed.alive./duration..s.*600) %>%
  mutate(dpr = deaths/duration..s.*600) %>%
  mutate(apr = assists/duration..s.*600) %>%
  mutate(fpr = snd.firstbloods/snd.rounds) %>%
  mutate(mkpr = (snd.2.kill.round+snd.4.kill.round+snd.3.kill.round)/duration..s.*600) %>%
  mutate(sepr = scorestreaks.earned/duration..s.*600) %>%
  mutate(win = ifelse(win. == "W",1,0))
ratinghp$rating  = round(exp(predict(hppred,ratinghp))/(1+exp(predict(hppred, ratinghp))),2)

# Rating 0.1 ctf
ratingctf <- data %>%
  filter(mode == 'Capture The Flag') %>%
  mutate(kpr = kills/duration..s.*600) %>%
  mutate(ksapr = kills..stayed.alive./duration..s.*600) %>%
  mutate(dpr = deaths/duration..s.*600) %>%
  mutate(fpr = snd.firstbloods/snd.rounds) %>%
  mutate(apr = assists/duration..s.*600) %>%
  mutate(mkpr = (snd.2.kill.round+snd.4.kill.round+snd.3.kill.round)/duration..s.*600) %>%
  mutate(sepr = scorestreaks.earned/duration..s.*600) %>%
  mutate(win = ifelse(win. == "W",1,0))
ratingctf$rating  = round(exp(predict(ctfpred,ratingctf))/(1+exp(predict(ctfpred, ratingctf))),2)

rating <- rbind(rating, ratinghp)
rating <- rbind(rating, ratingctf)


rating %>%
  group_by(team,player) %>%
  summarise(UA = sum(ifelse(rating > 0.7,1,0)),rating = mean(rating), n = n()) %>%
  # filter(team == "Mindfreak") %>%
  # filter(n > 5) %>%
  arrange(desc(rating)) %>%
  data.frame() %>% head()

data %>%
  group_by(team,player, series.id) %>%
  summarise(utk = sum(kills..stayed.alive.)/sum(kills), utks = sum(kills..stayed.alive.), kills = sum(kills), n = n()) %>%
  # filter(team == "Red Reserve") %>%
  # filter(n > 5) %>%
  arrange(desc(utk)) %>%
  data.frame() %>% head()

rating %>%
  group_by(team,player) %>%
  summarise(maps.win = sum(win),UA = sum(ifelse(rating > 0.7,1,0)),rating = mean(rating), n = n()) %>%
  # filter(team == "Red Reserve") %>%
  # filter(n > 5) %>%
  arrange(desc(rating)) %>%
  data.frame() %>% head()
