library(jsonlite)
library(ggplot2)
library(scales)
library(png)
library(dplyr)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/sainte_marie_du_mont.png')

# set blank output
output <- data.frame()

# For each event folder
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 1:length(location)) {
  filenames <- list.files( path = paste('cwl-data/data/structured/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j])
  
  # for each game in each event folder
  for (i in 1:length(filenames)) {
    #read json
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # filter out by mode if desired
    if(data_json$mode == "Search & Destroy" & !is.null(nrow(data_json$events))){
      
      
      events <- (data_json$events)
      data <- subset(events, events$type == 'death')
      
      team_rounds <- data.frame(player.team = rep(data_json$teams$name,each = data_json$rounds),
                                round = rep(seq(1,data_json$rounds),2),
                                offdef = c(rep(c("off",'def'),length.out = data_json$rounds),rep(c("def",'off'),length.out = data_json$rounds)),
                                win = c(data_json$teams$round_scores[[1]],data_json$teams$round_scores[[2]]))
      team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team, gun = data_json$players$fave_weapon)
      #convert list of lists to df
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      
      #if kill was earliest in round, label first blood
      # data <- data %>%
      #   dplyr::group_by(round) %>%
      #   dplyr::mutate(first_kill = ifelse(round_time_ms == min(round_time_ms),1,0)) %>%
      #   data.frame() %>%
      #   filter(first_kill == 1) # take only first bloods
      data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name') # name players
      data <- merge(data, team_players, by.x = 'data.id', by.y = 'name') 
      data <- merge(data, team_rounds, by.x = c('player.team.x', 'round'),
                    by.y = c('player.team', 'round')) # win or not?
      data <- data %>%
        group_by(round) %>%
        arrange(time_ms) %>%
        mutate(trade = ifelse(round_time_ms == max(round_time_ms),2,
                              ifelse((player.team.y == lead(player.team.x,1)) &
                                (lead(round_time_ms,1) - round_time_ms < 5000),1,0))) %>%
        mutate(avenge = ifelse(lag(trade,1) == 1, 1,0)) %>%
        mutate(lastdeath = ifelse(round_time_ms == max(round_time_ms),1,0)) %>%
        ungroup() %>%
        group_by(player.team.x) %>%
        # filter(!is.na(avenge)) %>%
        mutate(teamdeaths = n(), teamlastdeaths = sum(lastdeath,na.rm =T)) %>%
        group_by(teamdeaths, teamlastdeaths, data.id) %>%
        summarise(playerdeaths = n(), avenges = sum(avenge, na.rm = T), lastdeaths = sum(lastdeath, na.rm = T)) %>%
        data.frame()
      # add row plus gun, win and name
      output <- rbind(output, data.frame(map = data_json$map, id = data_json$id, data))
    }
  }
}

#avenges
output %>%
  mutate(otherdeaths = teamdeaths - playerdeaths - teamlastdeaths + lastdeaths) %>%
  group_by(data.id, map) %>%
  # filter(!is.na(avenge)) %>%
  summarise( avenges = sum(avenges)/ sum(otherdeaths)*100, n = sum(otherdeaths)) %>%
  filter(n > 150, map == "Ardennes Forest") %>%
  arrange(desc(avenges)) %>% head()

#deaths traded
output %>%
  group_by(data.id) %>%
  filter(trade != 2) %>%
  summarise( trades = mean(trade), n = n()) %>%
  filter(n > 250) %>%
  arrange(desc(trades)) %>% head()
