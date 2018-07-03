library(jsonlite)
library(ggplot2)
library(scales)
library(png)
library(dplyr)

setwd('C:/Users/Doug/Documents/CoD/')
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
    if(data_json$mode == "Hardpoint" & !is.null(nrow(data_json$events))){
      
      
      events <- (data_json$events)
      data <- subset(events, events$type == 'death')
      
      # team_rounds <- data.frame(player.team = rep(data_json$teams$name,each = data_json$rounds),
                                # round = rep(seq(1,data_json$rounds),2),
                                # offdef = c(rep(c("off",'def'),length.out = data_json$rounds),rep(c("def",'off'),length.out = data_json$rounds)),
                                # win = c(data_json$teams$round_scores[[1]],data_json$teams$round_scores[[2]]))
      team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team, gun = data_json$players$fave_weapon)
      #convert list of lists to df
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      
      # Split by hp and set
      if (length(data_json$hp_hill_names)==4) {
        hp <- rep(seq(1,4),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        set <- rep(seq(1,4),each = 60*1000*4)
        df <- data.frame(time,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
      } else {
        hp <- rep(seq(1,5),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        set <- rep(seq(1,5),each = 60*1000*4)
        df <- data.frame(time,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
      }
      data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name') # name players
      data <- merge(data, team_players, by.x = 'data.id', by.y = 'name') 
      # data <- merge(data, team_rounds, by.x = c('player.team.x', 'round'),
                    # by.y = c('player.team', 'round')) # win or not?
      # add row plus gun, win and name
      output <- rbind(output, data.frame(map = data_json$map, time = data_json$duration_ms/1000, id = data_json$id, data))
    }
  }
}


output %>%
  mutate(nade = ifelse(data.attacker.means_of_death == 'grenade_splash',1,0)) %>%
  dplyr::group_by(id, round, data.attacker.id) %>%
  dplyr::summarise(nade_per_game = sum(nade), time = max(time/60), n = n()) %>%
  ungroup() %>%
  group_by(data.attacker.id) %>%
  summarise(nade10 = sum(nade_per_game)/sum(time), minutes = sum(n)) %>%
  filter(minutes > 1000) %>%
  arrange(desc(nade10))
