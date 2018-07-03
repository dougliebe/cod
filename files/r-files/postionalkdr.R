library(dplyr)
library(ggplot2)
library(jsonlite)
library(ggplot2)
library(scales)
library(png)
library(dplyr)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/sainte_marie_du_mont.png')
output <- data.frame()

# Go through all games all events
location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 1:length(location)) {
  filenames <- list.files( path = paste('cwl-data/data/structured/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j])
  
  start_time = 5000
  for (i in 1:length(filenames)) {
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    if(data_json$mode == "Hardpoint" & !is.null(nrow(data_json$events))){
      # 
      # 
      events <- (data_json$events)
      data <- subset(events, events$type == 'death')
      pt_chart <-data.frame(team = rep(data_json$teams$name,each = length(data_json$teams$round_scores[[1]])),
                            hp = rep(rep(seq(1,length(data_json$hp_hill_names)),4)[1:length(data_json$teams$round_scores[[1]])],2),
                            set = rep(rep(seq(1,4),each = length(data_json$hp_hill_names))[1:length(data_json$teams$round_scores[[1]])],2),
                            score = c(data_json$teams$round_scores[[1]]-data_json$teams$round_scores[[2]],-data_json$teams$round_scores[[1]]+data_json$teams$round_scores[[2]]), 
                            win = rep(data_json$teams$is_victor, each = length(data_json$teams$round_scores[[1]])),
                            opp = rev(rep(data_json$teams$name,each = length(data_json$teams$round_scores[[1]]))))
      pt_chart$lead <- c(cumsum(pt_chart$score[1:length(data_json$teams$round_scores[[1]])]),
                         cumsum(pt_chart$score[(length(data_json$teams$round_scores[[1]])+1):(length(data_json$teams$round_scores[[1]])*2)]))
      # # # team_rounds <- data.frame(player.team = rep(data_json$teams$name,each = data_json$rounds),
      # #                           round = rep(seq(1,data_json$rounds),2),
      # #                           offdef = c(rep(c("off",'def'),length.out = data_json$rounds),rep(c("def",'off'),length.out = data_json$rounds)),
      # #                           win = c(data_json$teams$round_scores[[1]],data_json$teams$round_scores[[2]]))
      team_players <- data.frame(name = data_json$players$name, team = data_json$players$team, gun = data_json$players$fave_weapon)
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
      data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name')
      data <- merge(data, team_players, by.x ='data.attacker.id ', by.y = 'name')
      data <- merge(data, pt_chart, by = c('team', 'hp', 'set'))
      output <- rbind(output, data.frame(map = data_json$map, data, event = location[j],id = data_json$id))
      # output <- rbind(output, data.frame(map = data_json$map, offdef = data$offdef, x = data$data.attacker.pos.x, y = data$data.attacker.pos.y))
      # output <- rbind(output, data.frame(pt_chart, map = data_json$map, id = data_json$id, code = data_json$series_id, event = location[j]))
    }
  }
}

# take out big leads and scorestreaks 
output1 <- output %>% 
  filter(abs(lead) < 125) %>%
  filter(data.attacker.weapon != 'Artillery Barrage' |
           data.attacker.weapon != 'Glide Bomb' |
           data.attacker.weapon != 'Mortar Strike' |
           data.attacker.weapon != 'Fighter Pilot') %>%
  data.frame()


