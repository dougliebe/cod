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
if(data_json$mode == "Search & Destroy" & !is.null(nrow(data_json$events))){

  
    events <- (data_json$events)
    data <- subset(events, events$type == 'death')
    
    team_rounds <- data.frame(player.team = rep(data_json$teams$name,each = data_json$rounds),
                              round = rep(seq(1,data_json$rounds),2),
                              offdef = c(rep(c("off",'def'),length.out = data_json$rounds),rep(c("def",'off'),length.out = data_json$rounds)),
                              win = c(data_json$teams$round_scores[[1]],data_json$teams$round_scores[[2]]))
    team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team, gun = data_json$players$fave_weapon)
    #convert list of lists to df
    data.test = do.call(cbind.data.frame, data)
    data.test = do.call(cbind.data.frame, data.test)
    data.test = do.call(cbind.data.frame, data.test)
    #if kill was earliest in round, label first blood
    data <- data.test %>%
      dplyr::group_by(round) %>%
      dplyr::mutate(first_kill = ifelse(round_time_ms == min(round_time_ms),1,0)) %>%
      data.frame() %>%
      filter(first_kill == 1) # take only first bloods
    data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name') # name players
    data <- merge(data, team_rounds, by = c('player.team', 'round')) # win or not?
    # add row plus gun, win and name
    output <- rbind(output, data.frame(map = data_json$map, weapon = data$data.attacker.weapon, win = data$win, offdef = data$offdef))
}}}

output %>%
  dplyr::group_by(map,offdef,weapon) %>%
  dplyr::summarise(win = mean(win), n = n()) %>%
  filter(n > 20, map == "Valkyrie") %>%
  arrange(desc(win)) %>% data.frame()
