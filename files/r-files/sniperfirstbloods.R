library(jsonlite)
library(ggplot2)
library(scales)
library(png)
library(dplyr)

setwd('C:/Users/Doug/Documents/CoD/')
img <- readPNG('cwl-data/maps/ww2/sainte_marie_du_mont.png')
output <- data.frame()


location <- list.files(path = 'cwl-data/data/structured', pattern = "structured")
for(j in 1:length(location)) {
filenames <- list.files( path = paste('cwl-data/data/structured/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
print(location[j])


for (i in 1:length(filenames)) {
data_json <- fromJSON(filenames[i], simplifyVector = T)
if(data_json$mode == "Search & Destroy" & !is.null(nrow(data_json$events))){

  
    events <- (data_json$events)
    data <- subset(events, events$type == 'death')
    
    team_rounds <- data.frame(player.team = rep(data_json$teams$name,each = data_json$rounds),
                              round = rep(seq(1,data_json$rounds),2),
                              offdef = c(rep(c("off",'def'),length.out = data_json$rounds),rep(c("def",'off'),length.out = data_json$rounds)),
                              win = c(data_json$teams$round_scores[[1]],data_json$teams$round_scores[[2]]))
    team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team, gun = data_json$players$fave_weapon)
    data.test = do.call(cbind.data.frame, data)
    data.test = do.call(cbind.data.frame, data.test)
    data.test = do.call(cbind.data.frame, data.test)
    # data <- data.test %>%
    #   dplyr::group_by(round) %>%
    #   dplyr::mutate(first_kill = ifelse(round_time_ms == min(round_time_ms),1,0)) %>%
    #   data.frame() %>%
    #   filter(first_kill == 1)
    data <- merge(data, team_players, by.x = 'data.attacker.id', by.y = 'name')
    data <- merge(data, team_rounds, by = c('player.team', 'round'))
    output <- rbind(output, data.frame(map = data_json$map, weapon = data$data.attacker.weapon, win = data$win, offdef = data$offdef))
}}}

output %>%
  dplyr::group_by(map,offdef,weapon) %>%
  dplyr::summarise(win = mean(win), n = n()) %>%
  filter(n > 20, map == "Valkyrie") %>%
  arrange(desc(win)) %>% data.frame()
