library(jsonlite)
library(dplyr)
setwd('~/Documents/CoD/')
filenames <- list.files("structured-2017-12-10-dallas", pattern="*.json", full.names=TRUE)

for (i in 1:length(filenames)) {
data_json <- read_json(filenames[i], simplifyVector = T)

#output df
output <- data.frame()

# For each file

if (data_json$mode == "Hardpoint") {
events <- (data_json$events)
data <- subset(events, events$type == 'death')

# make team chart
team_players <- data.frame(name = data_json$players$name, killed.team = data_json$players$team)
levels(team_players$name) <- c(levels(team_players$name), 'HEADACHES')
team_players[as.character(team_players$name) == "ACHES",'name'] <- "HEADACHES"  #doesnt work
start_time = 5000

# Make point chart

pt_chart <-data.frame(team = rep(data_json$teams$name,each = length(data_json$teams$round_scores[[1]])),
           hp = rep(rep(seq(1,length(data_json$hp_hill_names)),4)[1:length(data_json$teams$round_scores[[1]])],2),
           set = rep(rep(seq(1,4),each = length(data_json$hp_hill_names))[1:length(data_json$teams$round_scores[[1]])],2),
           score = unlist(data_json$teams$round_scores))
team_chart <-data.frame(name = rep(team_players$name, each = (nrow(pt_chart)/2)),
           team = rep(team_players$killed.team, each = (nrow(pt_chart)/2)),
           hp = rep(rep(seq(1,length(data_json$hp_hill_names)),4)[1:(nrow(pt_chart)/2)],8),
           set = rep(rep(seq(1,4),each = length(data_json$hp_hill_names))[1:(nrow(pt_chart)/2)],8))
score_chart <- merge(team_chart, pt_chart, by = c('team', 'hp', 'set'))


# Split by hp and set
if (length(data_json$hp_hill_names)==4) {
  hp <- rep(seq(1,4),4, each = 60*1000)
  time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
  set <- rep(seq(1,4),each = 60*1000*4)
  df <- data.frame(time,hp, set)
  data = merge(data, df, by.x = 'time_ms', by.y = 'time')
  # data$data <- merge(data$data, team_players, by.x = 'id', by.y = 'name')
} else {
  hp <- rep(seq(1,5),4, each = 60*1000)
  time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
  set <- rep(seq(1,5),each = 60*1000*4)
  df <- data.frame(time,hp, set)
  data = merge(data, df, by.x = 'time_ms', by.y = 'time')
  # data$data <- merge(data$data, team_players, by.x = 'id', by.y = 'name')
}

df <- data.frame(name = data$data$id,hp = data$hp, set = data$set)
# Get deaths per set (or other in the future)
# Need to record which map too, since this can change

deaths <- df %>%
  group_by(name,set,hp) %>%
  summarise(deaths = n()) %>% data.frame()
deaths$map = data_json$map
deaths <- merge(deaths, score_chart, by = c('name','hp','set'))

# Store deaths per set per player
output <- rbind(output,deaths)
}}
