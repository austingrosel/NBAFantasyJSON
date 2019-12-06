library(readr)

ovr_start = proc.time()

Sys.setenv(tz="US/Eastern")

source("NBAScrapeFunctions.R")

schedule = get_league_schedule(year = 2019)
teams_df = get_teams()

today_date = gsub("-", "", Sys.Date())
print(today_date)

games_already_played = schedule[1:(which(schedule$date == today_date)[1] - 1),]

start = proc.time()
total_bs = data.frame()
for (i in 1:nrow(games_already_played)) {
  bs = get_boxscore(today = games_already_played[i, ]$date, 
                    game_id = games_already_played[i, ]$gameId, 
                    teams_df)
  if(nrow(bs) == 0) {
    next
  } 
  bs$date = games_already_played[i, ]$date
  total_bs = bind_rows(total_bs, bs)
  print(paste(i, games_already_played[i, ]$date))
}
seconds = proc.time() - start
print(paste("Loop took", round(seconds[3], 0), "seconds, or", round(seconds[3]/60, 2), "minutes."))

schedule_2019 = data.frame()
count = 1
for(team_id in teams_df$team_id) {
  sched = get_ind_schedule(year = 2019, team_id = team_id)
  Sys.sleep(2)
  schedule_2019 = rbind(schedule_2019, sched)
  print(count)
  count = count + 1
}

total_bs = merge(total_bs, schedule_2019[,c("team_id", "startDateEastern", "game_num")], by.x = c("team_id", "date"), by.y = c("team_id", "startDateEastern"), all.x = T)

bs_avg = total_bs %>%
  filter(!is.na(min)) %>%
  group_by(personId) %>%
  mutate(games = n(),
         fp_pts_avg = round(mean(fd_pts), 1),
         fp_pts_sd = round(sd(fd_pts), 2)) %>%
  ungroup()

seconds = proc.time() - ovr_start
print(paste("Entire script took", round(seconds[3], 0), "seconds, or", round(seconds[3]/60, 2), "minutes."))