source("CustomFunction.R")

schedule = get_schedule(year = 2018)
today_date = gsub("-", "", Sys.Date())

games_already_played = schedule[1:(which(schedule$date == today_date)[1] - 1),]

total_bs = data.frame()
for (i in 1:nrow(games_already_played)) {
  bs = get_boxscore(today = games_already_played[i, ]$date, 
                    game_id = games_already_played[i, ]$gameId, 
                    teams_df)
  bs$date = games_already_played[i, ]$date
  total_bs = bind_rows(total_bs, bs)
}

bs_avg = total_bs %>%
  group_by(personId) %>%
  mutate(fp_pts_avg = mean(fp_pts),
         fp_pts_sd = sd(fp_pts)) %>%
  ungroup()

selected_date = gsub("-", "", "2018-10-16")
today_bs = bs_avg %>% 
  filter(date == selected_date) %>%
  select(fullName, team_short_name, min, fp_pts, fp_pts_avg, fp_pts_sd) %>% 
  arrange(-fp_pts) %>%
  slice(1:10)

knitr::kable(today_bs)
