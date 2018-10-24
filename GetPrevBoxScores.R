library(htmlTable)
library(rJava)
library(readr)

Sys.setenv(tz="US/Eastern")
source("~/Desktop/NBAjsonShiny/CustomFunction.R")

schedule = get_schedule(year = 2018)
teams_df = get_teams()

today_date = gsub("-", "", Sys.Date())
#today_date = Sys.Date() - 1
print(today_date)

games_already_played = schedule[1:(which(schedule$date == today_date)[1] - 1),]

start = proc.time()
total_bs = data.frame()
for (i in 1:nrow(games_already_played)) {
  bs = get_boxscore(today = games_already_played[i, ]$date, 
                    game_id = games_already_played[i, ]$gameId, 
                    teams_df)
  bs$date = games_already_played[i, ]$date
  total_bs = bind_rows(total_bs, bs)
  print(paste(i, games_already_played[i, ]$date))
}
seconds = proc.time() - start
print(paste("Loop took", round(seconds[3], 0), "seconds."))

total_bs = merge(total_bs, schedule_2018[,c("team_id", "startDateEastern", "game_num")], by.x = c("team_id", "date"), by.y = c("team_id", "startDateEastern"), all.x = T)

bs_avg = total_bs %>%
  group_by(personId) %>%
  mutate(games = n(),
         fp_pts_avg = round(mean(fp_pts), 1),
         fp_pts_sd = round(sd(fp_pts), 2)) %>%
  ungroup()

fd_2017 = read_csv('~/Desktop/NBAjsonShiny/NBAFanDuel2017.csv')
fd_2017 = fd_2017 %>%
  group_by(PLAYER_ID) %>% 
  summarise(n_2017 = n(), 
            fp_2017 = round(mean(FAN_DUEL_PTS), 1), 
            sd_2017 = round(sd(FAN_DUEL_PTS), 1))
fd_2017$PLAYER_ID = as.character(fd_2017$PLAYER_ID)

selected_date = gsub("-", "", Sys.Date()-1)
today_bs = bs_avg %>% 
  filter(date == selected_date) %>%
  select(personId, fullName, team_short_name, min, fp_pts, fp_pts_avg, fp_pts_sd) %>% 
  left_join(., fd_2017, by = c("personId" ="PLAYER_ID")) %>%
  arrange(-fp_pts) %>%
  rename(fp_pts_today = fp_pts) %>%
  select(-personId, -n_2017, -sd_2017) %>%
  slice(1:30)
knitr::kable(today_bs)

player_names = c("Damian Lillard", "Jamal Murray", "Will Barton",
                 "Jrue Holiday", "Eric Bledsoe", "LaMarcus Aldridge",
                 "Brandon Ingram", "Karl-Anthony Towns", "Marc Gasol",
                 "Hassan Whiteside", "Evan Fournier", "Tyreke Evans")

my_players = bs_avg %>% 
  distinct(personId, fullName, team_short_name, games, fp_pts_avg, fp_pts_sd) %>% 
  left_join(., fd_2017, by = c("personId" ="PLAYER_ID")) %>%
  arrange(-fp_pts_avg) %>%
  select(-personId, -n_2017, -sd_2017)
my_players = my_players[my_players$fullName %in% player_names,]

season_bs = bs_avg %>% 
  distinct(personId, fullName, team_short_name, games, fp_pts_avg, fp_pts_sd) %>% 
  left_join(., fd_2017, by = c("personId" ="PLAYER_ID")) %>%
  arrange(-fp_pts_avg) %>%
  select(-personId, -n_2017, -sd_2017) %>%
  slice(1:30)
knitr::kable(season_bs)


x <- htmlTable(as.data.frame(today_bs), rnames = F)
y <- htmlTable(as.data.frame(season_bs), rnames = F)
z <- htmlTable(as.data.frame(my_players), rnames = F)

html_body <- paste0(
  "<p>Results from ", Sys.Date() - 1, "</p>", x,
  "<p>Seasonal Averages as of ", Sys.Date() - 1, "</p>", y,
  "<p>Current Team Roster:</p>", z
)

library(mailR)
sender <- "agrosel026@gmail.com"
recipients <- c("agrosel@mail.depaul.edu")
send.mail(from = sender,
          to = recipients,
          subject = paste("NBA Fantasy - Daily Reports:", Sys.Date() - 1),
          body = html_body,
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name="agrosel026@gmail.com", passwd="covnifjyzyzkygrn", ssl=TRUE),
          authenticate = TRUE,
          html = TRUE,
          send = TRUE)