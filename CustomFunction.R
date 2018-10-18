library(curl)
library(dplyr)
library(tidyr)
library(httr)
library(stringr)

get_ind_schedule = function(year = 2016, team_id = "1610612745") {
  url = paste0("http://data.nba.net/prod/v1/", year, "/teams/", team_id, "/schedule.json")
  json <- url %>%
    curl() %>%
    readr::read_lines() %>%
    jsonlite::fromJSON(simplifyVector = T)
  df = json$league$standard
  df = df[df$seasonStageId == 2, ]
  df$team_id = team_id
  df$game_num = 1:82
  df = df %>% select(team_id, gameId, startDateEastern, isHomeTeam, game_num)
  df = merge(df, teams_df[,c("team_id", "team_abbrev")], by = "team_id")
  return(df)
}

get_games_today <- function(today = "20181017") {
  url = paste0("http://data.nba.net/prod/v1/",today,"/scoreboard.json")
  #url = GET(url, add_headers("user-agent" = "Mozilla/5.0"))
  json <- url %>%
    curl() %>%
    readr::read_lines() %>%
    jsonlite::fromJSON(simplifyVector = T)
  df = json$games
  df$game_desc = paste(df$vTeam$triCode, 'vs.', df$hTeam$triCode, df$startTimeEastern)
  return(df)
}

get_boxscore <- function(today = "20181017", game_id = "0021800003", teams_df) {
  url = paste0("http://data.nba.net/10s/prod/v1/",today,"/",game_id,"_boxscore.json")
  
  json <- tryCatch({
      url %>%
      curl() %>%
      readr::read_lines() %>%
      jsonlite::fromJSON(simplifyVector = T)
    }, error=function(cond) {
      message(paste("URL does not seem to exist:", url))
    }
  )
  if(length(json) == 0) {
    return(data.frame())
  }
  df = json$stats$activePlayers
  if(is.null(df)) {
    return(data.frame())
  }
  df$sortKey = NULL
  df[,c(4, 7:24)] <- sapply(df[,c(4, 7:24)], as.numeric)
  df = df %>% mutate(
    DRE = round(points * 0.8 - (fga - tpa) * 0.7 - tpa * 0.6 + offReb * 0.1 + 
      defReb * 0.4 + assists * 0.5 + steals * 1.7 + blocks * 0.8 - 
      turnovers * 1.4 - pFouls * 0.1, 1),
    fp_pts = points + totReb * 1.2 + assists * 1.5 + blocks * 3 + steals * 3 - turnovers
  )
  df$minutes = str_split_fixed(df$min, ":", 2)[,1]
  df$seconds = str_split_fixed(df$min, ":", 2)[,2]
  df$min = as.numeric(df$minutes) + round(as.numeric(df$seconds)/60, 1)
  df$id = 1:nrow(df)
  df = merge(teams_df[c('team_id', 'team_short_name')], df, by.x = "team_id", by.y = "teamId")
  df$teamId = NULL
  df$pos = NULL
  
  players = get_players()
  players = players[,c("fullName", "personId", "jersey", "yearsPro", "pos", "age")]
  
  df = merge(players, df, by = "personId")
  df = df[order(df$id),]
  df$id = NULL
  
  df = df %>% select(-dnp, -minutes, -seconds)
  
  return(df)
}

get_players <- function(year = 2018) {
  url = paste0(url = "https://data.nba.net/prod/v1/",year,"/players.json")
  #url = GET(url, add_headers("user-agent" = "Mozilla/5.0"))
  json <- url %>%
    curl() %>%
    readr::read_lines() %>%
    jsonlite::fromJSON(simplifyVector = T)
  
  df = json$league$standard
  df$fullName = paste(df$firstName, df$lastName)
  df = df %>% select(firstName, lastName, fullName, personId, teamId, jersey, isActive,
                     pos, heightFeet, heightInches, weightPounds, dateOfBirthUTC, yearsPro) %>%
              filter(isActive == "TRUE")
  df$age = round((Sys.Date() - as.Date(df$dateOfBirthUTC))/365,1)
  return(df)
}

get_teams = function() {
  url = paste0(url = "http://data.nba.net/json/cms/noseason/sportsmeta/nba_teams.json")
  json <- url %>%
    curl() %>%
    readr::read_lines() %>%
    jsonlite::fromJSON(simplifyVector = T)
  
  df = json$sports_content$team$team
  return(df)
}

get_team_games <- function(date = "20180216") {
  url = paste0("http://data.nba.net/json/cms/noseason/scoreboard/",date,"/games.json")
  #url = GET(url, add_headers("user-agent" = "Mozilla/5.0"))
  json <- url %>%
    curl() %>%
    readr::read_lines() %>%
    jsonlite::fromJSON(simplifyVector = T)
  df = json$sports_content$games$game
  if(length(df) == 0) {
    df = data.frame()
    return(df)
  }
  home = json$sports_content$games$game$home$id
  home_team = json$sports_content$games$game$home$team_key
  visitor = json$sports_content$games$game$visitor$id
  visitor_team = json$sports_content$games$game$visitor$team_key
  df = df %>% select(id, season_id, date, city)
  df$home_id = home
  df$home_team = home_team
  df$visitor_id = visitor
  df$visitor_team = visitor_team
  return(df)
}

get_schedule = function(year = 2017) {
  url = paste0("http://data.nba.net/prod/v2/", year, "/schedule.json")
  json <- url %>%
    curl() %>%
    readr::read_lines() %>%
    jsonlite::fromJSON(simplifyVector = T)
  
  df = json$league$standard
  home = json$league$standard$hTeam$teamId
  visitor = json$league$standard$vTeam$teamId
  
  df$home_id = home
  df$visitor_id = visitor
  df = df[df$seasonStageId == 2, ]
  
  df = df %>% select(gameId, gameUrlCode, home_id, visitor_id)
  df = df %>% separate(gameUrlCode, c('date', 'teams'), sep="/")
  df$teams = paste(substr(df$teams, 1, 3), '@', substr(df$teams, 4, 6))
  return(df)
}

get_fanduel <- function(game_id = "0031700002") {
  url = paste0("http://stats.nba.com/stats/infographicfanduelplayer/?gameId=", game_id)
  json <- url %>%
    curl() %>%
    readr::read_lines() %>%
    jsonlite::fromJSON(simplifyVector = T)
  
  df = as.data.frame(json$resultSets$rowSet[[1]])
  if(nrow(df) == 0) {
    return(df)
  }
  colnames(df) = json$resultSets$headers[[1]]
  
  for(i in 9:ncol(df)) {
    df[, i] = as.numeric(as.character(df[, i]))
  }
  
  df$game_id = game_id
  return(df)
}
