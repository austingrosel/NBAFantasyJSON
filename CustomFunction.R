library(curl)
library(dplyr)
library(tidyr)
library(httr)
library(stringr)

get_schedule = function(year = 2017) {
  json <- paste0("http://data.nba.net/prod/v2/", year, "/schedule.json") %>%
    curl() %>%
    readr::read_lines() %>%
    jsonlite::fromJSON(simplifyVector = T)
  
  df = json$league$standard
  home = json$league$standard$hTeam$teamId
  visitor = json$league$standard$vTeam$teamId
  
  df = df %>%
    mutate(home_id = home,
           visitor_id = visitor
           ) %>%
    filter(seasonStageId == 2) %>%
    select(gameId, gameUrlCode, home_id, visitor_id) %>%
    separate(gameUrlCode, c('date', 'teams'), sep="/") %>%
    mutate(teams = paste(substr(teams, 1, 3), '@', substr(teams, 4, 6)))
  
  return(df)
}

get_teams = function() {
  json <- paste0(url = "http://data.nba.net/json/cms/noseason/sportsmeta/nba_teams.json") %>%
    curl() %>%
    readr::read_lines() %>%
    jsonlite::fromJSON(simplifyVector = T)
  
  df = json$sports_content$team$team
  return(df)
}

get_team_games <- function(date = "20191022") {
  json <- paste0("http://data.nba.net/json/cms/noseason/scoreboard/",date,"/games.json") %>%
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
  
  df = df %>% 
    select(id, season_id, date, city) %>%
    mutate(home_id = home,
           home_team = home_team,
           visitor_id = visitor,
           visitor_team = visitor_team
           )
  return(df)
}

get_team_games(date = "20191022")

get_players <- function(year = 2019) {
  json <- paste0(url = "https://data.nba.net/prod/v1/",year,"/players.json") %>%
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


get_boxscore <- function(today = "20181017", game_id = "0021800003", teams_df) {
  url = paste0("http://data.nba.net/10s/prod/v1/",today,"/",game_id,"_boxscore.json")
  
  json <- tryCatch({
    url %>%
      curl() %>%
      readr::read_lines() %>%
      jsonlite::fromJSON(simplifyVector = T)
  }, error=function(cond) {
    message(paste("URL does not seem to exist:", url))
  })
  
  if(length(json) == 0) {
    return(data.frame())
  }
  
  df = json$stats$activePlayers
  if(is.null(df)) {
    return(data.frame())
  }
  
  df$sortKey = NULL
  df = cbind(df[,c(1:3, 5, 9)], sapply(df[,c(7, 10:27)], as.numeric))
  df = df %>% mutate(
      DRE = round(points * 0.8 - (fga - tpa) * 0.7 - tpa * 0.6 + offReb * 0.1 + 
                    defReb * 0.4 + assists * 0.5 + steals * 1.7 + blocks * 0.8 - 
                    turnovers * 1.4 - pFouls * 0.1, 1),
      fd_pts = points + totReb * 1.2 + assists * 1.5 + blocks * 3 + steals * 3 - turnovers,
      bb_pts = points + totReb + assists + blocks * 3 + steals * 2,
      minutes = str_split_fixed(min, ":", 2)[,1],
      seconds = str_split_fixed(min, ":", 2)[,2],
      min = as.numeric(minutes) + round(as.numeric(seconds)/60, 1),
      id = 1:nrow(df)
    )
  df = merge(get_teams() %>% select(team_id, team_short_name), df, by.x = "team_id", by.y = "teamId")

  df = merge(get_players() %>% select(fullName, personId, jersey, yearsPro, pos, age), df, by = "personId") %>% 
    select(-id, -minutes, -seconds, -firstName, -lastName)
  
  return(df)
}

get_fanduel_boxscore <- function(game_id = "0021800003") {
  json <- paste0("http://stats.nba.com/stats/infographicfanduelplayer/?gameId=", game_id) %>%
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
  
  this_df = df %>%
    mutate(bb_pts = PTS + REB + AST + STL * 2 + BLK * 3,
           game_id = game_id) %>%
    setNames(tolower(names(.)))
  return(this_df)
}