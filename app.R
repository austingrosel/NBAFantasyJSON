library(shiny)
library(DT)
library(sendmailR)
library(ggplot2)
library(plotly)
library(scales)
library(curl)
library(ggthemes)

source('NBAScrapeFunctions.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css", "#score.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#away_name.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#away_tbl.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#home_name.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#home_tbl.recalculating { opacity: 1.0; }"),
  
  titlePanel("NBA Box Score"),
  
  tabsetPanel(
    tabPanel("Box Score",
      dateInput('date', label = h4("Date input")),
      uiOutput("select_todays_games"),
      fluidRow(h2(uiOutput("score")), align = "center"),
      fluidRow(
        column(6, 
               h2(uiOutput("away_name"))
        ),
        column(6,
               h2(uiOutput("home_name"))
        )
      ),
      fluidRow(
        column(6,
               DTOutput("away_tbl")
        ),
        column(6,
               DTOutput("home_tbl")
        )
      ),
      
      hr(),
      
      fluidRow(
        column(6,
               radioButtons("away_options", inline = T, label = "Stat:", choices = c("FP", "DRE", "MIN")),
               uiOutput("away_shot_stats"),
               plotlyOutput("away_plot")
        ),
        column(6,
               radioButtons("home_options", inline = T, label = "Stat:", choices = c("FP", "DRE", "MIN")),
               uiOutput("home_shot_stats"),
               plotlyOutput("home_plot")
        )
      )
    ),
    tabPanel("Season",
             DTOutput("season_tbl")
    )
  )


)

color_from_middle <- function (data, color1,color2) {
  max_val=max(abs(data))
  JS(sprintf("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'",
             max_val,color1,max_val,color1,color2,color2,max_val,max_val))
} 

server <- function(input, output) {
  
  today = reactive({
    gsub("-", "", input$date)
  })
  
  scoreboard = reactive({
    get_games_today(today = today())
  })
  
  output$select_todays_games = renderUI({
    selectInput("game_desc", "Games today:", scoreboard()$game_desc)
  })
  
  output$score = reactive ({
    validate(
      need(length(input$game_desc) > 0, "Game ID not found.")
    )
    validate(
      need(input$game_desc != " vs.  ", "No games happening today.")
    )
    invalidateLater(5 * 1000)
    games = get_games_today(today = today())
    this_game = games[games$game_desc == input$game_desc, ]
    validate(
      need(nrow(this_game) > 0, "Game ID not found.")
    )
    if(this_game$statusNum == 3) {
      HTML(paste0("FINAL: ", 
                  this_game$vTeam$triCode, ": ", this_game$vTeam$score, " ",
                  this_game$hTeam$triCode, ": ", this_game$hTeam$score))
    } else if (this_game$statusNum == 2) {
      HTML(paste0("Q", this_game$period$current, ": ", this_game$clock, " | ", 
                  this_game$vTeam$triCode, ": ", this_game$vTeam$score, " ",
                  this_game$hTeam$triCode, ": ", this_game$hTeam$score))
    }
  })
  
  boxscore = reactive({
    invalidateLater(30 * 1000)
    validate(
      need(length(input$game_desc) > 0, "Game ID not found.")
    )
    validate(
      need(input$game_desc != " vs.  ", "No games happening today.")
    )
    game_id = scoreboard()[scoreboard()$game_desc == input$game_desc, ]$gameId
    df = get_boxscore(today = today(), game_id = game_id, teams_df = teams_df)
    df = df[!is.na(df$DRE),]
  })
  
  teams = reactive({
    validate(
      need(nrow(boxscore()) > 0, "Game ID not found")
    )
    unique(boxscore()$team_short_name)
  })
  
  output$away_name = renderUI({
    HTML(paste("<b>", teams()[1], "</b>"))
  })
  
  away_team = reactive({
    boxscore() %>% 
      filter(team_short_name == teams()[1]) %>% 
      select(personId, jersey, fullName, pos, isOnCourt, age, min, DRE, fd_pts) %>% arrange(-min)
  })
  
  output$away_tbl = renderDT({
     validate(
       need(nrow(boxscore()) > 0, "Game ID not found")
     )
     DT::datatable(away_team(), selection = "single",
      options = list(dom = 't', columnDefs = list(list(visible=FALSE, targets=c(0,4,5)))), rownames= FALSE) %>%
       formatStyle(
         'isOnCourt',
         target = 'row',
         fontWeight = styleEqual(c(1, 0), c('bold', 'normal'))) %>%
       formatStyle('DRE',
                   background=color_from_middle(boxscore()$DRE, 'pink', 'lightblue'),
                   backgroundSize = '90% 80%',
                   backgroundRepeat = 'no-repeat',
                   backgroundPosition = 'center') %>%
       formatStyle('fd_pts',
                   background = styleColorBar(boxscore()$fd_pts, 'steelblue', angle = -90),
                   backgroundSize = '90% 80%',
                   backgroundRepeat = 'no-repeat',
                   backgroundPosition = 'center')
   })
   
   away_id = reactive({
     away_team()[input$away_tbl_rows_selected, ]$personId
   })
   
   away_p = reactive({
     fd = total_bs %>% 
       filter(personId == away_id())
     
     if(input$away_options == "FP") {
       ggplot(fd, aes(x = game_num, y = fd_pts)) + 
         geom_point(aes(color = min)) +
         geom_smooth(se = F) +
         scale_color_gradient(limits = c(10, 38), oob=squish) +
         ylim(0, max(total_bs[!is.na(total_bs$fd_pts),]$fd_pts)) +
         xlim(1, 82) + 
         ggtitle(paste(fd$fullName, 
                       "| AvgMin:", round(mean(fd$min, na.rm = T), 1),
                       "| AvgFp:", round(mean(fd$fd_pts, na.rm = T), 1))) +
         ggthemes::theme_fivethirtyeight()
     } else if (input$away_options == "DRE") {
       ggplot(fd, aes(x = game_num, y = DRE)) + 
         geom_point(aes(color = fd_pts)) +
         geom_smooth(se = F) + 
         scale_color_gradient(limits = c(15, 40), oob=squish) +
         ylim(-5, max(total_bs[!is.na(total_bs$fd_pts),]$DRE)) +
         xlim(1, 82) + 
         ggtitle(paste(fd$fullName, 
                       "| AvgMin:", round(mean(fd$min, na.rm = T), 1),
                       "| AvgFp:", round(mean(fd$fd_pts, na.rm = T), 1))) +
         ggthemes::theme_fivethirtyeight()
     } else {
       ggplot(fd, aes(x = game_num, y = min)) + 
         geom_point(aes(color = fd_pts)) +
         geom_smooth(se = F) +
         scale_color_gradient(limits = c(15, 40), oob=squish) +
         ylim(0, max(total_bs[!is.na(total_bs$fd_pts),]$min)) +
         xlim(1, 82) + 
         ggtitle(paste(fd$fullName, 
                       "| AvgMin:", round(mean(fd$min, na.rm = T), 1),
                       "| AvgFp:", round(mean(fd$fd_pts, na.rm = T), 1))) +
         ggthemes::theme_fivethirtyeight()
     }
   })
   
   output$away_plot = renderPlotly({
     validate(
       need(!is.null(input$away_tbl_rows_selected), "No away player selected.")
     )
     p = away_p()
     ggplotly(p)
   })
   
   output$home_name = renderUI({
     HTML(paste("<b>", teams()[2], "</b>"))
   })
   
   home_team = reactive({
     boxscore() %>% 
       filter(team_short_name == teams()[2]) %>% 
       select(personId, jersey, fullName, pos, isOnCourt, age, min, DRE, fd_pts) %>% arrange(-min)
   })
   
   output$home_tbl = renderDT({
     validate(
       need(nrow(boxscore()) > 0, "Game ID not found")
     )
     DT::datatable(home_team(), selection = 'single',
                   options = list(dom = 't', columnDefs = list(list(visible=FALSE, targets=c(0,4,5)))), rownames = FALSE) %>%
       formatStyle(
         'isOnCourt',
         target = 'row',
         fontWeight = styleEqual(c(1, 0), c('bold', 'normal'))) %>%
       formatStyle('DRE',
                   background=color_from_middle(boxscore()$DRE, 'pink', 'lightblue'),
                   backgroundSize = '90% 80%',
                   backgroundRepeat = 'no-repeat',
                   backgroundPosition = 'center') %>%
       formatStyle('fd_pts',
                   background = styleColorBar(boxscore()$fd_pts, 'steelblue', angle = -90),
                   backgroundSize = '90% 80%',
                   backgroundRepeat = 'no-repeat',
                   backgroundPosition = 'center')
   })
   
   home_id = reactive({
     home_team()[input$home_tbl_rows_selected, ]$personId
   })
   
   home_p = reactive({
     fd = total_bs %>% 
       filter(personId == home_id())
     
     if(input$home_options == "FP") {
       ggplot(fd, aes(x = game_num, y = fd_pts)) + 
         geom_point(aes(color = min)) +
         geom_smooth(se = F) +
         scale_color_gradient(limits = c(10, 38), oob=squish) +
         ylim(0, max(total_bs[!is.na(total_bs$fd_pts),]$fd_pts)) +
         xlim(1, 82) + 
         ggtitle(paste(fd$fullName, 
                       "| AvgMin:", round(mean(fd$min), 1),
                       "| AvgFp:", round(mean(fd$fd_pts), 1))) +
         ggthemes::theme_fivethirtyeight()
     } else if (input$home_options == "DRE") {
       ggplot(fd, aes(x = game_num, y = DRE)) + 
         geom_point(aes(color = fd_pts)) +
         geom_smooth(se = F) +
         scale_color_gradient(limits = c(15, 40), oob=squish) +
         ylim(-5, max(total_bs[!is.na(total_bs$fd_pts),]$DRE)) +
         xlim(1, 82) + 
         ggtitle(paste(fd$fullName, 
                       "| AvgMin:", round(mean(fd$min), 1),
                       "| AvgFp:", round(mean(fd$fd_pts), 1))) +
         ggthemes::theme_fivethirtyeight()
     } else {
       ggplot(fd, aes(x = game_num, y = min)) + 
         geom_point(aes(color = fd_pts)) +
         geom_smooth(se = F) +
         scale_color_gradient(limits = c(15, 40), oob=squish) +
         ylim(0, max(total_bs[!is.na(total_bs$fd_pts),]$min)) +
         xlim(1, 82) + 
         ggtitle(paste(fd$fullName, 
                       "| AvgMin:", round(mean(fd$min), 1),
                       "| AvgFp:", round(mean(fd$fd_pts), 1))) +
         ggthemes::theme_fivethirtyeight()
     }
   })
   
   output$home_plot = renderPlotly({
     validate(
       need(!is.null(input$home_tbl_rows_selected), "No home player selected.")
     )
     fd = total_bs %>% 
       filter(personId == home_id())
     p = home_p()
     ggplotly(p)
   })

   output$season_tbl = renderDataTable({
     datatable(total_bs %>% 
       group_by(personId, fullName) %>% 
       summarise(games = n(), 
                 min = mean(min, na.rm = T), 
                 mean_DRE = mean(DRE, na.rm = T), 
                 mean_fp = mean(fd_pts, na.rm = T), 
                 sd_fp = sd(fd_pts, na.rm = T), 
                 min_fp = min(fd_pts, na.rm = T), 
                 max_fp = max(fd_pts, na.rm = T)) %>% 
       arrange(-mean_fp) %>%
       ungroup() %>%
       select(-personId) %>%
       mutate_if(is.numeric, round, 1), filter = list(position = 'top', clear = FALSE),
       options = list(
         pageLength = 50
       ))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

