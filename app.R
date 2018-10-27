#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(sendmailR)
library(ggplot2)
library(plotly)
library(scales)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css", "#score.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#away_name.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#away_tbl.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#home_name.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#home_tbl.recalculating { opacity: 1.0; }"),
  
  # Application title
  titlePanel("NBA Box Score"),
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
           plotlyOutput("away_plot")
    ),
    column(6,
           radioButtons("home_options", inline = T, label = "Stat:", choices = c("FP", "DRE", "MIN")),
           plotlyOutput("home_plot")
    )
  )
)

color_from_middle <- function (data, color1,color2) {
  max_val=max(abs(data))
  JS(sprintf("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'",
             max_val,color1,max_val,color1,color2,color2,max_val,max_val))
} 

server <- function(input, output) {
  timeStop <- "23:45:20"
  
  toStop <- as.POSIXct(timeStop, format="%H:%M:%S")
  if (Sys.time() > toStop) {
    toStop <- toStop + 86400
  }
  secsToStop <- round(as.numeric(difftime(toStop, Sys.time(), units = "secs")) * 1000)
  timeToStop <- reactiveTimer(secsToStop)
  trick <- reactiveValues()
  trick$toFire <- FALSE
  
  observeEvent(timeToStop(), {
    if (trick$toFire) {
      stopApp()
    } else {
      trick$toFire <- TRUE
    }
  })
  
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
    invalidateLater(60 * 1000)
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
      select(personId, jersey, fullName, pos, isOnCourt, age, min, DRE, fp_pts) %>% arrange(-min)
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
       formatStyle('fp_pts',
                   background = styleColorBar(boxscore()$fp_pts, 'steelblue', angle = -90),
                   backgroundSize = '90% 80%',
                   backgroundRepeat = 'no-repeat',
                   backgroundPosition = 'center')
   })
   
   output$home_name = renderUI({
     HTML(paste("<b>", teams()[2], "</b>"))
   })
   
   home_team = reactive({
     boxscore() %>% 
       filter(team_short_name == teams()[2]) %>% 
       select(personId, jersey, fullName, pos, isOnCourt, age, min, DRE, fp_pts) %>% arrange(-min)
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
       formatStyle('fp_pts',
                   background = styleColorBar(boxscore()$fp_pts, 'steelblue', angle = -90),
                   backgroundSize = '90% 80%',
                   backgroundRepeat = 'no-repeat',
                   backgroundPosition = 'center')
   })
   
   away_p = reactive({
     id = away_team()[input$away_tbl_rows_selected, ]$personId
     fd = total_bs %>% 
       filter(personId == id)
     
     if(input$away_options == "FP") {
       ggplot(fd, aes(x = game_num, y = fp_pts)) + 
         geom_line() + geom_point(aes(color = min)) +
         scale_color_gradient(limits = c(10, 38), oob=squish) +
         ylim(0, max(total_bs[!is.na(total_bs$fp_pts),]$fp_pts)) +
         xlim(1, 82) + 
         ggtitle(paste(fd$fullName, 
                       "| AvgMin:", round(mean(fd$min), 1),
                       "| AvgFp:", round(mean(fd$fp_pts), 1))) +
         ggthemes::theme_fivethirtyeight()
     } else if (input$away_options == "DRE") {
       ggplot(fd, aes(x = game_num, y = DRE)) + 
         geom_line() + geom_point(aes(color = fp_pts)) +
         scale_color_gradient(limits = c(15, 40), oob=squish) +
         ylim(-5, max(total_bs[!is.na(total_bs$fp_pts),]$DRE)) +
         xlim(1, 82) + 
         ggtitle(paste(fd$fullName, 
                       "| AvgMin:", round(mean(fd$min), 1),
                       "| AvgFp:", round(mean(fd$fp_pts), 1))) +
         ggthemes::theme_fivethirtyeight()
     } else {
       ggplot(fd, aes(x = game_num, y = min)) + 
         geom_line() + geom_point(aes(color = fp_pts)) +
         scale_color_gradient(limits = c(15, 40), oob=squish) +
         ylim(0, max(total_bs[!is.na(total_bs$fp_pts),]$min)) +
         xlim(1, 82) + 
         ggtitle(paste(fd$fullName, 
                       "| AvgMin:", round(mean(fd$min), 1),
                       "| AvgFp:", round(mean(fd$fp_pts), 1))) +
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
   
   home_p = reactive({
     id = home_team()[input$home_tbl_rows_selected, ]$personId
     fd = total_bs %>% 
       filter(personId == id)
     
     if(input$home_options == "FP") {
       ggplot(fd, aes(x = game_num, y = fp_pts)) + 
         geom_line() + geom_point(aes(color = min)) +
         scale_color_gradient(limits = c(10, 38), oob=squish) +
         ylim(0, max(total_bs[!is.na(total_bs$fp_pts),]$fp_pts)) +
         xlim(1, 82) + 
         ggtitle(paste(fd$fullName, 
                       "| AvgMin:", round(mean(fd$min), 1),
                       "| AvgFp:", round(mean(fd$fp_pts), 1))) +
         ggthemes::theme_fivethirtyeight()
     } else if (input$home_options == "DRE") {
       ggplot(fd, aes(x = game_num, y = DRE)) + 
         geom_line() + geom_point(aes(color = fp_pts)) +
         scale_color_gradient(limits = c(15, 40), oob=squish) +
         ylim(-5, max(total_bs[!is.na(total_bs$fp_pts),]$DRE)) +
         xlim(1, 82) + 
         ggtitle(paste(fd$fullName, 
                       "| AvgMin:", round(mean(fd$min), 1),
                       "| AvgFp:", round(mean(fd$fp_pts), 1))) +
         ggthemes::theme_fivethirtyeight()
     } else {
       ggplot(fd, aes(x = game_num, y = min)) + 
         geom_line() + geom_point(aes(color = fp_pts)) +
         scale_color_gradient(limits = c(15, 40), oob=squish) +
         ylim(0, max(total_bs[!is.na(total_bs$fp_pts),]$min)) +
         xlim(1, 82) + 
         ggtitle(paste(fd$fullName, 
                       "| AvgMin:", round(mean(fd$min), 1),
                       "| AvgFp:", round(mean(fd$fp_pts), 1))) +
         ggthemes::theme_fivethirtyeight()
     }
   })
   
   output$home_plot = renderPlotly({
     validate(
       need(!is.null(input$home_tbl_rows_selected), "No home player selected.")
     )
     id = home_team()[input$home_tbl_rows_selected, ]$personId
     fd = total_bs %>% 
       filter(personId == id)
     p = home_p()
     ggplotly(p)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

