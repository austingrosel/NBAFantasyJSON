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

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css", "#away_name.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#away_tbl.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#home_name.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#home_tbl.recalculating { opacity: 1.0; }"),
  
  # Application title
  titlePanel("NBA Box Score"),
  dateInput('date', label = h4("Date input")),
  uiOutput("select_todays_games"),
  fluidRow(h2(uiOutput("score")), align = "center"),
  h2(uiOutput("away_name")),
  DTOutput("away_tbl"),
  
  hr(),
  
  h2(uiOutput("home_name")),
  DTOutput("home_tbl"),
  
  br()
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
      need(input$game_desc != " vs. ", "No games happening today.")
    )
    this_game = scoreboard()[scoreboard()$game_desc == input$game_desc, ]
    if(this_game$statusNum == 3) {
      HTML(paste0("FINAL: ", 
                  this_game$vTeam$triCode, ": ", this_game$vTeam$score, " ",
                  this_game$hTeam$triCode, ": ", this_game$hTeam$score))
    } else if (this_game$statusNum == 2) {
      HTML(paste0("Q", this_game$period$current, ": ", this_game$clock, " ", 
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
      need(input$game_desc != " vs. ", "No games happening today.")
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
  
   output$away_tbl = renderDT({
     validate(
       need(nrow(boxscore()) > 0, "Game ID not found")
     )
     DT::datatable(boxscore() %>% 
       filter(team_short_name == teams()[1]) %>% 
       select(fullName, pos, isOnCourt, age, min, DRE, fp_pts) %>% arrange(-min), 
      options = list(dom = 't')) %>%
       formatStyle(
         'isOnCourt',
         target = 'row',
         fontWeight = styleEqual(c(1, 0), c('bold', 'normal'))) %>%
       formatStyle('DRE',
                   background=color_from_middle(boxscore()$DRE, 'pink', 'lightblue'),
                   backgroundSize = '88% 88%',
                   backgroundRepeat = 'no-repeat',
                   backgroundPosition = 'center') %>%
       formatStyle('fp_pts',
                   background = styleColorBar(boxscore()$fp_pts, 'steelblue', angle = -90),
                   backgroundSize = '88% 88%',
                   backgroundRepeat = 'no-repeat',
                   backgroundPosition = 'center')
   })
   
   output$home_name = renderUI({
     HTML(paste("<b>", teams()[2], "</b>"))
   })
   
   output$home_tbl = renderDT({
     validate(
       need(nrow(boxscore()) > 0, "Game ID not found")
     )
     DT::datatable(boxscore() %>% 
       filter(team_short_name == teams()[2]) %>% 
       select(fullName, pos, isOnCourt, age, min, DRE, fp_pts) %>% arrange(-min), 
     options = list(dom = 't')) %>%
       formatStyle(
         'isOnCourt',
         target = 'row',
         fontWeight = styleEqual(c(1, 0), c('bold', 'normal'))) %>%
       formatStyle('DRE',
                   background=color_from_middle(boxscore()$DRE, 'pink', 'lightblue'),
                   backgroundSize = '88% 88%',
                   backgroundRepeat = 'no-repeat',
                   backgroundPosition = 'center') %>%
       formatStyle('fp_pts',
                   background = styleColorBar(boxscore()$fp_pts, 'steelblue', angle = -90),
                   backgroundSize = '88% 88%',
                   backgroundRepeat = 'no-repeat',
                   backgroundPosition = 'center')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

