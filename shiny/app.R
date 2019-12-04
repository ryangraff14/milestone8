#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ballr)
library(shiny)
library(tidyverse)
draft <- read_rds("cleaned.rds")
player <- read_rds("cleaned_player.rds")
# Define UI ----
ui <- fluidPage(
  navbarPage(
    "NBA Trade Machine",
    tabPanel("About",
  titlePanel("NBA Trade Machine"),
  sidebarLayout(
    sidebarPanel(
      h2("Installation"),
      p("This project requires, among others, the ballr package, which can be installed with:"),
      code('install.packages("devtools")
            library(devtools)
            install_github("rtelmore/ballr")'),
      br(),
      br(),
      br(),
      br(),
      # Change this !!!
      img(src = "nba_logo.png"),
      br(),
      "Data is sourced from the NBA, basketball-reference.com, and the ballr package"),
    mainPanel(
      h1("NBA Trade Machine"),
      p("Do you ever look at an NBA trade and wonder, is that really an equal swap? Which team won that trade?  How much are those draft picks really worth?  Look no further; welcome to the NBA Trade Machine!"),
      br(),
      p("On this site, you can craft your own trades and see which side one the trade in terms of stats such as 3pt shooting percentage, Win Shares, and vorp.  Additionally, examine the average stats of all 60 respective NBA draft slots, and view regressions of their values among different statistics." ),
      br(),
      h2("Key Stats Glossary"),
      h4("VORP"),
      p("Value Over Replacement Player (available since the 1973-74 season in the NBA); a box score estimate of the points per 100 team possessions that a player contributed above a replacement-level (-2.0) player, translated to an average team and prorated to an 82-game season"),
      h4("Win Shares per 48 minutes"),
      p("Win Shares is a player statistic which attempts to divvy up credit for team success to the individuals on the team.  It is calculated using player, team and league-wide statistics, and is used in this trade machine on a per-game level."),
      h4("BPM"),
      p("Box Plus/Minus (BPM) is a box score-based metric for evaluating basketball players' quality and contribution to the team.  BPM relies on a player's box score information and the team's overall performance to estimate a player's performance relative to league average. BPM is a per-100-possession stat, the same scale as Adjusted Plus/Minus: 0.0 is league average, +5 means the player is 5 points better than an average player over 100 possessions (which is about All-NBA level), -2 is replacement level, and -5 is really bad."),
      h4("How the Trade Machine works?"),
      p("I have averaged out the values of each draft slot using every single draft from 1987 forward.  Why then? Because that was the start of the modern NBA (the last of Magic v Bird baby!)  Regardless, I take the average values of the picks along with the average career stat lines of whatever players are included in the deal and select 3 point shooting %, points per game, win shares per game, Box Plus/Minus, and VORP, doing the same for the selected picks. The machine then adds them up collectively, and then compares the values of each side.  For each value that is higher, that side gets a point.  Whichever side has more points is deemed more valuable.")
    )
  )
), 
tabPanel("Trade Machine",
         titlePanel("Trade Machine"),
         fluidRow(
           h3("Due to the nature of the machine, all 5 spots on each team must be filled out."),
           h3("However, if you want a player slot to be effectively empty (i.e. mostly or close to 0's), select Jarred Vanderbilt."),
           h3("If you want a pick slot to be effectively empty, select a 2nd round, 29th overall pick."),
           h3("Thank you for your cooperation."),
           column(6,
                  h2("Team A gives up:"),
                  h4("Pick #1"),
                  radioButtons("radioa1", h5("Round"), selected = character(0),
                               choices = list("Round 1" = 1, "Round 2" = 2)),
                  selectInput("selecta1", h5("Select pick #"),
                              choices = list("Select pick" = "", "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),
                              br(),
                  h4("Pick #2"),
                  radioButtons("radioa2", h5("Round"), selected = character(0), 
                                choices = list("Round 1" = 1, "Round 2" = 2)),
                  
                  selectInput("selecta2", h5("Select pick #"), 
                              choices = list("Select pick" = "", "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),                        
                  br(),
                  h4("Pick #3"),
                  radioButtons("radioa3", h5("Round"), selected = character(0),
                              choices = list("Round 1" = 1, "Round 2" = 2)),
                  selectInput("selecta3", h5("Select pick #"), 
                              choices = list("Select pick" = "", "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                              ),
                  selectizeInput("selecta4", h4("Select Player #1"), 
                              choices = player$Name, multiple = TRUE, options = list(maxItems=1)), 
                  selectizeInput("selecta5", h4("Select Player #2"), 
                                 choices = player$Name, multiple = TRUE, options = list(maxItems=1))),
           column(6,
                  h2("Team B gives up:"),
                  h4("Pick #1"),
                  radioButtons("radiob1", h5("Round"), selected = character(0),
                               choices = list("Round 1" = 1, "Round 2" = 2)),
                  selectInput("selectb1", h5("Select pick #"), 
                              # PUT IN SOMETHING THAT RETURNS 0!
                              choices = list("Select pick" = "","1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),
                  br(),
                  h4("Pick #2"),
                  radioButtons("radiob2", h5("Round"), selected = character(0),
                               choices = list("Round 1" = 1, "Round 2" = 2)),
                  
                  selectInput("selectb2", h5("Select pick #"), 
                              choices = list("Select pick" = "", "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),                        
                  br(),
                  h4("Pick #3"),
                  radioButtons("radiob3", h5("Round"), selected = character(0),
                               choices = list("Round 1" = 1, "Round 2" = 2)),
                  selectInput("selectb3", h5("Select pick #"), 
                              choices = list("Select pick" = "", "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),
                  selectizeInput("selectb4", h4("Select Player #1"), 
                                 choices = player$Name, multiple = TRUE, options = list(maxItems=1)), 
                  selectizeInput("selectb5", h4("Select Player #2"), 
                                 choices = player$Name, multiple = TRUE, options = list(maxItems=1))
                #column close
           )
           #Row close
         ),
         fluidRow(
           h1("So which team wins the trade?", align = "center"),
           h3(textOutput('trade_decision'), align = "center"),
           h5(textOutput('test'), align = "center")
        
         )),
tabPanel(
  "Individual Pick Values",
  titlePanel("Average Draft Pick Values"),
  fluidRow(
    h3("Average Statistics of First Round Picks Since 1987"),
    column(12,
           tableOutput('round1')
    )
  ),
  fluidRow(
    h3("Average Statistics of Second Round Picks Since 1987"),
    column(12,
           tableOutput('round2')
    )
  )
        
   # Throw in a table here with stat lines of stars      
         
         
         
         
         ),
tabPanel(
  "Advanced Stats Regressions",
  # Want to throw in dots of NBA Stars
  titlePanel("Advanced Stats of Draft Picks since 1987"),
  fluidRow(
    h3("Average VORP of Draft Picks 1-60 Since 1987"),
    column(12,
           plotOutput("vorpplot")
    )
  ),
  fluidRow(
    h3("Average BPM of Draft Picks 1-60 Since 1987"),
    column(12,
           plotOutput('bpmplot')
    )
  ),
  
  fluidRow(
    h3("Average Win Shares Per Game of Draft Picks 1-60 Since 1987"),
    column(12,
           plotOutput('wsplot')
    )
  )  
)
)
)

# Define server logic ----
server <- function(input, output) {
  
  output$round1 <- renderTable({
    draft %>% 
      filter(rd == "1") %>% 
      group_by(pk) %>% 
      # mutate or summarize
      summarize(fg_percent = mean(fg_percent), x3p_percent = mean(x3p_percent),ft_percent = mean(ft_percent), mp_17 = mean(mp_17), pts_18 = mean(pts_18),trb_19 = mean(trb_19), ast_20 = mean(ast_20), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp))
  })
  output$round2 <- renderTable({
    draft %>% 
      filter(rd == "2") %>% 
      group_by(pk) %>% 
      # mutate or summarize
      summarize(fg_percent = mean(fg_percent), x3p_percent = mean(x3p_percent),ft_percent = mean(ft_percent), mp_17 = mean(mp_17), pts_18 = mean(pts_18),trb_19 = mean(trb_19), ast_20 = mean(ast_20), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp))
  })
  output$vorpplot <- renderPlot({
    draft %>% 
      group_by(rk) %>% 
      summarize(vorp = mean(vorp)) %>% 
      ggplot(aes(x=rk, y=vorp)) + geom_point() + geom_smooth() + geom_vline(xintercept = 30.5, color = "blue")
  })
  output$bpmplot <- renderPlot({
    draft %>% 
      group_by(rk) %>% 
      summarize(bpm = mean(bpm)) %>% 
      ggplot(aes(x=rk, y=bpm)) + geom_point() + geom_smooth() + geom_vline(xintercept = 30.5, color = "blue")
  })
  output$wsplot <- renderPlot({
    draft %>% 
      group_by(rk) %>% 
      summarize(ws = mean(ws_48)) %>% 
      ggplot(aes(x=rk, y=ws)) + geom_point() + geom_smooth() + geom_vline(xintercept = 30.5, color = "blue")
  })
 
     output$test <- renderText({
       kane <- "Thank you for trading! Woj and Shams thank you for providing them jobs!"
       kane
     })
     
      output$trade_decision <- renderText({
        
        pick1a <-
          draft %>%
          # changed to and, not comma
          filter(rd == input$radioa1 & pk == input$selecta1) %>%
          group_by(pk) %>%
          # Potentially don't need this group by 
          summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)) %>%
          select(x3p_percent, pts_18, ws_48, bpm, vorp)
        
        pick2a <-
          draft %>%
          filter(rd == input$radioa2, pk == input$selecta2) %>%
          group_by(pk) %>%
          summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)) %>%
          select(x3p_percent, pts_18, ws_48, bpm, vorp)
        
        pick3a <-
          draft %>%
          filter(rd == input$radioa3, pk == input$selecta3) %>%
          group_by(pk) %>%
          summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)) %>%
          select(x3p_percent, pts_18, ws_48, bpm, vorp)
        
        picksa <- rbind(pick1a, pick2a, pick3a)
        
        # Player B1
        ida4 <- player %>%
          filter(Name == input$selecta4) %>%
          select(ID)
        # took off quotes off input
        lettera4 <- str_split(input$selecta4, " ", simplify = TRUE)[,2] %>%
          substr(1, 1) %>%
          tolower()
        
        codea1 <- str_replace_all(paste("/players/", lettera4, "/", ida4, ".html"), fixed(" "), "")
        
        playera1_basic <- NBAPlayerPerGameStats(codea1) %>% 
          filter(season == "Career") %>%
          select(x3ppercent, pts)
        
        playera1_adv <- NBAPerGameAdvStatistics(season = 2018) %>%
          filter(player == input$selecta4) %>%
          select(ws_48, bpm, vorp)
        
        playera1 <- merge(playera1_basic, playera1_adv)
        # Player A2
        ida5 <- player %>%
          filter(Name == input$selecta5) %>%
          select(ID)
        
        lettera5 <- str_split(input$selecta5, " ", simplify = TRUE)[,2] %>%
          substr(1, 1) %>%
          tolower()
        
        codea2 <- str_replace_all(paste("/players/", lettera5, "/", ida5, ".html"), fixed(" "), "")
        
        playera2_basic <- NBAPlayerPerGameStats(codea2) %>% 
          filter(season == "Career") %>%
          select(x3ppercent, pts)
        
        playera2_adv <- NBAPerGameAdvStatistics(season = 2018) %>%
          filter(player == input$selecta5) %>%
          select(ws_48, bpm, vorp)
        
        playera2 <- merge(playera2_basic, playera2_adv)
        
        playera <- rbind(playera1, playera2)
        
        playera <- playera %>% select("x3p_percent"=x3ppercent, "pts_18" =pts, ws_48, bpm, vorp)
        
        teama <- rbind(picksa, playera) %>% 
          summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)) %>%
          select(x3p_percent, pts_18, ws_48, bpm, vorp)
        
        pick1b <-
          draft %>%
          filter(rd == input$radiob1, pk == input$selectb1) %>%
          group_by(pk) %>%
          summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)) %>%
          select(x3p_percent, pts_18, ws_48, bpm, vorp)
        
        pick2b <-
          draft %>%
          filter(rd == input$radiob2, pk == input$selectb2) %>%
          group_by(pk) %>%
          summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)) %>%
          select(x3p_percent, pts_18, ws_48, bpm, vorp)
        
        pick3b <-
          draft %>%
          filter(rd == input$radiob3, pk == input$selectb3) %>%
          group_by(pk) %>%
          summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)) %>%
          select(x3p_percent, pts_18, ws_48, bpm, vorp)
        
        picksb <- rbind(pick1b, pick2b, pick3b)
        # Player B1
        idb4 <- player %>%
          filter(Name == input$selectb4) %>%
          select(ID)
        
        letterb4 <- str_split(input$selectb4, " ", simplify = TRUE)[,2] %>%
          substr(1, 1) %>%
          tolower()
        
        codeb1 <- str_replace_all(paste("/players/", letterb4, "/", idb4, ".html"), fixed(" "), "")
        
        playerb1_basic <- NBAPlayerPerGameStats(codeb1) %>% 
          filter(season == "Career") %>%
          select(x3ppercent, pts)
        
        playerb1_adv <- NBAPerGameAdvStatistics(season = 2018) %>%
          filter(player == input$selectb4) %>%
          select(ws_48, bpm, vorp)
        
        playerb1 <- merge(playerb1_basic, playerb1_adv)
        # Player B2
        idb5 <- player %>%
          filter(Name == input$selectb5) %>%
          select(ID)
        
        letterb5 <- str_split(input$selectb5, " ", simplify = TRUE)[,2] %>%
          substr(1, 1) %>%
          tolower()
        
        codeb2 <- str_replace_all(paste("/players/", letterb5, "/", idb5, ".html"), fixed(" "), "")
        
        playerb2_basic <- NBAPlayerPerGameStats(codeb2) %>% 
          filter(season == "Career") %>%
          select(x3ppercent, pts)
        
        playerb2_adv <- NBAPerGameAdvStatistics(season = 2018) %>%
          filter(player == input$selectb5) %>%
          select(ws_48, bpm, vorp)
        
        playerb2 <- merge(playerb2_basic, playerb2_adv)
        
        playerb <- rbind(playerb1, playerb2)
        
        playerb <- playerb %>% select("x3p_percent"=x3ppercent, "pts_18" =pts, ws_48, bpm, vorp)
        
        teamb <- rbind(picksb, playerb) %>% 
          summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)) %>%
          select(x3p_percent, pts_18, ws_48, bpm, vorp)
        
        points_won <- sum(teama > teamb)
        
        outcome <- ifelse(points_won >= 3, "Team A's offering is more valuable and therefore *Team B* won the trade", "Team B's offering is more valuable and therefore *Team A* has won the trade")        
        
        paste(outcome)
        
        })
    # End reactive wrap?  
      
#  output$lebron <- renderPrint({ lebron_basic <- NBAPlayerPerGameStats("/players/j/jamesle01.html") %>% 
 #   filter(season == "Career") %>% 
  #  select(fgpercent, x3ppercent, ftpercent, mp, pts, trb, ast)
  
#  lebron_adv <- NBAPerGameAdvStatistics(season = 2009) %>% 
 #   filter(rk == "208") %>% 
  #  select(ws_48, bpm, vorp)
  
#  lebron <- merge(lebron_basic, lebron_adv) })
  # What if i move the folder to my shiny folder? Then can I use the vectors Ive created??
  
}

# Run the app ----
shinyApp(ui = ui, server = server)