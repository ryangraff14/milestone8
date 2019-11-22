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
      p("- Vorp - ......."),
      p("- Win Shares - ......."),
      p("- BPM - ......."),
      p("- How the Trade Machine works? - I have averaged out the values of each draft slot using every single draft from 1987 forward.  Why then? Because that was the start of the modern NBA (the last of Magic v Bird baby!)  Regardless, I take the average values of the pics along with the stat lines of whatever players are included in the deal, total them up, and then compare.")
    )
  )
), 
tabPanel("Trade Machine",
         titlePanel("Trade Machine"),
         fluidRow(
           column(6,
                  h4("Team A"),
                  h2("Pick #1"),
                  radioButtons("radioa1", h3("Round"),
                               choices = list("Round 1" = 1, "Round 2" = 2)),
                  selectInput("selecta1", h3("Select pick #"), 
                              choices = list("1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),
                              br(),
                  h2("Pick #2"),
                  radioButtons("radioa2", h3("Round"),
                                choices = list("Round 1" = 1, "Round 2" = 2)),
                  
                  selectInput("selecta2", h3("Select pick #"), 
                              choices = list("1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),                        
                  br(),
                  h2("Pick #3"),
                  radioButtons("radioa3", h3("Round"),
                              choices = list("Round 1" = 1, "Round 2" = 2)),
                  selectInput("selecta3", h3("Select pick #"), 
                              choices = list("1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                              ),
                  textInput("text", label = h3("Player #1"), value = ""),
                  textInput("text", label = h3("Player #2"), value = "")),
           column(6,
                  h4("Team B"),
                  h2("Pick #1"),
                  radioButtons("radiob1", h3("Round"),
                               choices = list("Round 1" = 1, "Round 2" = 2)),
                  selectInput("selectb1", h3("Select pick #"), 
                              # PUT IN SOMETHING THAT RETURNS 0!
                              choices = list("1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),
                  br(),
                  h2("Pick #2"),
                  radioButtons("radiob2", h3("Round"),
                               choices = list("Round 1" = 1, "Round 2" = 2)),
                  
                  selectInput("selectb2", h3("Select pick #"), 
                              choices = list("1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),                        
                  br(),
                  h2("Pick #3"),
                  radioButtons("radiob3", h3("Round"),
                               choices = list("Round 1" = 1, "Round 2" = 2)),
                  selectInput("selectb3", h3("Select pick #"), 
                              choices = list("1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),
                  textInput("text", label = h3("Player #1"), value = ""),
                  textInput("text", label = h3("Player #2"), value = "")
                #column close
           )
           #Row close
         ),
         fluidRow(
           h1("So which team wins the trade?", align = "center")
        
         # Trade Comparison Result will go here
         
         
         
         
         
         
         
         
         
         
         
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
    h3("Average Win Shares of Draft Picks 1-60 Since 1987"),
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
      ggplot(aes(x=rk, y=vorp)) + geom_point() + geom_smooth()
  })
  output$bpmplot <- renderPlot({
    draft %>% 
      group_by(rk) %>% 
      summarize(bpm = mean(bpm)) %>% 
      ggplot(aes(x=rk, y=bpm)) + geom_point() + geom_smooth()
  })
  output$wsplot <- renderPlot({
    draft %>% 
      group_by(rk) %>% 
      summarize(ws = mean(ws_48)) %>% 
      ggplot(aes(x=rk, y=ws)) + geom_point() + geom_smooth()
  })
 # output$pick1a <- render#????? 
   # draft %>% 
   #     filter(rd == input$radio1a, pk == input$select1a) %>% 
   #       group_by(pk) %>% 
   # summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)
   # select(x3p_percent, pts18, ws_48, bpm, vorp)
  
  # output$pick2a <- render#????? 
  # draft %>% 
  #     filter(rd == input$radio2a, pk == input$select2a) %>% 
  #       group_by(pk) %>% 
  # summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)
  # select(x3p_percent, pts18, ws_48, bpm, vorp)
  
  # output$pick3a <- render#????? 
  # draft %>% 
  #     filter(rd == input$radio3a, pk == input$select3a) %>% 
  #       group_by(pk) %>% 
  # summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)
  # select(x3p_percent, pts18, ws_48, bpm, vorp)
  
  #PLAYER 1A
  
  #PLAYER 2A
  
  # output$pick1b <- render#????? 
  # draft %>% 
  #     filter(rd == input$radio1b, pk == input$select1b) %>% 
  #       group_by(pk) %>% 
  # summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)
  # select(x3p_percent, pts18, ws_48, bpm, vorp)
  
  # output$pick2b <- render#????? 
  # draft %>% 
  #     filter(rd == input$radio2b, pk == input$select2b) %>% 
  #       group_by(pk) %>% 
  # summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)
  # select(x3p_percent, pts18, ws_48, bpm, vorp)
  
  # output$pick3b <- render#????? 
  # draft %>% 
  #     filter(rd == input$radio3b, pk == input$select3b) %>% 
  #       group_by(pk) %>% 
  # summarize(x3p_percent = mean(x3p_percent), pts_18 = mean(pts_18), ws_48 = mean(ws_48), bpm = mean(bpm), vorp = mean(vorp)
  # select(x3p_percent, pts18, ws_48, bpm, vorp)
  
  #PLAYER 1B
  
  #PLAYER 2B
  
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