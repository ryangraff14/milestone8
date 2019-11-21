#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
draft <- read_rds("cleaned.rds")
# Define UI ----
ui <- fluidPage(
  navbarPage(
    "NBA Trade Machine",
    tabPanel("About",
  titlePanel("NBA Trade Machine"),
  sidebarLayout(
    sidebarPanel(
      h2("Installation "),
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
      h1("Introducing Shiny"),
      p("Shiny is a new package from RStudio that makes it ", 
        em("incredibly easy "), 
        "to build interactive web applications with R."),
      br(),
      p("For an introduction and live examples, visit the ",
        a("Shiny homepage.", 
          href = "http://shiny.rstudio.com")),
      br(),
      h2("Features"),
      p("- Build useful web applications with only a few lines of code—no JavaScript required."),
      p("- Shiny applications are automatically 'live' in the same way that ", 
        strong("spreadsheets"),
        " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.")
    )
  )
), 
tabPanel("Trade Machine",
         titlePanel("Trade Machine"),
         fluidRow(
           column(6,
                  h4("Team A"),
                  h2("Pick #1"),
                  radioButtons("radio", h3("Round"),
                               choices = list("Round 1" = 1, "Round 2" = 2)),
                  selectInput("select", h3("Select pick #"), 
                              choices = list("Select Pick #" = NULL, "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30, selected = "Select Pick #"),
                  ),
                              br(),
                  h2("Pick #2"),
                  radioButtons("radio", h3("Round"),
                                choices = list("Round 1" = 1, "Round 2" = 2)),
                  
                  selectInput("select", h3("Select pick #"), 
                              choices = list("Select Pick #" = NULL, "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),                        
                  br(),
                  h2("Pick #3"),
                  radioButtons("radio", h3("Round"),
                              choices = list("Round 1" = 1, "Round 2" = 2)),
                  selectInput("select", h3("Select pick #"), 
                              choices = list("Select Pick #" = NULL, "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                              ),
                  textInput("text", label = h3("Player #1"), value = ""),
                  textInput("text", label = h3("Player #2"), value = "")),
           column(6,
                  h4("Team B"),
                  h2("Pick #1"),
                  radioButtons("radio", h3("Round"),
                               choices = list("Round 1" = 1, "Round 2" = 2)),
                  selectInput("select", h3("Select pick #"), 
                              choices = list("Select Pick #" = NULL, "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),
                  br(),
                  h2("Pick #2"),
                  radioButtons("radio", h3("Round"),
                               choices = list("Round 1" = 1, "Round 2" = 2)),
                  
                  selectInput("select", h3("Select pick #"), 
                              choices = list("Select Pick #" = NULL, "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
                  ),                        
                  br(),
                  h2("Pick #3"),
                  radioButtons("radio", h3("Round"),
                               choices = list("Round 1" = 1, "Round 2" = 2)),
                  selectInput("select", h3("Select pick #"), 
                              choices = list("Select Pick #" = NULL, "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4, "5th" = 5, "6th" = 6, "7th" = 7, "8th" = 8, "9th" = 9, "10th" = 10, "11th" = 11, "12th" = 12, "13th" = 13, "14th" = 14, "15th" = 15, "16th" = 16, "17th" = 17, "18th" = 18, "19th" = 19, "20th" = 20, "21st" = 21, "22nd" = 22, "23rd" = 23, "24th" = 24, "25th" = 25, "26th" = 26, "27th" = 27, "28th" = 28, "29th" = 29, "30th" = 30),
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
}

# Run the app ----
shinyApp(ui = ui, server = server)