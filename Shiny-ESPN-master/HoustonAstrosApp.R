library(shiny)
library(ggplot2)
library(tidyverse)

#Sean- lead on the Info, Damien helped
#Nathan - lead on the Houston Astros bar plot
#Sean - lead on the Comparison bar plot, Nathan contributed
#Damien - lead on the table, Sean helped
#Damien - lead on the conclusion, Sean helped



finalwrangle <- read.csv("finalwrangle.csv")

researchChoices <- as.list(names(finalwrangle) [5:14])

researchQuestions <- c("Runs", "Hits", "Home Runs", "Runs Batted In", 
                       "Base on Balls", "Strikeouts", "Batting Average", "On-Base Percentage",
                       "Slugging Percentage", "On Base + Slugging Average")

names(researchChoices) <- researchQuestions

Team <- c("Colorado Rockies", "Los Angeles Dodgers", "Arizona Diamondbacks", "Minnesota Twins", 
                      "Boston Red SoxRed ", "Chicago Cubs", "New York Yankees",
                       "Cleveland Indians", "Washington Nationals")

ui <- fluidPage(
 
       titlePanel("Did the Houston Astros Cheat in the 2017 Postseason?"),
  
      sidebarPanel(
        selectInput(inputId = "Questions", label = "Choose a Statistic:", choices = researchChoices)
       ),
    mainPanel(
      selectInput(inputId = "teams", label = "Playoff Teams:", choices = Team), 
      tabsetPanel(
        tabPanel("Info", textOutput("Info")
        ),
        tabPanel("The Astros Playoff Statistics", plotOutput("Plot2")
        ),
        tabPanel("Comparing The Astros to Other Playoff Teams", plotOutput("Plot")
        ),
        tabPanel("Table", DT::dataTableOutput("Table")
        ),
        tabPanel("Conclusion", textOutput("Conclusion"))
        )
        )
   
    )


 

server <- function(input, output) {
  
  use_data <- reactive({
    data <- finalwrangle %>%
      filter(Team == input$teams | Team == "Houston Astros")
  })
    
    use_data2 <- finalwrangle %>%
        filter(Team == "Houston Astros")
    
  output$Plot <- renderPlot(
  
       ggplot(data = use_data(), aes_string(x = "Team", y = input$Questions, fill = "Location")) +
        geom_bar(position = "dodge", stat = "identity")
       
    
  )
  
  
  output$Plot2 <- renderPlot(
    
    ggplot(data = use_data2, aes_string(x = "Team", y = input$Questions, fill = "Location")) +
      geom_bar(position = "dodge", stat = "identity")
    
  )
  
  
  
  
  output$Table <- DT::renderDataTable(DT::datatable(use_data()))
  
  output$Info <- renderText("Our study focuses on the 2017 MLB postseason, and more specifically, the differences in the statistics in home and away games, between the Houston Astros and the other nine teams that competed 
in the postseason that year. For more background information as to why we are so interested in this specific topic, the Houston Astros 
were caught cheating during the postseason in 2017. The Astros were doing something known as sign stealing, and were using cameras in center field to film 
the opposing team’s catcher giving specific signs to the pitcher. Members of the Astros organization, both staff and players, would watch the live feed, then signal 
to the batter which pitch to expect. Obviously, this gives the Houston Astros a huge advantage at home. The Houston Astros went on to win the 2017 World Series. As for our research questions, we hope to answer the following: Is there a clear difference between the home and away 
statistics when we just focus on the Houston Astros? And how do the Houston Astros home and away statistics compare to the other postseason teams' home
and away stats? 
We used data from MLB.com to help answer these questions."
  )
  
  output$Conclusion <- renderText("It is important to address that some teams stats are not accurately
                                  represented. The data used in our study focuses on the 2017 MLB Postseason; 
                                  as a result, not all teams participated in the same amount of games.
                                  For example, the Houston Astros and the LA Dodgers played in the final world series, 
                                  so their stats for runs, hits, strikeouts, homeruns, will be significantly higher than
                                  the Colorado Rockies and Minnesota Twins who only played one game in postseason. Conversely,
                                  teams like the Rockies and Twins may have the same batting average, on-base percentage, slugging 
                                  percentage, and on-base plus slugging percentage, as the Astros or Dodgers; however, 
                                  it is important to consider the difference in games played between these teams. As stated in the 
                                  introduction, the goal of this study was to compare the Houston Astros to the rest of the 
                                  participants in the 2017 MLB Postseason; more specifically, the home and away statistics.
                                  In this study, we analyzed the differences in the statistics of the Houston Astros
                                  Home and Away games, and how they compared to the rest of the postseason 
                                  teams' statistics. The areas of interest are: Runs (R), Hits (H), Home Runs (HR), Runs Batted In (RBI), 
                                  Base on Balls (BB), Strike Outs (SO), Batting Average (AVG), On-Base Percentage (OBP),
                                  Slugging Percentage (SLG), and On-Base plus Slugging Percentage (OPS). When looking at the Houston 
                                  Astros individually, it is clear that they performed better at home than away during the postseason.
                                  When comparing the Astros to postseason teams that played in a similar amount of games (i.e. the LA Dodgers,
                                  and New York Yankees), the Astros yet again perform better at home than their competition do. Although we do 
                                  not know if there were any lurking variables that had an impact on the data, we can conclude that the Astros 
                                  had a clear advantage at home compared to other teams in the 2017 MLB postseason. Overall,
                                  The Houston Astros were caught cheating during their home games, which in turn helped
                                  the team go on to win the 2017 World Series.       
                                  ")
  
  
    }
     

  
  



shinyApp(ui = ui, server = server)
