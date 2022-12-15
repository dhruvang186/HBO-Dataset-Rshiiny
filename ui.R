library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(shinydashboard)
library(ggplot2)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
# Load data
hbo <- read.csv("D:/Downloads/HBO/HBO/titles.csv")
hbo2<-hbo[!(hbo$age_certification==""),]
view2 = hbo2 %>% 
  select(title,age_certification,type) %>%
  filter(complete.cases(.))

view3 = hbo %>% 
  select(title,type,runtime,imdb_score,seasons,release_year) %>% 
  filter(type == "SHOW" & 
           seasons > 5 & 
           imdb_score > 8 &
           release_year > 2010)
View4 = hbo %>% 
  select(title,imdb_score,type,release_year) %>% 
  filter(imdb_score > 8 &
           release_year == 2020)
# Define UI
ui=dashboardPage(skin = "black",
  dashboardHeader(title = "HBO Dataset",
                  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/dhruvang-patel-62b325202/" ,icon("linkedin"), "My Profile", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://github.com/dhruvang186", icon("github"), "Source Code", target="_blank"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Raw Data", tabName = "rawdata", icon = icon("table"))
    )
  ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                tabBox(id = "Tab1",width=12,
                       tabPanel("First Analysis",icon=icon("chart-pie"),h2("What is the ratio of movies and shows on the streaming device?"),
                        fluidRow(
                         box(plotlyOutput("plot1"),background = "black", width = 6),
                         column(width = 5, tags$br() ,
                                tags$p("What can viewers anticipate if they sign up for HBO's streaming service? The data indicates that viewers may anticipate seeing more movies than series.")
                       ))),
                       tabPanel(title = "Second Analysis",icon=icon("list-alt"),h2("How many general and explicit movies and shows are there in the streaming device?"),
                                fluidRow(
                                  box(plotlyOutput("plot2"),background = "black", width = 6),
                                  column(width = 5, tags$br() ,
                                         tags$p("The graph illustrates the type of content HBO offers. The majority of movies are graded R, PG-13, or PG when it comes to content. TV-MA and TV-14 rated shows are more widely seen when it comes to programming. As a result, the majority of adult content that is only permitted for kids to see under parental supervision is available on the HBO platform."),
                                         
                                ))),
                       tabPanel(title = "Third Analysis",icon=icon("bar-chart-o"),h2("Which are the latest best performing movies and shows on HBO?"),
                                fluidRow(box(plotOutput("plot3"),background = "black", width = 10)),
                                column(width = 8, tags$br() ,
                                       tags$p("Here is the bar graph of the top OTT series with an IMDB rating of at least 8 and at least five seasons that were published after 2010. Ott streaming providers may utilise this information to include these episodes to their top series section, which will boost engagement on the platform."),
                                )),
                       tabPanel(title = "Fourth Analysis",icon=icon("table"),h2("Top performing movies and shows in 2020"),
                                fluidPage(
                                  dataTableOutput("best")
                                )),
                       
),
        ),
        tabItem(tabName = "rawdata",
                h1("Raw dataset"),
                fluidPage(
                  dataTableOutput("hbodataset")
                )
                
        )
    )
  )#body
 )#page


server<-function(input, output){
  output$plot1 <- renderPlotly({
    typemovies <- hbo %>% group_by(type) %>% summarise(count = n())
    fig6 <- plot_ly(typemovies, labels = ~type, values = ~count, type = 'pie')
    
    fig6 <- fig6 %>% layout(title = "Movies vs Shows",
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig6
  })
  
  output$hbodataset<-renderDataTable(hbo)
  output$best<-renderDataTable(View4)
  
  output$plot2 <- renderPlotly({
    ggplotly(ggplot(data = hbo2, aes(x = age_certification,
                                      fill = type ))+
               geom_bar(width = 0.5)+
               scale_fill_manual(values=c("lightblue","pink"))+
               theme_classic()+
               coord_flip()+
               labs(x = "Age Certification",
                    title = "Age Certification Of HBO"))
  })
  output$plot3 <- renderPlot({
    ggplot(data = view3,
           aes(x = title,
               y = seasons,
           ))+
      geom_bar(stat = "identity", fill = "lightgreen", color="black", width = 0.5)+
      theme_classic()+
      coord_flip()+
      labs(x = "Show Title",
           y = "Number of Seasons",
           title = "Show's after 2010 which has more than 5 Season, IMBD greater than 8")
    
  })
}
shinyApp(ui = ui, server = server, options = list(height = 1080))