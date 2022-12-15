#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
server<-function(input, output){
  output$plot <- renderPlot({
    ggplot(data = hbo,
           aes(x = type))+
      geom_bar(aes(fill = type))+
      theme_classic()+
      labs(x = "Types",
           title = "Number of Movies and Shows on HBO")
  })
}

