

library(shiny)
library(igraph)

gtest = data.frame(cbind(Article = c(1:10), from = c(11:20), to = c(21:30)))
runApp(list(
  ui = shinyUI(
    fluidPage(    
      titlePanel("Articles by similarities"),
      sidebarLayout(      
        sidebarPanel(
          selectInput("article", "Article:", choice = gtest$Article)
        ),
        mainPanel(
          plotOutput("g3plot")  
        )
      )
    ))))
  