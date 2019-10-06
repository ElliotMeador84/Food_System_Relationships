

library(shiny)
library(igraph)

# Define a server for the Shiny app
server = function(input, output) {
  g3 = reactive({
    g2 = gtest[gtest$Article==input$article,]
    g2 = g2[order(g2[[3]],decreasing = TRUE), ]
    graph.data.frame(g2[1:5,2:3], directed=TRUE)
  })
  
  
  # Fill in the spot we created for a plot
  
  output$g3plot = renderPlot({
    plot.igraph(g3(), layout=layout.mds)
  })
}


