library(shiny)
library(igraph)

rm(list = ls())

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("graph", 
                  label = "Choose graph to display",
                  choices = c("Stress", 
                              "KK", 
                              'DH'), 
                  selected = "Network Graph"), 
      sliderInput(
        'nodes', 
        label = '# of Nodes', 
        min = 0, 
        max = 50, 
        value = 5
      ), 
      sliderInput(
        'edges', 
        label = '# of Edges', 
        min = 0, 
        max = .15, 
        value = 0.05
      )), 
    mainPanel(
      plotOutput("myplot")
    )
  )
  
)

server <- function(input, output, session) {
  
  output$myplot <- renderPlot({
    g <- erdos.renyi.game(input$nodes, input$edges)
    # g <- erdos.renyi.game(10, .25)
    V(g)$name <- as.character(1:vcount(g))
    
    
   graph_aes <-  function(g, layout_style = 'stress'){
     g %>% 
       as_tbl_graph() %>% 
       filter(!node_is_isolated()) %>% 
       ggraph(layout = layout_style) +
      geom_edge_parallel(aes(alpha = ..index..),
                         color = 'black',
                         show.legend = F)+
        geom_node_point(aes(color = name), 
                        size = 2.5,
                        show.legend = F)+
       scale_color_viridis(discrete = T)+
       scale_edge_color_viridis(discrete = T)+
        theme_graph(foreground = T, 
                    border = T)+
       coord_equal()
    }
    # Plot Network graph is selected
    if (input$graph == "KK"){
        graph_aes(g, 'kk')
    } else if (input$graph == 'Stress'){
      graph_aes(g, 'stress')
    } else {
      graph_aes(g, 'dh')
    }
  })
  
  
}

shinyApp(ui, server)
