library(tidyverse)
library(lubridate)
library(igraph)
library(ggraph)
library(tidygraph)
library(graphlayouts)
library(tidytext)
library(glue)

library(visNetwork)
library(shiny)
library(shinythemes)




source('/Users/johne.meador/OneDrive - SRUC/all_functions.R')

setwd('/Users/johne.meador/OneDrive - SRUC/Food_System_Relationships/data/')

load('CSA_tweets.RData')



CSA_weekly <- CSA_tweets %>%
  seperate_mentions() %>%
  mutate(week = floor_date(created_at, 'week')) %>%
  mutate(week_n = group_indices(., week)) %>%
  select(screen_name,
         mentions_screen_name,
         week_n,
         week,
         text)


if (exists('layout_tibble') == F) {
  layout_tibble <-
    graph_from_data_frame(CSA_weekly) %>%
    layout_with_stress() %>%
    as_tibble() %>%
    set_names('x', 'y')
  
}

layout_tibble$name <-
  V(graph_from_data_frame(CSA_weekly))$name


names <- as.character(layout_tibble$name)


name_colors <- sample(Spectral_n(length(names)))
names(name_colors) <- names



######################
#### Stakeholders ####
######################


# choices
load('profile_ls.RData')
stake_holder_choices <- profile_ls %>%
  flatten_chr() %>%
  .[order(.)] %>%
  str_remove('^@')




###################
#### Shiny APP ####
###################

############
#### UI ####
############
ui <- fluidPage(theme = shinytheme("darkly"),
  titlePanel("Activity in the Network Core"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'graph_sent',
        label = 'Network-Discussion',
        choices = c('Stakeholders',
                    'Graph',
                    'Discussion'),
        selected = 'Stakeholders'
      ),
      sliderInput(
        'week_n',
        label = 'Week since 1 March 2019',
        min = min(CSA_weekly$week_n),
        max = max(CSA_weekly$week_n),
        value = 1,
        step = 1
      ),
      ## Stakeholder UI ##
      selectInput(
        inputId = 'stakeholder',
        label = 'Starter-Stakeholder',
        choices = stake_holder_choices,
        selected = 'CSANetwork'
      ),
      selectInput(
        inputId = 'neighbor_size',
        label = 'Neighbourhood size',
        choices = 1:5,
        selected = 1
      )
    ),
    mainPanel(plotOutput("myplot"))
  )
  
)





server <- function(input, output, session) {
  output$myplot <- renderPlot({
    if (input$graph_sent == 'Graph') {
      graph_tbl <-   CSA_weekly %>%
        as_tbl_graph() %>%
        mutate(from = row_number()) %>%
        activate(nodes) %>%
        left_join(layout_tibble) %>% # add layout
        activate(edges) %>%
        filter(week_n == input$week_n) %>% #
        activate(nodes) %>%
        mutate(degree = centrality_degree(mode = 'all')) %>%
        filter(!node_is_isolated())
      
      
      # layout
      graph_layout <-  graph_tbl  %>%
        activate(nodes) %>%
        as_tibble() %>%
        select(x, y)
      
      graph_tbl <- graph_tbl %>%
        activate(nodes) %>%
        select(-x,-y)
      
      # caption
      week_of <-  graph_tbl %>%
        activate(edges) %>%
        as_tibble() %>%
        pull(week_n) %>%
        unique()
      
      caption_text <- CSA_weekly %>%
        filter(week_n == week_of) %>%
        pull(week) %>%
        unique() %>%
        format('%B of %Y')
      
      
      edge_sync <- graph_tbl %>%
        activate(nodes) %>%
        as_tibble() %>%
        select(from, name)
      
      
      graph_tbl %>%
        ggraph(layout = graph_layout) +
        geom_edge_arc(
          strength = 0.05,
          color = Greys[3],
          aes(alpha = ..index..),
          show.legend = F
        ) +
        geom_node_point(aes(size = degree,
                            color = name)) +
        scale_color_manual(values = name_colors,
                           guide = F) +
        scale_size(range = c(0.05, 3.5), 
                   name = 'Degree') +
        theme_graph(
          background = 'black',
          caption_colour = 'white',
          foreground = 'white',
          border = T
        ) +
        labs(caption = caption_text)
    } else if (input$graph_sent == 'Discussion') {
      CSA_filter <-  CSA_weekly %>%
        as_tbl_graph() %>%
        activate(nodes) %>%
        left_join(layout_tibble) %>% # add layout
        activate(edges) %>%
        filter(week_n == input$week_n)  #
      
      
      week_of <-  CSA_filter %>%
        activate(edges) %>%
        as_tibble() %>%
        pull(week_n) %>%
        unique()
      
      caption_text <- CSA_weekly %>%
        filter(week_n == week_of) %>%
        pull(week) %>%
        unique() %>%
        format('%B of %Y')
      
      other_stop <- str_c(c('t.co',
                            'https',
                            't.co'))
      
      key_word <- str_c(
        c(
          'plant',
          'grow',
          'food',
          'eat',
          'health',
          'sustainable',
          'local',
          'regional',
          'healthy',
          'climate',
          'climate change',
          'farm',
          'agriculture'
        )
      )
      
      key_word_title <-  key_word %>%
        str_to_title() %>%
        .[order(.)] %>%
        str_c(collapse = ' | ') %>%
        str_wrap(60)
      
      key_word_cols <- Spectral_n(length(key_word))
      names(key_word_cols) <- key_word
      
      
      
      
      CSA_unnest <- CSA_filter %>%
        activate(edges) %>%
        as_tibble() %>%
        select(from, text) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        filter(!word %in% other_stop,
               word %in% key_word)
      
      found_words <- CSA_unnest %>%
        pull(word) %>%
        unique()
      
      words_to_add <- key_word[which(!key_word %in%
                                       found_words)]
      
      words_to_add_tibble <- words_to_add %>%
        enframe(name = NULL) %>%
        mutate(n = 1) %>%
        set_names('word', 'n')
      
      
      
      CSA_unnest %>%
        count(word) %>%
        bind_rows(words_to_add_tibble) %>%
        mutate(word = fct_rev(word)) %>%
        ggplot(aes(word, n)) +
        geom_col(aes(fill = word,
                     color = word),
                 show.legend = F) +
        scale_color_manual(
          values =
            adjustcolor(key_word_cols, 1),
          limits = key_word,
          breaks = key_word,
          drop = F
        ) +
        scale_fill_manual(
          values =
            adjustcolor(key_word_cols, .85),
          limits = key_word,
          breaks = key_word,
          drop = F
        ) +
        scale_y_log10() +
        coord_flip() +
        theme_bw() +
        labs(
          subtitle = caption_text,
          caption = paste0('Key words = {',
                           key_word_title,
                           '}'),
          x = 'Key words',
          y = 'Mentions per week'
        )
      
      
      
      
    } else {
      # database
      g <-  CSA_weekly %>%
        filter(week_n == input$week_n) %>% #
        graph_from_data_frame()
      
      
      
      g_sub <- neighbor_subgraph(g,
                                 input$neighbor_size, #
                                 input$stakeholder)#
      
      
      
      
      
      
      
      g_sub %>%
        as_tbl_graph() %>%
        ggraph() +
        geom_edge_arc(
          arrow = gg_arrow(1.25),
          start_cap = gg_gap(10),
          end_cap = gg_gap(10),
          strength = 0.05,
          color = Greys[3],
          show.legend = F
        ) +
        geom_node_text(aes(label = name,
                           color = name),
                       show.legend = F) +
        scale_color_manual(values = name_colors) +
        theme_graph(background = 'black', 
                    caption_colour = 'white', 
                    foreground = 'white', 
                    border = T)
    }
    
    
  })
  
  
}


shinyApp(ui, server)
