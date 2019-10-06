library(tidyverse)
library(igraph)
library(igraph)
library(ggraph)
library(tidygraph)
library(graphlayouts)

source('/Users/johne.meador/OneDrive - SRUC/all_functions.R')


setwd('/Users/johne.meador/OneDrive - SRUC/Food_System_Relationships/')

load('data/CSA_tweets.RData')

df_tweets <- CSA_tweets %>%
  add_dates() %>%
  seperate_mentions() %>%
  filter(year >= 2019) %>%
  select(screen_name, mentions_screen_name)

g <- graph_from_data_frame(df_tweets)

tibble(name = V(g)$name,
       core = coreness(g, 'all')) %>%
  ggplot(aes(log(core))) +
  geom_density()

V(g)$core <- coreness(g, 'all')


layout_stress <- layout_with_stress(g) %>%
  as_tibble() %>%
  set_names('x', 'y')

ggraph(g, layout = layout_stress) +
  geom_edge_fan(aes(alpha = ..index..),
                show.legend = F) +
  geom_node_point(aes(size = core,
                      color = factor(core))) +
  scale_color_manual(values = sample(Spectral_n(vcount(g))),
                     guide = F)


sug_graph <- induced_subgraph(graph = g,
                              vids = which(degree(g) > 1))



sug_graph_layout <- layout_with_stress(sug_graph) %>%
  as_tibble() %>%
  set_names('x', 'y')

vcount(sug_graph)

ggraph(sug_graph,
       layout = sug_graph_layout) +
  geom_edge_fan(aes(alpha = ..index..),
                show.legend = F) +
  geom_node_point(aes(size = core,
                      color = core)) +
  scale_color_gradient2(low = 'tomato',
                        midpoint = 35,
                        high = 'navyblue') +
  scale_size(range = c(1, 5))
