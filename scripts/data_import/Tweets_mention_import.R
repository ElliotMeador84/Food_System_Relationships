library(tidyverse)
library(rtweet)
library(lubridate)
source('~/all_functions.R')
#
flatten <- purrr::flatten
#
# Twitter log in info -----------------------------------------------------

op_is_window <- Sys.info()[1] == "Windows"

if(op_is_window == T){
    source("C:/R/Source_files/Food_System_Relationships/keys/twitter_key.R")
} else {
    'Need to set up other token'
}


# CSA List  ---------------------------------------------------------

#
# # CSA List  ---------------------------------------------------------

load('C:/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/profile_ls.RData')
#
# # Import stakeholder list -------------------------------------------------

tweets_timeline_df <- map_df(profile_ls,
                             possibly(function(x) {
                                 search_tweets(x, n = 5,
                                               retryonratelimit = T) %>%
                                     mutate(
                                         date = date(created_at),
                                         current_time = Sys.Date(),
                                         diff_time = date - current_time
                                     ) %>%
                                     filter(diff_time >= -3)
                             }, NULL))






first_mentions <- tweets_timeline_df %>%
    unnest(mentions_screen_name) %>%
    mutate(mentions_screen_name = str_c('@', mentions_screen_name)) %>%
    pull(mentions_screen_name) %>%
    .[!is.na(.)] %>%
    .[!. %in% unlist(profile_ls)] %>%
    unique()




first_mentions_df <- map_df(first_mentions, possibly(function(x) {
    search_tweets(x,
                  n = 5,
                  retryonratelimit = T) %>%
        mutate(
            date = date(created_at),
            current_time = Sys.Date(),
            diff_time = date - current_time
        ) %>%
        filter(diff_time >= -3)
}, NULL))



# Merge -------------------------------------------------------------------

Tweets_mention_new <- bind_rows(
    tweets_timeline_df %>%
        mutate_if(is.list, simplify_all) %>%
        as_tibble()  %>%
        mutate_if(is.list, as.character)
    
    ,
    
    first_mentions_df %>%
        mutate_if(is.list, simplify_all) %>%
        as_tibble()  %>%
        mutate_if(is.list, as.character)
    
) %>%
    distinct()



load('C:/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/Tweets_mention.RData')

Tweets_mention <- 
    bind_rows(Tweets_mention,
              Tweets_mention_new) %>%
    distinct()



save(Tweets_mention,
     file = '~/R/Transforming_Food_Systems/data/import/Tweets_mention.RData')


rm(list = ls())












