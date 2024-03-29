library(tidyverse)
library(rtweet)
library(lubridate)

op_is_window <- Sys.info()[1] == "Windows"

source('/Users/emeador/OneDrive - SRUC/all_functions.R')


#
flatten <- purrr::flatten
#
# Twitter log in info ---------


if (op_is_window == T) {
    source("C:/R/Source_files/Food_System_Relationships/keys/twitter_key.R")
}



# CSA List  -----------

load('/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/profile_ls.RData')


# # Import stakeholder list ----------

tweets_timeline_df <- map_df(profile_ls,
            possibly(function(x) {
            search_tweets(x, n = 15,
            retryonratelimit = T) %>%
            mutate(
                    date = date(created_at),
                    current_time = Sys.Date(),
                    diff_time = date - current_time
                    ) %>%
                    filter(diff_time >= -5)
                    }, NULL))






first_mentions <- tweets_timeline_df %>%
    unnest(mentions_screen_name) %>%
    mutate(mentions_screen_name = 
               str_c('@', mentions_screen_name)) %>%
    pull(mentions_screen_name) %>%
    .[!is.na(.)] %>%
    .[!. %in% unlist(profile_ls)] %>%
    unique()





first_mentions_df <- map_df(first_mentions, possibly(function(x) {
    search_tweets(x,
                  n = 15,
                  retryonratelimit = T) %>%
        mutate(
            date = date(created_at),
            current_time = Sys.Date(),
            diff_time = date - current_time
        ) %>%
        filter(diff_time >= -5)
}, NULL))



# Merge ----------

## run from here once rate limits are done

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





load('/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/Tweets_mention.RData')







Tweets_mention <-
    bind_rows(Tweets_mention,
              Tweets_mention_new) %>%
    distinct()


save(Tweets_mention, file = '/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/Tweets_mention.RData')







rm(list = ls())
