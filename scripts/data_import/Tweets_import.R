library(tidyverse)
library(rtweet)
library(lubridate)

message('  Recieved this error when the token was created for other projects --\n\nError in file(file, mode) : cannot open the connection
In addition: Warning message:
In file(file, mode) :\n\n  Need to consider whether or not it is neccesary to authenticate each time.')


flatten <- purrr::flatten

# Twitter log in info ---------

op_is_window <- Sys.info()[1] == "Windows"

if(op_is_window == T){
    source("C:/R/Source_files/Food_System_Relationships/keys/twitter_key.R")
} 
# else {
#     source("~/Documents/R/twitter_key.R")
# }



if(op_is_window == T){
    source('C:/R/all_functions.R')
} else {
    source('/Users/emeador/OneDrive - SRUC/all_functions.R')
}


# CSA List  -----------



if(op_is_window == T){
    load('/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/profile_ls.RData')
} else {
    load('/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/profile_ls.RData')
}



# Import stakeholder list -------------



profile_timeline_df <- map_df(profile_ls, possibly(function(x) {
    get_timeline(x, n = 10) %>%
        mutate(
            date = date(created_at),
            current_time = Sys.Date(),
            diff_time = date - current_time
        ) %>%
        filter(diff_time >= -3)
}, NULL))

# Pull Mentions --------------





first_mentions <- profile_timeline_df %>%
    select(mentions_screen_name) %>% 
    unnest(mentions_screen_name) %>%
    mutate(mentions_screen_name = str_c('@', mentions_screen_name)) %>%
    pull(mentions_screen_name) %>%
    .[!is.na(.)] %>%
    .[!. %in% unlist(profile_ls)]





first_mentions_df <- map_df(first_mentions, possibly(function(x) {
    get_timeline(x, n = 10) %>%
        mutate(
            date = date(created_at),
            current_time = Sys.Date(),
            diff_time = date - current_time
        ) %>%
        filter(diff_time >= -3)
}, NULL))


# Merge -----------------

CSA_tweets_update <- bind_rows(
    profile_timeline_df %>%
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


# Merge with existing -----------

if(op_is_window == T){
    load('/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/CSA_tweets.RData')
} else {
    load('/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/CSA_tweets.RData')
}



CSA_tweets <-
    bind_rows(CSA_tweets,
              CSA_tweets_update) %>%
    distinct()


if(op_is_window == T){
    
save(CSA_tweets,
     file = '/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/CSA_tweets.RData')
} else {
save(CSA_tweets,
     file = '/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/CSA_tweets.RData')
}

rm(list = ls())

