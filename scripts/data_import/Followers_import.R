library(tidyverse)
library(rtweet)
library(lubridate)
source('~/all_functions.R')

flatten <- purrr::flatten

op_is_window <- Sys.info()[1] == "Windows"

if(op_is_window == T){
source('C:/R/all_functions.R')
}
# Twitter log in info -----------------------------------------------------

op_is_window <- Sys.info()[1] == "Windows"

if(op_is_window == T){
    source("C:/R/Source_files/Food_System_Relationships/keys/twitter_key.R")
} else {
    'Need to set up other token'
}




# Get new friends ---------------------------------------------------------

load('C:/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/CSA_tweets.RData')




users <- CSA_tweets %>% 
    select(mentions_screen_name) %>% 
    separate_rows(mentions_screen_name, sep = '\\\"') %>% 
    mutate(mentions_screen_name = str_trim(mentions_screen_name), 
           length = str_length(mentions_screen_name)) %>% 
    filter(!length <= 2) %>% distinct(mentions_screen_name) %>% 
    pull(mentions_screen_name)


users_unique <- users[!users %in% CSA_tweets$screen_name] %>% 
    unique()


users_unique_df_new <- lookup_users(users_unique)


load('C:/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/users_unique_df.RData') 



# Merge with old ----------------------------------------------------------


CSA_unique_users <- 
    bind_rows(users_unique_df,
              users_unique_df_new) %>%
    distinct()


save(CSA_unique_users,
     file = 'C:/Users/emeador/OneDrive - SRUC/Food_System_Relationships/data/users_unique_df.RData')

rm(list = ls())






