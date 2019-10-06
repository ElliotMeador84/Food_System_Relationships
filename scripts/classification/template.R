library(tidyverse)
library(openxlsx)
library(glue)


source('/Users/johne.meador/OneDrive - SRUC/all_functions.R')
setwd('/Users/johne.meador/OneDrive - SRUC/Food_System_Relationships/data/')

load('CSA_tweets.RData')
load('users_unique_df.RData')


random_string <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

# Create random ID's

set.seed(123)
alfa <- CSA_tweets %>% 
  select(screen_name, description) %>% 
  distinct() %>% 
  mutate(match_id = random_string(nrow(.))) %>% 
  select(match_id, screen_name, description) 


## Add new empty columns
add_columns <- function(df, columns){
  new <- rep(NA_character_, length(columns))
  names(new) <- columns
  mutate(df, !!! new)
}


key_terms <- c('Farmer', 
               'Policy', 
               'Academic', 
               'Consumer', 
               'Retailer',
               'Restaurant', 
               'Food Advocate',
               'SME (small-to-medium enterprise)',
               'Agricultural input supply')

new_varies <- glue('{key_terms} rank - 0 Not likely to 10 very likely')


charlie <- add_columns(tango, new_varies) %>% 
  sample_n(10)


        ##############################
        ### Create/Modify Workbook ###
        ##############################


wb <- createWorkbook()


    ################
    ## Intro page ##
    ################


# Style
hs1 <- createStyle(fgFill = "#DCE6F1", 
                   halign = "CENTER", 
                   textDecoration = "italic",
                   border = "Bottom")

addWorksheet(wb, 
             sheetName = 'Project Overview', 
             gridLines = F)


writeData(wb, 
          'Project Overview',
          x = 'This is a text and nothing more.', 
          startCol = 2, 
          startRow = 4)


saveWorkbook(wb, file = "tweet_classification/development/tgrains_twitter_classification.xlsx", overwrite = TRUE)




setwd('/Users/johne.meador/Documents/R/Food_System_Relationships/')








