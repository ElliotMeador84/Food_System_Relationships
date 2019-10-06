library(tidyverse)
library(janitor)
library(scales)
library(glue)
library(facetscales)

setwd('/Users/johne.meador/OneDrive - SRUC/')
list.files('Data/world_bank/world_bank_internet/')


data_files <- list('Data/world_bank/world_bank_internet/API_IT.NET.USER.ZS_DS2_en_csv_v2_248921.csv', 
     'Data/world_bank/world_bank_cell_phone/API_IT.CEL.SETS_DS2_en_csv_v2_250267.csv', 
     'Data/world_bank/world_bank_gdp/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_247793.csv',
     'Data/world_bank/world_bank_female_agriculture/API_SL.AGR.EMPL.FE.ZS_DS2_en_csv_v2_249101.csv', 
     'Data/world_bank/world_bank_rural/API_SP.RUR.TOTL.ZS_DS2_en_csv_v2_250784.csv')


all_countries <- 
  map_df(data_files, function(x){
  read_csv(x, skip = 3) %>% 
  clean_names() %>% 
  gather(year, value, -country_name, -country_code, -indicator_name, -indicator_code) %>% 
  mutate(year = parse_number(year))
  }) %>% 
  mutate(cameroon = case_when(
    str_detect(country_name, 'Cameroon')~'Cameroon', 
    T ~ 'Other'
  )) %>% 
  drop_na(value) %>% 
  filter(between(year, 1990, 2017)) %>% 
  mutate(cameroon = fct_rev(cameroon))

?facet_grid_sc()
  
cameroon_plot <- all_countries %>% 
  filter(str_detect(country_name, 'Cameroon'), 
         !str_detect(indicator_name, 'Rural')) %>% 
  ggplot(aes(year, value))+
  geom_line(color = 'black',
            size = 1, 
            show.legend = F)+
  scale_color_manual(values = c(
    adjustcolor('grey', .25), 
    'black'
  ), name = 'Country', 
  labels = c('All other', 'Cameroon'))+
  scale_x_continuous(limits = c(1990, 2021))+
  theme_minimal()+
  theme(plot.margin = margin(1,1,1,1, 'cm'), 
        legend.position = c(.85, .25))+
  facet_wrap(~indicator_name, scales = 'free',
             labeller = label_wrap_gen(width = 50, multi_line = TRUE))+
  labs(title = 'Cameroon - World Bank Indicators', 
       caption = 'Data are from https://data.worldbank.org/indicator', 
       x = 'Year',
       y = NULL)
  
ggsave(cameroon_plot, 
 width = 11, 
 height = 8, 
 filename = 'Research_proposals/DIDA/png/world_bank.png')  
  
  
  
  
  
  
  
  
  
  
  
  

