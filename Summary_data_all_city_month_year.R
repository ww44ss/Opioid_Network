
## Winston Saunders 20 Feb 2020
##
## Opiod data cruncher
## This script reads the parsed WaPo data and condenses fields to monthly summaries by state, city, reporter, and buyer
## 
## it takes about an hour to run on the i7 processor
##
## Rev_1: changed file directory pointers
## REv_2: Adpated for network. 


library(tidyverse)
library(lubridate)
library(gridExtra)
library(R.utils)
yo <- function(x){x}




## Opiod Backbone


## get list of files

read_directory <- "C:/Users/wisaund/OneDrive - Microsoft/Documents/2019 Data Science/Washington_Post_Opiod_Data/Split_Data/"
write_directory <- "C:/Users/wisaund/OneDrive - Microsoft/Documents/2019 Data Science/Washington_Post_Opiod_Data/Networks/"

## get files in directory

all_files <- list.files(read_directory)

cat(all_files, "\r")

## subset

raw_data_files <- all_files %>%
  str_subset("[0-9]{1,2}_testchunk.csv") %>%
  yo

#cat(raw_data_files, "\r")

## test shot for debug

# col_type_spec <- "dccccccccccccccccccccccccdccccccdddcccdcccc"
# 
# full_filename <- str_c(directory, raw_data_files[1])
# 
# raw_data <- read_csv(full_filename, col_types = col_type_spec, na = c("", "NA", "null"))
# 
# raw_data <- raw_data %>% select(-X1)
# 
# raw_data <- raw_data %>%
#     mutate(YEAR = str_extract(TRANSACTION_DATE, "[0-9]{4}$")) %>%
#     yo
#   



## reduce raw data to state level


state_to_state <- function(raw_data, summary_data) {
  
  intermediate_data <- raw_data %>%
    select(REPORTER_STATE, REPORTER_NAME, REPORTER_CITY,BUYER_STATE, BUYER_NAME, BUYER_CITY,CALC_BASE_WT_IN_GM, YEAR) %>%
    group_by(REPORTER_STATE, REPORTER_NAME, REPORTER_CITY,BUYER_STATE, BUYER_NAME, BUYER_CITY, YEAR) %>%
    summarize(TOTAL_WT_IN_GM = sum(CALC_BASE_WT_IN_GM)) %>%
    ungroup() %>%
    arrange(desc(TOTAL_WT_IN_GM)) %>%
    yo
  
  
  if(!is.null (summary_data) )
  {summary_data <- summary_data %>%
    bind_rows(intermediate_data) %>%
    group_by(REPORTER_STATE, REPORTER_NAME, REPORTER_CITY,BUYER_STATE, BUYER_NAME, BUYER_CITY, YEAR) %>%
    summarize(TOTAL_WT_IN_GM = sum(TOTAL_WT_IN_GM)) %>%
    ungroup() %>%
    arrange(desc(TOTAL_WT_IN_GM)) %>%
    yo  
  
  }
  else
  {
    
    summary_data <- intermediate_data
  
    }
  
  
}


summary_data <- NULL

n_samp <- length(raw_data_files)  ## modify depending on sample (1 to about 90)
n_avail <- length(raw_data_files)

sample_ratio <- n_samp/n_avail

cat("n_samp = ", n_samp, "     n_avail = ", n_avail, "\r")

## set up random read order

set.seed(867*5309)

samp <- sample(length(raw_data_files), n_samp, replace = FALSE, prob = NULL)

cat(samp, "\r")

## this is just for display
count_thru = 0

for (data_file in raw_data_files[samp]) {
  
  col_type_spec <- "dccccccccccccccccccccccccdccccccdddcccdcccc"
  full_filename <- str_c(read_directory, data_file)
  
  count_thru <- count_thru + 1
  cat("n = ", count_thru, "   file = ", full_filename, "\r")
  
  raw_data <- read_csv(full_filename, col_types = col_type_spec, na = c("", "NA", "null"))
  
  ## clean and process raw data
  
  raw_data <- raw_data %>% select(-X1)
  
  raw_data <- raw_data %>%
    mutate(YEAR = str_extract(TRANSACTION_DATE, "[0-9]{4}$")) %>%
    #mutate(MONTH = str_replace(TRANSACTION_DATE, "[0-9]{6}$", "")) %>%
    yo
  
  #pass to summary analysis
  summary_data <- state_to_state(raw_data, summary_data)
  
}



## save data
write_csv(summary_data, str_c(write_directory, "summary_data_all_city_month_year_name.csv"))


## test plot of the data 

plot_data<-summary_data %>%
  group_by(BUYER_STATE) %>%
  summarize(TOTAL_WT_IN_GM = sum(TOTAL_WT_IN_GM)) %>%
  ungroup() %>%
  yo


ggplot(plot_data, (aes(x = BUYER_STATE, y = TOTAL_WT_IN_GM/1000000))) + geom_col(color = "grey90", width = .4, fill = "tomato") + 
  #theme_grey()  + 
  scale_x_discrete(expand = c(0, 0)) +
  theme(axis.text.x = element_text(colour = "black", size = 10, angle = 90, hjust = 1, vjust = 0.5), 
        axis.text.y = element_text(colour = "black", size = 12))+
  labs(title = "Opioids Bought", x = "State", y = "Weight in tonnes")


