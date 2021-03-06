#### Preamble ####
# Purpose: The purpose of this code is to clean-up the 2017 GSS data obtained 
# from the U of T library. That data is available to U of T students, but it needs 
# to be put into a tidy format before it can be analysed. This code does that.
# The main issue is that the data are released with codes for variables, whereas,
# we want the variable. e.g. sex is 1 or 2, but we want sex is female or male. (This
# sounds trite in that case, but gets more difficult with more involved variables.)
# So we create a dictionary type dataset that has the variable names and their 
# possible values. In that we embed some R code that will do a replacement. We 
# then apply that dataset to the raw dataset. Finally we do all the usual cleaning.
# to the dataset. You will end up with a dataset called gss.csv.
# Authors: Rohan Alexander and Sam Caetano
# Contact: rohan.alexander@utoronto.ca
# Date: 7 October 2020
# License: MIT
# Pre-reqs: You need to have downloaded the data from the library. To do that: 
## 1. Go to: http://www.chass.utoronto.ca/
## 2. Data centre --> UofT users or http://dc.chass.utoronto.ca/myaccess.html
## 3. Click SDA @ CHASS, should redirect to sign in. Sign in.
## 4. Continue in English (you're welcome to use the French, but we probably can't
## help you too much).
## 5. Crtl F GSS, click
## 6. Click "Data" on the one you want. We used 2017, but you may want a different 
## wave. In particular the General Social Survey on social identity (cycle 27), 
## 2013 has some variables on voter participation if you're into that sort of 
## thing. You're welcome to pick any year but this code applies to 2017.
## 7. Click download
## 8. Select CSV data file, data definitions for STATA (gross, but stick with it for now).
## 9. Can select all variables by clicking button next to green colored "All". Then continue.
## 10. Create the files, download and save
# Check: 
## You WILL need to change the raw data name. Search for .csv - line 41
## You may need to adjust the filepaths depending on your system. Search for: read_


#### Workspace set-up ####
library(janitor)
library(tidyverse)

setwd("/Users/labibchowdhury/coursework/STA304/sta304-ps2/")
# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("AAoTMnOv.csv")
dict <- read_lines("gss_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("gss_labels.txt")


#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))

# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
gss <- raw_data %>% 
  select(slm_01,
         vismin,
         uhw_16gr,
         famincg2,
         dwelc,
         ehg3_01b,
         srh_110,
         srh_115,
         agec)


# Calculate % of data filtered
num_before_filter_slm_01 <- gss %>% count()
num_after_filter_slm_01 <- gss %>%filter(gss$slm_01 <= 10) %>% count()
p_slm_01 <- num_after_filter_slm_01  / num_before_filter_slm_01

num_before_filter_vismin <- gss %>% count()
num_after_filter_vismin <- gss %>%filter(gss$vismin <= 2) %>% count()
p_vismin_01 <- num_after_filter_vismin / num_before_filter_vismin



# Removes observations with fields we're not interested in
gss <- gss %>%
  filter(slm_01 <= 10) %>%
  filter(vismin <= 2) %>%
  filter(uhw_16gr <= 5) %>%
  filter(famincg2 <= 6) %>%
  filter(dwelc <= 4) %>%
  filter(ehg3_01b <= 6) %>%
  filter(srh_110 <= 5) %>%
  filter(srh_115 <= 5) %>%
  filter(agec <= 80)



gss <- gss %>%
  mutate_at(.vars = vars(vismin:srh_115),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

# Fix the names
gss <- gss %>% 
  clean_names() %>% 
  rename(feelings_life = slm_01,
         education = ehg3_01b,
         hours_worked = uhw_16gr,
         family_income = famincg2,
         vis_minority = vismin,
         hh_type = dwelc,
         age = agec,
         self_rated_health = srh_110,
         self_rated_mental_health = srh_115)


# Find mean of feelings_life col
mean_feelings_life <- round(mean(gss$feelings_life), 0)

# Create a binary variable "feeling_life_binary" for feelings_life
gss <- gss %>% 
  mutate(feelings_life_binary = ifelse(feelings_life >= mean_feelings_life, 1, 0))
write_csv(gss, "gss.csv")