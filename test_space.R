#add libraries -don't use pacman - it won't work when published to shinyapps
library(shiny)
library(tidyverse)
library(glue)
library(readxl)
library(janitor)
library(plotly)
library(scales)

#disable scientific notation
options(scipen = 999)

#data
##read in data - this is reading from an extract of 19/259311
data_raw <- read_excel("mv2040_outcomes_framework/MV2040 Indicators and Outcomes DRAFT baseline- August 2019.XLSX", sheet = "data") %>% 
  clean_names() 

indicator_list <- read_excel("mv2040_outcomes_framework/MV2040 Indicators and Outcomes DRAFT baseline- August 2019.XLSX", sheet = "indicator_list") %>% 
  clean_names() %>% 
  mutate(theme = str_to_title(theme))

indicators_to_goals <- read_excel("mv2040_outcomes_framework/MV2040 Indicators and Outcomes DRAFT baseline- August 2019.XLSX", sheet = "inds_goals") %>% 
  clean_names() %>% 
  mutate(theme = str_to_title(theme))

goals <- read_excel("mv2040_outcomes_framework/MV2040 Indicators and Outcomes DRAFT baseline- August 2019.XLSX", sheet = "goals") %>% 
  clean_names()

data_format <- read_excel("mv2040_outcomes_framework/MV2040 Indicators and Outcomes DRAFT baseline- August 2019.XLSX", sheet = "data_format") %>% 
  clean_names()