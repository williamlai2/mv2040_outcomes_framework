#add libraries -don't use pacman - it won't work when published to shinyapps
library(shiny)
library(tidyverse)
library(glue)
library(readxl)
library(janitor)
library(plotly)
library(scales)
library(shinydashboard)
library(shinycssloaders)
library(apputils)

#disable scientific notation
options(scipen = 999)

#data
##read in data - this is reading from an extract of 19/259311
data_raw <- read_excel("MV2040 Indicators and Outcomes DRAFT baseline- August 2019.XLSX", sheet = "data") %>% 
  clean_names() %>% 
  mutate(err_low = round(value - lower, 1), err_high = round(upper - value, 1))

indicator_list <- read_excel("MV2040 Indicators and Outcomes DRAFT baseline- August 2019.XLSX", sheet = "indicator_list") %>% 
  clean_names() %>% 
  mutate(theme = str_to_title(theme))

indicators_to_goals <- read_excel("MV2040 Indicators and Outcomes DRAFT baseline- August 2019.XLSX", sheet = "inds_goals") %>% 
  clean_names() %>% 
  mutate(theme = str_to_title(theme))

goals <- read_excel("MV2040 Indicators and Outcomes DRAFT baseline- August 2019.XLSX", sheet = "goals") %>% 
  clean_names()

data_format <- read_excel("MV2040 Indicators and Outcomes DRAFT baseline- August 2019.XLSX", sheet = "data_format") %>% 
  clean_names()

#change progress data
change_progress_data_full <- data_raw %>% 
  filter(type %in% c("baseline", "progress")) %>% 
  select(id, year, value, type) %>% 
  arrange(desc(year)) %>% 
  group_by(id, type) %>% 
  slice(1) %>% #takes baseline and latest progress figure
  select(-year) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  left_join(indicator_list, by = "id") %>% 
  select(id, baseline, progress, desired) %>% 
  ungroup() %>% 
  mutate(change = case_when(desired == "Increase" & progress > baseline ~ "Good",
                            desired == "Increase" & progress < baseline ~ "Bad",
                            desired == "Decrease" & progress < baseline ~ "Good",
                            desired == "Decrease" & progress > baseline ~ "Bad",
                            TRUE ~ "N/A")) 

progress_by_theme <- change_progress_data_full %>% 
  left_join(indicator_list, by = "id") %>% 
  select(id, baseline, progress, desired = desired.x, change, theme) %>% 
  mutate(theme = factor(theme, levels = c("Fair", "Thriving", "Connected", "Green", "Beautiful"))) %>% 
  mutate(change = factor(change, levels = c("Good", "N/A", "Bad"))) %>% 
  group_by(theme, .drop=FALSE) %>% 
  count(change) %>% 
  ungroup() 

theme_pct <- progress_by_theme %>% 
  pivot_wider(names_from = "change", values_from = "n") %>% 
  clean_names() %>% 
  mutate(total = good + n_a + bad) %>% 
  mutate(pct_good = good/total, pct_na = n_a/total, pct_bad = bad/total)

## colours for themes
colour_table <- tibble(theme = c("Fair", "Thriving", "Connected", "Green", "Beautiful"),
                       col_code = c("#E55048", "#31788F", "#6A4479", "#4EA546", "#E3A51E"))

# the theme list
theme_list <- indicator_list %>% 
  distinct(theme) %>% 
  pull()

# the measure list
measure_list <- indicator_list %>% 
  select(measure) %>% 
  pull()


###############################

#test
progress_by_theme <- change_progress_data_full %>% 
  left_join(indicator_list, by = "id") %>% 
  select(id, baseline, progress, desired = desired.x, change, theme) %>% 
  mutate(theme = factor(theme, levels = c("Fair", "Thriving", "Connected", "Green", "Beautiful"))) %>% 
  mutate(change = factor(change, levels = c("Good", "N/A", "Bad"))) %>% 
  group_by(theme, .drop=FALSE) %>% 
  count(change) %>% 
  ungroup() 

theme_pct <- progress_by_theme %>% 
  pivot_wider(names_from = "change", values_from = "n") %>% 
  clean_names() %>% 
  mutate(total = good + n_a + bad) %>% 
  mutate(pct_good = good/total, pct_na = n_a/total, pct_bad = bad/total)

percent(theme_pct_good %>% filter(theme == "Fair") %>% select(pct_good) %>% pull(), accuracy = 1L)
progress_by_theme %>% filter(change == "Good") %>% select(n) %>% pull()

data_raw %>% 
  filter(type %in% c("baseline", "progress")) %>% 
  select(id, year, value, type) %>% 
  arrange(desc(year)) %>% 
  group_by(id, type) %>% 
  slice(1) %>% #takes baseline and latest progress figure
  select(-year) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  left_join(indicator_list, by = "id") %>% 
  select(id, baseline, progress, desired) %>% 
  ungroup() %>% 
  mutate(change = case_when(desired == "Increase" & progress > baseline ~ "Good",
                            desired == "Increase" & progress < baseline ~ "Bad",
                            desired == "Decrease" & progress < baseline ~ "Good",
                            desired == "Decrease" & progress > baseline ~ "Bad",
                            TRUE ~ "N/A")) 
