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


toward_target <- data_raw %>% 
  filter(type != "prior")  %>% 
  select(id, year, value, type) %>% 
  arrange(desc(year)) %>% 
  group_by(id, type)  %>% 
  slice(1) %>% #takes baseline and latest progress figure
  select(-year) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  left_join(indicator_list, by = "id") %>% 
  select(id, baseline, progress, target, desired, theme) %>% 
  mutate(toward = (progress - baseline)/(target - baseline) * 100) %>% # progress as a percentage towards the target from baseline
  mutate(theme = factor(theme, levels = c("Fair", "Thriving", "Connected", "Green", "Beautiful")))  

ggplot(toward_target, aes(id, toward, fill = theme)) +
  geom_col(width = 1, col = "white") +
  coord_polar() +
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) + ylim(-20, 100) + 
  scale_fill_manual(values = c("Fair" = "#E55048", "Thriving" = "#31788F", "Connected" = "#6A4479", "Green" = "#4EA546", "Beautiful" = "#E3A51E"), drop = FALSE) +
  labs(title = "Progress towards targets", x = NULL, y = "Progress (%)", fill = "Theme") +
  theme(legend.position = c(0.92, 0.9)) + #legend position
  theme(axis.text = element_text(face = "bold", size = 12)) + # xlab titles
  guides(fill = guide_legend(title.theme = element_text(face = "bold", size = 11))) + #legend title
  theme(legend.text = element_text(size = 10, face = "bold")) + #legend labels 
  theme(plot.title = element_text(size = 14, face = "bold")) #graph title
