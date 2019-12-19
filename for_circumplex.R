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


# progress towards the targets
towards_target <- data_raw %>% 
  filter(type != "prior")  %>% 
  select(id, year, value, type) %>% 
  arrange(desc(year)) %>% 
  group_by(id, type)  %>% 
  slice(1) %>% #takes baseline and latest progress figure
  select(-year) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  left_join(indicator_list, by = "id") %>% 
  select(id, baseline, progress, target, desired, theme) %>% 
  mutate(toward_pct = (progress - baseline)/(target - baseline) * 100) %>% # progress as a percentage towards the target from baseline
  mutate(theme = factor(theme, levels = c("Fair", "Thriving", "Connected", "Green", "Beautiful"))) %>% 
  left_join(indicator_list, by = "id") %>% 
  select(id, baseline, progress, target, theme = theme.x, desired = desired.x, toward_pct, measure, category) %>% 
  mutate(toward_pct = (ifelse(is.na(toward_pct), 0, toward_pct))) %>% 
  mutate(toward_pct = round(toward_pct, 1))

# order for circ data
cat_order <- towards_target %>% 
  arrange(theme) %>%
  ungroup() %>% 
  distinct(category) %>% 
  pull()

#circ_data  
circ_data1 <- towards_target %>%
  ungroup() %>% 
  select(theme, category, toward_pct) %>% 
  group_by(theme, category) %>% 
  mutate(mean_toward_pct = mean(toward_pct)) %>% 
  distinct(category, mean_toward_pct) %>% 
  mutate(text = str_replace_all(category, " ", "\n")) %>% 
  ungroup() %>% 
  mutate(category = factor(category, levels = cat_order)) %>% 
  arrange(category)

# text data order
circ_data2 <- circ_data1 %>% 
  select(text) %>% 
  pull()

# with text factor
circ_data3 <- circ_data1 %>% 
  mutate(text = factor(text, levels = circ_data2))

# non-interactive circumplex grouped by category
ggplot(circ_data3, aes(text, mean_toward_pct, fill = theme)) +
  geom_col(width = 1, col = "white") +
  coord_polar() + theme_minimal() +
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) + ylim(-50, 100) + 
  scale_fill_manual(values = c("Fair" = "#E55048", "Thriving" = "#31788F", "Connected" = "#6A4479", "Green" = "#4EA546", "Beautiful" = "#E3A51E"), drop = FALSE) +
  labs(title = "Progress towards targets by category and theme", x = NULL, y = "Progress (%)", fill = "Theme") +
  theme(legend.position = c(0.92, 0.92)) + #legend position
  theme(axis.text = element_text(face = "bold", size = 12)) + # xlab titles
  guides(fill = guide_legend(title.theme = element_text(face = "bold", size = 11))) + #legend title
  theme(legend.text = element_text(size = 10, face = "bold")) + #legend labels 
  theme(plot.title = element_text(size = 14, face = "bold")) #graph title

# other version of the circumplex - only the percentages
circ_inds1 <- towards_target %>%
  ungroup() %>% 
  filter(desired == "Increase") %>% 
  left_join(data_format, by = "id") %>% 
  filter(value_format == "Percentage") %>% 
  select(theme, measure, baseline, progress, target) %>% 
  mutate(current = case_when(!is.na(progress) ~ progress,
                             TRUE ~ baseline)) %>% 
  select(-baseline, -progress) %>% 
  arrange(theme) %>% 
  mutate(text = str_replace_all(measure, " ", "\n"))

# order
circ_inds2 <- circ_inds1 %>% 
  select(text) %>% 
  pull()

#reordered
circ_inds3 <- circ_inds1 %>% 
  mutate(text = factor(text, levels = circ_inds2))

# non-interactive circumplex - selected measures only
ggplot(circ_inds3, aes(text, current, fill = theme)) +
  geom_col(width = 1, col = "white", alpha = 0.5) + ylim(0, 100) + coord_polar() + #current
  geom_col(aes(text, target), alpha = 0.5, width = 1, col = "white") + ylim(0, 100) + coord_polar() + # target
  theme_minimal() +
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
  scale_fill_manual(values = c("Fair" = "#E55048", "Thriving" = "#31788F", "Connected" = "#6A4479", "Green" = "#4EA546", "Beautiful" = "#E3A51E"), drop = FALSE) +
  labs(title = "Progress towards targets", subtitle = "Selected measures only", x = NULL, y = "Percentage", fill = "Theme", caption = "The darker shading indicates the current state, the lighter shading indicates the target for 2040.") +
  theme(legend.position = c(0.92, 0.9)) + #legend position
  theme(axis.text = element_text(face = "bold", size = 10)) + # xlab titles
  guides(fill = guide_legend(title.theme = element_text(face = "bold", size = 11))) + #legend title
  theme(legend.text = element_text(size = 10, face = "bold")) + #legend labels 
  theme(plot.title = element_text(size = 14, face = "bold")) #graph title
