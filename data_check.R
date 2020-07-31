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

progress_by_influence <- change_progress_data_full %>% 
  left_join(indicator_list, by = "id") %>% 
  select(id, baseline, progress, desired = desired.x, change, influence) %>% 
  mutate(influece = factor(influence, levels = c("Lead", "Advocate", "Contribute"))) %>% 
  mutate(change = factor(change, levels = c("Good", "N/A", "Bad"))) %>% 
  group_by(influence, .drop=FALSE) %>% 
  count(change) %>% 
  ungroup() 

influence_pct <- progress_by_influence %>% 
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
  mutate(toward_pct = round(toward_pct, 1)) %>% 
  left_join(data_format, by = "id")

towards_fair <- towards_target %>% filter(theme == "Fair")
towards_thriving <- towards_target %>% filter(theme == "Thriving")
towards_connected <- towards_target %>% filter(theme == "Connected")
towards_green <- towards_target %>% filter(theme == "Green")
towards_beautiful <- towards_target %>% filter(theme == "Beautiful")

# text for box in individual themes
ind_theme_text <- tags$body(HTML("<b>Notes:</b></br>",
                                 "Progress towards the target is calculated as <b>(the progress value - the baseline value) divided by (the target value - the baseline value) multiplited by 100</b>. This is rounded to one decimal place.</br>",
                                 "</br>Progress of 100 per cent means that the target has been achieved. Positive values <b>('Good')</b> indicate progression towards the target. Negative values <b>('Bad')</b> indicate regression away from the target.</br>",
                                 "</br>As much of the information for the MV2040 outcomes framework depends on data from external sources, <b>progress data is not yet available for some sources</b>.</br>",
                                 "</br>Hover over the bars for more information. For full details about the measures, see the <b>'Individual measures'</b> tab."))

# circumplexes ________________________________________________________________________________________________________________________________________________________
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
circ1 <- ggplot(circ_data3, aes(text, mean_toward_pct, fill = theme)) +
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
circ2 <- ggplot(circ_inds3, aes(text, current, fill = theme)) +
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


# for plotly graphs in theme summaries ____________________________________________________________________________________
data_ind_plot <- data_raw %>% 
  left_join(indicator_list, by = "id") %>% 
  left_join(data_format, by = "id") %>% 
  select(id, year, value, type, err_low, err_high, measure, source, value_unit, theme) %>% 
  left_join(colour_table, by = "theme")

make_indiv_plotly <- function(ind_val_id, rangemode_val = "tozero"){
  #plotly graph
  graph_data <- data_ind_plot %>% filter(id == {ind_val_id})
  graph_data_actual <- graph_data %>% filter(id == {ind_val_id}) %>% filter(type != "target")
  date_label <- format(graph_data_actual$year, "%b %Y")
  theme_colour <- graph_data %>% filter(id == {ind_val_id}) %>% distinct(col_code) %>% pull()
  value_unit <- graph_data %>% filter(id == {ind_val_id}) %>% distinct(value_unit) %>% pull()
  graph_title <- graph_data %>% filter(id == {ind_val_id}) %>% distinct(measure) %>% pull()
  
  plot_ly() %>%
    # a trace with all the data, dashed
    add_trace(data = graph_data,  x = ~year, y = ~value,
              name = 'Target', type = 'scatter', mode = 'lines+markers',
              line = list(shape = 'linear', color = theme_colour, width= 3, dash = 'dash'),
              marker = list(color = "aqua", size = 9)) %>% 
    # a trace that overwrites the actual
    add_trace(data = graph_data_actual %>% filter(id == {ind_val_id}),  x = ~year, y = ~value,
              name = 'Actual', type = 'scatter', mode = 'lines+markers',
              line = list(shape = 'linear', color = theme_colour, width= 5, dash = 'solid'),
              marker = list(color = theme_colour, size = 9),
              text=paste(date_label, graph_data_actual$value, sep=', '),
              hoverinfo='text',
              error_y = list(type = "data", symmetric = FALSE, array = ~err_high, arrayminus = ~err_low)) %>%
    layout(title = graph_title,
           xaxis = list(title = 'Year'),
           yaxis = list (title = value_unit, rangemode = {rangemode_val}))
}

make_indiv_plotly("F011")

################## progress towards

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
  select(id, baseline, progress, target, theme = theme.x, desired = desired.x, toward_pct, measure) %>% 
  mutate(toward_pct = (ifelse(is.na(toward_pct), 0, toward_pct))) %>% 
  mutate(toward_pct = round(toward_pct, 1)) %>% 
  left_join(data_format, by = "id")

#circumplex
ggplot(towards_target, aes(id, toward_pct, fill = theme)) +
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
