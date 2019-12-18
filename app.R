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
                                 "As much of the information for the MV2040 outcomes framework depends on data from external sources, <b>progress data is not yet available for some sources</b>.</br>",
                                 "</br>Progress towards the target is calculated as <b>(the progress value - the baseline value) divided by (the target value - the baseline value) multiplited by 100</b>. This is rounded to one decimal place.</br>",
                                 "</br>Progress of 100 per cent means that the target has been achieved. Positive values indicate progression towards the target. Negative values indicate regression away from the target.</br>",
                                 "</br>For full information about the measures, see the <b>'Individual measures'</b> tab."))

# functions _________________________________________________________________________________________________________
# function to get values for each indicator - returns a list
ind_vals <- function(indicator_id) {
    #data
    data <- data_raw %>% 
        filter(id == {indicator_id})
    #indicator details
    ind <- indicator_list %>% 
        filter(id == {indicator_id})
    #source
    source <- ind %>% 
        select(source) %>% 
        pull()
    #commentary
    commentary <- ind %>% 
        select(commentary) %>% 
        pull()
    #target source
    target_source <- ind %>% 
        select(target_source) %>% 
        pull()
    #rationale
    rationale <- ind %>% 
        select(rationale) %>% 
        pull()
    #format
    fmt <- data_format %>% 
        filter(id == {indicator_id})
    #additional info
    add_inf <- indicators_to_goals %>% 
        filter(id == {indicator_id}) %>% 
        left_join(goals, by = "sd_number")
    #strategic direction
    strat_dir <- add_inf %>% 
        select(strategic_direction) %>% 
        pull()
    #value unit
    val_unit <- fmt %>% 
        select(value_unit) %>% 
        pull()
    #title
    title_det <- ind %>% 
        select (measure) %>% 
        pull()
    #theme
    theme_det <- ind %>% 
        select (theme) %>% 
        pull()
    #category
    category_det <- ind %>% 
        select (category) %>% 
        pull()
    #definition
    definition_det <- ind %>% 
        select (definition) %>% 
        pull()
    #colour
    colour_select <- add_inf %>% 
        select(theme) %>% 
        left_join(colour_table, by = "theme") %>% 
        select(col_code) %>% 
        pull()
    #desired change
    desired <- ind %>% 
        select(desired) %>% 
        pull()
    #change progress
    change_progress <- change_progress_data_full %>% 
        filter(id == {indicator_id}) %>% 
        select(change) %>% 
        pull()
    #baseline year
    base_year <- data_raw %>%
        filter(id == {indicator_id}) %>% 
        filter(type == "baseline") %>% 
        mutate(year = as.numeric(format(year, '%Y'))) %>% 
        select(year) %>% 
        pull()
    #drop year -baseline
    baseline_year <- data_raw %>%
        filter(id == {indicator_id}) %>% 
        filter(type == "baseline") %>% 
        select(year) %>% 
        pull()
    #drop value -baseline
    baseline_value <- data_raw %>%
        filter(id == {indicator_id}) %>% 
        filter(type == "baseline") %>% 
        select(value) %>% 
        pull()
    
        
    #return list
    return_vals <- list("data" = data,"indicator_details" = ind, "source" = source, "commentary" = commentary, "target_source" = target_source, "rationale" = rationale, "format" = fmt,
                        "additional_info" = add_inf, "value_unit" = val_unit, "title" = title_det, "theme" = theme_det, "category" = category_det, "definition" = definition_det,
                        "theme_colour" = colour_select, "strategic_direction" = strat_dir, "desired_change" = desired, "change_progress" = change_progress, "baseline_year" = baseline_year, "baseline_value" = baseline_value)
}

#function for a plotly graph - takes in the output from the indicator, then an optional rangemode value
make_plotly <- function(ind_vals_output, rangemode_val = "tozero"){
    #plotly graph
    graph_data <- ind_vals_output$data
    graph_data_actual <- ind_vals_output$data %>% filter(type != "target")
    date_label <- format(graph_data_actual$year, "%b %Y")
    plot_ly() %>%
        # a trace with all the data, dashed
        add_trace(data = graph_data,  x = ~year, y = ~value,
                  name = 'Target', type = 'scatter', mode = 'lines+markers',
                  line = list(shape = 'linear', color = ind_vals_output$theme_colour, width= 3, dash = 'dash'),
                  marker = list(color = "aqua", size = 9)) %>% 
        # a trace that overwrites the actual
        add_trace(data = graph_data_actual,  x = ~year, y = ~value,
                  name = 'Actual', type = 'scatter', mode = 'lines+markers',
                  line = list(shape = 'linear', color = ind_vals_output$theme_colour, width= 5, dash = 'solid'),
                  marker = list(color = ind_vals_output$theme_colour, size = 9),
                  text=paste(date_label, graph_data_actual$value, sep=', '),
                  hoverinfo='text',
                  error_y = list(type = "data", symmetric = FALSE, array = ~err_high, arrayminus = ~err_low)) %>%
        layout(xaxis = list(title = 'Year'),
               yaxis = list (title = ind_vals_output$value_unit, rangemode = {rangemode_val}),
               shapes = list(x0 = ind_vals_output$baseline_year, x1 = ind_vals_output$baseline_year, y0 = 0, y1 = ind_vals_output$baseline_value, line = list(color = "black"),
                             type = "line", xref = "x", yref = "y", opacity = 0.3, fillcolor = "black"), #baseline vertical line
               annotations = list(list(x = ind_vals_output$baseline_year, y = 0, text = "Baseline", xref = "x", yref = "y", showarrow = TRUE, arrowhead = 0, opacity = 0.6, ax = 0, ay = -10), #baseline year
                                  list(x = "2025-11-01", y = 0, text = "End of 2021-2025 \nCouncil Term", xref = "x", yref = "y", showarrow = TRUE, arrowhead = 0, opacity = 0.5, ax = 0, ay = -40)) # term
               )
}

# function for theme progress towards the targets
make_theme_plotly <- function(dataset, colour){
    plot_ly(data = {dataset}, x = ~toward_pct, y = ~reorder(measure, toward_pct), name = 'Progress towards targets',
            type = 'bar', orientation = 'h',
            hoverinfo = "text",
            text = ~paste('</br> Measure: ', measure,
                          '</br> Unit: ', value_format,
                          '</br> Baseline: ', baseline,
                          '</br> Progress value: ', progress,
                          '</br> Target: ', target,
                          '</br> Progress towards target (%): ', toward_pct),
            marker = list(color = glue("{colour}"),
                          line = list(color = glue("{colour}"), width = 1))) %>%
        layout(yaxis = list(title = "", showgrid = FALSE, showline = FALSE, showticklabels = TRUE),
               xaxis = list(title = "Progress towards target (%)", zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
    
}

#the app ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# dashboard input
header <- dashboardHeader(
    title = "MV2040 outcomes framework",
    titleWidth = 300,
    tags$li(a(img(src = 'mvcc_logo.jpg',
                  height = "45px"),
              style = "padding-top:2px; padding-bottom:2px;"),
            class = "dropdown")
)

sidebar <- dashboardSidebar(
    width = 200,
    sidebarMenu(
        menuItem("Individual measures", tabName = "measures"),
        menuItem("Summary",
                 menuSubItem("Progress towards targets", tabName = "progress"),
                 menuSubItem("Fair", tabName = "summary_fair"),
                 menuSubItem("Thriving", tabName = "summary_thriving"),
                 menuSubItem("Connected", tabName = "summary_connected"),
                 menuSubItem("Green", tabName = "summary_green"),
                 menuSubItem("Beautiful", tabName = "summary_beautiful")
        ),
        menuItem("Notes", tabName = "notes")
    )
)

body <- dashboardBody(
    #supress error messages
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    #override red with 'fair' red
    tags$style(
        type = 'text/css', 
        '.bg-red {background-color: #E55048!important; }'
    ),
    #override blue with 'thriving' blue
    tags$style(
        type = 'text/css', 
        '.bg-blue {background-color: #31788F!important; }'
    ),
    #override purple with 'connected' purple
    tags$style(
        type = 'text/css', 
        '.bg-purple {background-color: #6A4479!important; }'
    ),
    #override green with 'green' green
    tags$style(
        type = 'text/css', 
        '.bg-green {background-color: #4EA546!important; }'
    ),
    #override yellow with 'beautiful' yellow
    tags$style(
        type = 'text/css', 
        '.bg-yellow {background-color: #E3A51E!important; }'
    ),
    tabItems(
        # First tab content
        tabItem(tabName = "measures",
                #select
                fluidRow(
                    box(selectInput(inputId = "selected_theme",
                                    label = "Select a theme",
                                    choices = theme_list),
                    ),
                    box(uiOutput("measure_output"))
                ),
                
                # infoBoxes dynamic colours based on function in server
                fluidRow(
                    infoBoxOutput("ibox_theme"),
                    infoBoxOutput("ibox_sd"),
                    infoBoxOutput("ibox_cat")
                ),
                
                # info about the graph
                fluidRow(
                    box(title = "Measure", width = 4, background = "black", textOutput('title')),
                    box(title = "Definition", width = 4, background = "black", textOutput('definition')),
                    box(title = "Source", width = 4, background = "black", textOutput('source'))
                ),
                
                # the graph
                fluidRow(plotlyOutput("measure_graph") %>% 
                             withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                ),
                
                
                #about progress towards desired change
                fluidRow(
                    infoBoxOutput("ibox_desired"),
                    infoBoxOutput("ibox_progress"),
                    infoBox(title = "Target source", textOutput('target_source'), icon = shiny::icon("bullseye"), color = "black")
                ),
                
                #commentary and rationale below the graph
                fluidRow(
                    box(title = "Commentary", width = 6, textOutput('commentary')),
                    box(title = "Rationale", width = 6, textOutput('rationale'))
                ),
        ),
        # Second tab content
        tabItem(tabName = "progress",
                h2("Summary of progress towards the 2040 targets by theme"),
                br(),
                fluidRow(
                    infoBox(title = "Theme", value = "Fair", color = "red", width = 3, icon=icon(list(src = "fair.png", width="80px"), lib="local")),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Fair") %>% select(pct_good) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Good' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Fair") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Fair") %>% select(pct_bad) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Bad' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Theme", value = "Thriving", color = "blue", width = 3, icon=icon(list(src = "thriving.png", width="80px"), lib="local")),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Thriving") %>% select(pct_good) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Good' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Thriving") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Thriving") %>% select(pct_bad) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Bad' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Theme", value = "Connected", color = "purple", width = 3, icon=icon(list(src = "connected.png", width="80px"), lib="local")),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Connected") %>% select(pct_good) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Good' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Connected") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Connected") %>% select(pct_bad) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Bad' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Theme", value = "Green", color = "green", width = 3, icon=icon(list(src = "green.png", width="80px"), lib="local")),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Green") %>% select(pct_good) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Good' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Green") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Green") %>% select(pct_bad) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Bad' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Theme", value = "Beautiful", color = "yellow", width = 3, icon=icon(list(src = "beautiful.png", width="80px"), lib="local")),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Beautiful") %>% select(pct_good) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Good' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Beautiful") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Beautiful") %>% select(pct_bad) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Bad' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    box(title = "Notes", "Figures are rounded and may not add up to 100 per cent")
                ),
        ),
        
        #theme tabs - fair
        tabItem(tabName = "summary_fair",
                fluidRow(
                    infoBox(title = "Theme", value = "Fair", color = "red", width = 12, icon=icon(list(src = "fair.png", width="80px"), lib="local")),
                    box(ind_theme_text, width = 12)
                ),
                fluidRow(
                    plotlyOutput("towards_fair_graph") %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                )
        ),
        
        #theme tabs - thriving
        tabItem(tabName = "summary_thriving",
                fluidRow(
                    infoBox(title = "Theme", value = "Thriving", color = "blue", width = 12, icon=icon(list(src = "thriving.png", width="80px"), lib="local")),
                    box(ind_theme_text, width = 12)
                ),
                fluidRow(
                    plotlyOutput("towards_thriving_graph") %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                )
        ),
        
        #theme tabs - connected
        tabItem(tabName = "summary_connected",
                fluidRow(
                    infoBox(title = "Theme", value = "Connected", color = "purple", width = 12, icon=icon(list(src = "connected.png", width="80px"), lib="local")),
                    box(ind_theme_text, width = 12)
                ),
                fluidRow(
                    plotlyOutput("towards_connected_graph") %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                )
        ),
        
        #theme tabs - green
        tabItem(tabName = "summary_green",
                fluidRow(
                    infoBox(title = "Theme", value = "Green", color = "green", width = 12, icon=icon(list(src = "green.png", width="80px"), lib="local")),
                    box(ind_theme_text, width = 12)
                ),
                fluidRow(
                    plotlyOutput("towards_green_graph") %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                )
        ),
        
        #theme tabs - beautiful
        tabItem(tabName = "summary_beautiful",
                fluidRow(
                    infoBox(title = "Theme", value = "Beautiful", color = "yellow", width = 12, icon=icon(list(src = "beautiful.png", width="80px"), lib="local")),
                    box(ind_theme_text, width = 12)
                ),
                fluidRow(
                    plotlyOutput("towards_beautiful_graph") %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                )
        ),
        
        #third tab
        tabItem(tabName = "notes",
                fluidRow(
                    box(title = "Notes:", "Work in progress!", width = 12),
                    box(title = 'MV2040', tags$body(HTML("The MV2040 Strategy is Council’s long-term strategy and vision for a healthy ‘city of neighbourhoods’.",
                                                         "Given Council’s commitment and investment in MV2040 - it will be important to develop systems to ensure we’re making evidenced based progress towards our Vision’s 20 strategic directions with associated targets.</br>",
                                                         "</br>This MV2040 Outcomes Framework will assist Council to monitor, evaluate and report on its progress towards a healthy city.</br>")), width = 12)
                )
        )
    )
)


# Define server logic 
server <- function(input, output) {
    #filtered_measure list
    filtered_measures <- reactive({
        indicator_list %>% 
            filter(theme == input$selected_theme) %>% 
            select(measure) %>% 
            pull()
    })
    #takes filted input by theme and outputs measures for selection
    output$measure_output <- renderUI({
        selectInput(inputId = "selected_measure",
                    label = "Select a measure",
                    choices = filtered_measures())
    })
    
    #selected_id from measure
    selected_id <- reactive({
        indicator_list %>% 
            filter(measure == input$selected_measure) %>% 
            select(id) %>% 
            pull()
    })
    
    #get values - from id in function
    get_vals <- reactive({
        ind_vals(selected_id())
    })
    
    #plotly graph
    output$measure_graph <- renderPlotly({  
        print(
            make_plotly(get_vals())
        )
    })
    #text - strategic direction
    output$strategic_direction <- renderText({
        glue("{get_vals()$strategic_direction}")
    })
    #text - theme
    output$theme <- renderText({
        glue("{get_vals()$theme}")
    })
    #text - category
    output$category <- renderText({
        glue("{get_vals()$category}")
    })
    #text - title
    output$title <- renderText({
        glue("{get_vals()$title}")
    })
    #text - definition
    output$definition <- renderText({
        glue("{get_vals()$definition}")
    })
    #text - source
    output$source <- renderText({
        glue("{get_vals()$source}")
    })
    #text - commentary
    output$commentary <- renderText({
        glue("{get_vals()$commentary}")
    })
    #text - target_source
    output$target_source <- renderText({
        glue("{get_vals()$target_source}")
    })
    #text - rationale
    output$rationale <- renderText({
        glue("{get_vals()$rationale}")
    })
    #text - desired change with baseline number
    output$desired_change <- renderText({
        glue("{get_vals()$desired_change} from baseline value towards target")
    })
    #text - change progress
    output$change_progress <- renderText({
        glue("{get_vals()$change_progress}")
    })
    #text - target source
    output$target_source <- renderText({
        glue("{get_vals()$target_source}")
    })
    
    # the theme box
    output$ibox_theme <- renderInfoBox({
        if (input$selected_theme  == "Fair")
        {
            ic <- apputils::icon(list(src = "fair.png", width = "80px"), lib = "local")
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "red", icon = ic)
        }
        else if (input$selected_theme  == "Thriving")
        {
            ic <- apputils::icon(list(src = "thriving.png", width = "80px"), lib = "local")
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "blue", icon = ic)
        }
        else if (input$selected_theme  == "Connected")
        {
            ic <- apputils::icon(list(src = "connected.png", width = "80px"), lib = "local")
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "purple", icon = ic)
        }
        else if (input$selected_theme  == "Green")
        {
            ic <- apputils::icon(list(src = "green.png", width = "80px"), lib = "local")
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "green", icon = ic)
        }
        else {
            ic <- apputils::icon(list(src = "beautiful.png", width = "80px"), lib = "local")
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "yellow", icon = ic)
        }
    })
    
    
    # the strategic direction box
    output$ibox_sd <- renderInfoBox({
        if (input$selected_theme  == "Fair")
        {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "red", icon = icon("map-signs"))
        }
        else if (input$selected_theme  == "Thriving")
        {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "blue", icon = icon("map-signs"))
        }
        else if (input$selected_theme  == "Connected")
        {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "purple", icon = icon("map-signs"))
        }
        else if (input$selected_theme  == "Green")
        {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "green", icon = icon("map-signs"))
        }
        else {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "yellow", icon = icon("map-signs"))
        }
    })
    
    # the category box
    output$ibox_cat <- renderInfoBox({
        if (input$selected_theme  == "Fair")
        {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "red", icon = icon("chart-bar"))
        }
        else if (input$selected_theme  == "Thriving")
        {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "blue", icon = icon("chart-bar"))
        }
        else if (input$selected_theme  == "Connected")
        {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "purple", icon = icon("chart-bar"))
        }
        else if (input$selected_theme  == "Green")
        {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "green", icon = icon("chart-bar"))
        }
        else {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "yellow", icon = icon("chart-bar"))
        }
    })
    
    # desired change box
    output$ibox_desired <- renderInfoBox({
        if (get_vals()$desired_change  == "Increase")
        {
            infoBox(title = "Desired change", textOutput('desired_change'), icon = shiny::icon("arrow-up"), color = "black")
        }
        else {
            infoBox(title = "Desired change", textOutput('desired_change'), icon = shiny::icon("arrow-down"), color = "black")
        }
    })
    
    # the progress towards desired change box
    output$ibox_progress <- renderInfoBox({
        if (get_vals()$change_progress  == "Good")
        {
            infoBox(title = "Progress", textOutput('change_progress'), icon = shiny::icon("smile"), color = "aqua")
        }
        else if (get_vals()$change_progress  == "Bad")
        {
            infoBox(title = "Progress", textOutput('change_progress'), icon = shiny::icon("frown"), color = "maroon")
        }
        else {
            infoBox(title = "Progress", textOutput('change_progress'), icon = shiny::icon("meh"), color = "orange")
        }
    })
    
    #individual plotly graphs for theme progress
    output$towards_fair_graph <- renderPlotly({  
        print(
            make_theme_plotly(towards_fair, "#E55048")
        )
    })
    output$towards_thriving_graph <- renderPlotly({  
        print(
            make_theme_plotly(towards_thriving, "#31788F")
        )
    })
    output$towards_connected_graph <- renderPlotly({  
        print(
            make_theme_plotly(towards_connected, "#6A4479")
        )
    })
    output$towards_green_graph <- renderPlotly({  
        print(
            make_theme_plotly(towards_green, "#4EA546")
        )
    })
    output$towards_beautiful_graph <- renderPlotly({  
        print(
            make_theme_plotly(towards_beautiful, "#E3A51E")
        )
    })
    
}

# for the app
ui <- dashboardPage(header,
                    sidebar,
                    body)

shinyApp(ui, server)