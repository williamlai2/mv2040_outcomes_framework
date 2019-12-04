#add libraries -don't use pacman - it won't work when published to shinyapps
library(shiny)
library(tidyverse)
library(glue)
library(readxl)
library(janitor)
library(plotly)
library(scales)
library(shinydashboard)

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

## colours for shinydashboard boxes
colour_boxes <- tibble(theme = c("Fair", "Thriving", "Connected", "Green", "Beautiful"),
                       col_code = c("red", "blue", "violet", "green", "yellow"))

# the theme list
theme_list <- indicator_list %>% 
    distinct(theme) %>% 
    pull()

# the measure list
measure_list <- indicator_list %>% 
    select(measure) %>% 
    pull()

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
        
    #return list
    return_vals <- list("data" = data,"indicator_details" = ind, "source" = source, "commentary" = commentary, "rationale" = rationale, "format" = fmt,
                        "additional_info" = add_inf, "value_unit" = val_unit, "title" = title_det, "theme" = theme_det, "category" = category_det, "definition" = definition_det,
                        "theme_colour" = colour_select, "strategic_direction" = strat_dir, "desired_change" = desired, "change_progress" = change_progress, "baseline_year" = base_year)
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
               yaxis = list (title = ind_vals_output$value_unit, rangemode = {rangemode_val}))
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
    width = 100,
    sidebarMenu(
        menuItem("Measures", tabName = "measures"),
        menuItem("Summary", tabName = "summary"),
        menuItem("Notes", tabName = "notes")
    )
)

body <- dashboardBody(
    #supress error messages
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
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
                
                # the graph
                fluidRow(plotlyOutput("measure_graph"),
                ),
                
                # info about the graph
                fluidRow(
                    box(title = "Measure", width = 4, background = "black", textOutput('title')),
                    box(title = "Source", width = 4, background = "black", textOutput('source')),
                    box(title = "Definition", width = 4, background = "black", textOutput('definition'))
                ),
                
                #about progress towards desired change
                fluidRow(
                    infoBoxOutput("ibox_desired"),
                    infoBoxOutput("ibox_progress")
                ),
                
                #commentary and rationale below the graph
                fluidRow(
                    box(title = "Commentary", width = 6, textOutput('commentary')),
                    box(title = "Rationale", width = 6, textOutput('rationale'))
                ),
        ),
        # Second tab content
        tabItem(tabName = "summary",
                h2("Summary of progress towards the 2040 targets by theme"),
                br(),
                fluidRow(
                    infoBox(title = "Theme", value = "Fair", color = "red", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Fair") %>% select(pct_good) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Good' progress",
                             icon = shiny::icon("check-circle"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Fair") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("arrows-h"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Fair") %>% select(pct_bad) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Bad' progress",
                             icon = shiny::icon("times-circle"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Theme", value = "Thriving", color = "blue", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Thriving") %>% select(pct_good) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Good' progress",
                             icon = shiny::icon("check-circle"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Thriving") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("arrows-h"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Thriving") %>% select(pct_bad) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Bad' progress",
                             icon = shiny::icon("times-circle"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Theme", value = "Connected", color = "purple", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Connected") %>% select(pct_good) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Good' progress",
                             icon = shiny::icon("check-circle"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Connected") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("arrows-h"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Connected") %>% select(pct_bad) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Bad' progress",
                             icon = shiny::icon("times-circle"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Theme", value = "Green", color = "green", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Green") %>% select(pct_good) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Good' progress",
                             icon = shiny::icon("check-circle"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Green") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("arrows-h"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Green") %>% select(pct_bad) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Bad' progress",
                             icon = shiny::icon("times-circle"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Theme", value = "Beautiful", color = "yellow", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Beautiful") %>% select(pct_good) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Good' progress",
                             icon = shiny::icon("check-circle"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Beautiful") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("arrows-h"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Beautiful") %>% select(pct_bad) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Bad' progress",
                             icon = shiny::icon("times-circle"), color = "maroon", width = 3),
                ),
        ),
        #third tab
        tabItem(tabName = "notes",
                fluidRow(
                    box(title = "Notes:", "Work in progress!")
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
    #text - rationale
    output$rationale <- renderText({
        glue("{get_vals()$rationale}")
    })
    #text - desired change with baseline number
    output$desired_change <- renderText({
        glue("{get_vals()$desired_change} from {get_vals()$baseline_year} figure towards target")
    })
    #text - change progress
    output$change_progress <- renderText({
        glue("{get_vals()$change_progress}")
    })
    
    # the theme box
    output$ibox_theme <- renderInfoBox({
        if (input$selected_theme  == "Fair")
        {
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "red",  fill = TRUE)
        }
        else if (input$selected_theme  == "Thriving")
        {
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "blue",  fill = TRUE)
        }
        else if (input$selected_theme  == "Connected")
        {
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "purple",  fill = TRUE)
        }
        else if (input$selected_theme  == "Green")
        {
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "green",  fill = TRUE)
        }
        else {
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "yellow",  fill = TRUE)
        }
    })
    
    
    # the strategic direction box
    output$ibox_sd <- renderInfoBox({
        if (input$selected_theme  == "Fair")
        {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "red",  fill = TRUE)
        }
        else if (input$selected_theme  == "Thriving")
        {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "blue",  fill = TRUE)
        }
        else if (input$selected_theme  == "Connected")
        {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "purple",  fill = TRUE)
        }
        else if (input$selected_theme  == "Green")
        {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "green",  fill = TRUE)
        }
        else {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "yellow",  fill = TRUE)
        }
    })
    
    # the category box
    output$ibox_cat <- renderInfoBox({
        if (input$selected_theme  == "Fair")
        {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "red",  fill = TRUE)
        }
        else if (input$selected_theme  == "Thriving")
        {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "blue",  fill = TRUE)
        }
        else if (input$selected_theme  == "Connected")
        {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "purple",  fill = TRUE)
        }
        else if (input$selected_theme  == "Green")
        {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "green",  fill = TRUE)
        }
        else {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "yellow",  fill = TRUE)
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
            infoBox(title = "Progress", textOutput('change_progress'), icon = shiny::icon("check-circle"), color = "aqua")
        }
        else if (get_vals()$change_progress  == "Bad")
        {
            infoBox(title = "Progress", textOutput('change_progress'), icon = shiny::icon("times-circle"), color = "maroon")
        }
        else {
            infoBox(title = "Progress", textOutput('change_progress'), icon = shiny::icon("arrows-h"), color = "orange")
        }
    })
    
}

# for the app
ui <- dashboardPage(header,
                    sidebar,
                    body)

shinyApp(ui, server)