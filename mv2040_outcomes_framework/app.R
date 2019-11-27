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
        left_join(colour_table) %>% 
        select(col_code) %>% 
        pull()
    #return list
    return_vals <- list("data" = data,"indicator_details" = ind, "source" = source, "commentary" = commentary, "rationale" = rationale, "format" = fmt,
                        "additional_info" = add_inf, "value_unit" = val_unit, "title" = title_det, "theme" = theme_det, "category" = category_det, "definition" = definition_det,
                        "theme_colour" = colour_select, "strategic_direction" = strat_dir)
}

#function for a plotly graph - takes in the output from the indicator, then an optional rangemode value
make_plotly <- function(ind_vals_output, rangemode_val = "tozero"){
    #plotly graph
    plot_ly() %>%
        # a trace with all the data, dashed
        add_trace(data = ind_vals_output$data,  x = ~year, y = ~value,
                  name = 'Target', type = 'scatter', mode = 'lines+markers',
                  line = list(shape = 'linear', color = ind_vals_output$theme_colour, width= 2, dash = 'dash')) %>% 
        # a trace that overwrites the actual
        add_trace(data = ind_vals_output$data %>% filter(type != "target") ,  x = ~year, y = ~value,
                  name = 'Actual', type = 'scatter', mode = 'lines+markers',
                  line = list(shape = 'linear', color = ind_vals_output$theme_colour, width= 4, dash = 'solid'),
                  error_y = list(type = "data", symmetric = FALSE, array = ~err_high, arrayminus = ~err_low)) %>%
        layout(xaxis = list(title = 'Year'),
               yaxis = list (title = ind_vals_output$value_unit, rangemode = {rangemode_val}))
}

#the app ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
shinyApp(
    ui = tagList(
        navbarPage(
            "MV2040 Outcomes Framework",
            tabPanel("Measures",
                     sidebarPanel(
                         selectInput(inputId = "selected_theme",
                                     label = "Select a theme",
                                     choices = theme_list),
                         uiOutput("measure_output") #from below
                     ),
                     mainPanel(
                         h2(textOutput('theme')),
                         strong("Strategic direction:"), 
                         h4(textOutput('strategic_direction')),
                         strong("Category:"), 
                         h4(textOutput('category')),
                         strong("Measure:"), 
                         h4(textOutput('title')),
                         plotlyOutput("measure_graph"),
                         strong("Source:"),
                         h5(htmlOutput('source')),
                         strong("Definition:"),
                         h5(htmlOutput('definition')),
                         strong("Commentary:"),
                         h5(htmlOutput('commentary')),
                         strong("Rationale:"), 
                         h5(htmlOutput('rationale')),
                     )
            ),
            tabPanel("About", 
                     mainPanel(
                         h4("The MV2040 framework has not been finalised and is subject to change"),
                         h4("95% confidence intervals have been included on datasets were available. Data has been rounded in most cases."),
                         br(),
                         strong("To do:"),
                         h4("add main 'dashboard' page"),
                         h4("add mv logo and mv2040 link"),
                         h4("Fix the error message at the beginning too")
                     ))
        )
    ),
    
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
    }
)

