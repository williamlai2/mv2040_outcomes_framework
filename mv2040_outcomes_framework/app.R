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
data_raw <- read_excel("../data_in/MV2040 Indicators and Outcomes DRAFT baseline- August 2019.xlsx", sheet = "data") %>% 
    clean_names()

indicator_list <- read_excel("../data_in/MV2040 Indicators and Outcomes DRAFT baseline- August 2019.xlsx", sheet = "indicator_list") %>% 
    clean_names()

indicators_to_goals <- read_excel("../data_in/MV2040 Indicators and Outcomes DRAFT baseline- August 2019.xlsx", sheet = "inds_goals") %>% 
    clean_names()

goals <- read_excel("../data_in/MV2040 Indicators and Outcomes DRAFT baseline- August 2019.xlsx", sheet = "goals") %>% 
    clean_names()

data_format <- read_excel("../data_in/MV2040 Indicators and Outcomes DRAFT baseline- August 2019.xlsx", sheet = "data_format") %>% 
    clean_names()

## colours for themes
colour_table <- tibble(theme = c("FAIR", "THRIVING", "CONNECTED", "GREEN", "BEAUTIFUL"),
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
                        "theme_colour" = colour_select)
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
                  line = list(shape = 'linear', color = ind_vals_output$theme_colour, width= 4, dash = 'solid')) %>%
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
                         selectInput(inputId = "selected_measure",
                                     label = "Select a measure",
                                     choices = measure_list)
                     ),
                     mainPanel(
                         h2(textOutput('theme')), 
                         h3(textOutput('category')),
                         h3(textOutput('title')),
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
            tabPanel("About", "The MV2040 framework has not been finalised and is subject to change.")
        )
    ),
    
    # Define server logic 
    server <- function(input, output) {
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

