# Loading libraries
library(tidyverse)
library(ggthemes)
library(lubridate)
library(shiny)
library(scales)

# Importing the US data from the source repository

import_all_US <- function() {
    
    # Definitions:
    
    # data source base URL and suffix
    base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/"
    suffix <- ".csv"
    
    # start date, and a string version
    start_date <- make_date(year = 2020, month = 4, day = 12)
    start_date_string <- format(start_date, "%m-%d-%Y")
    
    # end date, assuming that the data source is always two days behind,
    # and using the day period function
    end_date <- Sys.Date() - days(2)
    
    # number of days between the start and end days
    number_days <- day(as.period(end_date - start_date))
    
    # initializing the first tibble, adding a date to each row,
    # and removing extraneous columns
    csv_tibble <- read_csv(paste0(base_url, start_date_string, suffix)) %>%
        mutate(Date = start_date, Day = 0) %>% 
        select(-c(Country_Region, Last_Update))
    
    # binding all of the remaining csv files to the csv_tibble
    for (i in 1:number_days) {
        
        # creating a date object
        tmp_date <- start_date + days(i)
        
        # storing a coerced string to use for data extraction
        tmp_date_string <- format(tmp_date, "%m-%d-%Y")
        
        # importing one csv file, appending the date object to each row,
        # and removing extraneous columns
        tmp_csv <- read_csv(paste0(base_url, tmp_date_string, suffix)) %>%
            mutate(Date = tmp_date, Day = i) %>%
            select(-c(Country_Region, Last_Update))
        
        # testing if the CSV has the "Total_Test_Results" or
        # "Case_Fatality_Ratio" columns
        logical_TTR <- "Total_Test_Results" %in% colnames(tmp_csv)
        logical_CFR <- "Case_Fatality_Ratio" %in% colnames(tmp_csv)
        
        if (logical_TTR) {
            tmp_csv <- tmp_csv %>% rename(People_Tested = Total_Test_Results)
        }
        
        if (logical_CFR) {
            tmp_csv <- tmp_csv %>% rename(Mortality_Rate = Case_Fatality_Ratio)
        }
        
        # appending the temporary csv tibble to the main tibble
        csv_tibble <- bind_rows(csv_tibble, tmp_csv)
        
    }
    
    return(csv_tibble)
}

COVID_Data <- import_all_US()

# Define UI for the simple COVID Data Shiny App
ui <- fluidPage(
    
    # Inputs that depend on the update button, using eventReactive
    
    # Grouping Variable
    group_variable <- radioButtons(inputId = "Group",
                                   label = "Select national or state results",
                                   choiceNames = c("National", "State"),
                                   choiceValues = c(FALSE, TRUE)),
    
    # State Variable (if the state option is selected)
    state_variable <- selectInput(inputId = "State",
                                  label = "Select a state, if applicable",
                                  choices = as.list(unique(COVID_Data$Province_State))),
    
    # Y variable
    y_variable <- radioButtons(inputId = "YVar",
                               label = "Select the metric to graph",
                               choiceNames = c("Cases Per Day",
                                               "Deaths Per Day",
                                               "Percent Positive Tests Per Day"),
                               choiceValues = c("Cases_Per_Day",
                                                "Deaths_Per_Day",
                                                "Percent_Positive")),
    
    # Date range input
    date_range <- dateRangeInput(inputId = "DateRange", 
                                 label = "Choose the date range to graph",
                                 start = make_date(year = 2020, 
                                                   month = 4, 
                                                   day = 12),
                                 end = Sys.Date() - days(2),
                                 min = make_date(year = 2020,
                                                 month = 4,
                                                 day = 12),
                                 max = Sys.Date() - days(2)),
    
    
    # Update summary table of graphing
    update_table <- actionButton(inputId = "UpdateData", label = "Update"),
    
    
    # Inputs that are independent of the update button
    
    
    # button for whether or not there is a smooth line or just points
    span_option <- radioButtons(inputId = "SpanOption", 
                                label = "Smooth Line?",
                                choiceNames = c("Yes", "No"),
                                choiceValues = c(TRUE, FALSE)),
    
    # Slider for graph smoothing parameter
    span_slider <- sliderInput(inputId = "Span",
                               label = "Smoothing Span",
                               min = 0.01,
                               max = 1,
                               value = 0.1,),
    
    # Outputs
    
    # Plot
    plotOutput("plot")
    
)

# Define server logic required to use the UI information
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

server <- function(input, output) {
    
    # String variable, dependent on the update button, that
    # equals the string version of the Y variable to be graphed
    delayed_yvar <- eventReactive(input$UpdateData, {input$YVar})
    
    
    # String variable, dependent on the update button, that equals the
    # y axis label to be used in the graph
    delayed_ylab <- eventReactive(input$UpdateData, {
        if (delayed_yvar() == "Percent_Positive") {
            "Percent Positive Tests Per Day"
        } else if (delayed_yvar() == "Deaths_Per_Day") {
            "Deaths Per Day"
        } else {
            "Cases Per Day"
        }
    })
    
    # String variable, dependent on the update button, that changes the title
    # of the graph depending on whether or not the graph is for the state level
    # or US level
    delayed_group <- eventReactive(input$UpdateData, {
        
        # Group == FALSE is default, implies national level
        if (input$Group) {
            
            # value if state is selected
            paste("for", input$State)
        } else {
            
            # value if national is selected
            "for the US"
        }
    })
    
    # String variable, dependent on the update button, that constructs the full
    # title string
    delayed_title <- eventReactive(input$UpdateData, {
        paste(delayed_ylab(), delayed_group())
    })
    
    # Generating a filtered, processed data table
    # that depends on the update button
    filtered_summary_data <- eventReactive(input$UpdateData, {
        
        # When Group == TRUE, the data is filtered by the selected state 
        # and the date range, and no grouping is needed
        if (input$Group) {
            COVID_Data %>%
                
                # filtering by date and state
                filter(Province_State == input$State, 
                       Date >= input$DateRange[1], 
                       Date <= input$DateRange[2]) %>%
                
                # creating a new data table of relevant dates and metrics
                summarise(Date = Date,
                          Cases_Per_Day = Confirmed - lag(Confirmed),
                          Deaths_Per_Day = Deaths - lag(Deaths),
                          Percent_Positive = 100*(Cases_Per_Day/(People_Tested - lag(People_Tested))))
            
        } else {
            
            # When Group != TRUE (the default behavior), the data is not filtered by
            # state, and needs to be grouped based on date for data aggregation
            COVID_Data %>%
                
                # filtering for the relevant dates
                filter(Date >= input$DateRange[1], 
                       Date <= input$DateRange[2]) %>%
                
                # grouping the data by date
                group_by(Date) %>%
                
                # creating a data table with cumulative values for relevant metrics
                summarise(US_Cases = sum(Confirmed, na.rm = TRUE),
                          US_Deaths = sum(Deaths, na.rm = TRUE),
                          US_Tests = sum(People_Tested, na.rm = TRUE)) %>%
                
                # using mutate in order to be able to get lag to work
                # (as it didn't want to work in the summarize statement for some reason)
                mutate(Cases_Per_Day = US_Cases - lag(US_Cases),
                       Deaths_Per_Day = US_Deaths - lag(US_Deaths),
                       Percent_Positive = 100*(Cases_Per_Day/(US_Tests - lag(US_Tests))))
        }
    })
    
    output$plot <- renderPlot({
        
        # Defining a function to get the ggplot specifications just right
        graph_function <- function(Data, yvar, titlevar) {
            
            # Conditional statement governing whether there is a smoothed line or not
            if (input$SpanOption) {
                
                # base layer
                ggplot(data = Data, aes_string(x = "Date", y = delayed_yvar())) +
                    
                    # points
                    geom_point() +
                    
                    # smooth line
                    stat_smooth(span = input$Span, se = FALSE) +
                    
                    # formatting the x axis so that it shows each and every month
                    scale_x_date(date_breaks = "months", date_labels = "%b %y") +
                    
                    # formatting the y axis so that it always displays with commas and
                    # as an integer
                    scale_y_continuous(labels = label_comma(accuracy = 1)) +
                    
                    # putting the y label
                    ylab(yvar) +
                    
                    # putting the plot title
                    ggtitle(titlevar) +
                    
                    # using a built in theme from ggthemes to make the graph presentable
                    theme_classic()
                
            } else {
                
                # base layer
                ggplot(data = Data, aes_string(x = "Date", y = delayed_yvar())) +
                    
                    # points
                    geom_point() +
                    
                    # formatting the x axis so that it shows each and every month
                    scale_x_date(date_breaks = "months", date_labels = "%b %y") +
                    
                    # formatting the y axis so that it always displays with commas and
                    # as an integer
                    scale_y_continuous(labels = label_comma(accuracy = 1)) +
                    
                    # putting the y label
                    ylab(yvar) +
                    
                    # putting the plot title
                    ggtitle(titlevar) +
                    
                    # using a built in theme from ggthemes to make the graph presentable
                    theme_classic()
            }
        }
        
        # Conditional statements to first filter invalid data, and then graph
        
        if (delayed_yvar() == "Percent_Positive") {
            
            filtered_summary_data() %>%
                
                # percent values need to be between 0 and 100%, inclusive
                filter(Percent_Positive > 0, Percent_Positive < 100) %>%
                
                graph_function(yvar = delayed_ylab(), titlevar = delayed_title())
            
        } else if (delayed_yvar() == "Deaths_Per_Day") {
            
            filtered_summary_data() %>%
                
                # Deaths has to be greater than zero
                filter(Deaths_Per_Day > 0) %>%
                
                graph_function(yvar = delayed_ylab(), titlevar = delayed_title())
            
        } else {
            
            filtered_summary_data() %>%
                
                # Cases has to be greater than zero
                filter(Cases_Per_Day > 0) %>%
                
                graph_function(yvar = delayed_ylab(), titlevar = delayed_title())
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
