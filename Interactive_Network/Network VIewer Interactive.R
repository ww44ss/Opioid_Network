#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tidyverse)
library(lubridate)
library(gridExtra)
library(R.utils)
library(igraph)
library(kableExtra)

yo <- function(x){x}


## get list of files

directory <- "C:/Users/wisaund/OneDrive - Microsoft/Documents/2019 Data Science/Washington_Post_Opiod_Data/"

## save data
summary_data <- read_csv(str_c(directory, "summary_data_30_city_year_name.csv"), col_names=TRUE, col_types = "ccccccid")


##

summary_data_store <- summary_data

summary_data_store -> summary_data

summary_data <- summary_data %>%
    group_by(REPORTER_STATE, BUYER_STATE) %>%
    filter(REPORTER_STATE != "GU") %>%
    filter(BUYER_STATE != "MP") %>%
    summarize(TOTAL_WT_IN_GM = sum(TOTAL_WT_IN_GM)) %>%
    arrange(desc(TOTAL_WT_IN_GM)) %>%
    ungroup() %>%
    yo


reporter_states <- summary_data%>%
    select(REPORTER_STATE) %>%
    yo

buyer_states <- summary_data%>%
    select(BUYER_STATE) %>%
    yo

sources <- summary_data %>%
    distinct(REPORTER_STATE) %>%
    rename(state = REPORTER_STATE) %>%
    yo


destinations <- summary_data %>%
    distinct(BUYER_STATE) %>%
    rename(state = BUYER_STATE) %>%
    yo

nodes <- full_join(sources, destinations, by = "state") %>%
    mutate(id = row_number()) %>%
    yo

edges <- summary_data %>%
    left_join(nodes, by = c("REPORTER_STATE" = "state")) %>%
    rename(report = id) %>%
    yo

edges <- edges %>%
    left_join(nodes, by = c("BUYER_STATE" = "state")) %>%
    rename(buy = id) %>%
    yo

edges <- edges %>%
    select(report, buy, TOTAL_WT_IN_GM) %>%
    rename(weight = TOTAL_WT_IN_GM) %>%
    yo


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Opioid Networks"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Nodes",
                        "Number of nodes:",
                        min = 5,
                        max = 200,
                        value = 60)),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        
        sales_network <- network(summary_data %>% filter(REPORTER_STATE == "OH" | REPORTER_STATE == "CA") %>% slice(1:input$Nodes), vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE, loops = FALSE)
        
        plot(sales_network, vertex.cex = 3, loop.cex = 2.5, object.scale = 0.01, label.cex = .73, displaylabels = TRUE, vertex.col = rgb(.1,.5,.9))#, mode = "circle")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
