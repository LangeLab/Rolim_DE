library(shiny)
library(shinydashboard)
source("cleaning.R")
source("heatmap.R")
source("logomaps.R")
# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    dashboardHeader(title="Supplemental tool for RoLim"),
    # Sidebar Inputs
    dashboardSidebar(
        h4("Configurations for Clustering"),
        uiOutput("checkSamples"),
        selectInput("heatmapvalue",
                    label="Choose the variable for Clustering",
                       choices=c("Enrichment Score", "Quantitative intensity"),
                       selected=NULL),
        checkboxInput("whetherNormal",
                      "Normalization for clustering?"),
        h4("Configurations for Logomap"),
        selectInput("logomapmethod",
                    label="Choose method for generating logomaps:",
                    choices=c("bits","prob"),
                    selected="bits"),
        selectInput("ticktype",
                    label="Choose type of ticks for logomap",
                    choices=c("1,2,3...",
                              "...,P2,P1,P1',P2'...(for even length only)",
                              "...-1,0,1,...(for odd length only)"),
                    selected="1,2,3...")
        ),
    # Main Outputs
    dashboardBody(
        #textOutput("value"),
        plotOutput("heatmap"),
        uiOutput("logomaps")
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$checkSamples<-renderUI({
        checkboxGroupInput("checkSamples", 
                           h5("Check the samples to be included in analysis"), 
                           choiceNames=as.list(sample.list),
                           choiceValues=as.list(sample.list))
        })
    all.pattern <- reactive({find_all_pattern(input$checkSamples)})
    
    output$heatmap <- renderPlot({generate_heatmap(input$checkSamples, input$heatmapvalue, input$whetherNormal)})
    output$logomaps<-renderUI({
        lapply(all.pattern(),function(x){
            output[[paste0("Plotfor",x)]]<-renderPlot({generate_logomap(x, input$checkSamples, input$logomapmethod, input$ticktype)})
        })
        lapply(all.pattern(),function(x){
            output[[paste0("Tablefor",x)]]<-renderTable({generate_summarytable(x, input$checkSamples)})
        })
        myTabs = lapply(all.pattern(), function(x){
            tabPanel(title=x, 
                     tableOutput(paste0("Tablefor",x)),
                     plotOutput(paste0("Plotfor",x))
                     )
        })
        do.call(tabsetPanel, myTabs)
    })
 
}

# Run the application 
shinyApp(ui = ui, server = server)
