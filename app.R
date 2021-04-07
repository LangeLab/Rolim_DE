library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(openxlsx)
source("cleaning.R")
source("heatmap.R")
source("logomaps.R")

ui <- dashboardPage(
    dashboardHeader(title="Supplemental Tool for RoLim"),
    
    dashboardSidebar(
        textInput("jobID",label="Please input the string indicating job ID sent by email from RoLim:"),
        textInput("title",label="Please input the title of task:"),
        uiOutput("checkSamples"),
        checkboxInput("whetherUpload",
                      "Upload extra quantitative files?"),
        conditionalPanel('input.whetherUpload==1',
                         fileInput("quant_data", label="please upload .csv/.xlsx file of quantitative data with quantitative intensities of terminis:",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv", ".xlsx"))),
        selectInput("heatmapvalue",
                    label="Choose the variable for Clustering",
                    choices=c("Enrichment Score", "Quantitative intensity"),
                    selected=NULL),
        checkboxInput("whetherNormal",
                      "Z-score normalization for clustering?"),
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
    
    dashboardBody(
        fluidRow(box(
            plotOutput("heatmap")  %>% withSpinner(color="#0dc5c1"))),
        fluidRow(box(
            uiOutput("logomaps")  %>% withSpinner(color="#0dc5c1")))
    )
)

server <- function(input, output) {
    resultpath<-reactive({
        paste0("/media/data1/RoLiM/media/", input$jobID,"/", input$title,"/")
    })
    samplelist<-reactive({
        list.files(path=resultpath())
    })
    output$checkSamples<-renderUI({
        checkboxGroupInput("checkSamples", 
                           label="Check the samples to be included in analysis", 
                           choiceNames=as.list(samplelist()),
                           choiceValues=as.list(samplelist()))
    })
    all.pattern <- reactive({
        find_all_pattern(resultpath(),input$checkSamples)
    })
    pattern.summary.list <- reactive({
        ls<-list()
        i=1
        for(i in samplelist()){
            temp.tab<-read.csv(paste0(resultpath(), i, "/c4_05_2_3f/patterns/_pattern_summary_table.csv"), header=TRUE,sep=",")[,-1]
            temp.tab$Pattern<-as.character(temp.tab$Pattern)
            temp.tab$Enrichment..Sample.Frequency...Background.Frequency.<-as.numeric(temp.tab$Enrichment..Sample.Frequency...Background.Frequency.)
            ls[[i]]<-temp.tab
        }
        ls
    })
    quant_data<-reactive({
        req(input$quant_data)
        if(grepl(".xlsx",input$quant_data$name)){
            df <- read.xlsx(input$quant_data$datapath)}
        #match_checked_samples(df, input$checkSamples)
        df
    })
    output$heatmap <- renderPlot({
        validate(need(length(input$checkSamples) > 1, 
                      "Please select more than one samples"))
        if (input$whetherUpload==FALSE){
            generate_heatmap(resultpath(),pattern.summary.list(),input$checkSamples, input$heatmapvalue, input$whetherNormal)}else{
                validate(need(input$quant_data, 
                              "Please input your quantitative intensity data"))
                generate_heatmap(resultpath(),pattern.summary.list(),input$checkSamples, input$heatmapvalue, input$whetherNormal, quant_data())
            }
    })
    output$logomaps<-renderUI({
        validate(need(length(input$checkSamples) > 1, 
                      "Please select more than one samples"))
        lapply(all.pattern(),function(x){
            output[[paste0("Plotfor",x)]]<-renderPlot({generate_logomap(resultpath(),x, input$checkSamples, input$logomapmethod, input$ticktype)})
        })
        lapply(all.pattern(),function(x){
            output[[paste0("Tablefor",x)]]<-renderTable({generate_summarytable(pattern.summary.list(),x, input$checkSamples)})
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

shinyApp(ui, server)
