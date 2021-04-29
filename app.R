library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(openxlsx)
source("cleaning.R")
source("heatmap.R")
source("logomaps.R")

ui <- dashboardPage(
    dashboardHeader(title="RoLiMviz"),
    
    dashboardSidebar(
        textInput("jobID",label="Please input the string indicating job ID sent by email from RoLim:"),
        textInput("title",label="Please input the title of task:"),
        selectInput("InputFormat",
                    label="Select the format of foreground file uploaded to RoLiM:",
                    choices=c("Long format, with multiple samples", "Pre-aligned sequences only"),
                    selected=NULL),
        conditionalPanel("input.InputFormat=='Long format, with multiple samples'",
                         uiOutput("quant_data_selection")),
        uiOutput("checkSamples"),
        checkboxInput("whetherUpload",
                      "Upload extra quantitative files?"),
        conditionalPanel('input.whetherUpload==1',
                         fileInput("quant_data", label="please upload .csv/.xlsx file of quantitative data with quantitative intensities in long format:",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv", ".xlsx"))),
        selectInput("direction",
                    label="select the direction of clustering",
                    choices=c("row-wise","col-wise"),
                    selected=NULL),
        uiOutput("heatmapvalue"),
        checkboxInput("whetherNormal",
                      "Z-score normalization for clustering?"),
        checkboxInput("whetherlog2",
                      "log-2 scale for quantitative intensity?"),
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
        fluidRow(box(width=11,
            plotOutput("heatmap")  %>% withSpinner(color="#0dc5c1")
            )),
        fluidRow(box(width=11,
            uiOutput("logomaps")  %>% withSpinner(color="#0dc5c1")))
    )
)

server <- function(input, output, session){
    observe({
        query <- parseQueryString(session$clientData$url_search)
        updateTextInput(session, "jobID", value = query[["jobID"]])
        updateTextInput(session, "title", value = query[["title"]])
    })
    
    resultpath<-reactive({
    validate(need(input$jobID,"Please input task ID!"),
                 need(input$title, "Please input task title!"))
    paste0("/media/data1/RoLiM/media/", input$jobID,"/", input$title,"/")
    })

    samplelist<-reactive({
        temp<-list.files(path=resultpath())
        sl<-temp[temp!="summary"]
        if(!is.null(input$quant_data)){
            sl<-unique(quant_data()[,1])
        }
        sl
    })
    output$checkSamples<-renderUI({
        checkboxGroupInput("checkSamples", 
                           label="Check the samples to be included in analysis", 
                           choiceNames=as.list(samplelist()),
                           choiceValues=as.list(samplelist()))
    })
    output$heatmapvalue<-renderUI({
        cc<-c("Enrichment Score", "Quantitative intensity")[c(!input$direction=="col-wise",TRUE)]
            selectInput("heatmapvalue",
            label="Choose the variable for Clustering",
            choices=cc,
            selected=NULL)
    })
    
    pattern.summary.list <- reactive({
        ls<-list()
        i=1
        for(i in samplelist()){
            aa<-grep("_pattern_summary_table.csv",list.files(paste0(resultpath(), i, "/patterns/")))
            temp.tab<-read.csv(paste0(resultpath(), i, "/patterns/",list.files(paste0(resultpath(), i, "/patterns/"))[aa]), header=TRUE,sep=",")[,-1]
            temp.tab$Pattern<-as.character(temp.tab$Pattern)
            temp.tab$Enrichment..Sample.Frequency...Background.Frequency.<-as.numeric(temp.tab$Enrichment..Sample.Frequency...Background.Frequency.)
            ls[[i]]<-temp.tab
        }
        ls
    })
    
    all.pattern <- reactive({
        find_all_pattern(pattern.summary.list(),input$checkSamples)
    })
    
    quant_data_summary_table<-reactive({
        validate(need(input$title!="", ""))
        if (length(input$checkSamples)==1){
        df<-read.table(paste0(resultpath(),input$checkSamples,"/summary/",input$checkSamples,"_summary_table.txt"), fill=TRUE)
        }
        if (length(input$checkSamples)>1){
        df<-read.table(paste0(resultpath(),"summary/",input$title,"_summary_table.txt"), fill=TRUE)
        }
        cbind(df[,1:2],df[,unlist(lapply(df,is.double))])
    })
    
    output$quant_data_selection<-renderUI({
    checkboxGroupInput("colIntensity", 
                           label="Select the columns of quantitative intensities:", 
                           choiceNames=as.list(colnames(quant_data_summary_table())[-c(1:2)]),
                           choiceValues=as.list(colnames(quant_data_summary_table())[-c(1:2)]))
    })
    
    quant_data_summary_table_full<-reactive({
        validate(need(input$title!="", ""))
        if (length(input$checkSamples)==1){
            df<-read.table(paste0(resultpath(),input$checkSamples,"/summary/",input$checkSamples,"_summary_table.txt"), fill=TRUE)
        }
        if (length(input$checkSamples)>1){
            df<-read.table(paste0(resultpath(),"summary/",input$title,"_summary_table.txt"), fill=TRUE)
        }
        df
    })
    
    quant_data<-reactive({
        if(input$whetherUpload==TRUE){
        req(input$quant_data)
        if(grepl(".xlsx",input$quant_data$name)){
            df <- read.xlsx(input$quant_data$datapath)
        }
        #match_checked_samples(df, input$checkSamples)
        }else{
            if(input$direction=="row-wise"){
            if (length(input$colIntensity)>1){
            df <- cbind(quant_data_summary_table()[,1:2], apply(quant_data_summary_table()[,input$colIntensity],1,function(x) mean(x,na.rm=TRUE)))}
            if (length(input$colIntensity)==1){
            df <- cbind(quant_data_summary_table()[,1:2], quant_data_summary_table()[,input$colIntensity])
            }
            colnames(df)<-c("sample","sequence","intensity")
            }
            if(input$direction=="col-wise"){
            df <- transformdata(quant_data_summary_table(),input$checkSamples, input$colIntensity, quant_data_summary_table_full())
            colnames(df)<-c("sequence",colnames(df)[-1])
            }
        }
        df
    })

    output$heatmap <- renderPlot({
        if (input$direction=="row-wise"){
        validate(need(length(input$checkSamples) > 1, 
                      "Please select more than one samples"))
        validate(need(!is.null(quant_data()), 
                              "Please input your quantitative intensity data"))
        g<-generate_heatmap(resultpath(), pattern.summary.list(),input$checkSamples, 
                            input$heatmapvalue, input$whetherNormal, quant_data(),input$InputFormat,input$whetherlog2)}
        if (input$direction=="col-wise"){
        validate(need(length(input$colIntensity) > 1, 
                          "Please select more than one column of intensity"))
        g<-genrate_heatmap_col(quant_data(),input$colIntensity,input$whetherNormal,input$whetherlog2)
        }
        g
    })
    
    output$logomaps<-renderUI({
        validate(need(length(input$checkSamples) > 0, 
                      "Please select at least one sample"))
        lapply(all.pattern(),function(x){
            output[[paste0("Plotfor",x)]]<-renderPlot({generate_logomap(resultpath(), x, input$checkSamples, input$logomapmethod, input$ticktype, input$InputFormat)})
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
