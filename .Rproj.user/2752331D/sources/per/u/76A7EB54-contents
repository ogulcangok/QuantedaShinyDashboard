library(shiny)
library(quanteda)
library(readtext)
library(SnowballC)
library(ggplot2)
library(dplyr)
library("spacyr")
library(DT)
library(shinyBS)
library(lubridate)
library(tidyverse)
library(shinycssloaders)


Sys.setlocale('LC_ALL', 'C')
options(shiny.maxRequestSize=40*1024^2)



# Define UI for data upload app ----
ui <- fluidPage(# App title ----
                titlePanel("Insert App Name Here"),
                #Horizontal line ----
                tags$hr(),
                #Creating the main panel
                mainPanel(
                  
                  #Adding a file input. This input can get multiple files and file types text or csv.
                  
                  
                  column(12,
                         column(4,
                                fileInput("file1","Choose CSV File",multiple = TRUE,accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")))
                         
                  ),
                  
                  
                  #Creating First main tab DATA.
                  tabsetPanel(
                    tabPanel("Data",
                             
                             textInput("notes", "Add Corpus Notes"),#Add a text input for adding some notes to metadata
                             column(12,#create a column with a width 12 and assign each 3 widths an action button and two checkbox
                                    column(2, actionButton("addNotes", "Add Notes")),
                                    column(3, checkboxInput("metadisp", "Display Metadata", value = FALSE)),
                                    column(3, checkboxInput("summdisp", "Display Summary", value = FALSE))
                             ),
                             
                             conditionalPanel(#checks if metadisp or summdisp is selected. If so shows the corresponding data
                               condition = "input.metadisp == true",
                               h1("Metadata"),
                               tableOutput("metaInfo")),
                             conditionalPanel(
                               
                               condition = "input.summdisp == true",
                               h1("Summary"),
                               
                               withSpinner( DT::dataTableOutput("summary"))
                               
                               
                             ),
                             tableOutput("contents")
                             
                    ),
                    
                    tabPanel("Explore",#Second main tab EXPLORE.
                             
                             
                             tabsetPanel(#Explore has many subtabs. Data tab shows the top and blow 10 of the given filter.
                               
                               tabPanel("Graph",#Graph tab gives the boxplot of the given filter.
                                        column(12,
                                               column(5,selectInput("plotMenu","Select corpus filter",multiple = F,selectize = F,choices = c("Tokens", "fk"))),
                                               column(5,selectInput("plotMenuCategory","Select Category",multiple = F,selectize = F,choices = c("year","month")))),
                                        actionButton("boxShow","Show Plots"),
                                        #Select a filter from a combobox
                                        withSpinner( plotOutput("plotToken",dblclick = "plotTokenDbl"))
                               ),
                               
                               
                               tabPanel("Concordence",#Concordence tab draws a graph, which gives info about a word in all corpuses.
                                        tabsetPanel(
                                          tabPanel("Lexical Plot",
                                                   
                                                   selectInput("selectYear","Select Year",multiple = F,selectize = F,choices = ""),
                                                   
                                                   tableOutput("showSubset"),
                                                   textInput("keyWord", "Enter Your Key Word", width = 150),
                                                   textInput("keyWord2", "Enter Your Key Word", width = 150),
                                                   actionButton("lexicalShow","Show Plots"),
                                                   
                                                   withSpinner(plotOutput("concordencePlot",dblclick = "concordencePlotDbl"))),
                                          tabPanel("Text Data",
                                                   
                                                   column(3,textInput("saveName","Enter The Subset Name")),
                                                   
                                                   column(3,downloadButton("save", "Save Subset")),
                                                   textInput("concoInput","Enter a Key Word"),
                                                   actionButton("showConcordence","Calculate"),
                                                   withSpinner(tableOutput("concordenceTextOut")))
                                        ))
                               
                             )),
                    tabPanel("DFM",
                             tabsetPanel(
                               tabPanel("Plots",
                                        column(12,
                                               column(8,selectInput("plotSelect","Select Plot Type",c("Text Cloud","Frequency"),selected = NULL))
                                        ),
                                        actionButton("showTextPlot","Show Plot"),
                                        withSpinner( plotOutput("dfmPlot",dblclick = "dfmPlotDbl",width = 1000,height = 1000)  )      
                               ),
                               
                               tabPanel("Grouping",
                                        column(4,
                                               selectInput("groupSelect","Select Grouping Type",choices = c(""))),
                                        column(4,
                                               selectInput("plotSelectGroup", "Select Graph Type", c("Text Plot", "Baloon Plot"))),
                                        actionButton("groupPlotButton","Show Plots"),
                                        
                                        checkboxInput("showSort","Show Info"),
                                        conditionalPanel(
                                          condition = "input.showSort == true",
                                          h1("Sort Summary"),
                                          withSpinner( tableOutput("sort"))),
                                        withSpinner( plotOutput("groupPlot",dblclick = "groupPlotDbl",width = 1000,height = 1000))
                                        
                               ),
                               tabPanel("Frequency",
                                        h3("If the frequency is 0 it will give you your keyword's frequency."),
                                        column(11,
                                               column(6,
                                                      sliderInput("frequencyInput","Change the number of the top frequent words",min = 0,max = 30,value = 0)),
                                               column(5,
                                                      textInput("frequencyKeyWord","Enter Your Own Key Word"))),
                                        
                                        
                                        
                                        plotOutput("frequencyPlot",dblclick ="frequencyPlotDbl" )
                               ),
                               tabPanel("Keyness",
                                        
                                        column(4,
                                               selectInput("keynessYear1","Select Year","")),
                                        column(4,
                                               selectInput("keynessYear2", "Select Year", "")),
                                        withSpinner(plotOutput("keynessPlot",dblclick = "keynessPlotDbl"))),
                               tabPanel("Dictionary",
                                        
                                        fileInput("file2","Upload Your Dictionary",multiple = FALSE,accept = ".dic"),
                                        
                                        
                                        withSpinner( plotOutput("dictPlot",dblclick = "dictPlotDbl"))),
                               
                               tabPanel("Clustering",
                                        
                                        column(5,selectInput("clusterSelect","Select Filter",c("documents","features"))),
                                        column(5,selectInput("methodSelect","Select Method",c("Jaccard","cosine"))),
                                        textAreaInput("wordRemove","Enter The Words You Want To Remove"),
                                        actionButton("clusterGo","Show Plot"),
                                        
                                        withSpinner(plotOutput("clustering",dblclick = "clusteringDbl",width = 1500,height = 1000))),
                               tabPanel("Correspondence Analysis",
                                        
                                        selectInput("corrSelect","Select Grouping Type",choices = c("")),
                                        actionButton("corrPlot","Show Plot"),
                                        withSpinner( plotOutput("topic",dblclick = "topicDbl",))
                                        
                               )
                               
                               
                               
                             )        
                             
                             
                    )
                    
                    
                    
                    
                  )#end of main tabsetPanel
                  
                  
                ))#end of main layout


# Define server logic to read selected file ----
server <- function(input, output,session) {
  #Througout the server reactive is used maby times. Reactive stands for static in java. Simply it enables us to use the function anywhere we want.
  #All functions are from the downloaded packages. (non vanilla r)
  options(shiny.maxRequestSize=100*1024^2)
  
  modalPlot <- modalDialog("Token", size = "l", plotOutput("plotTokenModal"))
  modalConcor <- modalDialog("Plot",size = "l", plotOutput("modalConcorOut"))
  modalDfm <- modalDialog("Plot",size = "l",plotOutput("modalDfmOut"))
  modalGroup <-  modalDialog("Plot",size = "l",plotOutput("modalGroupOut"))
  modalFreq <- modalDialog("Plot",size = "l",plotOutput("modalFreqOut"))
  modalKey <- modalDialog("Plot",size = "l",plotOutput("modalKeyOut"))
  modalDict <- modalDialog("Plot",size = "l",plotOutput("modalDictOut"))
  modalCluster <- modalDialog("Plot",size = "l", plotOutput("modalClusterOut"))
  modalTopic <- modalDialog("Plot",size = "l",plotOutput("modalTopicOut"))
  
  
  
  
  
  readData <- reactive({#Main function to read data from the csv
    
    aidata <- readtext(input$file1$datapath,text_field = "Text")
    
    
    
    
    aidata <- aidata%>%separate(Date,c("day","month","year"),remove = F)
    
    
    
    
    
    
    aidata
    
    
  })
  
  updateLists <- reactive({
    updateSelectInput(inputId = "groupSelect",session  ,choices = colnames(readData() ))
    updateSelectInput(inputId = "selectYear",session  ,choices = sort(unique(readData()$year)))
    updateSelectInput(inputId = "keynessYear1",session  ,choices = sort(unique(readData()$year)) )
    updateSelectInput( inputId = "corrSelect",session,choices = colnames(readData() ))
    
    observe({
      listYear <- sort(unique(readData()$year))
      ch1 <- input$keynessYear1
      ch2 <- setdiff(listYear,ch1)
      
      
      
      updateSelectInput(inputId = "keynessYear2",session  ,choices = ch2)
    })
    updateSelectInput(inputId = "clusterYear",session  ,choices = sort(unique(readData()$year)))
    updateSelectInput(inputId = "similarityYear",session  ,choices = sort(unique(readData()$year)))
  })
  
  
  
  
  createCorp <- reactive({#creating the corpus from the read data.
    aicorp <- corpus(readData())
    
    aicorp
  })
  
  output$contents <- renderTable({#outputting the csv file as a table
    req(input$file1)
    readData()
    
  })
  observeEvent(input$addNotes, {#action Handler for adding notes the to metada 
    aicorp <- createCorp()
    metacorpus(aicorp, "notes") <- input$notes
    #aicorp
    output$metaInfo <- renderTable(metacorpus(aicorp, "notes"))
  })
  
  calculateRead <- reactive({#this is reactive because output of this func. will be used througout the whole server.
    aicorp <- createCorp()
    fk <- textstat_readability(aicorp, "Flesch.Kincaid")
    docvars(aicorp, "fk") <- fk
    tokenInfo <-summary(aicorp, showmeta = T)
    tokenInfo
  })
  
  showBox <- eventReactive(input$boxShow,{
    if (input$plotMenu == "Tokens" & input$plotMenuCategory == "year") {
      
      ggplot(calculateRead(), aes(x = as.factor(year), y = Tokens)) + geom_boxplot()
      
      
      
    }
    else if (input$plotMenu == "Tokens" & input$plotMenuCategory == "month") {
      
      ggplot(calculateRead(), aes(x = as.factor(month), y = Tokens)) + geom_boxplot()
      
    }
    else if (input$plotMenu == "fk" & input$plotMenuCategory == "year"){
      
      ggplot(calculateRead(), aes(x = as.factor(year), y = fk)) + geom_boxplot()
    }
    
    else {
      ggplot(calculateRead(), aes(x = as.factor(month), y = fk)) + geom_boxplot()
    }
    
    
    
  })
  
  
  
  output$plotToken <- renderPlot({#and drawing the plot of the calculateHead()
    showBox()
  })
  
  
  observeEvent(input$plotTokenDbl,{
    
    if (input$plotMenu == "Token")
    {
      output$plotTokenModal <- renderPlot(ggplot(calculateRead(), aes(x = as.factor(year), y = Tokens)) + geom_boxplot())}
    else{output$plotTokenModal <- renderPlot(ggplot(calculateRead(), aes(x = as.factor(year), y = fk)) + geom_boxplot())}
    showModal(modalPlot)
    
  })
  
  createSubset <- reactive({#reactive method to create subsets
    updateLists()
    aiSet <- corpus_subset(createCorp(), year == input$selectYear)
    aiSet
  })
  
  output$showSubset <- renderPrint({
    createSubset() 
  })
  
  concoText <- eventReactive(input$showConcordence,{
    aicorp <- createCorp()
    kwic(aicorp, input$concoInput , valuetype = "regex")
    
  })
  
  
  output$concordenceTextOut <- renderTable(
    {
      
      concoText()
    }
  )
  
  createKW <- reactive({#concordence tab functions
    options(width = 200)
    scikw <- kwic(createSubset(), input$keyWord)
    scikw
  })
  
  concoPlot <- eventReactive(input$lexicalShow,{
    
    if (input$keyWord2 == "")
    {
      textplot_xray(createKW(), sort = T)
    }
    else
    {
      textplot_xray(
        kwic(createSubset(), input$keyWord),
        kwic(createSubset(), input$keyWord2),
        sort = T
      ) +
        aes(color = keyword) + scale_color_manual(values = c("blue", "red"))
    }
    
  })
  
  output$concordencePlot <- renderPlot({#graphs of the concordence tab
    
    concoPlot()
    
  })
  
  
  observeEvent(input$concordencePlotDbl,{
    
    if (input$keyWord2 == "")
    {
      output$modalConcorOut <- renderPlot( textplot_xray(createKW(), sort = T))
    }
    else
    {
      output$modalConcorOut <- renderPlot(
        textplot_xray(
          kwic(createSubset(), input$keyWord),
          kwic(createSubset(), input$keyWord2),
          sort = T
        ) +
          aes(color = keyword) + scale_color_manual(values = c("blue", "red")))
    }
    showModal(modalConcor)
    
    
  })
  
  
  
  
  saveKW <- reactive({#reactive function to save the current subset as a .rda file to the working directory.
    
    
    KWsubset <-
      corpus_subset(createCorp(),
                    docnames(createCorp()) %in% createKW()$docname)#subsets the documents of which names match the kwic docs(home)
    KWsubset
  })
  
  
  
  output$save <- downloadHandler(
    
    filename = function() {
      paste(input$saveName,".rda")
    },
    content = function(file) {
      KWsubset <- saveKW()
      save(KWsubset, file = file)
    }
  )
  
  modalText <- modalDialog("Text View",size = "l", tableOutput("modalTextOut"))
  
  
  output$summary <-  DT::renderDataTable(selection = 'single',{#this func. is displaying the summary of the data.
    
    aicorp <- createCorp()
    
    s = input$summary_rows_selected
    if(length(s))
    {
      
      output$modalTextOut <- renderTable(texts(createCorp())[s])
      showModal(modalText)
      
      
      
      
      
    }
    fk <- textstat_readability(aicorp, "Flesch.Kincaid")
    docvars(aicorp, "fk") <- fk
    summary(aicorp)
    
    
    
    
  })
  
  createDFM <- reactive({
    
    ai.dfm <- dfm(createCorp(),remove = stopwords("SMART"),stem = T, remove_punct = T,remove_numbers =T)
    ai.dfm
    
    
  })
  
  createDFMW <- reactive({
    ai.dfmw <- dfm_weight(createDFM(),type = "tfidf")
    ai.dfmw
    
  })
  
  createDFMT <- reactive({
    
    ai.trim <- dfm_trim(createDFMW(),min_count = 100,max_count = 300,verbose = T)
  })
  
  getTopFeatures <- renderTable({
    
    topfeatures(createDFMW())
  })
  
  createTextCloud <- reactive({
    
    textplot_wordcloud(createDFMT(), max.words =Inf,  random.order = FALSE,
                       rot.per = .25, 
                       colors = RColorBrewer::brewer.pal(8,"Dark2"))
  })
  
  
  
  createFrequency <- reactive({
    aifr <- textstat_frequency(createDFMW(), n = 100)
    
  })
  
  frequencyDiag <- reactive({
    ggplot(createFrequency(), aes(x = feature, y = frequency)) +
      geom_point() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  groupPlots <- eventReactive(input$showTextPlot,{
    if(input$plotSelect == "Text Cloud" )
    {
      createTextCloud()
    }
    else if(input$plotSelect == "Frequency")
    {
      frequencyDiag()
      
    }
    
  })
  
  output$dfmPlot <- renderPlot({
    
    groupPlots()
    
  })
  
  observeEvent(input$dfmPlotDbl,{
    if(input$plotSelect == "Text Cloud")
    {
      output$modalDfmOut <- renderPlot(textplot_wordcloud(createDFMT(), max.words =Inf,  random.order = FALSE,
                                                          rot.per = .25, scale = c(0.9, 0.9), 
                                                          colors = RColorBrewer::brewer.pal(8,"Dark2")))
    }
    else
    {output$modalDfmOut <- renderPlot(ggplot(createFrequency(), aes(x = feature, y = frequency)) +
                                        geom_point() + 
                                        theme(axis.text.x = element_text(angle = 90, hjust = 1)))}
    showModal(modalDfm)
    
  })
  
  createGroup <- reactive({
    
    updateLists()
    
    ai.grp <- dfm(createCorp(), groups = input$groupSelect, remove = stopwords("SMART"),remove_numbers = TRUE, remove_punct = TRUE,stem = T)
    
    ai.grp
    
  })
  
  groupSort <- reactive ({
    
    dfm_sort(createGroup())[, 1:20]
  })
  output$sort <- renderTable({
    
    groupSort()
  })
  
  groupPlotShow <- eventReactive(input$groupPlotButton,{
    
    if(input$plotSelectGroup == "Text Plot")
    {
      ai.grp <- createGroup()
      dfm_sort(ai.grp)[, 1:20]
      textplot_wordcloud(ai.grp, comparison = T,max.words=400, title.size=2,random.order=FALSE, rot.per = .01)
    }
    else
    {
      
      aigr.trm <- dfm_trim(createGroup(), min_count = 500, verbose = T)
      dt <- as.table(as.matrix(aigr.trm))
      library("gplots")
      balloonplot(t(dt), main ="Words", xlab ="", ylab="",
                  label = FALSE, show.margins = FALSE,colsrt = 90,colmar = 5,rowmar = 5)
    }
    
  })
  
  output$groupPlot <- renderPlot({
    updateLists()
    groupPlotShow()
  })
  
  observeEvent(input$groupPlotDbl,{
    if(input$plotSelectGroup == "Text Plot")
    {
      output$modalGroupOut <- renderPlot(textplot_wordcloud(createGroup(), comparison = T,scale = c(0.9, 0.9)))
    }
    else
    {
      
      
      
      output$modalGroupOut <- renderPlot({ 
        aigr.trm <- dfm_trim(createGroup(), min_count = 500, verbose = T)
        dt <- as.table(as.matrix(aigr.trm))
        library("gplots")
        balloonplot(t(dt), main ="Words", xlab ="", ylab="",
                    label = FALSE, show.margins = FALSE)})
    }
    showModal(modalGroup)
    
  })
  
  filterTerm <- reactive({
    
    freq_grouped <- textstat_frequency(createDFMW(),n=input$frequencyInput,
                                       groups = "year")
    freq_grouped
    
  })
  
  filterKW <- reactive({
    fregr <- subset(filterTerm(), feature %in% input$frequencyKeyWord) 
    cat(input$frequencyKeyWord)
    fregr
    
  })
  
  output$frequencyPlot <- renderPlot({
    if(input$frequencyInput == 0)
    {
      freq_grouped <- textstat_frequency(createDFMW(),
                                         groups = "year")
      fregr <- subset(freq_grouped, feature %in% input$frequencyKeyWord) 
      ggplot(fregr,  aes(group, frequency)) +
        geom_point()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else
    {
      ggplot(data = filterTerm(), aes(x = nrow(filterTerm()):1, y = frequency)) +
        geom_point() +
        facet_wrap(~ group, scales = "free") +
        coord_flip() +
        scale_x_continuous(breaks = nrow(filterTerm()):1,
                           labels = filterTerm()$feature) +
        labs(x = NULL, y = "Relative frequency")
      
      
    }
    
    
  })
  
  observeEvent(input$frequencyPlotDbl,{
    
    if(input$frequencyInput == 0)
    {
      
      output$modalFreqOut <- renderPlot({
        freq_grouped <- textstat_frequency(createDFMW(),
                                           groups = "year")
        fregr <- subset(freq_grouped, feature %in% input$frequencyKeyWord) 
        ggplot(fregr,  aes(group, frequency)) +
          geom_point()+
          theme(axis.text.x = element_text(angle = 90, hjust = 1))})
    }
    else
    {
      output$modalFreqOut <- renderPlot({
        ggplot(data = filterTerm(), aes(x = nrow(filterTerm()):1, y = frequency)) +
          geom_point() +
          facet_wrap(~ group, scales = "free") +
          coord_flip() +
          scale_x_continuous(breaks = nrow(filterTerm()):1,
                             labels = filterTerm()$feature) +
          labs(x = NULL, y = "Relative frequency")})
      
      
    }
    showModal(modalFreq)
    
    
  })
  
  
  
  output$keynessPlot <- renderPlot({
    updateLists()
    ai.sub <- corpus_subset(createCorp(), 
                            year %in% c(input$keynessYear1, input$keynessYear2))
    
    ai.subdfm <- dfm(ai.sub, groups = "year", remove = stopwords("english"), 
                     remove_punct = TRUE,stem=T)
    result_keyness <- textstat_keyness(ai.subdfm, target = input$keynessYear1)
    
    
    textplot_keyness(result_keyness) 
    
  })
  
  observeEvent(input$keynessPlotDbl,{
    updateLists()
    output$modalKeyOut <- renderPlot({
      ai.sub <- corpus_subset(createCorp(), 
                              year %in% c(input$keynessYear1, input$keynessYear2))
      
      ai.subdfm <- dfm(ai.sub, groups = "year", remove = stopwords("english"), 
                       remove_punct = TRUE,stem=T)
      result_keyness <- textstat_keyness(ai.subdfm, target = input$keynessYear1)
      
      
      textplot_keyness(result_keyness) 
      
      
    })
    
    showModal(modalKey)
    
  })
  
  createDict <- reactive({
    
    dict <- input$file2$datapath
    
    
    myDict <- dictionary(file= dict, format = "LIWC")
    
    ai.dict <- dfm(createCorp(), groups = "year",  remove = stopwords("english"), remove_punct = TRUE, dictionary =myDict,stem = T)
    topfeatures(ai.dict)
    
    dt <- as.table(as.matrix(ai.dict))
    dt
  })
  
  output$dictPlot <- renderTable({
    createDict()
  })
  
  output$dictPlot <- renderPlot({
    
    library("gplots")
    balloonplot(t(createDict()), main ="", xlab ="", ylab="",
                label = FALSE, show.margins = F,colsrt = 60,colmar = 9)
    
    
  })
  
  observeEvent(input$dictPlotDbl,{
    
    output$modalDictOut <- renderPlot({
      
      library("gplots")
      balloonplot(t(createDict()), main ="Words", xlab ="", ylab="",
                  label = FALSE, show.margins = FALSE,colsrt = 60,colmar = 9)
      
      
    })
    showModal(modalDict)
  })
  
  
  drawClus <- eventReactive(input$clusterGo,{
    
    if(input$clusterSelect == "documents")
    {
      
      quantdfm <- dfm(createCorp(), 
                      remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("SMART"), stem = TRUE)
      ai.dfmw <- dfm_weight(quantdfm, type ="tfidf" )
      ai.trm <- dfm_trim(ai.dfmw, min_count = 100, max_count = 200,  verbose = T)
      
      wordDfm <- dfm_sort(ai.dfmw)
      
      wordDfm <- dfm_remove(wordDfm, c(input$wordRemove))# to remove the words(for a second round, we need to add a tab for that) change the words with text input
      wordDfm <-(wordDfm)[1:100, ]  #remove t (transpose) for document clustering
      
      library(proxy)
      d <- simil(as.matrix(wordDfm), method=input$methodSelect)
      
      library(MASS)
      library(dendextend)
      wordCluster <- hclust(d, method = "ward.D")
      plot(wordCluster, cex = .75,labels = docnames(wordDfm))
      
      
    }
    else{
      
      quantdfm <- dfm(createCorp(), 
                      remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("SMART"), stem = TRUE)
      ai.dfmw <- dfm_weight(quantdfm, type ="tfidf" )
      ai.trm <- dfm_trim(ai.dfmw, min_count = 100, max_count = 200,  verbose = T)
      
      wordDfm <- dfm_sort(ai.dfmw)
      
      wordDfm <- dfm_remove(wordDfm, c(input$wordRemove))# to remove the words(for a second round, we need to add a tab for that)
      wordDfm <-t(wordDfm)[1:100, ]  #remove t (transpose) for document clustering
      
      library(proxy)
      d <- simil(as.matrix(wordDfm), method=input$methodSelect)
      
      library(MASS)
      library(dendextend)
      wordCluster <- hclust(d, method = "ward.D")
      plot(wordCluster, cex = .75,labels = docnames(wordDfm))
    }
    
  })
  
  
  output$clustering <- renderPlot({
    
    updateLists()
    drawClus()
    
    
  })
  
  
  
  observeEvent(input$clusteringDbl,
               {
                 
                 output$modalClusterOut <- renderPlot({
                   
                   updateLists()
                   if(input$clusterSelect == "documents")
                   {
                     
                     quantdfm <- dfm(createCorp(), 
                                     remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("SMART"), stem = TRUE)
                     ai.dfmw <- dfm_weight(quantdfm, type ="tfidf" )
                     ai.trm <- dfm_trim(ai.dfmw, min_count = 100, max_count = 200,  verbose = T)
                     
                     wordDfm <- dfm_sort(ai.dfmw)
                     wordDfm <- dfm_remove(wordDfm, "jia")# to remove the words(for a second round, we need to add a tab for that)
                     wordDfm <-(wordDfm)[1:100, ]  #remove t (transpose) for document clustering
                     
                     library(proxy)
                     d <- simil(as.matrix(wordDfm), method=input$methodSelect)
                     
                     library(MASS)
                     library(dendextend)
                     wordCluster <- hclust(d, method = "ward.D")
                     plot(wordCluster, cex = .75,labels = docnames(wordDfm))
                     
                     
                   }
                   else{
                     
                     quantdfm <- dfm(createCorp(), 
                                     remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("SMART"), stem = TRUE)
                     ai.dfmw <- dfm_weight(quantdfm, type ="tfidf" )
                     ai.trm <- dfm_trim(ai.dfmw, min_count = 100, max_count = 200,  verbose = T)
                     
                     
                     wordList <- c(input$wordRemove)
                     wordDfm <- dfm_sort(ai.dfmw)
                     wordDfm <- dfm_remove(wordDfm, c(input$wordRemove))# to remove the words(for a second round, we need to add a tab for that)
                     wordDfm <-t(wordDfm)[1:100, ]  #remove t (transpose) for document clustering
                     
                     library(proxy)
                     d <- simil(as.matrix(wordDfm), method=input$methodSelect)
                     
                     library(MASS)
                     library(dendextend)
                     wordCluster <- hclust(d, method = "ward.D")
                     plot(wordCluster, cex = .75,labels = docnames(wordDfm))
                   }
                   
                   
                 })
                 
                 showModal(modalCluster)
               })
  
  
  drawCorr <- eventReactive(input$corrPlot,{
    
    
    quantdfm <- dfm(createCorp(), groups = input$corrSelect,
                    remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("SMART"), stem = TRUE)
    aigr.trm <- dfm_trim(quantdfm, min_count = 300, verbose = T)
    
    dt <- as.table(as.matrix(aigr.trm))
    library("FactoMineR")
    library(factoextra)
    res.ca <- CA(dt, graph = F)
    fviz_ca_biplot(res.ca, repel = TRUE)
    
    
  })
  
  
  output$topic <- renderPlot({
    updateLists()
    drawCorr()
    
  })
  
  
  observeEvent(input$topicDbl,{
    updateLists()
    output$modalTopicOut <- renderPlot({
      
      quantdfm <- dfm(createCorp(), groups = input$corrSelect,
                      remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("SMART"), stem = TRUE)
      aigr.trm <- dfm_trim(quantdfm, min_count = 300, verbose = T)
      
      dt <- as.table(as.matrix(aigr.trm))
      library("FactoMineR")
      library(factoextra)
      res.ca <- CA(dt, graph = F)
      fviz_ca_biplot(res.ca, repel = TRUE)
      
      
      
    })
    
    showModal(modalTopic)
    
  })
  
  
}
# Create Shiny app ----
shinyApp(ui, server)
