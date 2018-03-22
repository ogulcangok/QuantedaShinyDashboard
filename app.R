## app.R ##
library(shinydashboard)
library(quanteda)
library(readtext)
library(SnowballC)
library(ggplot2)
library(dplyr)
library(spacyr)
library(DT)
library(shinyBS)
library(anytime)
library(lubridate)
library(tidyverse)
library(shinycssloaders)
options(shiny.maxRequestSize=30*1024^2)


ui <- dashboardPage(
  dashboardHeader(title = "Quanteda UI"),
  dashboardSidebar(
    
    sidebarMenu(
      
      fileInput("file2","If you have already created your corpus, you can upload it here",multiple = TRUE,accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv",".rda",".rds",".RData")),
      
      
      menuItem("Welcome Page",tabName = "welcome",icon = icon("home")),
      menuItem("Corpus Creation",tabName = "corpus",icon = icon("book")),
      menuItem("Data", tabName = "dataTab", icon = icon("list-alt")),
      menuItem("Explore", tabName = "ExploreTab", icon = icon("search"),
               menuItem("Box Plot", tabName = "boxPlot",icon = icon("archive")),
               menuItem("Concordence Plot",tabName = "concor",icon = icon("bar-chart"),
                        menuSubItem("Lexical Plots","lex"),
                        menuSubItem("Text Data","tex"))
               
               
      ),
      menuItem("DFM", tabName = "dfmTab", icon = icon("cogs"),
               menuItem("Plots",tabName = "plots", icon = icon("bar-chart")),
               menuItem("Grouping",tabName = "grouping",icon = icon("object-group")),
               menuItem("Frequency",tabName = "frequency",icon = icon("line-chart")),
               menuItem("Keyness",tabName = "keyness",icon = icon("key")),
               menuItem("Dictionary",tabName = "dictionary",icon = icon("book")),
               menuItem("Clustering",tabName = "clustering",icon = icon("clone")),
               menuItem("Correspondence Analysis", tabName = "correspondence",icon = icon("area-chart")),
               menuItem("Spacy",tabName = "Spacy",icon = icon("flag"))
               
               
               
               
      )#end of dfm
      
      
      
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      
      
      
      tabItem(tabName = "welcome",
              htmlOutput("manual")
              
              
              
      ),
      
      
      
      
      tabItem(tabName = "corpus",
              box(title = "Upload your CSV",
                  fileInput("file1","Upload your csv for initialize the app.",multiple = TRUE,accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv",".rda",".rds",".RData")),
                  selectInput(inputId = "readSelection",label = "Select Readability Measure",choices = c("all", "ARI", "ARI.simple", "Bormuth",
                                                                                                         "Bormuth.GP", "Coleman", "Coleman.C2", "Coleman.Liau", "Coleman.Liau.grade",
                                                                                                         "Coleman.Liau.short", "Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK",
                                                                                                         "Danielson.Bryan", "Danielson.Bryan.2", "Dickes.Steiwer", "DRP", "ELF",
                                                                                                         "Farr.Jenkins.Paterson", "Flesch", "Flesch.PSK", "Flesch.Kincaid", "FOG",
                                                                                                         "FOG.PSK", "FOG.NRI", "FORCAST", "FORCAST.RGL", "Fucks", "Linsear.Write",
                                                                                                         "LIW", "nWS", "nWS.2", "nWS.3", "nWS.4", "RIX", "Scrabble", "SMOG", "SMOG.C",
                                                                                                         "SMOG.simple",      "SMOG.de", "Spache", "Spache.old", "Strain",
                                                                                                         "Traenkle.Bailer", "Traenkle.Bailer.2", "Wheeler.Smith", "meanSentenceLength",
                                                                                                         "meanWordSyllables")),
                  textInput("corpusName","Enter Corpus Name to save"),
                  
                  textInput("notes","Add Notes"),
                  
                  
                  downloadButton("createCorpus2","Create Corpus")
                  
              ),
              
              box(title = "Spacy Options",checkboxInput("spacyCheck","Option1"),actionButton("startSpacy","Start Spacy"))
              
              
              
      ),
      
      
      tabItem(tabName = "dataTab",
              fluidPage(
                
                box(title = "Summary",withSpinner(dataTableOutput("summary")),collapsible = T,width = 500),
                box(title = "Notes",tableOutput("metaInfo"))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "boxPlot",
              
              box("Box Plot",selectInput("plotMenu","Select Plot Filter",c("Tokens","Readability")),
                  selectInput("plotMenuCategory","Select Category",multiple = F,selectize = F,choices = c("year","month")),
                  actionButton("boxShow","Show Plots")),
              box("Graph",withSpinner(plotOutput("plotToken")),collapsible = T,width = 12)
      ),#end of boxPlot
      
      tabItem(tabName = "lex",
              
              box("Concordence",
                  selectInput("selectYear","Select Year",multiple = F,selectize = F,choices = "",selected = ""),
                  column(3,textInput("keyWord", "Enter Your Key Word", width = 150)),
                  column(3,textInput("keyWord2", "Enter Your Key Word", width = 150)),
                  actionButton("lexicalShow","Show Plots")),
              
              
              box("Graph",withSpinner(plotOutput("concordencePlot")),collapsible = T,width = 12)
              
      ),
      
      tabItem(tabName = "tex",
              
              box(textInput("saveName","Enter The Subset Name"),
                  
                  downloadButton("save", "Save Subset"),
                  textInput("concoInput","Enter a Key Word"),
                  actionButton("showConcordence","Calculate"),width = 12),
              box("Table",withSpinner(tableOutput("concordenceTextOut")),width = 12,collapsible = T)
              
              
      ),
      tabItem(tabName = "plots",
              
              box(selectInput("plotSelect","Select Plot Type",c("Text Cloud","Frequency"),selected = NULL),
                  actionButton("showTextPlot","Show Plot")),
              box( withSpinner( plotOutput("dfmPlot",dblclick = "dfmPlotDbl",width = 1000,height = 1000)),width = 12)
              
      ),#end of plot tab
      
      tabItem(tabName = "grouping",
              
              box( column(4,
                          selectInput("groupSelect","Select Grouping Type",choices = c(""))),
                   column(4,
                          selectInput("plotSelectGroup", "Select Graph Type", c("Text Plot", "Baloon Plot"))),
                   actionButton("groupPlotButton","Show Plots")),
              box(withSpinner( plotOutput("groupPlot",dblclick = "groupPlotDbl")),width = 12)
              
              
      ),#end of grouping
      
      tabItem(tabName = "frequency",
              box( h3("If the frequency is 0 it will give you your keyword's frequency."),
                   column(11,
                          column(6,
                                 sliderInput("frequencyInput","Change the number of the top frequent words",min = 0,max = 30,value = 0)),
                          column(5,
                                 textInput("frequencyKeyWord","Enter Your Own Key Word")))
                   ,width = 12),
              
              box(plotOutput("frequencyPlot",dblclick ="frequencyPlotDbl" ),width = 12)
              
      ),#end of frequency
      
      tabItem(tabName = "keyness",
              
              box(column(4,
                         selectInput("keynessYear1","Select Year","")),
                  column(4,
                         selectInput("keynessYear2", "Select Year", ""))),
              box(withSpinner(plotOutput("keynessPlot",dblclick = "keynessPlotDbl")),width = 12)
              
      ),#end of keyness
      
      tabItem(tabName = "dictionary",
              
              box( fileInput("file2","Upload Your Dictionary",multiple = FALSE,accept = ".dic")),
              box( withSpinner( plotOutput("dictPlot",dblclick = "dictPlotDbl")),width = 12)
              
              
      ),#end of dictionary
      
      tabItem(tabName = "clustering",
              
              box( column(5,selectInput("clusterSelect","Select Filter",c("documents","features"))),
                   column(5,selectInput("methodSelect","Select Method",c("Jaccard","cosine"))),
                   textAreaInput("wordRemove","Enter The Words You Want To Remove"),
                   actionButton("clusterGo","Show Plot")),
              box( withSpinner(plotOutput("clustering",dblclick = "clusteringDbl")),width = 12)
              
              
              
      ),#end of clustering
      
      tabItem(tabName = "correspondence",
              
              box(selectInput("corrSelect","Select Grouping Type",choices = c("")),
                  actionButton("corrPlot","Show Plot")),
              
              box( withSpinner( plotOutput("topic",dblclick = "topicDbl" )),width = 12)
              
              
      ),#end of corr
      
      tabItem(tabName = "Spacy",
              
              box(actionButton("startSpacy","Start Spacy"),downloadButton("saveRDA","Save the Current Session"),tableOutput("spacyOut")),
              box(tableOutput("rdaTable"))
              
              
              
      )
      
      
      
      
      
      
      
      
    )#end of dashboard body tabs
  )#end of dashboardbody
)#end of ui

server <- function(input, output,session) {
  
  
  
  output$manual <- renderPrint({
    includeMarkdown("manual.Rmd")
    
  })
  
  
  modalConcor <- modalDialog("Plot",size = "l", plotOutput("modalConcorOut"))
  modalDfm <- modalDialog("Plot",size = "l",plotOutput("modalDfmOut"))
  modalGroup <-  modalDialog("Plot",size = "l",plotOutput("modalGroupOut"))
  modalFreq <- modalDialog("Plot",size = "l",plotOutput("modalFreqOut"))
  modalKey <- modalDialog("Plot",size = "l",plotOutput("modalKeyOut"))
  modalDict <- modalDialog("Plot",size = "l",plotOutput("modalDictOut"))
  modalHelpGeneral <- modalDialog("Help",size= "m",htmlOutput("modalHelpGeneralOut"),easyClose = T)
  
  
  
  modalText <- modalDialog("Text View",size = "l", tableOutput("modalTextOut"))
  
  
  
  
  readDataFromCsv <- reactive({#Main function to read data from the csv
    
    
    aidata <- readtext(input$file1$datapath,text_field = "Text")
    
    
    
    aidata <- aidata%>% separate(Date,c("day","month","year"),remove = F)
    
    aidata
    
    
    
    
    
    
  })
  
  createCorpus <- reactive({
    
    withProgress(message = "Creating corpus",{
      incProgress(0.1,"Parsing Dates")
      Sys.sleep(3)
      incProgress(0.1,"Separating day,month,year...")
      Sys.sleep(3)
      incProgress(0.1,"Counting tokens...")
      Sys.sleep(3)
      incProgress(0.1,"Calculating readability...")
      Sys.sleep(3)
      incProgress(0.6,"Finishing...")
      Sys.sleep(3)
      aicorp <- corpus(readDataFromCsv())
      metacorpus(aicorp, "notes") <- input$notes
      readability <- textstat_readability(aicorp, input$readSelection)
      docvars(aicorp, input$readSelection) <- readability
      Sys.sleep(3)
      incProgress(10,"Done!")
      Sys.sleep(3)
      
      
    })
    
    aicorp
    
  })
  
  output$createCorpus2 <- downloadHandler(
    
    filename = function() {
      paste(input$corpusName,Sys.Date(),sep = '')
    },
    content = function(file) {
      
      
      saveRDS(createCorpus(), file=file)#save the parsed corpus since it takes very long time
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  createCorp <- reactive({
    
    aicorp <- readRDS(input$file2$datapath)
    aicorp
    
  })
  
  output$summary <-  DT::renderDataTable(selection = 'single',{#this func. is displaying the summary of the data.
    
    aicorp <- createCorp()
    
    s = input$summary_rows_selected
    if(length(s))
    {
      
      output$modalTextOut <- renderTable(texts(createCorp())[s])
      showModal(modalText)
      
      
      
      
      
    }
    
    summary(aicorp)
    
    
    
    
  })
  
  
  output$metaInfo <- renderTable(metacorpus(createCorp(), "notes"))
  
  
  updateLists <- reactive({
    updateSelectInput(inputId = "groupSelect",session  ,choices = colnames(createCorp()$documents ))
    updateSelectInput(inputId = "selectYear",session  ,choices = sort(unique(createCorp()$documents$year)))
    updateSelectInput(inputId = "keynessYear1",session  ,choices = sort(unique(createCorp()$documents$year)) )
    updateSelectInput( inputId = "corrSelect",session,choices = colnames(createCorp()$documents ))
    observe({
      listYear <- sort(unique(createCorp()$documents$year))
      ch1 <- input$keynessYear1
      ch2 <- setdiff(listYear,ch1)
      
      
      
      updateSelectInput(inputId = "keynessYear2",session  ,choices = ch2)
    })
    updateSelectInput(inputId = "clusterYear",session  ,choices = sort(unique(createCorp()$documents$year)))
    updateSelectInput(inputId = "similarityYear",session  ,choices = sort(unique(createCorp()$documents$year)))
  })
  
  
  
  calculateRead <- reactive({#this is reactive because output of this func. will be used througout the whole server.
    aicorp <- createCorp()
    readability <- textstat_readability(aicorp, input$readSelection)
    docvars(aicorp, "readability") <- readability
    tokenInfo <-summary(aicorp, showmeta = T)
    tokenInfo
  })
  
  modalPlot <- modalDialog("Token", size = "l", plotOutput("plotTokenModal"))
  
  showBox <- eventReactive(input$boxShow,{
    if (input$plotMenu == "Tokens" & input$plotMenuCategory == "year") {
      
      ggplot(calculateRead(), aes(x = as.factor(year), y = Tokens)) + geom_boxplot()
      
      
      
    }
    else if (input$plotMenu == "Tokens" & input$plotMenuCategory == "month") {
      
      ggplot(calculateRead(), aes(x = as.factor(month), y = Tokens)) + geom_boxplot()
      
    }
    else if (input$plotMenu == "Readability" & input$plotMenuCategory == "year"){
      
      ggplot(calculateRead(), aes(x = as.factor(year), y =  readability)) + geom_boxplot()
    }
    
    else {
      ggplot(calculateRead(), aes(x = as.factor(month), y =  readability)) + geom_boxplot()
    }
    
    
    
  })
  
  
  
  output$plotToken <- renderPlot({#and drawing the plot of the calculateHead()
    showBox()
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
    updateLists()
    concoPlot()
    
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
  
  observeEvent(input$startSpacy,{
    
    withProgress(message = "Spacy is in progress",{
      
      spacy_initialize()
      parsedtxt <- spacy_parse(createCorp()) #configurations:pos;tag;entity; lemma; dependency. refer to help
      incProgress(amount = 10)
      entity_extract(parsedtxt) #entitiy, entity type
      #multi-word entities into single "tokens":
      consen <- entity_consolidate(parsedtxt)
      #you can filter tokens according to their tags: 
      per <-filter(consen, entity_type=="EVENT")#change to pos==NOUN or tag ==NN
      pers <- group_by(per, lemma)
      pers <-summarise(pers)
      
      spacy_finalize()
      output$spacyOut <- renderTable({  pers })
      output$saveRDA <- downloadHandler(
        
        filename = function() {
          paste("parsedtxt",Sys.Date(),sep = '')
        },
        content = function(file) {
          
          
          saveRDS(parsedtxt, file=file)#save the parsed corpus since it takes very long time
        }
      )
      
    })
    
    
    
    
    
  })
  
  
  
  
}

shinyApp(ui, server)
