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
library(SocialMediaLab)
library(RMongo)
library(jsonlite)
options(shiny.maxRequestSize=30*1024^2)
options(shiny.sanitize.errors = F)
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
  
  readData <- reactive({
    
    parsedCSV <- read.csv(input$file4$datapath)
    updateSelectInput(inputId = "csvFilter",session,choices = unique(parsedCSV$entity_type))
    updateSelectInput(inputId = "csvFilter2",session,choices = unique(parsedCSV$entity_type))
    updateSelectInput(inputId = "textFilter",session,choices = c("all",sort(unique(parsedCSV$doc_id))))
    updateSelectInput(inputId = "textFilter2",session,choices = c("all",sort(unique(parsedCSV$doc_id))))
    parsedCSV
    
  })
  # getJson <- eventReactive(input$querySend,{
  #   
  #   cmd <-   paste0("mongoexport -d newsarchive -c wired -q '{ $text: { $search: \"",input$dbQuery,"\" } }' --jsonArray --out ~/Documents/tempData.json")
  #   json <- system(cmd)
  #   
  #   json
  #   
  # })
  
  observeEvent(input$querySend,{
    cmd <-   paste0("mongoexport -d newsarchive -c wired -q '{ $text: { $search: \"",input$dbQuery,"\" } }' --jsonArray --out ~/Documents/tempData.json")
    system(cmd,wait = T)
    
  })
  
  readDataFromCsv <- reactive({#Main function to read data from the csv
    
    if(input$csv == T)
    {
      aidata <- readtext(input$file1$datapath)
      
      aidata <- aidata%>% separate(Date,c("day","month","year"),remove = F)
    }
    
    
    
    if (input$json== T)
    {
      
      aidata <- jsonlite::fromJSON("tempData.json")
      colnames(aidata)[grepl("date",colnames(aidata))] <- "Date"
      colnames(aidata)[grepl("content",colnames(aidata))] <- "Text"
      aidata$Date <- as.Date(aidata$Date)
      aidata <- aidata%>% separate(Date,c("year","month","day"),remove = F)
    }
    
    aidata
  })
  
  createCorpus <- reactive({
    
    withProgress(message = "Creating corpus",{
      incProgress(0.1,"Parsing Dates")
      Sys.sleep(0.6)
      incProgress(0.1,"Separating day,month,year...")
      Sys.sleep(0.6)
      incProgress(0.1,"Counting tokens...")
      Sys.sleep(0.6)
      incProgress(0.1,"Calculating readability...")
      Sys.sleep(0.6)
      incProgress(0.6,"Finishing...")
      Sys.sleep(0.6)
      aidata <- readDataFromCsv()
      aidata$Text <- as.character(aidata$Text)
      aicorp <- corpus(aidata,text_field = "Text")
      metacorpus(aicorp, "notes") <- input$notes
      readability <- textstat_readability(aicorp, input$readSelection)
      docvars(aicorp, input$readSelection) <- readability
      Sys.sleep(0.6)
      incProgress(10,"Done!")
      Sys.sleep(0.6)
      
      
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
  
  
  filterEntity <- reactive({
    
    parsedCSV <- readData()
    if(input$textFilter == "all")
    {  
      
      a <-filter(parsedCSV,entity_type == input$csvFilter)
      
      a
    }
    
    
    else{
      a <-filter(parsedCSV,entity_type == input$csvFilter)
      name <- paste("text",input$textFilter,sep = "")
      b <- filter(a, doc_id == name)
      b
    }
    
  })
  
  
  output$csvOut <- renderTable({
    
    filterEntity()
    
  })
  
  output$csvInfo <- renderText({
    
    a <-sum(readData()$entity_type == input$csvFilter)
    a
    
  })
  
  csvPlot <- reactive({
    
    entity_type <- readData()$entity_type
    if(input$csvPlotOption1 == T)
    {
      
      if(input$textFilter2 == "all")
      {  
        
        
        ggplot(readData(),aes(x = as.factor(entity_type)))+ geom_bar()
      }
      else
      {
        name <- paste("text",input$textFilter2,sep = "")
        b <- filter(readData(), doc_id == name)
        
        ggplot(b , aes(x = as.factor(entity_type)))+ geom_bar()
      }
    }#end of plottype1
    
    else if(input$csvPlotOption2 == T)
    {
      
      
      a <-filter(readData(),entity_type == input$csvFilter2)
      ggplot(a,aes(x = as.factor(doc_id)))+ geom_bar()
      
      
      
      
    }
    
  })
  
  
  output$csvPlot <- renderPlot({
    csvPlot()
    
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
  
  
  
  
  output$saveRDA <- downloadHandler(
    
    filename = function() {
      paste(input$spacyName,Sys.Date(),sep = '')
    },
    content = function(file) {
      
      
      withProgress(message = "Spacy is in progress",{
        aicorp <- createCorp()
        spacy_initialize()
        incProgress(amount = 0.4)
        parsedtxt <- spacy_parse(aicorp) #configurations:pos;tag;entity; lemma; dependency. refer to help
        incProgress(amount = 10,message = "Done.")
        saveRDS(parsedtxt, file=file)#save the parsed corpus since it takes very long time
        spacy_finalize()
      }
      )
    })
  spacyOutput <- eventReactive(input$spacyShow,{
    spacy_initialize()
    
    parsedtxt <- readRDS(input$file3$datapath) 
    if(input$spacyOptions == "Entity Extract" )
    { 
      
      # spacy_initialize()
      a <- entity_extract(parsedtxt) 
      spacy_finalize()
      a
    }
    
    else if (input$spacyOptions == "Entity Consolidate" )
    {
      
      consen <- entity_consolidate(parsedtxt)
      #you can filter tokens according to their tags: 
      per <-filter(consen, entity_type=="EVENT")#change to pos==NOUN or tag ==NN
      pers <- group_by(per, lemma)
      pers <-summarise(pers)
      spacy_finalize()
      
      
      pers
    }
  })
  output$saveCSV <- downloadHandler(
    filename = function() {
      paste(input$csvName, ".csv", sep = "")
    },
    content = function(file) {
      spacy_initialize()
      parsedtxt <- readRDS(input$file3$datapath) 
      if(input$spacyOptions == "Entity Extract" )
      { 
        
        # spacy_initialize()
        a <- entity_extract(parsedtxt) 
        spacy_finalize()
        
        write.csv(a,file)
        
        
        
      }
      
      else if (input$spacyOptions == "Entity Consolidate" )
      {
        
        consen <- entity_consolidate(parsedtxt)
        #you can filter tokens according to their tags: 
        per <-filter(consen, entity_type=="EVENT")#change to pos==NOUN or tag ==NN
        pers <- group_by(per, lemma)
        pers <-summarise(pers)
        spacy_finalize()
        
        write.csv(pers,file)
      }
      
      
    }
    
  )
  
  output$spacyOut <- renderTable({ spacyOutput() })
  
  output$demoGo <- downloadHandler(
    
    filename = function() {
      paste("twitterData",sep = " ")
    },
    content = function(file) {
      apiKey <- "bIhOZeVfPfdXDxdHfWtUJ0up9"
      apiSecret <-  "Bd1WfuhYrZUpYE7ODGO65lnknvIUvoC35IJcvr4lRH3cOCbAHT"
      accessToken <- "947756430672711680-avkhywucJgaXhbOjiCd6aXGXVZRHISj"
      tokenSecret <- "EDiYOUcLix4j6rjPxrcUopQ0eASynASBjNpjH9o92GYY2"
      
      AuthenticateWithTwitterAPI(api_key = apiKey, api_secret = apiSecret, access_token = accessToken, access_token_secret = tokenSecret)
      a <-  CollectDataTwitter(searchTerm = input$demoText, numTweets = 500,verbose = T)
      twCorp <- corpus(a)
      
      saveRDS(twCorp,file)
      
      
      
      
    }
    
    
    
    
    
    
  )
  
  
  output$demoOut <- renderDataTable({
    
    #twitter()
  })
  
  
  
  
}#End of server
server

#shinyApp(server,ui)
