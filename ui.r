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
ui <- dashboardPage(
  
  
  dashboardHeader(title = "Quanteda UI"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Welcome Page",tabName = "welcome",icon = icon("home")),
      
      menuItem("Corpus Operations",icon = icon("folder-open-o"),
               menuItem("Corpus Creation",tabName = "corpus",icon = icon("newspaper-o")),
               #menuItem("Database Conncetion",tabName = "database",icon=icon("database")),
               menuItem("Corpus Summary", tabName = "dataTab", icon = icon("list-alt")),
               menuItem("Explore Your Corpus", tabName = "ExploreTab", icon = icon("search"),
                        menuItem("Box Plot", tabName = "boxPlot",icon = icon("archive")),
                        menuItem("Concordence Plot",tabName = "concor",icon = icon("bar-chart"),
                                 menuSubItem("Lexical Plots","lex"),
                                 menuSubItem("Text Data","tex"))
                        
                        
               ),
               menuItem("Corpus Plots", tabName = "dfmTab", icon = icon("cogs"),
                        menuItem("Plots",tabName = "plots", icon = icon("bar-chart")),
                        menuItem("Grouping",tabName = "grouping",icon = icon("object-group")),
                        menuItem("Frequency",tabName = "frequency",icon = icon("line-chart")),
                        menuItem("Keyness",tabName = "keyness",icon = icon("key")),
                        menuItem("Dictionary",tabName = "dictionary",icon = icon("book")),
                        menuItem("Clustering",tabName = "clustering",icon = icon("clone")),
                        menuItem("Correspondence Analysis", tabName = "correspondence",icon = icon("area-chart"))
                        
                        
                        
                        
                        
               )#end of dfm
      )  ,
      
      menuItem("Spacy Operations",icon= icon("envelope-open-o"),
               
               
               menuItem("Spacy Initialize",tabName = "spacy",icon = icon("ra")),
               menuItem("Discover CSV",tabName = "csv",icon = icon("file-excel-o")),
               menuItem("CSV Plots",tabName = "csvPlot",icon = icon("bar-chart-o"))
               
               
               
      )
      #menuItem("Live Data Visulize Demo",tabName = "dataDemo",icon = icon("superpowers"))
      
      
      
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      
      
      
      tabItem(tabName = "welcome",
              htmlOutput("manual")
              
              
              
      ),
      
      
      
      
      tabItem(tabName = "corpus",
              box(title = "Choose Your Source",
                  checkboxInput("csv","I want to use my own csv"),checkboxInput("json","I want to use your database"),
                  conditionalPanel(
                    condition = "input.csv == true",  
                    fileInput("file1","Upload your csv for initialize the app.",multiple = TRUE,accept = " ")
                    
                    
                    
                    
                  ),
                  conditionalPanel(
                    condition = "input.json == true",
                    selectInput("collectionSelect","Select Source",choices = "Wired"),
                    textAreaInput("dbQuery",label = "Enter Your Query"),
                    actionButton("querySend","Send Query")
                    
                  )),
              
              box(title = "Corpus Options",
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
                  downloadButton("createCorpus2","Create Corpus and Download")
                  
              )
              
              
              
              
              
      ),
      
      
      
      tabItem(tabName = "spacy",
              tags$h3("This operation will take a long time. When it is done app will automaticly download the parsed text.  \n
                      "),
              box(title = "Spacy Options",width = 12,
                  
                  textInput("spacyName","Enter parsed data name"),
                  downloadButton("saveRDA","Parse and Download the Corpus")
              ),
              box(title = "Spacy Output",width = 12,
                  fileInput("file3","If you want to see the output, upload the parsed text file to the slot below",multiple = TRUE,accept = " "),
                  selectInput("spacyOptions",label = "Spacy Options",choices = c("Entity Extract","Entity Consolidate"),multiple = F),
                  checkboxInput("saveCSV","Save output as CSV",value = F),
                  conditionalPanel(
                    condition = "input.saveCSV == true",
                    textInput("csvName","Enter csv name"),
                    downloadButton("saveCSV","Save CSV")
                  ),
                  
                  actionButton("spacyShow","Run"),
                  
                  withSpinner(tableOutput("spacyOut")))
              ),
      
      
      tabItem(tabName = "csv",
              
              box(fileInput("file4","Upload the CSV output of Spacy to use it",multiple = F,accept = " "),
                  selectInput("textFilter","Choose the text",choices = " "),
                  selectInput("csvFilter","Choose the entity filter",choices = " ")
                  
              ),
              box(title = "Info About the CSV",
                  h5("Below is the number of the selected entity type"),
                  textOutput("csvInfo")),
              
              
              
              box(tableOutput("csvOut"),title = "CSV Table",width = 12)
      ),
      
      
      tabItem(tabName = "csvPlot",
              
              
              box("Options",
                  checkboxInput("csvPlotOption1","Quantity of entity types in each text"),
                  checkboxInput("csvPlotOption2","Quantity of a specific entity type in texts"),
                  
                  conditionalPanel(
                    condition = "input.csvPlotOption1 == true",
                    selectInput("Select the text no",inputId = "textFilter2",choices = "" )),
                  conditionalPanel(
                    condition = "input.csvPlotOption2 == true",
                    selectInput("Select the entity type",inputId = "csvFilter2",choices = "" ))
                  
              ),
              box("Plot",plotOutput("csvPlot"),width = 12)
              
      ),
      
      
      tabItem(tabName = "dataTab",
              fluidPage(
                
                
                
                box(fileInput("file2","If you have already created your corpus, you can upload it here",multiple = TRUE,accept = " "),
                    title = "Corpus",tableOutput("metaInfo"),width = 12),
                box(title = "Summary",withSpinner(dataTableOutput("summary")),collapsible = T,width = 12)
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "boxPlot",
              
              box(width = 12,"Box Plot",selectInput("plotMenu","Select Plot Filter",c("Tokens","Readability")),
                  selectInput("plotMenuCategory","Select Category",multiple = F,selectize = F,choices = c("year","month")),
                  actionButton("boxShow","Show Plots")),
              box("Graph",withSpinner(plotOutput("plotToken")),collapsible = T,width = 12)
      ),#end of boxPlot
      
      tabItem(tabName = "lex",
              
              box(width = 12,"Concordence",
                  selectInput("selectYear","Select Year",multiple = F,selectize = F,choices = "",selected = ""),
                  column(3,textInput("keyWord", "Enter Your Key Word", width = 150)),
                  column(3,textInput("keyWord2", "Enter Your Key Word", width = 150)),
                  actionButton("lexicalShow","Show Plots")),
              
              
              box("Graph",withSpinner(plotOutput("concordencePlot")),collapsible = T,width = 12)
              
      ),
      
      tabItem(tabName = "tex",
              
              box(#textInput("saveName","Enter The Subset Name"),
                  
                  #downloadButton("save", "Save Subset"),
                title = "Concordence View",
                  textInput("concoInput","Enter a Key Word"),
                  actionButton("showConcordence","Calculate"),width = 12),
              box("Table",withSpinner(tableOutput("concordenceTextOut")),width = 12,collapsible = T)
              
              
      ),
      tabItem(tabName = "plots",
              
              box(width = 12,selectInput("plotSelect","Select Plot Type",c("Text Cloud","Frequency"),selected = NULL),
                  actionButton("showTextPlot","Show Plot")),
              box( withSpinner( plotOutput("dfmPlot",dblclick = "dfmPlotDbl",width = 1000,height = 1000)),width = 12)
              
      ),#end of plot tab
      
      tabItem(tabName = "grouping",
              
              box(width = 12, column(4,
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
              
              box(width = 12,column(4,
                                    selectInput("keynessYear1","Select Year","")),
                  column(4,
                         selectInput("keynessYear2", "Select Year", ""))),
              box(withSpinner(plotOutput("keynessPlot",dblclick = "keynessPlotDbl")),width = 12)
              
      ),#end of keyness
      
      tabItem(tabName = "dictionary",
              
              box(width = 12, fileInput("file2","Upload Your Dictionary",multiple = FALSE,accept = ".dic")),
              box( withSpinner( plotOutput("dictPlot",dblclick = "dictPlotDbl")),width = 12)
              
              
      ),#end of dictionary
      
      tabItem(tabName = "clustering",
              
              box(width = 12, column(5,selectInput("clusterSelect","Select Filter",c("documents","features"))),
                  column(5,selectInput("methodSelect","Select Method",c("Jaccard","cosine"))),
                  textAreaInput("wordRemove","Enter The Words You Want To Remove"),
                  actionButton("clusterGo","Show Plot")),
              box( withSpinner(plotOutput("clustering",dblclick = "clusteringDbl")),width = 12)
              
              
              
      ),#end of clustering
      
      tabItem(tabName = "correspondence",
              
              box(width = 12,selectInput("corrSelect","Select Grouping Type",choices = c("")),
                  actionButton("corrPlot","Show Plot")),
              
              box( withSpinner( plotOutput("topic",dblclick = "topicDbl" )),width = 12)
              
              
      )#end of corr
      
      #* tabItem(tabName = "dataDemo",
      #      box(width = 12,downloadButton("demoGo","Go"),title = "Live Data Demo",
      #           textInput("demoText","Enter the search filter"),
      #          withSpinner(dataTableOutput("demoOut")))
      #  )
      
      
      
      
      
      
      
      
      
  )#end of dashboard body tabs
  )#end of dashboardbody
  
)#end of ui

return(ui)
