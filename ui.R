#---------------------------------------------------------------------#
#               Text Mining udpipe App                               #
#---------------------------------------------------------------------#


library(igraph)
library(ggraph)
# library(stringr)
library(wordcloud)
# library(wordcloud2)
library(udpipe)
# library(lattice)
library(dplyr)
library(shiny)
library(magrittr)
library(shinycssloaders)

shinyUI(
  fluidPage(
  
    titlePanel("Text Mining App - udpipe"),
  
    sidebarLayout( 
      
      sidebarPanel(  
        
              fileInput("file1", "Upload data (txt file)"),
              checkboxGroupInput("upos", "List of UPOS:",
                                 choices = c("adjective (ADJ)" = "ADJ",
                                   "noun(NOUN)" = "NOUN",
                                   "proper noun (PROPN)" = "PROPN",
                                   "adverb (ADV)" = "ADV",
                                   "verb (VERB)" = "VERB"
                                 ),
                                 selected = c("ADJ",
                                              "NOUN",
                                              "PROPN"
                                              )),
             checkboxGroupInput("ngrams", "Select ngram levels:",
                                choices = c("2" = 2,
                                            "3" = 3,
                                            "4" = 4
                                ),
                                selected = c(2,
                                             3,
                                             4
                                )),
             radioButtons(inputId = "filetype", label = "Select the file type(for downloading graph)", choices = list("png", "pdf"))
     ),   # end of sidebar panel
    
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                      tabPanel("Overview",
                               h4(p("Data input")),
                               p("This app supports only text (.txt) data file.",align="justify"),
                               p("Please refer to the link below for sample txt file."),
                               a(href="https://github.com/nikhlesh17/Text-Analytics/blob/master/ipl2.txt"
                                 ,"Sample data input file"),   
                               br(),
                               h4('How to use this App'),
                               p('To use this app, click on', 
                                 span(strong("Upload data (txt file)")),
                                 'and upload the txt data file. Then click on different tabs to look at different outputs.
                                 It may take few seconds for the tabs to display data or plots. '),
                               p(span(strong("Annotated Output")),'- This tab displays 100 rows of table of annotated documents. 
                                 It also has a download button using which the entire data can be downloaded as a CSV file.
                                 The data also changes based on the selection in the UPOS checkbox.'),
                               p(span(strong("Wordclouds")),'- This tab displays two wordclouds - first one based on only ',
                                 span(strong('NOUNS'),'while the second one based on only',span(strong('VERBS')))),
                               p(span(strong("Co-occurrences")),'- This tab displays a plot of top-30 co-occurrences 
                                 at document level using a network plot. The plot changes based on the selection made in UPOS
                                 checkbox. It also has a table displaying the top 20 ngrams. The data in the table changes based on
                                 ngrams checkbox.'),
                               p(),
                               p(span(strong("Please note that the Export data and graph facilities would work properly only when the 
                                             app is opened in browser. This is the default functionality. Although, the data can still 
                                             be exported but the file name and location would have to be provided."))),
                               p(),
                               p('Important R packages required include ',span(strong("Shiny, dplyr, udpipe, igraph, ggraph,
                                             wordcloud,shinycssloaders,magittr"))),
                               p(),
                               p(span(strong('Team Details--'))),
                               p('Nikhlesh Daga - 11910059'),
                               p('Kapil Mahajan - 11910074'),
                               p('Shailendra Kumar - 11910103')),
                  
                      tabPanel("Annotated output", 
                               dataTableOutput('x') %>% withSpinner(color="#0dc5c1"),
                               # Button
                               downloadButton("downloadData", "Download data as CSV")),
                      
                      tabPanel("Wordclouds",
                               plotOutput('wc_nouns') %>% withSpinner(color="#0dc5c1"),
                               plotOutput('wc_verbs')) ,
                      
                      tabPanel("Co-occurrences",
                               plotOutput('cooc') %>% withSpinner(color="#0dc5c1"),
                               downloadButton("download_file", "Download the plot"),
                               dataTableOutput('ngrams'))
        
      ) # end of tabsetPanel
          )# end of main panel
            ) # end of sidebarLayout
              )  # end if fluidPage
                ) # end of UI
  


