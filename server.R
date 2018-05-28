

shinyServer(function(input, output) {
# Read in the data file
 Dataset <- reactive({

   if (is.null(input$file1)) {   # locate 'file1' from ui.R

                 return(NULL) } else{

     Data <- as.data.frame(readLines(input$file1$datapath))
     colnames(Data) <- c('txt')
     Data$txt <- as.character(Data$txt)

     return(Data)
   }
 })
  
 
# Function to annotate the read-in data
annotated_doc <- function(){

  english_model = udpipe_load_model("C:/Users/ndaga/Documents/english-ud-2.0-170801.udpipe")
  # 
  annotated_output <- data.frame(udpipe_annotate(english_model, x = Dataset()$txt))
  annotated_output <- annotated_output %>%
                      select(-sentence)
  return(annotated_output)
} 


# Annotated output    
output$x = renderDataTable({
    annotated_output <- annotated_doc() %>% filter(upos %in% input$upos)
    return(annotated_output[1:100,])
})  

# Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {"annotated_output.csv"},
  content = function(file) {
    write.csv(annotated_doc() %>% filter(upos %in% input$upos), file)
  }
)

# Wordcloud for Nouns    
output$wc_nouns = renderPlot({ 
  # library(textrank)
  all_nouns <- annotated_doc() %>% subset(annotated_doc()$upos == "NOUN") 
  top_nouns <- txt_freq(all_nouns$lemma)  # txt_freq() calcs noun freqs in desc order
  set.seed(1234)
  wordcloud(words = top_nouns$key, freq = top_nouns$freq, min.freq = 1,
                    max.words=200, random.order=FALSE, rot.per=0.35,
                    colors=brewer.pal(8, "Dark2"))
  
})

# Wordcloud for Verbs    
output$wc_verbs = renderPlot({
  all_verbs <- annotated_doc() %>% subset(annotated_doc()$upos == "VERB") 
  top_verbs <- txt_freq(all_verbs$lemma)
  set.seed(1234)
  wordcloud(words = top_verbs$key, freq = top_verbs$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"))
})

coocplot <- function(){
  annotated_output <- annotated_doc() %>% filter(upos %in% input$upos) 
  cooc_gen <- cooccurrence(x = annotated_output$lemma,
                           relevant = annotated_output$upos %in% input$upos) 
  wordnetwork <- head(cooc_gen, 50)
  wordnetwork <- igraph::graph_from_data_frame(wordnetwork) 
  
  ggraph(wordnetwork, layout = "fr") +  
    
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "red") +  
    geom_node_text(aes(label = name), col = "darkblue", size = 4) +
    
    theme_graph(base_family = "Calibri") +  
    theme(legend.position = "top") +
    
    labs(title = "Co-occurrences")
  
}

coocplot2 <- function(){
  annotated_output <- annotated_doc() %>% filter(upos %in% input$upos) 
  cooc_gen <- cooccurrence(x = annotated_output$lemma,
                           relevant = annotated_output$upos %in% input$upos) 
  wordnetwork <- head(cooc_gen, 50)
  wordnetwork <- igraph::graph_from_data_frame(wordnetwork) 
  
  ggraph(wordnetwork, layout = "kk") +  
    
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "red") +  
    geom_node_text(aes(label = name), col = "darkblue", size = 4) +
    
    theme_graph(base_family = "Calibri") +  
    theme(legend.position = "top") +
    
    labs(title = "Co-occurrences")
  
}

# co-occurrence   
output$cooc = renderPlot({
  coocplot()
})

# ,extensions = 'Buttons', options = list(dom = 'Bfrtip',
#                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print')

## call the plot function when downloading the image
output$download_file <- downloadHandler(
  filename =  function() {
    paste("co-occurrences", input$filetype, sep=".")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    if(input$filetype == "png")
      png(file) # open the png device
    else
      pdf(file) # open the pdf device
    print(coocplot())
    dev.off()  # turn the device off

  }
)

# Ngrams
output$ngrams = renderDataTable({
  annotated_output <- annotated_doc() %>% filter(upos %in% input$upos)
  colloc_ngrams <- keywords_collocation(x = annotated_output,   # try ?keywords_collocation
                                      term = "token", 
                                     group = c("doc_id", "paragraph_id", "sentence_id"),
                                    ngram_max = 5) 
  colloc_ngrams <- colloc_ngrams %>% filter(ngram %in% input$ngrams)  %>% select(keyword,freq) 
  ngrams <- head(colloc_ngrams, 50) 
  return(ngrams)
  
})

})
