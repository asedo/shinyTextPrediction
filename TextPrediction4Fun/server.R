library(LaF)
library(NLP)
library(tm) 
library(stringi)
rm(list = ls())
#setwd("C:/Users/Arturo/Google Drive/Coursera/Capstone/ShinyApp/TextPrediction4Fun")
load("predictNextWord.Rdata")


shinyServer(
function(input, output) {
  output$summary <- renderText({
  
  
    process.string = input$textvar
    
    if(getLastChar(process.string) == " ") {lc.space = 1} else {lc.space = 0}
    # lc.space represents if the last character is space
    
    if(!grepl(" ", process.string) || (lc.space == 0) ){
      ## we will be predicting current word
      get.next.word = 0
    } else {get.next.word = 1}
    
    
    if(process.string == "") { 
      output = ""
      print(output)
      #If there is no space in the string, that means we are looking to predict current word with 1 ngram
      }else if(!grepl(" ", process.string)){
      output = grepstring(paste0("^",process.string), tdm_list[[1]]) 
      print(output)  
    } else {
      compstring = unlist(strsplit(process.string, split = " "))
      total.words = length(compstring)
      compstring;total.words
      search.str = NULL
      
      if(total.words >= 3){
        search.str[1] = paste0("^",paste(compstring[ total.words-2], compstring[total.words-1], compstring[total.words], sep = " "))     
        search.str[2] = paste0("^",paste(compstring[total.words-1], compstring[total.words], sep = " ") )
        search.str[3] = paste0("^",compstring[total.words] )
        search.str[4] = paste0("^",compstring[total.words] )
        #get ng.level for predicting current word or predicting next word
        if(get.next.word == 0){ng.level=3} 
        else{ng.level = 4}
        
      } else if( total.words == 2 ){
        search.str[1] = paste0("^",paste(compstring[total.words-1], compstring[total.words], sep = " ") )
        search.str[2] = paste0("^",compstring[total.words] )
        search.str[3] = paste0("^",compstring[total.words] )
        #get ng.level for predicting current word or predicting next word  
        if(get.next.word == 0){ng.level=2} 
        else{ng.level = 3}
        
      } else if(total.words == 1){ 
        search.str[1] = paste0("^",compstring[total.words])  
        search.str[2] = paste0("^",compstring[total.words] )
        ng.level = 2
        #get ng.level for predicting current word or predicting next word  
        if(get.next.word == 0){ng.level=1} 
        else{ng.level = 2}
        
      } else { 
        search.str[1] = paste0("^", compstring[total.words] )
        ng.level = 1
      }
      
      i = 1
      
      while(ng.level > 0 ){
        print(ng.level)
        print(search.str[i])
        output <- grepstring(search.str[i], tdm_list[[ng.level]])
        if (length(output) > 0){
          ng.level = 0
        }else{
          ng.level = ng.level -1
        }
        i = i+1
      }  
      
      
      print(output)
      
    }
    
    output.string = output
    
    #paste0('input$text is "', input$text, ' this is more text')
    #paste0('process.string is... ', input$textvar)
    #paste0('process.string is...', output.string)
    paste('',output.string)  
    })
  
  
#  output$summary2 <- renderText({
 #    print(" this is some additional text")
  #})
  
}

)
