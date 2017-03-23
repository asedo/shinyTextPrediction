library(LaF)
library(NLP)
library(tm) 
library(stringi)
#library(dplyr)
#library(R.utils)
##library(RWeka)

rm(list=ls())
# C:\Users\Arturo\Google Drive\Coursera\Capstone\ShinyApp\TextPrediction4Fun\Data


setwd("C:/Users/Arturo/Google Drive/Coursera/Capstone/ShinyApp/TextPrediction4Fun")
set.seed(123)
# Step 1 : Read sample data into a character array.
# Since there is so much data, we need to sample the data to be able to process it. otherwise it crashes my computer :-)

process_files <- function(filenames) {
  sample_data <- NULL
  for( file in filenames) {
    #cat('processing ',file,'\n')
    nlines <- determine_nlines(file)
    #cat('lines:',nlines,'\n')
    sample <- sample_lines(file, nlines/120)
    #cat('lines selected: ',length(sample),'\n')
    sample <- iconv(sample,'UTF-8','Latin1')
    sample_data <- c(sample_data,sample)
  }
  return(sample_data)
}

#filenames <- c( "./TextPrediction4Fun/Data/en_US.blogs.txt","./TextPrediction4Fun/Data/en_US.news.txt", "./TextPrediction4Fun/Data/en_US.twitter.txt" )
filenames <- c( "./Data/en_US.blogs.txt","./Data/en_US.news.txt", "./Data/en_US.twitter.txt" )

catall <- process_files(filenames)
# There were a few strange characters that I was not able to get rid of by any other means... let's get rid of them here before processing using gsub("[:alnum:]' ]" ....
catall <- gsub("[^[:alnum:]///' ]", "", catall)
empty <- which(is.na(catall))
if(length(empty) > 0)  
  catall <- catall[-empty]

training =testing = NULL
index <- 1:length(catall)
inTrain = createDataPartition(y = index, p = .7 )[[1]]
training$orig.string = catall[inTrain]
testing$orig.string =  catall[-inTrain]
length(training$orig.string)
length(testing$orig.string)


# Step 2 : Build Corpus
catallCorpus <- Corpus(VectorSource(training$orig.string))
# Do some cleanup
catallCorpus <- tm_map(catallCorpus, content_transformer(tolower))
catallCorpus <- tm_map(catallCorpus, removePunctuation)
catallCorpus<- tm_map(catallCorpus, removeNumbers)
catallCorpus <- tm_map(catallCorpus, stripWhitespace)
# Have not figured out exactly why we need this statement but it is required after tm 6.0
catallCorpus <- tm_map( catallCorpus, PlainTextDocument)


BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

TrigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

FourgramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
FivegramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)


tokenizetdm <- function(allCorpus, tokenLevel){
  
  # Start the clock!
  ptm <- proc.time()
  if(tokenLevel == 1)       { tdm <- TermDocumentMatrix( allCorpus)
  } else if(tokenLevel == 2)  { tdm <- TermDocumentMatrix(catallCorpus,control=list(tokenize = BigramTokenizer))
  } else if(tokenLevel == 3)  { tdm <- TermDocumentMatrix(catallCorpus,control=list(tokenize = TrigramTokenizer))
  } else if(tokenLevel == 4)  { tdm <- TermDocumentMatrix(catallCorpus,control=list(tokenize = FourgramTokenizer)) 
  } else if(tokenLevel == 5)  { tdm <- TermDocumentMatrix(catallCorpus,control=list(tokenize = FivegramTokenizer)) 
  } else tokenstring = "Error"
  
  tdm <- removeSparseTerms(tdm, 0.9999)
  tdmterms <- tdm$dimnames$Terms
  freqs <- rowSums( as.matrix(tdm) )
  df = data.frame(names= tdmterms, freq= freqs, stringsAsFactors = FALSE) 
  
  # Stop the clock and print
  #cat("tokenLevel is : ", tokenLevel)
  #print(proc.time() - ptm)
  
  df <- df[order(df$freq, decreasing = TRUE),]
  
  return(df)  
}

#tdm_list[[i]]
tdm_list <- list()
for(i in 1:5) {
  tdm_list[[i]] <- tokenizetdm(catallCorpus, i)
}


getLastChar = function(x){
  substr(x,nchar(x), nchar(x))
}


grepstring <- function( searchstring, tdm_l){
  grep(searchstring, tdm_l$names)
  last.word <- tdm_l$names[head(grep(searchstring, tdm_l$names),1)]
  last.word <- stri_extract_last_words( last.word)  
  return(last.word)
}

#########################
# Execute following code only if you want to same image. 
#########################
#rm(catallCorpus)
#rm(filenames)
#rm(catall)
#rm(empty)
#rm(BigramTokenizer)
#rm(FourgramTokenizer)
#rm(TrigramTokenizer)
#rm(tokenizetdm)
#rm(process_files)


save.image(file = "predictNextWord.Rdata", ascii = FALSE, safe = TRUE)
load("predictNextWord.Rdata")
#########################
################################
#   Get input from training set
################################

length(training$orig.string)
for(i in 1:length(training$orig.string)) {
  split.document = unlist(strsplit(training$orig.string[i], split = " "))
  training$numwords[i] = length(split.document)
   if (training$numwords[i] >4){
     endsample = sample(c(4,train$numwords[i]),1)
     
     if(endsample >= 4){
       train[i]$fourwords = paste(compstring[endsample-3], compstring[ endsample-2], compstring[endsample-1], compstring[endsample], sep = " ")
       train[i]$threewords = paste(compstring[ endsample-2], compstring[endsample-1], compstring[endsample], sep = " ")     
       train[i]$twowords = paste(compstring[endsample-1], compstring[endsample], sep = " ") 
       train[i]$oneword = compstring[endsample] 
       
     } if(endsample == 3){
       train[i]$fourwords = ""
       train[i]$threewords = paste(compstring[ endsample-2], compstring[endsample-1], compstring[endsample], sep = " ")     
       train[i]$twowords = paste(compstring[endsample-1], compstring[endsample], sep = " ") 
       train[i]$oneword = compstring[endsample] 
       
     } else if( total.words == 2 ){
       train[i]$fourwords = ""
       train[i]$threewords = ""
       train[i]$twowords = paste(compstring[total.num.words.train-1], compstring[total.num.words.train], sep = " ")
       train[i]$oneword = compstring[total.num.words.train]
       
     } else { 
       train[i]$fourwords = ""
       train[i]$threewords = ""
       train[i]$twowords = ""
       train[i]$oneword = ""
   }
  
   }
}

length(training)
length(total.num.words.train)
min(total.num.words.train)
input = NULL
input$textvar = "he looked at me and said thank you for thi"
input$textvar = "my babydoll"
input$textvar = "my color"
input$textvar = "my "
input$textvar = "witho"
input$textvar = "hello ther"
input$textvar = "this is really "
process.string = input$textvar
if(getLastChar(process.string) == " ") {lc.space = 1} else {lc.space = 0}
# lc.space represents if the last character is space

if(!grepl(" ", process.string) || (lc.space == 0) ){
## we will be predicting current word
get.next.word = 0
} else {get.next.word = 1}
#cat("process.string is :",process.string)
#cat("get.next.word is : ",get.next.word)
#cat("lc.space is : ",lc.space)

#If there is no space in the string, that means we are looking to predict current word with 1 ngram
if(!grepl(" ", process.string)){
  output = grepstring(process.string, tdm_list[[1]]) 
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

