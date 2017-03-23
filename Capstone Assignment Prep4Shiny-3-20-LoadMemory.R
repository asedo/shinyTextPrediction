library(caret)
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

# This code was written to calculate accuracy of the prediction algorithm
# training =testing = NULL
# index <- 1:length(catall)
# inTrain = createDataPartition(y = index, p = .7 )[[1]]
# training$orig.string = catall[inTrain]
# testing$orig.string =  catall[-inTrain]
# length(training$orig.string)
# length(testing$orig.string)


# Step 2 : Build Corpus
catallCorpus <- Corpus(VectorSource(catall))
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
rm(catallCorpus)
rm(filenames)
rm(catall)
rm(empty)
rm(BigramTokenizer)
rm(FourgramTokenizer)
rm(TrigramTokenizer)
rm(FivegramTokenizer)
rm(tokenizetdm)
rm(process_files)
rm(testing)
rm(training)


save.image(file = "predictNextWord.Rdata", ascii = FALSE, safe = TRUE)
load("predictNextWord.Rdata")
#########################

