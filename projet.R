library(text2vec)
library(Matrix)
library(sparsesvd)
library(tictoc)
library(MASS)
library(dplyr)

vocabulary_size <- 30000
#read corpus text9
corpus <- readLines('text9', n=1, warn=FALSE)

#tokenize corpus by space
words <- unlist(space_tokenizer(corpus))

#iterator token
iterator <- itoken(corpus, tokenizer=space_tokenizer, progressbar=FALSE)

#create vocabulairy (just keep 30000 most frequent words)
vocabulary <- create_vocabulary(iterator)
vocabulary <- as.data.frame(vocabulary,stringsAsFactors = FALSE)
vocabulary <- vocabulary %>% 
              select(-doc_count) %>%
              arrange(desc(term_count)) %>%
              mutate(id = seq(nrow(vocabulary))) %>%
              mutate(id = replace(id, id > 30000, 0))
              


build_dataset <- function(word, dictionary){
  data <- c()
  
  #traverse through all the text we have and produce a vector
  #where each element corresponds to the id of the word found at that index
  for (i in 1:length(word)){
    require(svMisc)
    progress(i, max.value = length(word))
    #if w is in the dictionary use then word ID,
    #else use the id of the special token UNK
    #add index in data
    
    data <- c(data, dictionary$id[which(dictionary$term == word[i])])
    
  }
  
  return(data)
}

cl <- parallel::makeCluster(3)
doParallel::registerDoParallel(cl)
data <- foreach(i = words[1:10], .combine = 'c') %dopar% {
  vocabulary$id[which(vocabulary$term == i)]
}
parallel::stopCluster(cl)


data <- build_dataset(words, vocabulary)


#id for each word unique in the dictionary 
pruned_vocabulary$id <- 1:nrow(pruned_vocabulary)

#Data generation for CBOW

#data_index is updated by 1 everytime we read a set of data point
data_index <- 1


generate_batch_cbow <- function(batch_size, window_size){
  #Description: function for data generation process for CBOW model
  #Input: size of batch and size of window (amount of words we're looking at from each side of a given word)
  #Output: context word and target word
  
  #span defines the total window size,where
  #data we consider at an instance looks as follow. 
  #[ skip_window target skip_window]
  #e.g if skip_window = 2 then span = 5
  span <- 2 * window_size + 1 
  
  # 2 lists to hold target words (batch) 
  # and context words (lables)
  batch <- list()
  label <- list()
  
  #the buffer holds the data contained within the span
  buffer <- list()
  
  #fill the buffer and update the data_index
  for (i in seq(span){
    buffer <- lappend(buffer, pruned_vocabulary$id[data_index])
    data_index <- (data_index + 1) %% nrow(pruned_vocabulary)
  }
  
  # here we do the batch reading
  # we iterate through each batch index
  # for each batch index, we iterate through span elements
  # to fill in the columns of batch array 
  for (i in seq(batch_size)){
    #target label at the center of the buffer
    target <- window_size - 1
    #we only need to know the words around a given word, not the word itself
    target_to_avoid <- window_size - 1
    
    #add selected target to avoid list for next time
    col_idx <- 1
    for (j in seq(span)){
      #ignore the target word when creating the batch
      if(j != span/2){
        batch[[i]][col_idx] = buffer[j]
        col_idx <- col_idx + 1
      }
      label[[i]][1] <- buffet[]
    }
    
    
  }
  
}

lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}
