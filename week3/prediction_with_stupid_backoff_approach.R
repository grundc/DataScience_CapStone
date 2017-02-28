# setwd("U://My Documents/GitHub/DataScience_CapStone") # ROCHE

# setwd("C:/Users/Grund/Documents/GitHub/DataScience_CapStone") # HOME

library(tidytext)
library(dplyr)
library(stringi)
library(tidyr)


load("./data/4_gram.rda") # object FourGrams
load("./data/3_gram.rda") # object ThreeGrams
load("./data/2_gram.rda") # object TwoGrams


FourGrams <- FourGrams %>% separate(FourGram, c("word1", "word2", "word3", "word4"), sep=" ")
ThreeGrams <- ThreeGrams %>% separate(ThreeGram, c("word2", "word3", "word4"), sep=" ")
TwoGrams <- TwoGrams %>% separate(TwoGram, c("word3", "word4"), sep=" ")

predictNextWord("on the road")

predictNextWord <- function(history, wordVector) {
  
  histDF <- createInputHistory(history)
  
  matchesFourGrams <- FourGrams %>% inner_join(histDF, by=c("word1", "word2", "word3"))
  Nsum <- sum(matchesFourGrams$n)
  matchesFourGrams <- matchesFourGrams %>% mutate(prob = n/Nsum, ngram=4)
  
  
  matchesThreeGrams <- ThreeGrams %>% inner_join(histDF, by=c("word2", "word3"))
  Nsum <- sum(matchesThreeGrams$n)
  matchesThreeGrams <- matchesThreeGrams %>% mutate(prob = n/Nsum * 0.4, ngram=3)
  
  
  matchesTwoGrams <- TwoGrams %>% inner_join(histDF, by=c("word3")) 
  Nsum <- sum(matchesTwoGrams$n)
  matchesTwoGrams <- matchesTwoGrams %>% mutate(prob = n/Nsum * 0.4^2, ngram=2)
  
  
  matches <- matchesFourGrams %>% select(word4,n,prob,ngram)
  matches <- rbind(matches, matchesThreeGrams %>% select(word4,n,prob,ngram) %>% anti_join(matches, by=c("word4")))
  matches <- rbind(matches, matchesTwoGrams %>% select(word4,n,prob,ngram) %>% anti_join(matches, by=c("word4")))
  
  matches %>% arrange(desc(prob))
}



createInputHistory <- function(history) {
  
  histDF <- data.frame(text = history, stringsAsFactors = F)
  histDF <- histDF %>% separate(text, c("word1", "word2", "word3"), sep=" ", fill="left", extra="merge")
  # Doing the data string
  if (!is.na(histDF[1,1])) {
    
    while(!is.na(histDF[1,1])) {
      histDFBackup <-histDF
      histDF <- histDF %>% select(word3) %>% 
        separate(word3, c("word1", "word2", "word3"), sep=" ", fill="left", extra="merge")
      
    }
    if (is.na(histDF[1,2])) {
      histDF <- histDFBackup
    }
    else {
      histDF[1,1] <- histDFBackup[1,1]
    }
  }
  
  histDF
}
