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



predictNextWord <- function(history) {
      
      histDF <- data.frame(text = history, stringsAsFactors = F)
      histDF <- histDF %>% separate(text, c("word1", "word2", "word3"), sep=" ", fill="left")
      
        print("Trying FourGram")
        matches <- FourGrams %>% inner_join(histDF, by=c("word1", "word2", "word3"))
        print(nrow(matches))
        if (nrow(matches) == 0)
        {
          # BAcking Off 1 word
          print("Trying ThreeGram")
          matches <- ThreeGrams %>% inner_join(histDF, by=c("word2", "word3")) %>% select(word2, word3, word4, n)
          print(nrow(matches))
          if (nrow(matches) == 0)
          {
            # BAcking Off 2 word
            print("Trying TwoGram")
            matches <- TwoGrams %>% inner_join(histDF, by=c("word3")) %>% select(word3, word4, n)
            print(nrow(matches))
          }
        }
  
      matches # %>% arrange(desc(n)) %>% top_n(30)
}

matches <- predictNextWord("you must be")
matches %>% filter(word4 %in% c("insane","insensitive","callous", "asleep")) %>% arrange(desc(n)) %>% top_n(4)












teststring <- "ich bin heute nicht zur Schule gegangen"
teststring <- "ich bin"
testdf <- data.frame(text = teststring, stringsAsFactors = F)
testdf %>% separate(text, c("word1", "word2", "word3", "word4"), sep=" ", fill="left")
# testdf %>% unnest_tokens(ngram,text, token="ngrams" , n=4)

history <- "going"
histDF <- data.frame(text = history, stringsAsFactors = F)
histDF <- histDF %>% separate(text, c("word1", "word2", "word3"), sep=" ", fill="left")


history <- "going to home myself or by car"
histDF <- data.frame(text = history, stringsAsFactors = F)
histDF <- histDF %>% separate(text, c("word1", "word2", "word3"), sep=" ", fill="left", extra="merge")

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

matches <- ThreeGrams %>% inner_join(histDF, by=c("word2", "word3"))  %>% select(word2, word3, word4, n)
    
matches %>% arrange(desc(n)) %>% top_n(5)
histDF









