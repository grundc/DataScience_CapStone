# setwd("U://My Documents/GitHub/DataScience_CapStone") # ROCHE

# setwd("C:/Users/Grund/Documents/GitHub/DataScience_CapStone") # HOME

library(tidytext)
library(dplyr)
library(stringi)
library(tidyr)


load("./data/4_gram.rda") # object FourGrams
load("./data/3_gram.rda") # object ThreeGrams
load("./data/2_gram.rda") # object TwoGrams

load("./data/4_gram.rda") # object FourGrams
FourGrams <- FourGrams %>% separate(FourGram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
             filter(tolower(substr(word1, start=1,stop=1)) %in% letters) %>%
             filter(tolower(substr(word2, start=1,stop=1)) %in% letters) %>%
             filter(tolower(substr(word3, start=1,stop=1)) %in% letters) %>%
             filter(tolower(substr(word4, start=1,stop=1)) %in% letters)

load("./data/3_gram.rda") # object ThreeGrams
ThreeGrams <- ThreeGrams %>% separate(ThreeGram, c("word2", "word3", "word4"), sep=" ")  %>%
            filter(tolower(substr(word2, start=1,stop=1)) %in% letters) %>%
            filter(tolower(substr(word3, start=1,stop=1)) %in% letters) %>%
            filter(tolower(substr(word4, start=1,stop=1)) %in% letters)

load("./data/2_gram.rda") # object TwoGrams
TwoGrams <- TwoGrams %>% separate(TwoGram, c("word3", "word4"), sep=" ")  %>%
            filter(tolower(substr(word3, start=1,stop=1)) %in% letters) %>%
            filter(tolower(substr(word4, start=1,stop=1)) %in% letters)

# Q2
predictNextWord("me about his",c("marital", "horticultural", "spiritual", "financial"))
predictNextWord("me about his")
# Q3
predictNextWord("arctic monkeys this",c("month", "morning", "weekend", "decade"))
# Q4
predictNextWord("helps reduce your",c("stress", "hunger", "sleepiness", "happiness"))
# Q5
predictNextWord("to take a",c("look", "picture", "minute", "walk"))
# Q6
predictNextWord("to settle the",c("incident", "account", "case", "matter"))
# Q7
predictNextWord("groceries in each",c("toe", "finger", "hand", "arm"))
# Q8
predictNextWord("bottom to the",c("middle", "top", "center", "side"))
# Q9
predictNextWord("bruises from playing",c("daily", "inside", "weekly", "outside"))
# Q10
predictNextWord("of Adam Sandler's",c("stories", "pictures", "novels", "movies"))

predictNextWord("me about his")



predictNextWord <- function(history, wordVector = vector('character')) {
  
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
  
  if (length(wordVector) > 0) {
    matches %>% filter(word4 %in% wordVector) %>% arrange(desc(prob))
  }
  else {
    matches %>% arrange(desc(prob))
  }
  
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
