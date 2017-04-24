home <- TRUE

if (home == TRUE) {
      setwd("C:/Users/Grund/Documents/GitHub/DataScience_CapStone") # HOME
} else {
      setwd("U://My Documents/GitHub/DataScience_CapStone") # ROCHE 
}

library(tidytext)
library(dplyr)
library(stringi)
library(tidyr)

set.seed(1001)

full_df <- data.frame()
filenames <- c("en_US.news.txt","en_US.twitter.txt","en_US.blogs.txt")

  for(filename in filenames)
  {
    con <- file(paste("./data/",filename, sep=""), "r") 
    complete_file <- readLines(con, encoding="UTF-8")
    Integrated <- rbinom(length(complete_file),1, prob=0.2) # take only 20% of the original file
    file_df <- data.frame(text = complete_file, Integrated=Integrated, stringsAsFactors = F) %>% filter(Integrated == 1)
    full_df <- rbind(full_df, file_df)
    rm(complete_file)
    rm(file_df)
    close(con)
   
  }

# 2-grams
TwoGrams <- full_df %>% unnest_tokens(TwoGram, text, token="ngrams" , n=2)
TwoGrams <- TwoGrams %>% count(TwoGram) %>% filter(n > 1)

# TwoGrams <- TwoGrams %>% separate(TwoGram, c("word1", "word2"), sep=" ")
# TwoGrams <- TwoGrams %>%
#       filter(!word1 %in% stop_words$word) %>%
#       filter(!word2 %in% stop_words$word)
# TwoGrams <- TwoGrams %>% unite(TwoGram, word1, word2, sep = " ")

outputfile <- paste("./data/2_gram.rda", sep="")
save(TwoGrams, file = outputfile)
rm(TwoGrams)

# 3-grams
ThreeGrams <- full_df %>% unnest_tokens(ThreeGram, text, token="ngrams" , n=3)
ThreeGrams <- ThreeGrams %>% count(ThreeGram) %>% filter(n > 1)
outputfile <- paste("./data/3_gram.rda", sep="")
save(ThreeGrams, file = outputfile)
rm(ThreeGrams)

# 4-grams
FourGrams <- full_df %>% unnest_tokens(FourGram, text, token="ngrams" , n=4)
FourGrams <- FourGrams %>% count(FourGram) %>% filter(n > 1)
outputfile <- paste("./data/4_gram.rda", sep="")
save(FourGrams, file = outputfile)
rm(FourGrams)
