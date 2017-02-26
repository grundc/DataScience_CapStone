


library(tidytext)
library(dplyr)
library(stringi)
library(tidyr)

filename <- "sample_en_US_twitter.txt"

con <- file(paste("./data/",filename, sep=""), "r") 
complete_file <- readLines(con, encoding="UTF-8")
close(con)


file_df <- data.frame(line = 1:length(complete_file), text = complete_file, stringsAsFactors = F)



FourGrams <- file_df %>% unnest_tokens(Fourgram, text, token="ngrams" , n=4)

FourGramsCount <- FourGrams %>% count(Fourgram)

FourGramsCount %>% separate(Fourgram, c("word1", "word2", "word3", "word4"), sep=" ")

FourGramsDF <- FourGramsCount %>% separate(Fourgram, c("word1", "word2", "word3", "word4"), sep=" ")


teststring <- "ich bin heute nicht zur Schule gegangen"
# teststring <- "ich bin"
testdf <- data.frame(text = teststring, stringsAsFactors = F)
testdf %>% separate(text, c("word1", "word2", "word3", "word4"), sep=" ")
# testdf %>% unnest_tokens(ngram,text, token="ngrams" , n=4)


predictNextWord <- function(history) {
      
      histDF <- data.frame(text = history, stringsAsFactors = F)
      histDF <- histDF %>% separate(text, c("word1", "word2", "word3"), sep=" ")
      
      matches <- FourGramsDF %>% inner_join(histDF, by=c("word1", "word2", "word3"))
      if (nrow(matches) == 0)
      {
            # BAcking Off 1 word
            matches <- FourGramsDF %>% inner_join(histDF, by=c("word2", "word3"))
            if (nrow(matches) == 0)
            {
                  # BAcking Off 2 word
                  matches <- FourGramsDF %>% inner_join(histDF, by=c("word3"))
            }
      }
      
      matches %>% top_n(n=10, wt=n)
}

predictNextWord("bla bla back")

