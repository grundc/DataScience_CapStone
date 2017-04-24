

home <- TRUE

if (home == TRUE) {
      setwd("C:/Users/Grund/Documents/GitHub/DataScience_CapStone") # HOME
} else {
      setwd("U://My Documents/GitHub/DataScience_CapStone") # ROCHE 
}


library(tm)


# #Create Corpus
# docs <- Corpus(DirSource("./Data/sample"))
# docs
# writeLines(as.character(docs[[1]]))
# 
# summary(docs)


con <- file("data/sample_en_US_twitter.txt", "r") 

textVector <- readLines(con)


corpora <- VCorpus(VectorSource(textVector))

summary(corpora)
writeLines(as.character(corpora[[1]]))

corpora

corpora <- tm_map(corpora,removePunctuation)

corpora <- tm_map(corpora,content_transformer(tolower))
corpora <- tm_map(corpora,removeNumbers)
corpora <- tm_map(corpora,removeWords, stopwords("english"))
corpora <- tm_map(corpora,stripWhitespace)

