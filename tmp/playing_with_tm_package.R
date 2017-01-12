

library(tm)

con <- file("data/sample_en_US_twitter.txt", "r") 

textVector <- readLines(con)


corpora <- VCorpus(VectorSource(textVector))

inspect(corpora[1:2])

