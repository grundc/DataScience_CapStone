

library(tm)

con <- file("data/sample_en_US_twitter.txt", "r") 

textVector <- readLines(con)


corpora <- VCorpus(VectorSource(textVector))

inspect(corpora[1:2])

# --------------------------------------------------
# DocumentTermMatrix / TermDocumentMatrix
# --------------------------------------------------

data("crude")
tdm <- TermDocumentMatrix(crude,
                          control = list(removePunctuation = TRUE,
                                         stopwords = TRUE))
dtm <- DocumentTermMatrix(crude,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =
                                                           FALSE),
                                         stopwords = TRUE))

# dtm <- DocumentTermMatrix(crude,
#                           control = list(list(removePunctuation = TRUE,
#                                               stopwords = TRUE)))

inspect(tdm[202:205, 1:5])
# inspect(tdm[c("price", "texas"), c("127", "144", "191", "194")])
inspect(dtm[1:5, 273:276])

#--------------------------------------------------


# -------------------------------------------------
# termFreq
# -------------------------------------------------

data("crude")
termFreq(crude[[14]])

termFreq(crude)

strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))

ctrl <- list(tokenize = strsplit_space_tokenizer,
             removePunctuation = list(preserve_intra_word_dashes = TRUE),
             stopwords = c("reuter", "that"),
             stemming = TRUE,
             wordLengths = c(4, Inf))

termFreq(crude[[14]], control = ctrl)

# --------------------------------------------------


