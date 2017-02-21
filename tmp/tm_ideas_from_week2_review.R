


sample <- c(sampleBlogs,sampleTwitter,sampleNews)

writeLines(sample, "./final/en_US/samples/sample.txt")

samplecorpus <- Corpus((DirSource("./final/en_US/samples/")))



words_per_line <- stri_count_words(sample) --library(stringi)
summary( words_per_line )




unigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))}
unigramDTC <- DocumentTermMatrix(samplecorpus, control = list(tokenize = unigramTokenizer))

bigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
bigramDTC <- DocumentTermMatrix(samplecorpus, control = list(tokenize = bigramTokenizer))

trigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}
trigramDTC <- DocumentTermMatrix(samplecorpus, control = list(tokenize = trigramTokenizer))


unigramDTC_freq <- sort(colSums(as.matrix(unigramDTC)),decreasing = TRUE)
unigramDTC_data <- data.frame(word = names(unigramDTC_freq), frequency = unigramDTC_freq)




http://rpubs.com/vidz/milestonereport1


corpus <- VCorpus(VectorSource(data.sample))

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)



options(mc.cores=1)


getFreq <- function(tdm) {
  freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  return(data.frame(word = names(freq), freq = freq))
}
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
makePlot <- function(data, label) {
  ggplot(data[1:30,], aes(reorder(word, -freq), freq)) +
    labs(x = label, y = "Frequency") +
    theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1)) +
    geom_bar(stat = "identity", fill = I("grey50"))
}



freq1 <- getFreq(removeSparseTerms(TermDocumentMatrix(corpus), 0.9999))
freq2 <- getFreq(removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = bigram)), 0.9999))
freq3 <- getFreq(removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = trigram)), 0.9999))




http://rpubs.com/shibashismukherjee/MilestoneReport


