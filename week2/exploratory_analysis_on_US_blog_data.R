# setwd("U://My Documents/GitHub/DataScience_CapStone") # ROCHE

# setwd("C:/Users/Grund/Documents/GitHub/DataScience_CapStone") # HOME


# ------------------------------------------
# Code that always needs to run
# ------------------------------------------

library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(igraph)


# STEP 1: Setup data

con <- file("data/en_US.blogs.txt", "r") 
blog_file <- readLines(con, encoding="UTF-8")
close(con)

blog_file_df <- data.frame(line = 1:length(blog_file), text = blog_file, stringsAsFactors = F)

rm(blog_file)

# object.size(blog_file_df) / 1000


# STEP 2: Basic Tokenization and visualizing term usage

blog_words <- blog_file_df %>% unnest_tokens(word, text, token="words")

blog_words %>% count(word, sort=TRUE) %>% top_n(10)

blog_words %>% anti_join(stop_words) %>% count(word, sort=TRUE) %>% top_n(20) %>%
  ggplot(aes(word,n)) +
  geom_bar(stat="identity") +
  xlab(NULL) + coord_flip()


# STEP 3 Analyse term frequency (tf) and inverse term frequency (itf)


# ---------- TESTING , NOT FULL DATASET -------------------------------
perBlogWords <- blog_file_df %>% mutate(blog = paste("blog",line, sep="")) %>% filter(line %in% c(1,4, 18, 13)) %>% 
                      unnest_tokens(word, text, token="words") %>%          # tokenization
                      count(blog, word, sort=TRUE) %>% ungroup()            # counting words per blog entry

total_blog_words <- perBlogWords %>% group_by(blog) %>% summarize(total=sum(n))                # counting total words per blog entry


summary_words <- left_join(perBlogWords, total_blog_words)


rm(perBlogWords)
rm(total_blog_words)

# head(summary_words)


summary_words <- summary_words %>% arrange(desc(n)) %>% group_by(blog) %>% mutate(rank=row_number(), term_freq = n/total)

# head(summary_words)

# Zipf's law
freq_by_rank %>% 
  ggplot(aes(rank, term_freq, color=as.factor(blog))) +
  geom_line(size=1.2, alpha=0.8) +
  scale_x_log10() +
  scale_y_log10()


summary_words <- summary_words %>% bind_tf_idf(word,blog, n)

# head(summary_words)

# summary_words %>% filter(blog == "blog13") %>% arrange(blog, rank)

# ----------- TESTING END ---------------------------------------------

# -----------START FOR THE FULL DATASET -----------------------------

perBlogWords <- blog_file_df %>% mutate(blog = paste("blog",line, sep="")) %>% 
  unnest_tokens(word, text, token="words") %>%          # tokenization
  count(blog, word, sort=TRUE) %>% ungroup()            # counting words per blog entry

rm(blog_file_df)

total_blog_words <- perBlogWords %>% group_by(blog) %>% summarize(total=sum(n))                # counting total words per blog entry


summary_words <- left_join(perBlogWords, total_blog_words)

rm(perBlogWords)
rm(total_blog_words)

# head(summary_words)


summary_words <- summary_words %>% arrange(desc(n)) %>% group_by(blog) %>% mutate(rank=row_number(), term_freq = n/total)

summary_words <- summary_words %>% bind_tf_idf(word,blog, n)

# head(summary_words)

# ----------END --------------------------------------------------


# STEP 3: Analyse n-grams

blogBigrams <- blog_file_df %>% mutate(blog = paste("blog",line, sep="")) %>% 
      unnest_tokens(bigram, text, token="ngrams" , n=2)

rm(blog_file_df)

# blogBigrams %>% count(bigram, sort=TRUE)

blogBigrams <- blogBigrams %>% separate(bigram, c("word1", "word2"), sep=" ") %>%  # separate into words
                                    filter(!word1 %in% stop_words$word) %>%                 # filter out where any common stop word
                                    filter(!word2 %in% stop_words$word) %>%
                                    unite(bigram, word1, word2, sep=" ") %>%                # recombine words
                                    count(bigram, sort=TRUE)


blogBigrams <- blogBigrams %>% filter(n > 4)
                  
# > dim(blogBigrams)
# [1] 113343      2




blogBigramgraph <- blogBigrams %>% filter(n > 50) %>% 
                        separate(bigram, c("word1", "word2"), sep=" ") %>% 
                        filter(word1 == "love" | word2 == "love") %>%
                        graph_from_data_frame()                         # Genrates an igraph

blogBigramgraph <- blogBigrams %>% filter(n > 500) %>% 
                        separate(bigram, c("word1", "word2"), sep=" ") %>% 
                        graph_from_data_frame()                         # Genrates an igraph


# Visualizing with basic plotting system
plot(blogBigramgraph, vertex.size=4, edge.arrow.size=0.15, edge.arrow.width=2, edge.color="black")



# https://briatte.github.io/ggnet/
library(GGally)


# For conversion netween igraph and network object also "intergraph" library is needed
# install.packages("intergraph")
library(intergraph)


# network.vertex.names(net)
net <- asNetwork(blogBigramgraph)
ggnet2(net, size = 8, color="yellow", label=TRUE, label.size = 5, label.color="black" , arrow.size = 6, arrow.gap = 0.01, edge.label = "n", edge.label.color = "blue")

