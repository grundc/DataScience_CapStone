# setwd("U://My Documents/GitHub/DataScience_CapStone") # ROCHE

# setwd("C:/Users/Grund/Documents/GitHub/DataScience_CapStone") # HOME


# ------------------------------------------
# Code that always needs to run
# ------------------------------------------

library(tidytext)
library(dplyr)


# STEP 1: Setup data

con <- file("data/en_US.blogs.txt", "r") 
blog_file <- readLines(con, encoding="UTF-8")
close(con)

blog_file_df <- data.frame(line = 1:length(blog_file), text = blog_file, stringsAsFactors = F)


# STEP 2: Basic Tokenization and Measuring visualizing term frequency

blog_words <- blog_file_df %>% unnest_tokens(word, text, token="words")

blog_words %>% count(word, sort=TRUE) %>% top_n(10)

blog_words %>% anti_join(stop_words) %>% count(word, sort=TRUE) %>% top_n(10)
