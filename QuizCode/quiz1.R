



library(dplyr)
library(stringi)

con <- file("data/en_US.twitter.txt", "r") 

complete_file <- readLines(con)

file_df <- data.frame(line = 1:length(complete_file), text = complete_file)

file_df %>% filter(grepl("biostats",text))

love_count <- file_df %>% filter(grepl("love",text)) %>% count()
hate_count <- file_df %>% filter(grepl("hate",text)) %>% count()

love_count / hate_count

file_df %>% filter(grepl("A computer once beat me at chess, but it was no match for me at kickboxing",text)) 

file_df %>% filter(text == "A computer once beat me at chess, but it was no match for me at kickboxing")



file_df <- file_df %>% mutate(textsize = stri_length(text))
max(file_df$textsize)




con <- file("data/en_US.blogs.txt", "r") 
complete_file <- readLines(con)
file_df <- data.frame(line = 1:length(complete_file), text = complete_file)
file_df <- file_df %>% mutate(textsize = stri_length(text))
max(file_df$textsize)


