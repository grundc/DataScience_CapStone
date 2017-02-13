
setwd("U://My Documents/GitHub/DataScience_CapStone") # ROCHE
library(dplyr)
library(tidytext)
library(ggplot2)

fileSample <- "fileSampleDF.rda"
set.seed(1001)

filenames <- c("en_US.news.txt","en_US.twitter.txt","en_US.blogs.txt")

all_file_df <- data.frame()
  
  if (!file.exists(fileSample))
  {
    
    for(filename in filenames)
    {
      con <- file(paste("./data/",filename, sep=""), "r") 
      complete_file <- readLines(con, encoding="UTF-8")
      Integrated <- rbinom(length(complete_file),1, prob=0.1)
      file_df <- data.frame(line = 1:length(complete_file), text = complete_file, stringsAsFactors = F, Integrated=Integrated, file = filename)
      file_df <- file_df %>% filter(Integrated==1)
      all_file_df <- rbind(all_file_df,file_df)
      rm(complete_file)
      close(con)
    }
    
   
    save(all_file_df, file=fileSample)
    rm(file_df)
    rm(all_file_df)
  }
  

load(fileSample)

all_files_words <- all_file_df %>% unnest_tokens(word, text, token="words")

all_files_words %>% anti_join(stop_words) %>% count(file,word) %>% group_by(file) %>% top_n(n=10, wt=n) %>%
  ggplot(aes(word,n, group=factor(file))) +
  geom_bar(stat="identity") + 
  xlab(NULL) + coord_flip() + facet_grid(. ~ file) 







