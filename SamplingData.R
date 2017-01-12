# setwd("U://My Documents/GitHub/DataScience_CapStone")


# --------------------------------------------------------------------------
# ?connections

# file(description = "", open = "", blocking = TRUE,
#      encoding = getOption("encoding"), raw = FALSE,
#      method = getOption("url.method", "default"))
# 
# url(description, open = "", blocking = TRUE,
#     encoding = getOption("encoding"),
#     method = getOption("url.method", "default"))
# 
# gzfile(description, open = "", encoding = getOption("encoding"),
#        compression = 6)
# 
# bzfile(description, open = "", encoding = getOption("encoding"),
#        compression = 9)
# 
# xzfile(description, open = "", encoding = getOption("encoding"),
#        compression = 6)
# 
# unz(description, filename, open = "", encoding = getOption("encoding"))
# 
# pipe(description, open = "", encoding = getOption("encoding"))
# 
# fifo(description, open = "", blocking = FALSE,
#      encoding = getOption("encoding"))
# 
# socketConnection(host = "localhost", port, server = FALSE,
#                  blocking = FALSE, open = "a+",
#                  encoding = getOption("encoding"),
#                  timeout = getOption("timeout"))
# 
# open(con, ...)
# ## S3 method for class 'connection'
# open(con, open = "r", blocking = TRUE, ...)
# 
# close(con, ...)
# ## S3 method for class 'connection'
# close(con, type = "rw", ...)
# 
# flush(con)
# 
# isOpen(con, rw = "")
# isIncomplete(con)
# ---------------------------------------------------------------------------------------



con <- file("data/en_US.twitter.txt", "r") 
rows <- character()

count <- 0

for (row in readLines(con, 10000) )
{
  #row <- readLines(con, 1) 
  if (rbinom(1,1, prob=0.1) == 1)
  {
    rows <- c(rows,row)
  }

  # count <- count + 1
  # if (count >= 100)
  #   {
  #     break()
  #   }
    
  
  # readLines(con, 5) 
}

close(con)
write(rows,"data/sample_en_US_twitter.txt")



