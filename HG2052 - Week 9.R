load("data/conv.RData")   # loads the data file

if(!require(stringr)){
  install.packages("stringr")   # installs the 'stringr' package (for 'str_count') if it isn't installed
  library(stringr)   # loads the package on first install
}

conv$count <- str_count(conv$text, "\\[lah\\]")   # counts the number of 'lahs' in each line

sum(conv[conv$speaker == "A:",]$count)
sum(conv[conv$speaker == "B:",]$count)

conv$wordcount <- str_count(conv$text, " ") + 1   # counts the number of words in each line

sum(conv[conv$speaker == "A:",]$wordcount)
sum(conv[conv$speaker == "B:",]$wordcount)

sum(conv[conv$speaker == "A:",]$duration)
sum(conv[conv$speaker == "B:",]$duration)
