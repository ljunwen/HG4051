# calculates the word count of transcripts

if(!require(stringr)){
   install.packages("stringr")   # installs the 'stringr' package (for 'str_count') if it isn't installed
   library(stringr)   # loads the package on first install
}
   
dir_path <- "Course transcripts/"
directory <- list.files(path = dir_path, pattern = "*.txt$")

# creates table for word counts

wordcount <- data.frame(matrix(vector(), 20, 2, dimnames=list(c(), c("Transcript", "Wordcount"))), stringsAsFactors=F)

for (i in seq_along(directory)) {
  
   # imports each transcript into R
   
   conv_file <- paste0(dir_path, directory[i])
   conv <- readChar(conv_file, file.info(conv_file)$size)
   
   # extracts the lines in the transcript that matches the search string
   
   wordcount$Transcript[i] <- directory[i]
   wordcount$Wordcount[i] <- str_count(conv, " ")
   
}

# rearranges the table to the same order as 'HG4051 Assignment 2 - Filler data.txt'

wordcount$Participant <- substr(wordcount$Transcript, 1, 3)   # extracts the 1st to 3rd characters from the string in 'Transcript'
wordcount$Course <- substr(wordcount$Transcript, 5, 5)   # extracts the 5th character from the string in 'Transcript'
wordcount <- with(wordcount, wordcount[order(Course, Participant),])   # reorders the table according to Course and Participant
wordcount <- wordcount[,c(1,2)]

# writes the data to file

path <- ""
write.table(wordcount, file = paste0(path, "wordcount.txt"), sep = "\t", row.names = F)