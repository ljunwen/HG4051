# import the AntConc wordlist and collocates list to R

path <- ""
wordlist <- read.delim(file = paste0(path, "HG4051 Assignment 3 - antconc_wordlist.txt"), header = FALSE, stringsAsFactors = FALSE)
collocates <- read.delim(file = paste0(path, "antconc_kena.txt"), header = FALSE, stringsAsFactors = FALSE)
word <- "kena"
write_to_file <- "n"


# adjusts the columns of wordlist and removes non-Unicode words

wordlist <- wordlist[-c(1:3), -c(1,4)]
wordlist <- wordlist[, c(2,1)]
colnames(wordlist) <- c("Word", "Frequency")
wordlist <- subset(wordlist, grepl("[a-z]+",wordlist$Word))


# adjusts the columns of the AntConc collocates

collocates <- collocates[-c(1:2), -1]
colnames(collocates) <- c("Frequency", "Freq_L", "Freq_R", "MI_stat", "Collocate")
collocates$LR_test <- NA
collocates$Fisher_test <- NA


# loads required packages

if(!require(DescTools)){
  install.packages("DescTools")   # installs the 'DescTools' package (for 'GTest') if it isn't installed
  library(DescTools)   # loads the package on first install
}


# calculates likelihood ratio statistic for each collocate in the list

xtabs_table <- data.frame(matrix(vector(), 4, 3, dimnames=list(c(), c("Word.A","Word.B","Total"))),stringsAsFactors=T)

for (i in seq_along(collocates$Collocate)) {
   xtabs_table[1,] <- c("Yes", "Yes", collocates$Frequency[i])
   xtabs_table[2,] <- c("Yes", "No", sum(collocates$Frequency) - collocates$Frequency[i])
   xtabs_table[3,] <- c("No", "Yes", subset(wordlist, wordlist$Word == collocates$Collocate[i])$Frequency)
   xtabs_table[4,] <- c("No", "No", sum(wordlist$Frequency) - subset(wordlist, wordlist$Word == collocates$Collocate[i])$Frequency - subset(wordlist, wordlist$Word == word)$Frequency)
   xtabs_table$Total <- as.numeric(xtabs_table$Total)

   collocates$LR_test[i] <- GTest(xtabs(Total ~ Word.A + Word.B, data = xtabs_table))$statistic
   collocates$Fisher_test[i] <- fisher.test(xtabs(Total ~ Word.A + Word.B, data = xtabs_table))$p.value
}

collocates <- with(collocates, collocates[order(LR_test, decreasing = TRUE),])
# collocates <- with(collocates, collocates[order(Fisher_test, decreasing = FALSE),])
row.names(collocates) <- seq_along(collocates[,1])

rm(xtabs_table)


# outputs data to text file if write_to_file = "y"

if (write_to_file == "y") {
   write.table(collocates, paste0(path, "likelihood ratio ranking.txt"), sep = "\t", row.names = FALSE)
}