# import the frequency lists of the text and reference corpus to R

path <- ""
reference <- read.delim(file = paste0(path, "reference_freq.txt"), header = FALSE, stringsAsFactors = FALSE)
text <- read.delim(file = paste0(path, "text_freq.txt"), header = FALSE, stringsAsFactors = FALSE)
write_to_file <- "n"


# adjusts the columns of the reference corpus frequency list and removes non-Unicode words

reference <- reference[-c(1:3), -c(1,4)]
reference <- reference[, c(2,1)]
colnames(reference) <- c("Word", "Frequency")
reference <- subset(reference, grepl("[a-z]+", reference$Word))


# adjusts the columns of the text frequency list and removes non-Unicode words

text <- text[-c(1:3), -c(1,4)]
text <- text[, c(2,1)]
colnames(text) <- c("Word", "Frequency")
text <- subset(text, grepl("[a-z]+", text$Word))
text$LR_test <- NA
text$Fisher_test <- NA


# loads required package

if(!require(DescTools)){
  install.packages("DescTools")   # loads the 'DescTools' package (for 'GTest') and installs it if it isn't installed
}


# calculates likelihood ratio statistic for each word in the text frequency list relative to the reference corpus frequency list

xtabs_table <- data.frame(matrix(vector(), 4, 3, dimnames=list(c(), c("Word","Corpus","Total"))),stringsAsFactors=T)

for (i in seq_along(text$Word)) {
   xtabs_table[1,] <- c("Yes", "Text", text$Frequency[i])
   xtabs_table[2,] <- c("No", "Text", sum(text$Frequency) - text$Frequency[i])
   
   if (text$Word[i] %in% reference$Word) {
      xtabs_table[3,] <- c("Yes", "Reference", subset(reference, reference$Word == text$Word[i])$Frequency)
      xtabs_table[4,] <- c("No", "Reference", sum(reference$Frequency) - subset(reference, reference$Word == text$Word[i])$Frequency)
   } else {
      xtabs_table[3,] <- c("Yes", "Reference", 0)
      xtabs_table[4,] <- c("No", "Reference", sum(reference$Frequency))
   }
   xtabs_table$Total <- as.numeric(xtabs_table$Total)

   text$LR_test[i] <- GTest(xtabs(Total ~ Word + Corpus, data = xtabs_table))$statistic
   text$Fisher_test[i] <- fisher.test(xtabs(Total ~ Word + Corpus, data = xtabs_table))$p.value
}

text <- with(text, text[order(LR_test, decreasing = TRUE),])
# text <- with(text, text[order(Fisher_test, decreasing = FALSE),])
row.names(text) <- seq_along(text[,1])

rm(xtabs_table)


# outputs data to text file if write_to_file = "y"

if (write_to_file == "y") {
   write.table(text, paste0(path, "keyword results (text).txt"), sep = "\t", row.names = FALSE)
}


# to swap text and reference frequency lists, run the following, then run code again from line 34

temp <- text
text <- reference
reference <- temp
rm(temp)

text$LR_test <- NA
text$Fisher_test <- NA