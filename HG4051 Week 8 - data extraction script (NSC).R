# downloads the data file from the course's GitHub
download.file("https://github.com/ljunwen/HG4051/raw/main/data/NSC%20speaker%20metadata%20(Part%203).txt", "NSC speaker metadata (Part 3).txt", method = "libcurl")

if(!require(stringr)){
  install.packages("stringr")   # installs the 'stringr' package if it isn't installed
  library(stringr)   # loads the package (for 'str_match' and 'str_count')
}

# check transcripts for a specific string and adds the lines to the database if found

dir_path <- "D:/NSC Part 3/48k/Transcripts/Scripts Same Room/combined/"
directory <- list.files(path = dir_path, pattern = "*.txt$")

# enter search string here

search_string <- "\\[sia\\]"

# set whether to output data to text file (y/n)

write_to_file <- "n"
path <- ""

# creates table for extracted data

examples <- data.frame(matrix(vector(), 0, 8, dimnames=list(c(), c("SCD", "speaker", "tmin", "tmax", "text", "string", "wordcount", "conv"))), stringsAsFactors=F)

for (i in seq_along(directory)) {
  
   # imports each transcript into R
   
   conv_file <- paste0(dir_path, directory[i])
   conv <- read.delim(conv_file, header=TRUE, skipNul = TRUE, encoding="UTF-8", stringsAsFactors=FALSE)
   
   conv <- subset(conv, text != "<S>" & text != "<Z>")
   
   # extracts the wordcount for each speaker
   
   conv$wordcount <- str_count(conv$text, " ") + 1
   wordcount <- as.data.frame(xtabs(wordcount ~ SCD, data = conv))
   colnames(wordcount)[2] <- "Wordcount"
   conv <- merge(conv, wordcount, by = "SCD")
   conv <- conv[, -6]
   
   # extracts the lines in the transcript that matches the search string
   
   conv$string <- str_match(conv$text, search_string)[,1]
   examples_part <- subset(conv, grepl(search_string, text, perl = TRUE))
   
   # adds empty rows for speakers who did not utter the search string
   
   examples_part <- merge(examples_part[, c(1:5,7)], as.data.frame(unique(conv[, c(1,6)])), by = "SCD", all.y = TRUE)
   examples_part <- with(examples_part, examples_part[order(tmin), ])
   
   # notes down the conversation number of the transcript if lines are extracted
   
   if (length(examples_part[,1]) > 0) {
      examples_part$conv <- substr(directory[i],1,4)
   }
   
   # adds the extracted lines to the output table
   
   examples <- rbind(examples, examples_part)
}

# counts the number of tokens you searched for in each row

examples$count <- str_count(examples$text, search_string)
examples$count[is.na(examples$text == "")] <- 0

# resets row names (although not strictly necessary)

row.names(examples) <- seq_along(examples[,1])

# housekeeping - removing objects from previous loop

remove(examples_part, conv, conv_file, directory, i)

# data-cleaning - removing whitespace from SCD data

examples$SCD <- trimws(examples$SCD)


# creating metadata columns

metadata <- read.delim(paste0(path, "NSC speaker metadata (Part 3).txt"), header=TRUE, stringsAsFactors=FALSE)
examples <- merge(examples, metadata, by = "SCD")
examples <- examples[,c(20,1,11:19,2:6,8,9,7)]


# output data to text file if write_to_file = "y"

if (write_to_file == "y") {
   write_utf8_txt <- function(df, file) {
      con <- file(file, open = "w+", encoding = "native.enc")
      firstline <- paste0(colnames(df), collapse = "\t")
      data <- apply(df, 1, function(x) {paste0(x, collapse = "\t")})
      writeLines(c(firstline, data), con = con, useBytes = TRUE)
      close(con)
   }
  
   write_utf8_txt(examples, paste0(path, "examples.txt"))
}