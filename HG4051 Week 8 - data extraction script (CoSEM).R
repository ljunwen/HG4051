# downloads the data file from the course's GitHub
download.file("https://github.com/ljunwen/HG4051/raw/main/data/CoSEM%20ethnicity%20codes.txt", "CoSEM ethnicity codes.txt", method = "libcurl")

if(!require(stringr)){
  install.packages("stringr")   # installs the 'stringr' package (for 'str_match' and 'str_count') if it isn't installed
  library(stringr)   # loads the package on first install
}

# check transcripts for a specific string and adds the lines to the database if found

dir_path <- "D:/CoSEM_v3_anonymized_txtformat_2m_v2/"
directory <- list.files(path = dir_path, pattern = "CoSEM_v3_public_chunk_[0-9]+\\.txt$")

# enter search string here

search_string <- "\\bsia\\b"

# set whether to output data to text file (y/n)

write_to_file <- "n"
path <- ""

# creates table for extracted data

examples <- data.frame(matrix(vector(), 0, 6,dimnames=list(c(), c("ID", "V1", "V2", "String", "Wordcount", "File"))), stringsAsFactors=F)

for (i in seq_along(directory)) {
  
   # imports each transcript into R
   
   conv_file <- paste0(dir_path, directory[i])
   conv <- read.delim(conv_file, header=FALSE, skipNul = TRUE, encoding="UTF-8", quote = "", stringsAsFactors=FALSE)
   
   # extracts the wordcount for each speaker
   
   conv$ID <- paste0(str_match(conv$V1, "-([0-9]{2}[A-Z]+)-")[,2], "_", str_match(directory[i], "chunk_([0-9]+)\\.txt")[,2])
   conv$wordcount <- str_count(conv$V2, " ") + 1
   wordcount <- as.data.frame(xtabs(wordcount ~ ID, data = conv))
   colnames(wordcount)[2] <- "Wordcount"
   conv <- merge(conv, wordcount, by = "ID")
   conv <- conv[, -4]
   
   # extracts the lines in the transcript that matches the search string
   
   conv$String <- str_match(conv$V2, search_string)[,1]
   examples_part <- subset(conv, grepl(search_string, V2, perl = TRUE))
   
   # adds empty rows for speakers who did not utter the search string
   
   examples_part <- merge(examples_part[, c(1:3,5)], as.data.frame(unique(conv[, c(1,4)])), by = "ID", all.y = TRUE)
   
   # notes down the filename of the transcript if lines are extracted
   
   if (length(examples_part[,1]) > 0) {
      examples_part$File <- directory[i]
   }
   
   # adds the extracted lines to the output table
   
   examples <- rbind(examples, examples_part)
}

# counts the number of tokens you searched for in each row

examples$Count <- str_count(examples$V2, search_string)
examples$Count[is.na(examples$V2 == "")] <- 0

# resets row names (although not strictly necessary)

row.names(examples) <- seq_along(examples[,1])

# housekeeping - removing objects from previous loop

remove(examples_part, conv, conv_file, directory, i, wordcount)

# creating metadata columns

examples <- cbind(examples, str_match(examples$ID, "([0-9]{2})([A-Z]{2}C?)([A-Z])")[, c(2:4)])
examples$Line <- str_match(examples$V1, "([0-9]+)-[0-9]{2}[A-Z]{2}C?[A-Z]")[, 2]

metadata <- read.delim(paste0(path, "CoSEM ethnicity codes.txt"), header=TRUE, stringsAsFactors=FALSE)
examples <- merge(examples, metadata, by.x = "2", by.y = "Code")
examples <- examples[,c(2,9,10,13,12,4,5,8,6,11,7)]
colnames(examples) <- c("ID","Age", "Gender", "Ethnicity", "Nationality", "Text", "String", "Count", "Wordcount", "Line", "File")


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