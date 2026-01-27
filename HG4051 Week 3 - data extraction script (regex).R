if(!require(stringr)){
   install.packages("stringr")   # installs the 'stringr' package (for 'str_match' and 'str_count') if it isn't installed
   library(stringr)   # loads the package on first install
}

# creates /corpora folder if it is not already there
if (!dir.exists("corpora")) {
   dir.create("corpora")
}

# check transcripts for a specific string and adds the lines to the database if found

dir_path <- r"{corpora/NSC Part 3 transcripts (Same Room)/}"
if (Sys.info()[1] == "Windows") {dir_path <- gsub("\\\\", "/", dir_path)}
directory <- list.files(path = dir_path, pattern = "[0-9]{4}\\.txt$")

# enter search string here

search_string <- r"{\bpriest\b}"

# set whether to output data to text file (y/n)

write_to_file <- "n"

# creates table for extracted data

examples <- data.frame(matrix(vector(), 0, 7, dimnames=list(c(), c("SCD", "speaker", "tmin", "tmax", "text", "string", "conv"))), stringsAsFactors=F)

for (i in seq_along(directory)) {
  
   # imports each transcript into R
   
   conv_file <- gsub("//", "/", paste0(dir_path, "/", directory[i]))
   conv <- read.delim(conv_file, header=TRUE, skipNul = TRUE, encoding="UTF-8", stringsAsFactors=FALSE)
   
   conv <- subset(conv, text != "<S>" & text != "<Z>")
   
   # extracts the lines in the transcript that matches the search string
   
   conv$string <- str_match(conv$text, search_string)[,1]
   examples_part <- subset(conv, grepl(search_string, text, perl = TRUE))
   
   # notes down the conversation number of the transcript if lines are extracted
   
   if (length(examples_part[,1]) > 0) {
      examples_part$conv <- substr(directory[i],1,4)
   }
   
   # adds the extracted lines to the output table
   
   examples <- rbind(examples, examples_part)
}

# resets row names (although not strictly necessary)

row.names(examples) <- seq_along(examples[,1])

# housekeeping - removing objects from previous loop

remove(examples_part, conv, conv_file, directory, i)

# data-cleaning - removing whitespace from SCD data

examples$SCD <- trimws(examples$SCD)


# output data to text file if write_to_file = "y"

if (write_to_file == "y") {
   write_utf8_txt <- function(df, file) {
      con <- file(file, open = "w+", encoding = "native.enc")
      firstline <- paste0(colnames(df), collapse = "\t")
      data <- apply(df, 1, function(x) {paste0(x, collapse = "\t")})
      writeLines(c(firstline, data), con = con, useBytes = TRUE)
      close(con)
   }
  
   write_utf8_txt(examples, paste0(ifelse(exists("path"), path, path <- ""), "examples.txt"))
}