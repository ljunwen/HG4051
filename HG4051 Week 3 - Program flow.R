# segment 7
dir_path <- "data/Scripts Same Room/"
directory <- list.files(path = dir_path, pattern = "*.TextGrid$")



# segment 1

for (textgrid in directory) {   # creates a loop that repeats according to the number of items in the vector 'directory'
}



# segment 2
   
combined <- subset(combined, text != "<S>")
combined <- subset(combined, text != "<Z>")



# segment 3 - you can take 'conv[["A"]]' and 'conv[["B"]]' to be dataframes in your comment

for (m in 1:(length(conv[["A"]][,1]) + length(conv[["B"]][,1]))) {
}


      
# segment 4

if (start_timeA < start_timeB) {
   combined <- rbind(combined, convA_line)
   a <- a + 1
} else {
   combined <- rbind(combined, convB_line)
   b <- b + 1
}


      
# segment 5 - you only need to make reference to the arguments '0', '5', 'c("SCD", "speaker", "tmin", "tmax","text")' and 'stringsAsFactors=F' in the command 'data.frame' in your comments

combined <- data.frame(matrix(vector(), 0, 5, dimnames=list(c(), c("SCD", "speaker", "tmin", "tmax","text"))), stringsAsFactors=F)
   


# segment 6

if (a <= length(conv[["A"]][,1])) {
   convA_line <- as.data.frame(conv[["A"]][a,])
   start_timeA <- convA_line$tmin[1]
} else {
   start_timeA <- 10000
}


      
# segment 8

a <- 1
b <- 1



# segment 9

if (b <= length(conv[["B"]][,1])) {
   convB_line <- as.data.frame(conv[["B"]][b,])
   start_timeB <- convB_line$tmin[1]
} else {
   start_timeB <- 10000
}


      
# segment 10 - imports the TextGrid transcripts into R as dataframes (you do not need to write comments for this section)
   
if(!require(stringr)){
   install.packages("stringr")   # installs the 'stringr' package if it isn't installed
   library(stringr)   # loads the package
}
  
if (substr(textgrid,5,6) == "-1") {
   conv_id <- substr(textgrid,1,4)
   speaker <- "A"
   conv <- vector(mode = "list", length = 2)
   names(conv) <- c("A","B")
} else if (substr(textgrid,1,6) == paste0(conv_id,"-2")) {
   speaker <- "B"
} else {next}

# this section extracts the TextGrid information into dataframes

conv[[speaker]] <- readChar(paste0(dir_path, textgrid), file.info(paste0(dir_path, textgrid))$size, useBytes = TRUE)
conv[[speaker]] <- gsub("\r\n            "," ", conv[[speaker]])
conv[[speaker]] <- gsub("\"\r\n        ","\" \r\n        ", conv[[speaker]])
conv[[speaker]] <- str_split(conv[[speaker]],"\n")
conv[[speaker]] <- as.data.frame(str_match(conv[[speaker]][[1]], "xmin = ([0-9]+\\.?[0-9]*) +xmax = ([0-9]+\\.?[0-9]*) +text = \"(.*)\" $")[,c(2:4)])
conv[[speaker]] <- conv[[speaker]][15:(length(conv[[speaker]][,1]) - 1),]
colnames(conv[[speaker]]) <- c("tmin", "tmax", "text")
conv[[speaker]]$SCD <- gsub("\\.TextGrid", "", textgrid)
conv[[speaker]]$speaker <- paste0(speaker,":")
conv[[speaker]] <- conv[[speaker]][, c(4,5,1:3)]
conv[[speaker]]$tmin <- as.numeric(conv[[speaker]]$tmin)
conv[[speaker]]$tmax <- as.numeric(conv[[speaker]]$tmax)
  
if (substr(textgrid,5,6) == "-1") {next}



# end - writing the output file (you don't need to write comments for this section)

write_utf8_txt <- function(df, file) {
   con <- file(file, open = "w+", encoding = "native.enc")
   firstline <- paste0(colnames(df), collapse = "\t")
   data <- apply(df, 1, function(x) {paste0(x, collapse = "\t")})
   writeLines(c(firstline, data), con = con, useBytes = TRUE)
   close(con)
}

write_utf8_txt(combined, paste0(dir_path, "combined/", conv_id, ".txt"))