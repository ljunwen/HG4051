# downloads the data file from the course's GitHub
download.file("https://github.com/ljunwen/HG4051/raw/main/data/Week%204%20-%20sw2005.A.terminals.xml", "Week 4 - sw2005.A.terminals.xml")

path <- "D:/NSC Part 3/48k/Transcripts/Scripts Same Room/combined2/Switchboard NXT/"
file <- "2005-1.xml"
write_to_file <- "n"


transcript_str <- readChar(paste0(path, file), file.info(paste0(path, file))$size)

transcript_str <- gsub(">\n\t<nite:pointer","><nite:pointer", transcript_str)
transcript_str <- gsub("</word>\n","",transcript_str)
transcript_tmp <- as.data.frame(str_split(transcript_str,"\n"))
transcript_tmp <- transcript_tmp[-1,]


# XML

temp <- as.data.frame(str_match_all(transcript_tmp[1], "(\\w+)=\""))

temp <- temp[c(2:4,6,5,7,1,9),]

transcript <- data.frame(matrix(vector(), 0, length(temp[,1]), dimnames=list(c(), temp[,2])))

for (i in seq_along(transcript_tmp)) {
   for (j in seq_along(colnames(transcript))) {
     temp <- as.data.frame(str_match_all(transcript_tmp[i], paste0(colnames(transcript)[j],"=\"([a-zA-Z0-9\\.\\-#\\(\\)\\_\\[\\]]+)\"")))[,2]
     if (length(temp) == 0) {temp <- NA}
     transcript[i,j] <- temp
   }
}

colnames(transcript)[8] <- "phon_id"

transcript$start <- as.numeric(transcript$start)
transcript$end <- as.numeric(transcript$end)

transcript <- subset(transcript, grepl("\\w+", transcript$orth))


# raw transcript

transcript_raw <- data.frame(matrix(vector(), 0, 5, dimnames=list(c(), c("id","msstate","start","end","text"))))

temp <- as.character("")
start_time <- transcript$start[1]

for (i in seq_along(transcript[,1])) {
   temp <- paste(temp, transcript$orth[i])
   if ((i + 1) > length(transcript[,1])) {
      limit <- ""
   } else {
     limit <- str_match(transcript$id[i + 1], "s(\\w+)_")[,2]
   }
   if (str_match(transcript$id[i], "s(\\w+)_")[,2] != limit) {
      j <- length(transcript_raw[,1]) + 1
      transcript_raw[j,] <- NA
      transcript_raw$id[j] <- str_match(transcript$id[i], "(s\\w+)_")[,2]
      transcript_raw$msstate[j] <- transcript$msstate[i]
      transcript_raw$start[j] <- start_time
      transcript_raw$end[j] <- transcript$end[i]
      transcript_raw$text[j] <- trimws(temp)
      temp <- ""
      if (i + 1 <= length(transcript[,1])) {
         start_time <- transcript$start[i+1]
      }
   }
}

transcript_raw <- subset(transcript_raw, msstate != "non-aligned")
transcript_raw$start <- as.numeric(transcript_raw$start)
transcript_raw$end <- as.numeric(transcript_raw$end)

# output data to text file if write_to_file = "y"

if (write_to_file == "y") {
  write_utf8_txt <- function(df, file) {
    con <- file(file, open = "w+", encoding = "native.enc")
    firstline <- paste0(colnames(df), collapse = "\t")
    data <- apply(df, 1, function(x) {paste0(x, collapse = "\t")})
    writeLines(c(firstline, data), con = con, useBytes = TRUE)
    close(con)
  }
  
  write_utf8_txt(transcript_raw, paste0(dir_path, "raw transcripts/", substr(file,1,6), ".txt"))
}
