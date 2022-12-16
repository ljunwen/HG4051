download.file("https://raw.githubusercontent.com/ljunwen/HG4051/main/data/3009.txt", "3009.txt")

path <- "D:/"
conv <- read.csv(file = paste0(path, "3009.txt"), header=TRUE, sep="\t", skipNul = TRUE, encoding="UTF-8", stringsAsFactors=FALSE)
# alternatively:
conv <- read.delim(file = paste0(path, "3009.txt"), header=TRUE, skipNul = TRUE, encoding="UTF-8", stringsAsFactors=FALSE)

conv$count <- str_count(conv$text, "\\[lah\\]")

conv_a <- subset(conv, speaker == "A:")
conv_b <- subset(conv, speaker == "B:")

sum(conv_a$count)
sum(conv_b$count)

conv_a$wordcount <- str_count(conv_a$text, " ") + 1
conv_b$wordcount <- str_count(conv_b$text, " ") + 1

sum(conv_a$wordcount)
sum(conv_b$wordcount)
