download.file("https://raw.githubusercontent.com/ljunwen/HG4051/main/data/3009.txt", "3009.txt")

path <- ""
conv <- read.csv(file = paste0(path, "3009.txt"), header=TRUE, sep="\t", skipNul = TRUE, encoding="UTF-8", stringsAsFactors=FALSE)
# alternatively:
conv <- read.delim(file = paste0(path, "3009.txt"), header=TRUE, skipNul = TRUE, encoding="UTF-8", stringsAsFactors=FALSE)

class(conv$SCD)
class(conv$speaker)
class(conv$tmin)
class(conv$text)

conv$SCD <- as.factor(conv$SCD)
conv$speaker <- as.factor(conv$speaker)

class(conv$SCD)
class(conv$speaker)

levels(conv$SCD)
levels(conv$speaker)

conv$duration <- conv$tmax - conv$tmin

conv_bak <- conv

conv <- conv[, c(1,3:4,6,2,5)]

conv <- conv[, -1]


if(!require(stringr)){
  install.packages("stringr")   # installs the lmerTest package if it isn't installed
  library(stringr)   # loads the package
}

conv$count <- str_count(conv$text, "\\[lah\\]")
# alternatively:
conv$count <- stringr::str_count(conv$text, "\\[lah\\]")

conv_a <- subset(conv, speaker == "A:")
conv_b <- subset(conv, speaker == "B:")

# alternatively:
conv_a <- conv[conv$speaker == "A:",]
conv_b <- conv[conv$speaker == "B:",]

sum(conv_a$count)
sum(conv_b$count)

conv_a$wordcount <- str_count(conv_a$text, " ") + 1
conv_b$wordcount <- str_count(conv_b$text, " ") + 1

sum(conv_a$wordcount)
sum(conv_b$wordcount)

sum(conv_a$duration)
sum(conv_b$duration)

conv_a <- subset(conv_a, text != "<S>")
conv_b <- subset(conv_b, text != "<S>")

# alternatively:
conv_a <- conv_a[conv$text != "<S>",]
conv_b <- conv_b[conv$text != "<S>",]
