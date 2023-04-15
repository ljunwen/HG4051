# downloads the data file from the course's GitHub
download.file("https://github.com/ljunwen/HG4051/raw/main/data/Week%201%20-%20NSC%203009.txt", "Week 1 - NSC 3009.txt", method = "libcurl")

getwd()   # tells you where the current working directory is
setwd("C:/")   # sets the current working directory as C:/ (you can change it to wherever you want)

# time to write your own comments!
path <- ""
conv <- read.csv(file = paste0(path, "Week 1 - NSC 3009.txt"), header=TRUE, sep="\t", skipNul = TRUE, encoding="UTF-8", stringsAsFactors=FALSE)
# alternatively:
conv <- read.delim(file = paste0(path, "Week 1 - NSC 3009.txt"), header=TRUE, skipNul = TRUE, encoding="UTF-8", stringsAsFactors=FALSE)

class(conv)
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

conv$speaker[1] <- "C:"
conv$speaker[1] <- "A:"

head(conv)

# alternatively:
conv[c(1:6),]

conv[c(1:6), c(3,5)]

# alternatively:
head(conv[, c(3,5)])

conv[3, 5]

#alternatively:
conv$text[3]

conv$duration <- conv$tmax - conv$tmin

conv_bak <- conv

conv <- conv[, c(1,3:4,6,2,5)]

conv <- conv[, -1]


if(!require(stringr)){
  install.packages("stringr")   # installs the 'stringr' package (for 'str_count') if it isn't installed
  library(stringr)   # loads the package on first install
}

conv$count <- str_count(conv$text, "\\[lah\\]")
# alternatively:
conv$count <- stringr::str_count(conv$text, "\\[lah\\]")

conv_a <- subset(conv, speaker == "A:")
conv_b <- subset(conv, speaker == "B:")

# alternatively:
conv_a <- conv[conv$speaker == "A:",]
conv_b <- conv[conv$speaker == "B:",]

levels(conv_a$speaker)
levels(conv_b$speaker)

conv_a$speaker <- droplevels(conv_a$speaker)
conv_b$speaker <- droplevels(conv_b$speaker)

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