# downloads the data file from the course's GitHub
download.file("https://github.com/ljunwen/HG4051/raw/main/data/Week%204%20-%20sw2005.A.terminals.xml", "Week 4 - sw2005.A.terminals.xml")

path <- ""
file <- "Week 4 - sw2005.A.terminals.xml"
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
      transcript_raw$start[j] <- transcript$start[i]
      transcript_raw$end[j] <- transcript$end[i]
      transcript_raw$text[j] <- trimws(temp)
      temp <- ""
   }
}

transcript_raw$start <- as.numeric(transcript_raw$start)
transcript_raw$end <- as.numeric(transcript_raw$end)


# Kruskal-Wallis test

# downloads the data file from the course's GitHub
download.file("https://github.com/ljunwen/HG4051/raw/main/data/Week%203%20-%20RT%20data.txt", "Week 3 - RT data.txt", method = "libcurl")

path <- "D:/"
Data <- read.delim(file = paste0(path, "Week 3 - RT data.txt"), header = TRUE, stringsAsFactors = TRUE)
Data$Caffeine <- as.factor(Data$Caffeine)

kruskal.test(RT ~ Caffeine, data = Data)
summary(aov(RT ~ Caffeine, data = Data))   # one-way ANOVA for comparison

if(!require(PMCMRplus)){
  install.packages("PMCMRplus")   # installs the 'PMCMRplus' package if it isn't installed
  library(PMCMRplus)   # loads the package
}

summary(kwAllPairsConoverTest(RT ~ Caffeine, data = Data))   # 'P value adjustment method: single-step' means that Tukey's adjustment was used. Also, the warning about ties can be ignored.
pairs(emmeans(aov(RT ~ Caffeine, data = Data), "Caffeine"))   # pairwise comparisons using 'emmeans' for comparison


# Friedman's rank test

Data$Participant <- as.factor(Data$Participant)

friedman.test(RT ~ Caffeine | Participant, data = Data)
summary(aov(RT ~ Caffeine + Error(Participant), data = Data))   # repeated-measures ANOVA for comparison

summary(with(Data, frdAllPairsConoverTest(y = RT, groups = Caffeine, blocks = Participant, p.adjust.methods = "single-step")))   # 'P value adjustment method: single-step' means that Tukey's adjustment was used. Also, the warning about ties can be ignored.
pairs(emmeans(aov(RT ~ Caffeine + Error(Participant), data = Data), "Caffeine"))   # pairwise comparisons using 'emmeans' for comparison


# permutation test for ANOVA

if(!require(coin)){
  install.packages("coin")   # installs the 'coin' package if it isn't installed
  library(coin)   # loads the package
}

# one-way test

independence_test(RT ~ Caffeine, data = Data)

if(!require(rcompanion)){
  install.packages("rcompanion")   # installs the 'rcompanion' package if it isn't installed
  library(rcompanion)   # loads the package
}

pairwisePermutationTest(RT ~ Caffeine, data = Data, method = "fdr")
pairs(emmeans(aov(RT ~ Caffeine, data = Data), "Caffeine"))   # pairwise comparisons using 'emmeans' for comparison

# repeated-measures test

symmetry_test(RT ~ Caffeine | Participant, data = Data)
pairwisePermutationSymmetry(RT ~ Caffeine | Participant, data = Data, method = "fdr")
pairs(emmeans(aov(RT ~ Caffeine + Error(Participant), data = Data), "Caffeine"))   # pairwise comparisons using 'emmeans' for comparison


# aligned ranks transformation ANOVA

if(!require(ARTool)){
  install.packages("ARTool")   # installs the 'ARTool' package if it isn't installed
  library(ARTool)   # loads the package
}

# one-way test

(aligned_ranks <- art(RT ~ Caffeine, data = Data))
anova(aligned_ranks)
summary(aov(RT ~ Caffeine, data = Data))

art.con(aligned_ranks, "Caffeine")
pairs(emmeans(aov(RT ~ Caffeine, data = Data), "Caffeine"))   # pairwise comparisons using 'emmeans' for comparison

# factorial test

(aligned_ranks <- art(RT ~ Caffeine * Age.Group, data = Data))
anova(aligned_ranks)
summary(aov(RT ~ Caffeine * Age.Group, data = Data))

# post-hoc comparisons of main effects and interaction

art.con(aligned_ranks, "Caffeine")
art.con(aligned_ranks, "Age.Group")
art.con(aligned_ranks, "Caffeine:Age.Group")
contrast(emmeans(artlm.con(aligned_ranks, "Caffeine:Age.Group"), ~ Caffeine|Age.Group), method = "pairwise")
art.con(aligned_ranks, "Caffeine:Age.Group")

emmeans(lm(RT ~ Caffeine * Age.Group, data = Data), specs = consec ~ Caffeine|Age.Group)

# custom contrasts

emmeans(artlm.con(aligned_ranks, "Caffeine:Age.Group"), ~ CaffeineAge.Group)
contrast(emmeans(artlm.con(aligned_ranks, "Caffeine:Age.Group"), specs = ~ CaffeineAge.Group), list("< 30, 0 - 100" = c(1,0,-1,0,0,0), "< 30, 100 - 200" = c(0,0,1,0,-1,0), "> 30, 0 - 100" = c(0,1,0,-1,0,0), "> 30, 100 - 200" = c(0,0,0,1,0,-1)), adjust = "holm")