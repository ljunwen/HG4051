# downloads the data file from the course's GitHub
download.file("https://github.com/ljunwen/HG4051/raw/main/data/Week%204%20-%20RT%20data.txt", "Week 4 - RT data.txt", method = "libcurl")

# test for normality

path <- "D:/"
Data <- read.delim(file = paste0(path, "Week 4 - RT data.txt"), header = TRUE, stringsAsFactors = TRUE)
Data$Caffeine <- as.factor(Data$Caffeine)

# histograms

hist(subset(Data, Caffeine == 0)$RT, breaks = 12)
hist(subset(Data, Caffeine == 100)$RT, breaks = 12)
hist(subset(Data, Caffeine == 200)$RT, breaks = 12)

# QQ plots

qqnorm(subset(Data, Caffeine == 0)$RT)
qqline(subset(Data, Caffeine == 0)$RT, lty = 2)

qqnorm(subset(Data, Caffeine == 100)$RT)
qqline(subset(Data, Caffeine == 100)$RT, lty = 2)

qqnorm(subset(Data, Caffeine == 200)$RT)
qqline(subset(Data, Caffeine == 200)$RT, lty = 2)

# Shapiro-Wilk test

shapiro.test(subset(Data, Caffeine == 0)$RT)
shapiro.test(subset(Data, Caffeine == 100)$RT)
shapiro.test(subset(Data, Caffeine == 200)$RT)


# Kruskal-Wallis test

kruskal.test(RT ~ Caffeine, data = Data)
summary(aov(RT ~ Caffeine, data = Data))   # one-way ANOVA for comparison

if(!require(PMCMRplus)){
  install.packages("PMCMRplus")   # installs the 'PMCMRplus' package if it isn't installed
  library(PMCMRplus)   # loads the package
}

summary(kwAllPairsConoverTest(RT ~ Caffeine, data = Data))   # 'P value adjustment method: single-step' means that Tukey's adjustment was used. Also, the warning about ties can be ignored.

if(!require(emmeans)){
  install.packages("emmeans")   # installs the 'emmeans' package if it isn't installed
  library(emmeans)   # loads the package
}

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

oneway_test(RT ~ Caffeine, data = Data)

if(!require(rcompanion)){
  install.packages("rcompanion")   # installs the 'rcompanion' package if it isn't installed
  library(rcompanion)   # loads the package
}

pairwisePermutationTest(RT ~ Caffeine, data = Data, method = "fdr")
pairs(emmeans(aov(RT ~ Caffeine, data = Data), "Caffeine"))   # pairwise comparisons using 'emmeans' for comparison

# repeated-measures test

Data$Participant <- as.factor(Data$Participant)

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


# chi-square test

# guessing a coin-toss

Input =("
 Guess      Total
 Correct    57
 Incorrect  43
")

Data = read.table(textConnection(Input),header=TRUE)

xtabs(Total ~ Guess, Data)   # lays out data in a contingency table

chisq.test(xtabs(Total ~ Guess, Data))

# distribution of political affiliation across gender

Input =("
 Gender  Party        Total
 F       Democrat     762
 F       Independent  327
 F       Republican   468
 M       Democrat     484
 M       Independent  239
 M       Republican   477
")


Data = read.table(textConnection(Input),header=TRUE)

xtabs(Total ~ Gender + Party, Data)   # lays out data in a contingency table

chisq.test(xtabs(Total ~ Gender + Party, Data))
fisher.test(xtabs(Total ~ Gender + Party, Data))
