# downloads the data file from the course's GitHub
download.file("https://github.com/ljunwen/HG4051/raw/main/data/Week%206%20-%20RT%20data.txt", "Week 6 - RT data.txt", method = "libcurl")

# linear regression

path <- "D:/"
Data <- read.delim(file = paste0(path, "Week 6 - Ratings data.txt"), header = TRUE, stringsAsFactors = TRUE)

if(!require(car)){
  install.packages("car")   # installs the 'car' package if it isn't installed
  library(car)   # loads the package
}

full.lm <- lm(MR3 ~ P.Age, data = Data)
summary(full.lm)
with(Data, plot(P.Age, MR3, pch = 16, cex = 0.5, main = "Plot of MR3 over Participant Age", xlab = "Participant Age", ylab = "MR3"))
abline(full.lm)

full.lm <- lm(MR3 ~ S.Ethnicity, data = Data)
car::Anova(full.lm, type = 2)
summary(full.lm)

# releveling levels in a factor

levels(Data$S.Ethnicity)
Data$S.Ethnicity <- relevel(Data$S.Ethnicity, ref = "Malay")   # releveling, i.e., changing the reference level of a factor
levels(Data$S.Ethnicity)

# changing the contrasts for specific factors

contrasts(Data$S.Ethnicity)
contrasts(Data$S.Ethnicity) <- contr.sum
contrasts(Data$S.Ethnicity) <- as.matrix(rbind(c(-1,-1),c(1,0),c(0,1)))   # changes the reference level from the last to the first level

# emmeans plots

if(!require(emmeans)){
  install.packages("emmeans")   # installs the 'emmeans' package if it isn't installed
  library(emmeans)   # loads the package
}

(full.emm <- emmeans(full.lm, specs = pairwise ~ S.Ethnicity))
plot(full.emm)


# multiple regression

full.lm <- lm(MR3 ~ (S.Ethnicity + S.Education + S.AgeGroup + P.Age + P.Ethnicity)^2, data = Data)
car::Anova(full.lm, type = 3)
drop1(full.lm, test = "F")   # an alternative for the previous line

# checks for collinearity

vif(full.lm)
full.lm <- lm(MR3 ~ S.Ethnicity + S.Education + S.AgeGroup + P.Age + P.Ethnicity, data = Data)

# reduces the model to only significant variables

reduced.lm <- lm(MR3 ~ S.Ethnicity + S.Ethnicity:S.Education + S.Ethnicity:S.AgeGroup + S.Education:S.AgeGroup, data = Data)
car::Anova(reduced.lm, type = 3)
summary(reduced.lm)

# residual plots

plot(reduced.lm, which = 1)   # plots best-fit line between residuals and predicted values
plot(reduced.lm, which = 2)   # tries to match (standardised) residuals with normal distribution (theoretical quantiles)

# reduced model based on what is significant in the ANOVA table previously, plus interactions

reduced.lm <- lm(MR3 ~ S.Ethnicity + S.Education + S.AgeGroup + S.Ethnicity:S.Education + S.Ethnicity:S.AgeGroup + S.Education:S.AgeGroup, data = Data)
(effects_analysis <- car::Anova(reduced.lm, type = 3))
summary(reduced.lm)

# emmeans plots

if(!require(emmeans)){
  install.packages("emmeans")   # installs the 'emmeans' package if it isn't installed
  library(emmeans)   # loads the package
}

(reduced.emm <- emmeans(reduced.lm, specs = pairwise ~ S.Education|S.Ethnicity))
plot(reduced.emm)


# reordering or releveling levels in a factor

levels(Data$S.Education)
Data$S.Education <- factor(Data$S.Education, levels=c("Secondary school", "Polytechnic", "University"), ordered = T)
levels(Data$S.Education)

# changing the contrasts globally

options('contrasts')   # shows current contrast settings
options(contrasts=c('contr.treatment','contr.poly'))   # default settings
options(contrasts=c('contr.sum','contr.poly'))   # effects coding for unordered factors only
options(contrasts=c('contr.treatment','contr.treatment'))   # treatment coding for both unordered and ordered factors
options(contrasts=c('contr.sum','contr.sum'))   # effects coding for both unordered and ordered factors

# changing the contrasts for specific factors

contrasts(Data$S.Education)
contrasts(Data$S.Education) <- contr.sum


# saving the outputs

con <- file(paste0(path, "regression output.txt"), open = "w+")
writeLines(as.character(reduced.lm$call)[2], con = con)
writeLines("", con = con)
writeLines(paste0(c("factor",colnames(effects_analysis)), collapse = "\t"), con = con)
writeLines(apply(cbind(rownames(as.data.frame(effects_analysis)), as.data.frame(effects_analysis)), 1, function(x) {paste0(x, collapse = "\t")}), con = con)
writeLines("", con = con)
writeLines(paste0(colnames(as.data.frame(reduced.emm$emmeans)), collapse = "\t"), con = con)
writeLines(apply(as.data.frame(reduced.emm$emmeans), 1, function(x) {paste0(x, collapse = "\t")}), con = con)
writeLines("", con = con)
writeLines(paste0(colnames(as.data.frame(reduced.emm$contrasts)), collapse = "\t"), con = con)
writeLines(apply(as.data.frame(reduced.emm$contrasts), 1, function(x) {paste0(x, collapse = "\t")}), con = con)
close(con)