# downloads the data file from the course's GitHub
download.file("https://github.com/ljunwen/HG4051/raw/main/data/Week%207%20-%20Ratings%20data.txt", "Week 6 - RT data.txt", method = "libcurl")
download.file("https://github.com/ljunwen/HG4051/raw/main/data/Week%207%20-%20Lah%20examples.txt", "Week 7 - lah examples.txt", method = "libcurl")

# transformations

path <- ""
examples <- read.delim(file = paste0(path, "Week 7 - Lah examples.txt"), header = TRUE, stringsAsFactors = TRUE)
Data <- read.delim(file = paste0(path, "Week 7 - Ratings data.txt"), header = TRUE, stringsAsFactors = TRUE)
Data$Speaker <- as.factor(Data$Speaker)
Data$Sentence <- as.factor(Data$Sentence)
Data$Loop <- as.factor(Data$Loop)

hist(as.data.frame(xtabs(~ SCD, data = examples))$Freq)
shapiro.test(as.data.frame(xtabs(~ SCD, data = examples))$Freq)
hist(log10(as.data.frame(xtabs(~ SCD, data = examples))$Freq))
shapiro.test(log10(as.data.frame(xtabs(~ SCD, data = examples))$Freq))

full.lm <- lm(RC3 ~ P.Age, data = Data)
summary(full.lm)

Data$P.Age_centred <- scale(Data$P.Age, center = TRUE, scale = FALSE)
Data$P.Age_scaled <- scale(Data$P.Age, center = FALSE, scale = TRUE)
Data$P.Age_centred_scaled <- scale(Data$P.Age, center = TRUE, scale = TRUE)

full.lm <- lm(RC3 ~ P.Age_centred, data = Data)
summary(full.lm)

full.lm <- lm(RC3 ~ P.Age_scaled, data = Data)
summary(full.lm)

full.lm <- lm(RC3 ~ P.Age_centred_scaled, data = Data)
summary(full.lm)


# mixed-effects models


if(!require(lmerTest)){
  install.packages("lmerTest")   # installs the 'lmerTest' package if it isn't installed
  library(lmerTest)   # loads the package
}

if(!require(car)){
  install.packages("car")   # installs the 'car' package if it isn't installed
  library(car)   # loads the package
}

if(!require(MuMIn)){
  install.packages("MuMIn")   # installs the 'MuMIn' package if it isn't installed
  library(MuMIn)   # loads the package
}


# random intercepts

full.lm <- lm(RC3 ~ S.Gender + S.Ethnicity + S.Education + S.AgeGroup + P.Gender + P.Ethnicity + P.Education + P.Age, data = Data)
car::Anova(full.lm, type = "II")

ResponseId.lmer <- lmer(RC3 ~ S.Gender + S.Ethnicity + S.Education + S.AgeGroup + P.Gender + P.Ethnicity + P.Education + P.Age + (1|ResponseId), data = Data, REML = TRUE, control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
car::Anova(ResponseId.lmer, type = "II")

Speaker.lmer <- lmer(RC3 ~ S.Gender + S.Ethnicity + S.Education + S.AgeGroup + P.Gender + P.Ethnicity + P.Education + P.Age + (1|Speaker), data = Data, REML = TRUE, control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
car::Anova(Speaker.lmer, type = "II")

full.lmer <- lmer(RC3 ~ S.Gender + S.Ethnicity + S.Education + S.AgeGroup + P.Gender + P.Ethnicity + P.Education + P.Age + (1|ResponseId) + (1|Speaker), data = Data, REML = TRUE, control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
car::Anova(full.lmer, type = "II")

anova(ResponseId.lmer, full.lmer, refit = FALSE)
anova(Speaker.lmer, full.lmer, refit = FALSE)


# random slopes

full.lmer <- lmer(RC3 ~ S.Gender + S.Ethnicity + S.Education + S.AgeGroup + P.Gender + P.Ethnicity + P.Education + P.Age + (1 + S.Education|ResponseId) + (1|Speaker), data = Data, REML = TRUE, control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
reduced.lmer <- lmer(RC3 ~ S.Gender + S.Ethnicity + S.Education + S.AgeGroup + P.Gender + P.Ethnicity + P.Education + P.Age + (1|ResponseId) + (1|Speaker), data = Data, REML = TRUE, control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
anova(reduced.lmer, full.lmer, refit = FALSE)

car::Anova(full.lmer, type = "II")



# random intercepts - 2nd run

ResponseId.lmer <- lmer(RC3 ~ S.Gender + P.Age + (1|ResponseId), data = Data, REML = TRUE, control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
Speaker.lmer <- lmer(RC3 ~ S.Gender + P.Age + (1|Speaker), data = Data, REML = TRUE, control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
full.lmer <- lmer(RC3 ~ S.Gender + P.Age + (1|ResponseId) + (1|Speaker), data = Data, REML = TRUE, control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
anova(ResponseId.lmer, full.lmer, refit = FALSE)
anova(Speaker.lmer, full.lmer, refit = FALSE)


# random slopes - 2nd run

full.lmer <- lmer(RC3 ~ S.Gender + P.Age + (1 + S.Education|ResponseId) + (1|Speaker), data = Data, REML = TRUE, control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
reduced.lmer <- lmer(RC3 ~ S.Gender + P.Age + (1|ResponseId) + (1|Speaker), data = Data, REML = TRUE, control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
anova(reduced.lmer, full.lmer, refit = FALSE)

car::Anova(full.lmer, type = "II")


# marginal and conditional R^2

r.squaredGLMM(full.lmer)
