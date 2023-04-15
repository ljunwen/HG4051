# data from https://dlf.uzh.ch/openbooks/statisticsforlinguists/chapter/first-steps-in-r-importing-and-retrieving-corpus-data/

# downloads the data file from the course's GitHub
download.file("https://github.com/ljunwen/HG4051/raw/main/data/Week%209%20-%20Ditransitives%20data.txt", "Week 9 - Ditransitives data.txt", method = "libcurl")


path <- ""
Data <- read.delim(file = paste0(path,"Week 9 - Ditransitives data.txt"), header = TRUE, stringsAsFactors=TRUE)

# checking the data

levels(Data$R.Form)
levels(Data$R.Animacy)
levels(Data$T.Animacy)
range(Data$T.Length)
levels(Data$Verb)

xtabs(~ R.Form, data = Data)


# simple logistic regression

full.glm <- glm(R.Form ~ R.Animacy + T.Animacy + T.Length, data = Data, family = binomial)
(effects_analysis <- car::Anova(full.glm, type = "II"))
summary(full.glm)


# mixed effect logistic regression

if(!require(lmerTest)){
   install.packages("lmerTest")   # installs the 'lmerTest' package (for 'glmer') if it isn't installed
   library(lmerTest)   # loads the package on first install
}

if(!require(emmeans)){
   install.packages("emmeans")   # installs the 'emmeans' package (for 'emmeans') if it isn't installed
   library(emmeans)   # loads the package on first install
}

# test if random intercepts are significant

reduced.glm <- glm(R.Form ~ R.Animacy + T.Animacy + T.Length, data = Data, family = binomial)
full.glm <- glmer(R.Form ~ R.Animacy + T.Animacy + T.Length + (1|Verb), data = Data, family = binomial)
anova(full.glm, reduced.glm)

# test of fixed effects

full.glm <- glmer(R.Form ~ R.Animacy + T.Animacy + T.Length + (1|Verb), data = Data, family = binomial)
(effects_analysis <- car::Anova(full.glm, type = "II"))
summary(full.glm)


# saving the logistic regression model estimates

write.table(coef(summary(full.glm)),file = paste0(path,"glmer.txt"), sep="\t", row.names = TRUE, col.names =  NA)


# Estimated marginal means and pairwise comparisons.

(full.emm <- regrid(emmeans(full.glm, ~ R.Animacy), transform = "log"))   # the 'transform = "log", type = "response"' arguments sets the estimates as risk ratios
summary(full.emm, type = "response")
summary(contrast(full.emm, interaction = "pairwise"), type = "response")
plot(full.emm, type = "response")


# saving the emmeans output

con <- file(paste0(path,"logistic_regression_results.txt"), open = "w+", encoding = "native.enc")
writeLines(paste0("Analysis of: ", full.glm@call[[2]][2], " ~ ", full.glm@call[[2]][3]), con = con)
writeLines("", con = con)
writeLines(paste0(c("factor",colnames(effects_analysis)), collapse = "\t"), con = con)
writeLines(apply(cbind(rownames(as.data.frame(effects_analysis)),as.data.frame(effects_analysis)), 1, function(x) {paste0(x, collapse = "\t")}), con = con)
writeLines("", con = con)
writeLines(paste0(colnames(as.data.frame(summary(full.emm, type = "response"))), collapse = "\t"), con = con)
writeLines(apply(as.data.frame(summary(full.emm, type = "response")), 1, function(x) {paste0(x, collapse = "\t")}), con = con)
writeLines("", con = con)
writeLines(paste0(colnames(as.data.frame(summary(contrast(full.emm, interaction = "pairwise"), type = "response"))), collapse = "\t"), con = con)
writeLines(apply(as.data.frame(summary(contrast(full.emm, interaction = "pairwise"), type = "response")), 1, function(x) {paste0(x, collapse = "\t")}), con = con)
close(con)


# checking model predictions

Data$Predictions <- round(predict(full.glm, type="response"))
(fit <- xtabs(~ R.Form + Predictions, data = Data))
(fit[1,1] + fit[2,2]) / sum(fit)


# data-splitting

# define proportion of training set (the rest is test set)

df <- Data

train <- 0.5

bound <- floor((nrow(df)/(1 / train)))   # replace 'df' with your dataframe

df <- df[sample(nrow(df)), ]           # randomises the row order in your dataframe
df.train <- df[1:bound, ]              # get training set from the randomised rows
df.test <- df[(bound+1):nrow(df), ]    # the rest of the rows are saved as the test set

full_subset.glm <- glm(R.Form ~ R.Animacy + T.Animacy + T.Length, data = df.train, family = binomial)
car::Anova(full_subset.glm, type = "II")
summary(full_subset.glm)
summary(glm(R.Form ~ R.Animacy + T.Animacy + T.Length, data = Data, family = binomial))

df.test$Predictions <- round(predict(full_subset.glm, newdata = df.test, type="response"))
(fit <- xtabs(~ R.Form + Predictions, data = df.test))
(fit[1,1] + fit[2,2]) / sum(fit)
