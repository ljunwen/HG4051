# New R things we'll be covering today:
# - str_match - how to extract specific information from a context to be used elsewhere
# - merge - how to integrate speaker metadata with transcript data
# - xtabs - how to aggregate frequency information across conditions
# - lists - how to organise data in R as well as allow for more extensive automation


# CoSEM

# counts the number of speakers in each conversation from the IDs

single_convs <- as.data.frame(xtabs(~ File, data = unique(COSEM_examples[,c(1,11)])))
single_convs <- single_convs[order(single_convs$Freq),]
row.names(single_convs) <- seq_along(single_convs[,1])

# extracts the data for analysis

examples_subset <- subset(examples, Nationality == "Singaporean")
unique(examples_subset$Ethnicity)
examples_subset <- subset(examples_subset, Ethnicity != "Chinese-Indian" & Ethnicity != "Eurasian")
Data <- as.data.frame(xtabs(Count ~ ID, data = examples_subset))

Data <- merge(Data, examples_subset, by = "ID", all.x = TRUE)
Data <- Data[, c(12,1,3:6,2,10,8)]
Data <- unique(Data)
row.names(Data) <- seq_along(Data[,1])

Data[, c(3:5)] <- lapply(Data[, c(3:5)], as.factor)


# NSC

unique(examples$Ethnicity)
examples_subset <- subset(examples, Ethnicity != "OTHERS")
Data <- as.data.frame(xtabs(count ~ SCD, data = examples_subset))
Data <- merge(Data, examples_subset, by = "SCD", all.x = TRUE)
Data <- Data[, c(3,1,4:12,17,18,2,20)]
Data <- unique(Data)
row.names(Data) <- seq_along(Data[,1])

Data[, c(1:11,13)] <- lapply(Data[, c(1:11,13)], as.factor)


# calculated values

if(!require(car)){
  install.packages("car")   # installs the 'car' package (for 'Boxplot') if it isn't installed
}

Data$Freq.per.1000 <- with(Data, Freq / Wordcount * 1000)

hist(Data$Freq.per.1000)
car::Boxplot(Data$Freq.per.1000)


# log-transform

Data$Freq.per.1000[Data$Freq.per.1000 == 0] <- min(Data$Freq.per.1000[Data$Freq.per.1000 > 0]) / 2

Data$Log.Freq.per.1000 <- log10(Data$Freq.per.1000)
hist(Data$Log.Freq.per.1000)
car::Boxplot(Data$Log.Freq.per.1000)


# data-cleaning

Data_subset <- subset(Data, Freq > 0)
hist(Data_subset$Log.Freq.per.1000)
row.names(Data_subset) <- seq_along(Data_subset[,1])


# setting up a list

particles <- vector(mode = "list", length = 0)
particles <- c(particles, list(Data_subset))
names(particles)[1] <- "sia"

View(particles[["sia"]])

# alternatively:
View(particles[[1]])


# linear regression

full.lm <- lm(Log.Freq.per.1000 ~ Ethnicity, data = particles[["sia"]])
(effects_analysis <- anova(full.lm))
(full.emm <- emmeans(full.lm, pairwise ~ Ethnicity))


# residual plots

plot(full.lm, which = 1)   # plots best-fit line between residuals and predicted values
plot(full.lm, which = 2)   # tries to match (standardised) residuals with normal distribution (theoretical quantiles)


# mixed-model regression

if(!require(lmerTest)){
  install.packages("lmerTest")   # installs the 'lmerTest' package (for 'lmer') if it isn't installed
}

if(!require(emmeans)){
  install.packages("emmeans")   # installs the 'emmeans' package (for 'emmeans') if it isn't installed
}

full.lmer <- lmer(Log.Freq.per.1000 ~ Ethnicity + (1|File), data = particles[["sia"]])
(effects_analysis <- anova(full.lmer, type = 2))
(full.emm <- emmeans(full.lmer, pairwise ~ Ethnicity))


# saving the emmeans output

con <- file("D:/logistic_regression_results.txt", open = "w+", encoding = "native.enc")
writeLines(paste0("Analysis of: ", full.lmer@call[[2]][2], " ~ ", full.lmer@call[[2]][3]), con = con)
writeLines("", con = con)
writeLines(paste0(c("factor",colnames(effects_analysis)), collapse = "\t"), con = con)
writeLines(apply(cbind(rownames(as.data.frame(effects_analysis)),as.data.frame(effects_analysis)), 1, function(x) {paste0(x, collapse = "\t")}), con = con)
writeLines("", con = con)
writeLines(paste0(colnames(as.data.frame(full.emm$emmeans)), collapse = "\t"), con = con)
writeLines(apply(as.data.frame(full.emm$emmeans), 1, function(x) {paste0(x, collapse = "\t")}), con = con)
writeLines("", con = con)
writeLines(paste0(colnames(as.data.frame(full.emm$contrasts)), collapse = "\t"), con = con)
writeLines(apply(as.data.frame(full.emm$contrasts), 1, function(x) {paste0(x, collapse = "\t")}), con = con)
close(con)


# residual plots

qqnorm(resid(full.lmer))
qqline(resid(full.lmer), lty = 2)   # adds a line to the qqnorm plot