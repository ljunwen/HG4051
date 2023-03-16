# New R things we'll be covering today:
# - str_match - how to extract specific information from a context to be used elsewhere
# - merge - how to integrate speaker metadata with transcript data
# - xtabs - how to aggregate frequency information across conditions
# - lists - how to organise data in R as well as allow for more extensive automation


# CoSEM

examples_subset <- subset(examples, Nationality == "Singaporean")
unique(examples_subset$Ethnicity)
examples_subset <- subset(examples_subset, Ethnicity != "Chinese-Indian" & Ethnicity != "Eurasian")
Data <- as.data.frame(xtabs(Count ~ ID, data = examples_subset))

Data <- merge(Data, examples_subset, by = "ID", all.x = TRUE)
Data <- Data[, c(12,1,3:6,2,10,8)]
Data <- unique(Data)

Data[, c(3:5)] <- lapply(Data[, c(3:5)], as.factor)


# NSC

unique(examples$Ethnicity)
examples_subset <- subset(examples, Ethnicity != "OTHERS")
Data <- as.data.frame(xtabs(count ~ SCD, data = examples_subset))
Data <- merge(Data, examples_subset, by = "SCD", all.x = TRUE)
Data <- Data[, c(3,1,4:12,17,18,2,20)]
Data <- unique(Data)

Data[, c(1:11,13)] <- lapply(Data[, c(1:11,13)], as.factor)


# calculated values

Data$Freq.per.1000 <- with(Data, Freq / Wordcount * 1000)

hist(Data$Freq.per.1000)
boxplot(Data$Freq.per.1000)


# log-transform

Data$Freq.per.1000[Data$Freq.per.1000 == 0] <- min(Data$Freq.per.1000[Data$Freq.per.1000 > 0]) / 2

Data$Log.Freq.per.1000 <- log10(Data$Freq.per.1000)
hist(Data$Log.Freq.per.1000)


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
anova(full.lm)
emmeans(full.lm, pairwise ~ Ethnicity)


# residual plots

plot(full.lm, which = 1)   # plots best-fit line between residuals and predicted values
plot(full.lm, which = 2)   # tries to match (standardised) residuals with normal distribution (theoretical quantiles)


# mixed-model regression

if(!require(lmerTest)){
  install.packages("lmerTest")   # installs the 'lmerTest' package if it isn't installed
  library(lmerTest)   # loads the package (for 'lmer')
}

if(!require(emmeans)){
  install.packages("emmeans")   # installs the 'emmeans' package if it isn't installed
  library(emmeans)   # loads the package (for 'emmeans')
}

full.lmer <- lmer(Log.Freq.per.1000 ~ Ethnicity + (1|File), data = particles[["hor"]])
anova(full.lmer, type = 2)
emmeans(full.lmer, pairwise ~ Ethnicity)


# residual plots

qqnorm(resid(full.lmer))
qqline(resid(full.lmer), lty = 2)   # adds a line to the qqnorm plot
