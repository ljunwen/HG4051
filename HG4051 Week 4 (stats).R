# downloads the data file from the course's GitHub
download.file("https://github.com/ljunwen/HG4051/raw/main/data/Week%204%20-%20RT%20data.txt", "Week 4 - RT data.txt", method = "libcurl")

path <- ""
Data <- read.delim(file = paste0(path, "Week 4 - RT data.txt"), header = TRUE, stringsAsFactors = TRUE)
Data$Caffeine <- as.factor(Data$Caffeine)

# One-way ANOVA

fit.aov <- aov(RT ~ Caffeine, data = Data)

summary(fit.aov)

model.lm <- lm(RT ~ Caffeine, data = Data)   # this is equivalent to line 10

anova(model.lm)


# post-hoc comparisons

if(!require(emmeans)){
   install.packages("emmeans")   # installs the 'emmeans' package (for 'emmeans') if it isn't installed
   library(emmeans)   # loads the package on first install
}

(model.emm <- emmeans(model.lm, specs = pairwise ~ Caffeine))   # this calculates the emmeans for the fixed factor 'Caffeine' based on the model in 'model.lm', as well as the pairwise comparisons

(model.emm <- emmeans(model.lm, specs = pairwise ~ Caffeine, adjust = "bonferroni"))   # this changes the adjustment measure from the default 'Tukey's HSD' to 'Bonferroni'

(model.emm <- emmeans(model.lm, specs = consec ~ Caffeine))   # this changes the pairwise comparisons to consecutive comparisons

(model.emm <- emmeans(model.lm, specs = consec ~ Caffeine, adjust = "holm"))   # this changes the adjustment measure from the default 'mvt' to 'Holm-Bonferroni'

plot(model.emm)

emmeans(model.lm, specs = pairwise ~ Caffeine, at = list(Caffeine = c("100", "200")))   # this specifies the pairs to be tested for a planned comparison


# factorial ANOVA

model.lm <- lm(RT ~ Caffeine * Age.Group, data = Data)   # this specifies 'Caffeine' and 'Age.Group' as the main effects, and the '*' indicates we are looking at their interaction as well
model.lm <- lm(RT ~ Caffeine + Age.Group + Caffeine:Age.Group, data = Data)   # this is equivalent to line 41

anova(model.lm)

# more post-hoc comparisons

(model.emm <- emmeans(model.lm, specs = consec ~ Caffeine|Age.Group))   # this changes the pairwise comparisons to consecutive comparisons

plot(model.emm)


# repeated-measures ANOVA

if(!require(lmerTest)){
  install.packages("lmerTest")   # installs the 'lmerTest' package (for 'lmer') if it isn't installed
  library(lmerTest)   # loads the package on first install
}

model.lmer <- lmer(RT ~ Caffeine + (1|Participant), data = Data, REML = TRUE)
anova(model.lmer)

model.lm <- lm(RT ~ Caffeine, data = Data)
anova(model.lm)

# analysis with the factor of 'Age.Group'

model.lmer <- lmer(RT ~ Caffeine * Age.Group + (1|Participant), data = Data, REML = TRUE)
anova(model.lmer)

model.lm <- lm(RT ~ Caffeine * Age.Group , data = Data)
anova(model.lm)