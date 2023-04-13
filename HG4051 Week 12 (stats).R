# downloads the data file from the course's GitHub
download.file("https://github.com/ljunwen/HG4051/raw/main/data/Week%2012%20-%20BMI%20data.txt", "Week 12 - BMI data.txt", method = "libcurl")
download.file("https://github.com/ljunwen/HG4051/raw/main/data/Week%2012%20-%20Particles%20data.txt", "Week 12 - Particles data.txt", method = "libcurl")


path <- ""
BMI <- read.delim(paste0(path,"Week 12 - BMI data.txt"), header = TRUE, stringsAsFactors = TRUE)
particles <- read.delim(paste0(path,"Week 12 - Particles data.txt"), header = TRUE, stringsAsFactors = TRUE)


# scatterplots

plot(BMI$Height, BMI$Weight)

with(BMI, cor.test(Height, Weight, method = "pearson"))


if(!require(ggplot2)){
  install.packages("ggplot2")   # loads the 'ggplot2' package (for 'ggplot') and installs it if it isn't installed
}

ggplot(BMI, aes(x = Height, y = Weight, color = Sex, shape = Sex)) + geom_point()


with(subset(BMI, Sex == "Male"), cor.test(Height, Weight, method = "pearson"))
with(subset(BMI, Sex == "Female"), cor.test(Height, Weight, method = "pearson"))


# correlation plot

if(!require(corrplot)){
   install.packages("corrplot")   # loads the 'corrplot' package (for 'corrplot') and installs it if it isn't installed
}

corrplot(cor(particles[,c(13:20)]))


# boxplots

boxplot(particles$lah)

hist(particles$lah)

if(!require(robustbase)){
  install.packages("robustbase")   # loads the 'robustbase' package (for 'adjbox') and installs it if it isn't installed
}

adjbox(particles$lah)

ggplot(particles, aes(x = Gender, y = lah, fill = Gender)) + geom_boxplot()

ggplot(particles, aes(x = Gender, y = lah, fill = Gender)) + geom_boxplot() + scale_fill_discrete(name='Gender', breaks = c('F', 'M'), labels = c('Female', 'Male'))

ggplot(particles, aes(x = Gender, y = lah, fill = Gender)) + geom_boxplot() + scale_fill_discrete(name='Gender', breaks=c('F', 'M'), labels=c('Female', 'Male')) + facet_wrap(~Ethnicity)


# emmeans

full.lm <- lm(lah ~ Ethnicity, data = particles)
full.emm <- emmeans(full.lm, pairwise ~ Ethnicity)
plot(full.emm)

ggplot(plot(full.emm, plotit = FALSE), aes(x = Ethnicity, y = the.emmean)) +
  geom_linerange(mapping = aes(y = the.emmean, ymin = lower.CL, ymax = upper.CL), linewidth = 4, color = "blue", alpha = 0.25) +   # this plots the confidence interval bars
  geom_point(size = 2) +   # this plots the emmeans
  coord_flip() +   # this sets the correct orientation where the emmeans are plotted along the horizontal axis
  #  ylim(0,6) +   # use this to set the range on the emmeans axis if you need a standard range to compare confidence intervals across graphs
  #  facet_grid(Gender ~., labeller = label_both)   # used for interaction plots, e.g., with Gender as the other interaction factor; 'labeller = label_both' includes the factor name with the levels in the labels
  labs(y = "emmean")   # full set of arguments: labs(title = "", x = "", y = "")

ggplot(plot(full.emm, plotit = FALSE), aes(x = Ethnicity, y = the.emmean)) +
  geom_linerange(mapping = aes(y = the.emmean, ymin = lower.CL, ymax = upper.CL), linewidth = 4, color = "blue", alpha = 0.25) +   # this plots the confidence interval bars
  geom_point(size = 2) +   # this plots the emmeans
  coord_flip() +   # this sets the correct orientation where the emmeans are plotted along the horizontal axis
  #  ylim(0,6) +   # use this to set the range on the emmeans axis if you need a standard range to compare confidence intervals across graphs
  #  facet_grid(Gender ~., labeller = label_both) +   # used for interaction plots, e.g., with Gender as the other interaction factor; 'labeller = label_both' includes the factor name with the levels in the labels
  labs(x = "Speaker ethnicity", y = "emmeans ('lah')")   # full set of arguments: labs(title = "", x = "", y = "")


full.lm <- lm(lah ~ Ethnicity * Gender, data = particles)
full.emm <- emmeans(full.lm, pairwise ~ Gender|Ethnicity)

ggplot(plot(full.emm, plotit = FALSE), aes(x = Gender, y = the.emmean)) +
  geom_linerange(mapping = aes(y = the.emmean, ymin = lower.CL, ymax = upper.CL), linewidth = 4, color = "blue", alpha = 0.25) +   # this plots the confidence interval bars
  geom_point(size = 2) +   # this plots the emmeans
  coord_flip() +   # this sets the correct orientation where the emmeans are plotted along the horizontal axis
  #  ylim(0,6) +   # use this to set the range on the emmeans axis if you need a standard range to compare confidence intervals across graphs
  facet_grid(Ethnicity ~., labeller = label_both) +   # used for interaction plots, e.g., with Gender as the other interaction factor; 'labeller = label_both' includes the factor name with the levels in the labels
  labs(x = "Speaker gender", y = "emmeans ('lah')")   # full set of arguments: labs(title = "", x = "", y = "")
