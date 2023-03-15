# The code was written under R version 4.1.2 (2021-11-01) -- "Bird Hippie"
# The code was adapted for the data of the NPO "Sentient" 
library(ggpubr)
library(reshape2)
library(ggplot2)
library(DescTools)
library(ggsignif)
library(stringr)
library(dplyr)
setwd("C:/Trapich/data sciense/sentient")
data <- read.csv("data1.csv", header = TRUE)
View(data)
str(data)

#Before you start to explore the data, check for missing data (NA's or blank slots)
is.na(data)

#If there are numeric features - is the data scaled or do you need to make standardization

#Print some statistics summary data - example: sentient in dogs across groups
model <- lm(data$Dogs ~ data$Group)
model1 <- summary(model)
print(model1)

# Do a primary check and see what the data looks like by plotting it - 
# Sometimes histogram is the best way (for continuous data). Example: average score in neutral group
hist(data$av,
     main="Average score for part two - neutral group",
     xlab="Score",
     xlim=c(1,5),
     ylim = c(0,15),
     col="blue", cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
     freq=TRUE) 

#Sometimes boxplot will visualize it better. Example: sentient score for dogs in all groups 
boxplot(data$Dogs,
        main = "Sentient in dogs for all tested",
        xlab = "Level of Sentient",
        ylab = "number of votes",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

# For categorical data - sometimes barplots are better
barplot(data$Dogs,names.arg=data$Gen,xlab="Gender",ylab="Score for sentient",col="orange",
        main="Sentient for dogs across gender",border="red")

#Let's get some results: Check for normally distributed data:
#Normality test - Shapiro-Wilk. If significant - it's not normally distributed,
# Therefore, do NOT use parametric tests!
shapiro.test(data$Organic_reach) #This is one of the variables tested

#If data is normally distributed - ANOVA test to check for significant differences. Example: sentient score for dogs across groups
ano <- aov(Dogs ~ Group, data = data)
summary(ano)

# Post-hoc Tukey's test for multiple groups ANOVA
PostHocTest(aov(Dogs ~ Cond, data = data), method = "hsd")
# Or you can use:
TukeyHSD(ano)

#If data is not normally distributed or either not enough: compare means - wilcoxon test is the default
compare_means(Dogs ~ Group, data = data)

# We can present different boxplots side by side, when tested groups are separated by color 
# Prefer using colorblind friendly palettes to present the results
dat.m <- melt(data2,id.vars='Group', measure.vars=c('Emojis','Comments', 'Shares','Engagments' ,'Organic_reach', 'People_reached'))
plot_that <- ggplot(dat.m,aes(x=Group, y=value, color=Group, fill=Group)) +
  geom_boxplot(alpha = 0.4, fatten = 3) + 
  scale_y_continuous(n.breaks=4, limits=c(0,6)) +
  scale_color_manual(values = c("#E69F00", "#009E73","#0072B2"), aesthetics = c("color", "fill", "alpha")) 
par(cex.axis=4)
p <- plot_that + facet_wrap(~variable, ncol=3)  #You can split them to make it more readable, #By which parameter do you want to order them?
p <- p + ylab("Number of clicks (log10)") + ggtitle("Facebook data")  
p <- p + theme_classic() + theme(text = element_text(size = 16))
p <- p + stat_compare_means(method= "wilcox.test", paired = FALSE)
p

####Barplot for categorical questions 

#First, force the order of the answers to your liking and not according to the ABC (default)
data$Pets1 <- factor(data$Pets1,levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree" ))

#Then, stack the boxplots together
ggplot(data, aes(x = Emojis, fill = Group)) + scale_y_continuous(n.breaks=4, limits=c(0,5)) +
  geom_bar() + theme_classic() + theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5)) +
  ylab("Number of votes") + xlab("Categories") + ggtitle("Average of score per person for part 2")

# If you wish to rotate the text of X axis so it's more readable, add to "theme":
#, axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +

# You may want to look at the data one group at a time: example from the neutral group alone
# First, force the order of answers to your liking
neut$Pests10 <- factor(neut$Pests10,levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree" ))

#Then, stack the boxplots together
ggplot(neut, aes(x = Pests10)) + scale_y_continuous(n.breaks=5, limits=c(0,30)) + 
  geom_bar(fill = "blue") + theme_classic() + theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5)) +
  ylab("Number of votes") + xlab("Categories") + ggtitle("Pest species have no value and should be removed by whatever means necessary")
