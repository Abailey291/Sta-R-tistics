#First step
```{r}
#First, let's get our libraries in order
library("tidyverse")
library("GGally")
library("ggpubr")
library("ggsignif")
library("interactions")
library(MBESS)
library(lmtest)
library(car) #Check out Anova function! *It's special!!!
library(sjPlot)
```

#The basis of everything - flipping a coin with the binomial distribution
```{r}
X.prob<- dbinom(x = 0: 8 , size = 8, prob = 1/2)
sum(X.prob)
barplot(X.prob, names.arg = 0:8, space = 0, xlab = "x", ylab = "Pr(X = x)")

pbinom(q = 3, size = 8, prob = 1/2)
sum(dbinom(x = 0:3 , size = 8, prob = 1/2))

rbinom(n = 1, size = 8, prob = 1/2)
```

#ANOVA (Analysis of Variance)
```{r}

hot100=read.csv('/Users/jasondsc/Documents/GitHub/Sta-R-tistics/data/Hot_100_Audio_Features.csv') # read data
hot100=hot100[!is.na(hot100$spotify_track_explicit),] # remove NAs
hot100=hot100[!is.na(hot100$loudness),] # remove NAs

## ANOVAs
# let us try a one way anova 
hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
levels(hot100_4anova$key) # these are the levels of your first factor! 
anova0=aov(danceability ~ key, hot100_4anova)

summary(anova0) # summary of effects

# 1. Homogeneity of variances
# one check you can do is plot the residuals against the fitted values, there should be no relationship 
plot(anova0, 1)

# you can also run a Levne Test
car::leveneTest(danceability ~ key, hot100_4anova)
# despite a seemingly small if any relationship we get a significant Levene test... WHY?

# 2. Normality
# let us plot a q-qplot
plot(anova0, 2)

# Can check the normality of the residuals 
# Extract the residuals
aov_residuals = residuals(object = anova0 )
hist(aov_residuals)
# Run Shapiro-Wilk test
shapiro.test(aov_residuals )

## Running a two-way anova is just as easy!
# we will look at how explict tracks and key affect dancibility

hot100_4anova = hot100_4anova[!is.na(hot100_4anova$key),] # remove nas
hot100_4anova$key_binary = as.numeric(hot100_4anova$key)>=6

anova0=aov(danceability ~ spotify_track_explicit + key_binary, hot100_4anova)
summary(anova0)

# compute cell means
hot100_4anova %>% group_by(spotify_track_explicit, key_binary) %>% summarise(m=mean(danceability))

```
#Post-Hoc Analysis
```{r}

#install.packages("DescTools")
library(DescTools)

#Note: x must be an aov object!

#Part 1: Use a Bonferoni Correction (very simple!)

PostHocTest(anova0, method = "bonferroni", conf.level = 0.95)

#Part 2: Use a Tukey test

PostHocTest(anova0, method = "hsd", conf.level = 0.95)

#Part 3: Use another test (like Sheffe!)

PostHocTest(anova0, method = "scheffe", conf.level = 0.95)

# Another way of looking at post-hoc
TukeyHSD(anova0)
pairwise.t.test(hot100_4anova$danceability, hot100_4anova$key,
                p.adjust.method = "BH")

```
