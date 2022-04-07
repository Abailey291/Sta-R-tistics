#Welcome to the R Workshop! Today, we'll be using a few examples from last time, combined with some tips and tricks influenced by the Book of R, and finally a note on PCA (based on code from https://www.datacamp.com/community/tutorials/pca-analysis-r 
https://www.geeksforgeeks.org/principal-component-analysis-with-r-programming/) 

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

#The basis of everything - flipping a coin with the binomial distribution

X.prob<- dbinom(x = 0: 8 , size = 8, prob = 1/2)
sum(X.prob)
barplot(X.prob, names.arg = 0:8, space = 0, xlab = "x", ylab = "Pr(X = x)")

pbinom(q = 3, size = 8, prob = 1/2)
sum(dbinom(x = 0:3 , size = 8, prob = 1/2))

rbinom(n = 1, size = 8, prob = 1/2)

#Let's take a look at the normal distribution!

#Probability Density Function
xvalues<- seq(-4, 4, length = 50)
fx<-dnorm(xvalues, mean = 0, sd = 1)
fx

#The 68, 95, 99.7 Rule
pnorm(q = 1) - pnorm(q = -1)
pnorm(q = 2) - pnorm(q = -2)
pnorm(q = 3) - pnorm(q = -3)

#To figure out where you are on the normal curve
qnorm(p = 0.75, mean = 1, sd = 3)

#To generate a random normal distribution
rnorm(n = 100, 50, 24.8)

#Example
#Assume that the average weight of a snack food bag is 100.2grams, with an sd of 0.5 grams.The manufacturer then goes to weigh a randomly selected bag. What's the prob that the weight of the bag is less than 90 grams?

pnorm(90, 100.2, 0.5)

#What is the prob of the weight being between 97 and 101?

pnorm(101, 100.2, 0.5) - pnorm(97, 100.2, 0.5)

#What's the weight below the lowest 20% of bags

qnorm(0.2,100.2, 0.5)

-------------------------------------------------------------------------------------
# Say that we wanted to have a CI
main_data<-seq(1, 10, 50)
n<-length(main_data)
n
p_desired <- 0.05
shapiro.test(main_data)
hist(main_data)

temp.mean <- mean(main_data)

temp.mean
temp.sd<-sd(main_data)
temp.se<- temp.sd/sqrt(n)

p <- (1- (p_desired/2))
p
i <- n - 1 
crit_val<- qt(p, df = i)
confidence_error<- crit_val*temp.se

#To double check that we have 95% of our distribution in the center
pt(crit_val, i) - pt(-crit_val, i)

#To get both values at once
temp.mean + c(-1,1)*confidence_error

# t-tests

#One Sample t-test
#Say that the mean of the population is 45

example_data<- seq(20, 90, length = 50)

example_data.mean<- mean(example_data)
example_data.sd<- sd(example_data)
data_t_test<- (main_data.mean - 45)/(main_data.sd)
pt(data_t_test, df = (length(example_data.mean)-1))

#We can solve this just by writing

t.test(x = example_data, mu = 45, alternative = "two-sided")
t.test(x = example_data, mu = 45, alternative = "two-sided")$conf.int

#Two means

control<- c(1:50)

#Unpooled Variance (quite robust; called the Welsh's t-test)
statistic<-t.test(example_data, control, conf.level = 0.95)
statistic$p.value

#Dependent t-tests

before<- seq(1, 5)
after<- seq(6, 10)

difference<- after - before

t.test(x = after, y = before, alternative = "two-sided", conf.level = 0.95, paired = TRUE)

--------------------------------------------------------------------------------------
#ANOVA (Analysis of Variance) - Same example as last time!

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

####Post-Hoc Analysis

#install.packages("DescTools")
library(DescTools)
#Note: x must be an aov object for this method to work!

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

-------------------------------------------------------------------------------
#Data for Simple and Multiple Regression will be placed here ASAP! 
-------------------------------------------------------------------------------

#Logistic Regression - Review from last time!


hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100_4anova$spotify_track_explicit=plyr::mapvalues(hot100_4anova$spotify_track_explicit, c(TRUE, FALSE), c(1,0))

glm0= glm(spotify_track_explicit ~ danceability + energy + loudness + valence, hot100_4anova, family = 'binomial')
summary(glm0)
sjPlot::tab_model(glm0) # reports odds ratio
sjPlot::tab_model(glm0, transform = NULL) # reports log odds (see slides for explination)

plot(glm0)
residualPlots(glm0)

confint(glm0) #In log likelihood
confint.default(glm0) #In SE

# we can build a regression model and use it to predict held back data:
ids=sample(length(hot100_4anova$SongID),ceiling(length(hot100_4anova$SongID)/3))
sample1=hot100_4anova[ids,]
sample2=hot100_4anova[!(seq(1,length(hot100_4anova$SongID)) %in% ids),]

lm0= lm(danceability ~  energy + loudness + valence, sample1)

predicted_labels=predict(lm0, sample2) # predict dance-ability from regression above
error_predicted=predicted_labels-sample2$danceability # look at the error between true and predicted values 


------------------------------


#Hierarchical Regression

# let us now look at epilepsy data where patients, seen multiple times. Number of seizures were recorded per visit. Patients recieved treatment for 
# anti-convulsants

epilepsy=brms::epilepsy
epilepsy$Trt=as.factor(epilepsy$Trt)

lm0=lm(count ~ Age  + Trt, epilepsy)
summary(lm0)


## The above model (you built) is good, but it can be better.....
# We can take into account how observations are in fact nested within participants (repeated measures design)
# to do this we will build a model where we allow participants to receive a unique intercept 
# this intercept fit per particpant will take into account inter-individual differences 

lme0=lme4::lmer(count ~ Age  + Trt + (1| patient), epilepsy)
summary(lme0)
sjPlot::tab_model(lme0)
# we now see that the outputs of the regression are broken down into both fixed and random effects
# fixed effects are between and random are repeated (see slides for details)
coef(summary(lme0)) # this will give you the fixed effects reported in summary 

ranef(lme0)$patient # and these are the values for the random intercepts fit per person
# the random intercepts should have the same number of participants (in order)

# the above multi-level model assumes participants differ in terms of their overall number of seizures, and so to account for this
# each subject was given a different intercept (i.e., they all start at a different point) but their overall effect is consistent 

# what if we wanted to test a different hypothesis where the effect of the treatment may be different for each subject
# but that their starting point (intercept) was similar across everyone?

# here we would run a model with a random SLOPE and INTERCEPT 

lme1=lme4::lmer(count ~ Age  + Trt + (1 + Trt| patient), epilepsy)
summary(lme1)
sjPlot::tab_model(lme1)
coef(lme1) # notice how both the intercept and slope vary per subject 

------------------------------

#Bootstrapping - Review from last time!

bootstrap_data= data.frame( data$Sepal.Length[sample(nrow(data), replace = TRUE)], data$Petal.Length[sample(nrow(data), replace = TRUE)])
colnames(bootstrap_data) = c("Sepal.Length", "Petal.Length")

apply(bootstrap_data, 2, mean)
apply(bootstrap_data, 2, sd)

########## Exercise 7
##########################################
# repeat bootstrap 1000 times, save the mean and sd every time, plot a histogram of these values. What do you see?
# can we make CI around this distribution? This is what we call bootstrapping an estimate (i.e, bootstrap CI, mean etc)

bootstrap_mean_SL = c()
bootstrap_mean_PL = c()
bootstrap_sd_SL = c()
bootstrap_sd_PL = c()

for (i in 1:1000){
  
  bootstrap_data= data.frame( data$Sepal.Length[sample(nrow(data), replace = TRUE)], data$Petal.Length[sample(nrow(data), replace = TRUE)])
  colnames(bootstrap_data) = c("Sepal.Length", "Petal.Length")
  
  bootstrap_mean_SL[i]=apply(bootstrap_data, 2, mean)[1]
  bootstrap_mean_PL[i]=apply(bootstrap_data, 2, mean)[2]
  bootstrap_sd_SL[i]=apply(bootstrap_data, 2, sd)[1]
  bootstrap_sd_PL[i]=apply(bootstrap_data, 2, sd)[2]
  
  
}

hist(bootstrap_mean_SL)
quantile(bootstrap_mean_SL, 0.025)
quantile(bootstrap_mean_SL, 0.975)

--------------------------------------------

####BONUS Content (slightly more advanced)

#Doing a PCA analysis (example taken from online)
```{r}
# Loading Data
data(mtcars)

# Apply PCA using prcomp function
# Need to scale / Normalize as
# PCA depends on distance measure
mtcars_pca <- prcomp(mtcars[,c(1:7,10,11)], scale = TRUE,
				center = TRUE, retx = T)
names(mtcars_pca)
str(mtcars_pca)

# Summary #This is key for understanding the plot
summary(mtcars_pca)
mtcars_pca

# View the principal component loading - The relationship (correlation or anticorrelation, etc) between the initial variables and the principal components
mtcars_pca$rotation

# See the values of each sample by principal components
dim(mtcars_pca$x)
mtcars_pca$x

#Super cool way of looking at the data!

library("devtools")
install_github("vqv/ggbiplot")

library(ggbiplot)
ggbiplot(mtcars_pca)

ggbiplot(mtcars_pca, labels=rownames(mtcars))

mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))

ggbiplot(mtcars_pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars.country)

# Compute standard deviation
mtcars_pca$sdev

# Compute variance
my_pca.var <- (mtcars_pca$sdev)^2
my_pca.var

# Proportion of variance for a scree plot
propve <- my_pca.var / sum(my_pca.var)
propve

# Plot variance explained for each principal component
plot(propve, xlab = "principal component",
			ylab = "Proportion of Variance Explained",
			ylim = c(0, 1), type = "b",
			main = "Scree Plot")

# Plot the cumulative proportion of variance explained
plot(cumsum(propve),
	xlab = "Principal Component",
	ylab = "Cumulative Proportion of Variance Explained",
	ylim = c(0, 1), type = "b")

# Find Top n principal component
# which will atleast cover 90 % variance of dimension
which(cumsum(propve) >= 0.9)[1]

# Predict mpg using first 4 new Principal Components
# Add a training set with principal components
train.data <- data.frame(disp = mtcars$disp, mtcars_pca$x[, 1:4])

# Running a Decision tree algorithm
## Installing and loading packages
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

rpart.model <- rpart(disp ~ .,
					data = train.data, method = "anova")

rpart.plot(rpart.model)

