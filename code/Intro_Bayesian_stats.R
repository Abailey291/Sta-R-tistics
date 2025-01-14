#######################################################################################
#
#
#                                 Class XXX Bayes Theory
#                             Examples of a Bayesian analysis
#
#
#######################################################################################
######
# See slides on GitHub for theory
#####
########## simulate Monty Hall problem
stay=c()
switch=c()

for (i in 1:10000){
  doors=1:3
  door_win=sample(1:3,1)
  door_pick=sample(1:3,1)
  door_host_open=doors[!(1:3 %in% c(door_pick,door_win))]
  if (length(door_host_open)==2){
    door_host_open= door_host_open[sample(1:2,1)]
  }
  
  if (door_pick==door_win) {
    stay[i]=1
    switch[i]=0
  } else{
    stay[i]=0
    switch[i]=1
  }
  
  prob_stay[i]=sum(stay)/length(stay)
  prob_switch[i]=sum(switch)/length(switch)
  
  
}

sum(stay)/10000
sum(switch)/10000

plot(prob_stay)

# Plot probability over time
plot(prob_stay, type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "iteration", ylab = "probability", ylim = c(0,1))
lines(prob_switch, pch = 18, col = "blue", type = "b", lty = 2)
legend("topleft", legend=c("Stay", "Switch"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)

########## MCMC sampling 
library(coda)
# set up ground truth for sampling 
trueA <- 5
trueB <- 0
trueSd <- 10
sampleSize <- 31

# estimate x and y values in a linear relationship (linear regression where trueA is the slope and trueB is the intercept)
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)

# define useful functions
    likelihood <- function(param){
          a = param[1]
          b = param[2]
          sd = param[3]
          
          pred = a*x + b
          singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
          sumll = sum(singlelikelihoods)
          return(sumll)    
    }
    prior <- function(param){
          a = param[1]
          b = param[2]
          sd = param[3]
          aprior = dunif(a, min=0, max=10, log = T)
          bprior = dnorm(b, sd = 5, log = T)
          sdprior = dunif(sd, min=0, max=30, log = T)
          return(aprior+bprior+sdprior)
    }
    proposalfunction <- function(param){
          return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
    }
    run_metropolis_MCMC <- function(startvalue, iterations, burnin, thinning ){
          chain = array(dim = c(iterations+1,3))
          chain[1,] = startvalue
          for (i in 1:iterations){
            proposal = proposalfunction(chain[i,])
            
            probab = exp(likelihood(proposal)+ prior(proposal) - likelihood(chain[i,])- prior(chain[i,]))
            if (runif(1) < probab){
              chain[i+1,] = proposal
            }else{
              chain[i+1,] = chain[i,]
            }
          }
          chain=chain[seq(burnin,iterations,thinning),]
          
          return(mcmc(chain))
    }

# sample chain
chain1 = run_metropolis_MCMC(1, 100, 1,1)
summary(chain1)
plot(chain1)
acf(chain1[,1], lag.max= 10, type = "correlation", plot= TRUE)


chain1 = run_metropolis_MCMC(1, 1000, 1,1)
summary(chain1)
plot(chain1)
acf(chain1[,1], lag.max= 100, type = "correlation", plot= TRUE)


chain1 = run_metropolis_MCMC(1, 1000, 100,1)
summary(chain1)
plot(chain1)
acf(chain1[,3], lag.max= 100, type = "correlation", plot= TRUE)

chain1 = run_metropolis_MCMC(1, 10000, 1,1)
summary(chain1)
plot(chain1)
acf(chain1[,1], lag.max= 100, type = "correlation", plot= TRUE)

# thinning examples
chain1 = run_metropolis_MCMC(1, 10000, 1,2)
summary(chain1)
plot(chain1)
acf(chain1[,1], lag.max= 100, type = "correlation", plot= TRUE)

chain1 = run_metropolis_MCMC(1, 10000, 1,10)
summary(chain1)
plot(chain1)
acf(chain1[,1], lag.max= 100, type = "correlation", plot= TRUE)

# Burn in example 
chain1 = run_metropolis_MCMC(1, 10000, 500,1)
summary(chain1)
plot(chain1)
acf(chain1[,1], lag.max= 100, type = "correlation", plot= TRUE)

chain1 = run_metropolis_MCMC(1, 10000, 500,10)
summary(chain1)
plot(chain1)
acf(chain1[,1], lag.max= 100, type = "correlation", plot= TRUE)



########## Bayesian T-Test
library(BEST)

# Example of two-sample t-test where we will
# run bayes on first study/sample to set priors and then apply to them to second study 

hot100=read.csv('/Users/jasondsc/Documents/GitHub/Sta-R-tistics/data/Hot_100_Audio_Features.csv')
# clean data and split into two datasets
hot100=hot100[!is.na(hot100$spotify_track_explicit),]
hot100=hot100[!is.na(hot100$loudness),]
ids=sample(nrow(hot100), ceiling(nrow(hot100)/3))
train=hot100[ids,]
test=hot100[!(seq(1,nrow(hot100)) %in% ids),]

sample1=train$loudness[train$spotify_track_explicit==FALSE]
sample2=train$loudness[train$spotify_track_explicit==TRUE]
hist( sample1, col=rgb(0,0,1,1/4)) 
hist( sample2, col=rgb(1,0,0,1/4), add=T)  

BESTout_study1 <- BESTmcmc(sample1,sample2, priors=NULL, parallel=FALSE, numSavedSteps =5000 , thinSteps = 2,burnInSteps =500 )

plot(BESTout_study1)

plot(BESTout_study1, ROPE=c(-0.1,0.1))

plot(BESTout_study1, which="sd")

print(BESTout_study1)

plotAll(BESTout_study1)

# trace plots
BESTout_study1$sampleNum=1:5001
ggplot(BESTout_study1[1:5001,], aes(x=sampleNum, y=mu1)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")
ggplot(BESTout_study1[1:5001,], aes(x=sampleNum, y=mu2)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")
ggplot(BESTout_study1[1:5001,], aes(x=sampleNum, y=nu)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")
ggplot(BESTout_study1[1:5001,], aes(x=sampleNum, y=sigma1)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")
ggplot(BESTout_study1[1:5001,], aes(x=sampleNum, y=sigma2)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")

priors <- list(muM = c(mean(BESTout_study1$mu1),mean(BESTout_study1$mu2)), muSD = c(mean(BESTout_study1$sigma1), mean(BESTout_study1$sigma2)))

sample1=train$loudness[train$spotify_track_explicit==FALSE]
sample2=train$loudness[train$spotify_track_explicit==TRUE]

BESTout_study2 <- BESTmcmc(sample1,sample2, priors=priors, parallel=FALSE, numSavedSteps =5000 , thinSteps = 2,burnInSteps =500 )

plot(BESTout_study2)
print(BESTout_study2)
plotAll(BESTout_study2)

BESTout_study2$sampleNum=1:5001
ggplot(BESTout_study2[1:5001,], aes(x=sampleNum, y=mu1)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")
ggplot(BESTout_study2[1:5001,], aes(x=sampleNum, y=mu2)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")
ggplot(BESTout_study2[1:5001,], aes(x=sampleNum, y=nu)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")
ggplot(BESTout_study2[1:5001,], aes(x=sampleNum, y=sigma1)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")
ggplot(BESTout_study2[1:5001,], aes(x=sampleNum, y=sigma2)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")

cols =c("#87ceeb", "#e37f86")
# Overlay plots 
plot(BESTout_study1[,1:5], xlim=c(-3,0),col = scales::alpha(cols[1], 0.2))
par(new=T)
plot(BESTout_study2[,1:5],xlim=c(-3,0),col = scales::alpha(cols[2], 0.2))


# Example of paired t-test where we will
data("mice2",package = "datarium")
head(mice2)
diff_mice2=mice2$after-mice2$before
# frequentists paifred-ttest
t.test(mice2$after,mice2$before, paired = TRUE, alternative = "two.sided")

BESTout_mice2 <- BESTmcmc(diff_mice2, parallel=FALSE, numSavedSteps =100 , thinSteps = 1,burnInSteps =0 )
plot(BESTout_mice2)

BESTout_mice2$sampleNum=1:102
ggplot(BESTout_mice2[1:102,], aes(x=sampleNum, y=mu)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")
ggplot(BESTout_mice2[1:102,], aes(x=sampleNum, y=nu)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")
ggplot(BESTout_mice2[1:102,], aes(x=sampleNum, y=sigma)) + geom_line()+ theme_minimal() + xlab(label = "Sample number")



########## Bayesian Linear Regression 
suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))
library(datasets)
library(bridgesampling)
library(sjPlot)

data= iris

model_freq<-lm(Sepal.Length~ Petal.Width +Petal.Length +Species, data=data)
summary(model_freq)
confint(model_freq)
tab_model(model_freq)


?priors
model_bayes <- stan_glm(Sepal.Length~ Petal.Width +Petal.Length +Species, data=data, seed=1111, prior = NULL, warmup= 300, chains=3, iter= 5000)

# trace plots 
mcmc_trace(as.array(model_bayes))
mcmc_trace_data(model_bayes) # get trace raw data

# plot auto correlation function of mcmc samples
plot(model_bayes, "acf")
mcmc_acf(model_bayes)

mcmc_combo(as.array(model_bayes))

mcmc_dens(as.array(model_bayes))
mcmc_dens_chains_data(model_bayes) # gets data 4 density of posteriors 

# compute Rhat statistics (should be between 0.9-1.1)
rhat <- summary(model_bayes)[, "Rhat"]

# compute 95% HPD intervals 
hdi(model_bayes, ci = 0.95)

# compute bayesfactor 
bayesfactor(model_bayes)

model_bayes2 <- stan_glm(Sepal.Length~ Petal.Width +Petal.Length , data=data, seed=1111, prior = NULL, warmup= 300, chains=3, iter= 5000)

bayesfactor(bridge_sampler(model_bayes), bridge_sampler(model_bayes2))




# Run code in BRMS (Bayesian Regressions)
####################################################################
library(brms)
# Let us look at an experiment where participants preformed both a Flanker AND a Stroop Task
data=read.csv('~/Documents/GitHub/Sta-R-tistics/data/Cognitive_control.csv')

model_Dprime = brms::brm(
  RT ~  (1 |subject) + current.trial, data = data, iter=10000, thin=10)

get_prior(RT ~  (1 |subject) + current.trial, data = data)

summary(model_Dprime)
plot(model_Dprime)

model_Dprime2 <- update(model_Dprime, formula. = ~ . - Load:Task)

model_Dprime_loo=LOO(model_Dprime, reloo = TRUE)
model_Dprime2_loo=LOO(model_Dprime2, reloo = TRUE)
loo_compare(model_Dprime, model_Dprime2, reloo = TRUE)

model_C = brms::brm(
  C ~  (1 |subject_code) + Load+Task+Load:Task, 
  data = df2, control=list(adapt_delta=0.99), iter=10000, thin=10
)

get_prior(C ~  (1 |subject_code) + Load+Task+Load:Task, data = df2)

summary(model_C)
plot(model_C)

mod1_mcmc = as.mcmc(model_Dprime)
mod1_mcmc = coda::as.mcmc.list(mod1_mcmc)

mod2_mcmc = as.mcmc(model_C)
mod2_mcmc = coda::as.mcmc.list(mod2_mcmc)


coda::HPDinterval(runjags::combine.mcmc(mod1_mcmc))
coda::HPDinterval(runjags::combine.mcmc(mod2_mcmc))





