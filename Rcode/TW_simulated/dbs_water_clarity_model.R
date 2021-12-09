# rm(list=ls())
library(jagsUI)
library(rjags)

######################################
### SIMULATE DATA ####################
######################################


####### Generate data for dBS model #############
# number of lakes
n = 100
# intercept
a <- -1
# 
beta1 <- 0.8 # intercept before change point
beta2 <- 0 # slope before change point
# 
delta1 <- -1.5 # change in the intercept after change point
delta2 <- -1.9 # Change in slope after the change point
# Change-point
phi <- 1 
# residual SD
sigma <- 0.5
# centered values of covariate
x <- seq(-1, 3, length=n) 

# linear predictor (deterministic part of the model)
mui <- (beta1 + ifelse(x <= phi, 0, delta1)) + beta2*(x-phi) + ifelse(x <= phi, 0, delta2*pmax(0, x-phi ))

# Generate values (stochastic, random part of the model)
set.seed(1224) # so we all get the same random numbers
yi <- rnorm(n, mean=mui, sd=sigma)
plot(yi~x)
#################################################



#################################################################
########## BUGS CODE ############################################
#################################################################

# Define the model in the BUGS language and write a text file
sink("model.txt")
cat("
model {

#### Disjointed broken stick

for (i in 1:N){
	y2[i] ~ dnorm(temp2[i], tau2)
	temp2[i] <- (beta1+delta1*step(x[i]-phi)) +
	(beta2+delta2*step(x[i]-phi))*(x[i]-phi)
# pred2[i] <- temp2[i] # predicted value
# resid2[i] <- y2[i] - pred2[i] # residual

}

delta1 ~ dnorm(0, 0.01)
delta2 ~ dnorm(0, 0.01)
beta1 ~ dnorm(0, 0.001)
beta2 ~ dnorm(0, 0.001)

tau2 <- pow(sigma2, -2)
sigma2 ~ dunif(0, 10)
phi ~ dunif(low,up)

} # end model
",fill = TRUE)
sink()

low <- min(x)
up <- max(x)
N <- length(x)

# Load data
data <- list(y2=yi, N = N, x=x,low=low,up=up)


# Initial values
inits <- function (){
  list (beta1 = rnorm(1),beta2=rnorm(1),delta1 = rnorm(1),delta2=rnorm(1),
	phi = runif(1,low,up),
	sigma2=runif(1) )
}


# Parameters monitored
parameters <- c("beta1","beta2", "delta1","delta2","phi","sigma2")


# MCMC settings
ni <- 10000
nt <- 1
nb <- 5000
nc <- 1



out <- jags(data = data, inits = inits, parameters.to.save = parameters, 
model.file = "model.txt", n.chains = nc, n.thin = nt, n.iter = ni, 
n.burnin = nb,parallel = T)

# Summarize posteriors
print(out, dig = 3)


############## DATA 2 #############
######################################
### SIMULATE DATA ####################
######################################

# beta1 <- 0.8
# beta2 <- 0
# # 
# delta1 <- -1
# delta2 <- -1.9
# # Change-point
# phi <- 0.5 

####### Generate data for dBS model #############
# number of lakes
n = 100
# intercept
a <- -1
# 
beta1 <- -1.0
beta2 <- 0
# 
delta1 <- -1
delta2 <- -0.5
# Change-point
phi <- 0.0 
# residual SD
sigma <- 0.5
# centered values of covariate
x <- seq(-1, 3, length=n) 

# linear predictor (deterministic part of the model)
mui <- (beta1 + ifelse(x <= phi, 0, delta1)) + beta2*(x-phi) + ifelse(x <= phi, 0, delta2*pmax(0, x-phi ))

# Generate values (stochastic, random part of the model)
set.seed(122891) # so we all get the same random numbers
yi2 <- rnorm(n, mean=mui, sd=sigma)
plot(yi2~x)

# Load data
data <- list(y2=yi2, N = N, x=x,low=low,up=up)


# Initial values
inits <- function (){
  list (beta1 = rnorm(1),beta2=rnorm(1),delta1 = rnorm(1),delta2=rnorm(1),
        phi = runif(1,low,up),
        sigma2=runif(1) )
}


# Parameters monitored
parameters <- c("beta1","beta2", "delta1","delta2","phi","sigma2")


# MCMC settings
ni <- 10000
nt <- 1
nb <- 5000
nc <- 1



out2 <- jags(data = data, inits = inits, parameters.to.save = parameters, 
            model.file = "model.txt", n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb,parallel = T)

########## SCATTER PLOT 

# break-point 
kp <- out$summary["phi",1]
# break-point 
kp2 <- out2$summary["phi",1]

# Fake predictor
X3 <- x

# CRI for k

NN <- length(out$sims.list$phi)

linPredPopAve <- matrix(NA, ncol=length(X3), nrow=NN ) #container for predicted values
dim(linPredPopAve)

linPredPopAve2 <- matrix(NA, ncol=length(X3), nrow=NN ) #container for predicted values


ests1 <- out$sims.list$beta1
ests2 <- out$sims.list$beta2
ests3 <- out$sims.list$delta1
ests4 <- out$sims.list$delta2

ests12 <- out2$sims.list$beta1
ests22 <- out2$sims.list$beta2
ests32 <- out2$sims.list$delta1
ests42 <- out2$sims.list$delta2


for(i in 1:NN){
  for(t in 1:length(X3)){
    linPredPopAve[i,t] <- (ests1[i] + ifelse(X3[t] <= kp, 0, ests3[i])) + ests2[i]*(X3[t]-kp) + ifelse(X3[t] <= kp, 0, ests4[i] *pmax(0, X3[t] - kp) )
    linPredPopAve2[i,t] <- (ests12[i] + ifelse(X3[t] <= kp2, 0, ests32[i])) + ests22[i]*(X3[t]-kp2) + ifelse(X3[t] <= kp2, 0, ests42[i] *pmax(0, X3[t] - kp2) )
    
  }
}


dim(linPredPopAve)
probPredPopAve <- linPredPopAve
probPredPopAve2 <- linPredPopAve2

meanProbPopAve <- apply(probPredPopAve, 2, mean)
upperCI.PopAve <- apply(probPredPopAve, 2, quantile, probs=c(0.975) )
lowerCIA.PopAve <- apply(probPredPopAve, 2, quantile, probs=c(0.025) )

meanProbPopAve2 <- apply(probPredPopAve2, 2, mean)
upperCI.PopAve2 <- apply(probPredPopAve2, 2, quantile, probs=c(0.975) )
lowerCIA.PopAve2 <- apply(probPredPopAve2, 2, quantile, probs=c(0.025) )

################# PLOT ############################

res <- 10
name_figure <- "dBS_water_clarity.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)
def.par <- par(no.readonly = TRUE)

size.labels = 1
size.text = 1

axissize <- 1
x.label = 'Fire disturbance gradient'
y.label = 'Water clarity'


nf <- layout(matrix(c(1:1),nrow=1,ncol=1,byrow=TRUE),  TRUE) 
# layout.show(nf)
par(mar=c(0.0,0.1,0.1,0.1),oma=c(3,4,0,1),mai=c(0.0,0.05,0.05,0) )

# Scatter plot

plot(x,yi,  axes=F,ylim=c(min(yi),max(yi)),xlim=c(min(X3), max(X3)), ylab='', xlab='', type='n')

axis(side=1,cex.axis=axissize , mgp=c(1,0.5,0),tck= -0.01, labels=F)
axis(side=2,cex.axis=axissize , mgp=c(1,0.5,0),tck= -0.01, las=1, labels=F)


i.for <- order(X3)
i.back <- order(X3, decreasing = TRUE )
x.polygon <- c( X3[i.for] , X3[i.back] )
y.polygon <- c( lowerCIA.PopAve[i.for] , upperCI.PopAve[i.back] )
polygon( x.polygon , y.polygon , col = "lightgray" , border = NA )

i.for <- order(X3)
i.back <- order(X3, decreasing = TRUE )
x.polygon <- c( X3[i.for] , X3[i.back] )
y.polygon <- c( lowerCIA.PopAve2[i.for] , upperCI.PopAve2[i.back] )
polygon( x.polygon , y.polygon , col = "lightgray" , border = NA )

points(x,yi, cex=2.5, pch=16, col='royalblue4')
points(x,yi2, cex=2.5, pch=16, col='firebrick')

lines(X3,meanProbPopAve , lwd = 2, col="royalblue4", lty = 1)
lines(X3,meanProbPopAve2 , lwd = 2, col="firebrick", lty = 1)

mtext(y.label, line = 0.8, side = 2, cex = size.text,outer=T)
mtext(x.label, line = 0.8, side = 1, cex = size.text, outer=T)
box()

par(def.par)
dev.off()
