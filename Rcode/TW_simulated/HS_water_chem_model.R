# rm(list=ls())
library(jagsUI)




######################################
### SIMULATE DATA ####################
######################################


####### Generate data for broken-stick model #############
# number of lakes
n = 100
# intercept
a <- 0
# slope before change-point
b <- 0.0 
# Slope after change-point
delta <- 1.5
# Change-point
phi <- 0.5 
# residual SD
sigma <- 0.3
# centered values of covariate
x <- seq(-1, 3, length=n) 

# linear predictor (deterministic part of the model)
mui <- a + b * x + ifelse(x <= phi,0,delta*pmax(0,x-phi))


# Generate values (stochastic, random part of the model)
set.seed(12345)
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

# Piece-wise (broken-stick) regresson
for (i in 1:N) {
	y2[i] ~ dnorm(temp2[i], tau2)
	# temp2[i] <- a2 + (b2 + delta*step(x[i]-phi))*(x[i]-phi)  
	pred2[i] <- temp2[i] # predicted value
	resid2[i] <- y2[i] - pred2[i] # residual
 temp2[i] <- a2 + b2 * x[i] + delta * (x[i]-phi) * step(x[i]-phi)

}


# prior specifications for Model 2
tau2 <- pow(sigma2,-2)
sigma2 ~ dunif(0, 100)
# Priors
a2 ~ dnorm(0, 0.001)
b2 ~ dnorm(0, 0.001)
delta ~ dnorm(0, 0.001)
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
  list (a2 = rnorm(1), b2 = rnorm(1),delta = rnorm(1),
	phi = runif(1,low,up),
	sigma2=runif(1) )
}


# Parameters monitored
parameters <- c("a2", "b2", "delta","phi","sigma2")


# MCMC settings
ni <- 10000
nt <- 1
nb <- 5000
nc <- 1


out <- jags(data = data, inits = inits, parameters.to.save = parameters, 
model.file = "model.txt", n.chains = nc, n.thin = nt, n.iter = ni, 
n.burnin = nb, parallel = T)

# Summarize posteriors
print(out, dig = 3)

#### DATA 2 ##############
####### Generate data for broken-stick model #############
# number of lakes
n = 100
# intercept
a <- -0.5
# slope before change-point
b <- 0.0
# Slope after change-point
delta <- 0.8
# Change-point
phi <- 1.5 
# residual SD
sigma <- 0.3
# centered values of covariate
x <- seq(-1, 3, length=n) 

# linear predictor (deterministic part of the model)
mui <- a + b * x + ifelse(x <= phi,0,delta*pmax(0,x-phi))

# Generate values (stochastic, random part of the model)
set.seed(123)
yi2 <- rnorm(n, mean=mui, sd=sigma)

plot(yi2~x)

# Load data
data <- list(y2=yi2, N = N, x=x,low=low,up=up)


# Initial values
inits <- function (){
  list (a2 = rnorm(1), b2 = rnorm(1),delta = rnorm(1),
        phi = runif(1,low,up),
        sigma2=runif(1) )
}


# Parameters monitored
parameters <- c("a2", "b2", "delta","phi","sigma2")


# MCMC settings
ni <- 10000
nt <- 1
nb <- 5000
nc <- 1


out2 <- jags(data = data, inits = inits, parameters.to.save = parameters, 
            model.file = "model.txt", n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb, parallel = T)

# Summarize posteriors
print(out2, dig = 3)


########## SCATTER PLOT 



NN <- length(out$sims.list$phi)

# break-point 
kp <- out$summary["phi",1]
kp2 <- out2$summary["phi",1]

# Fake predictor
X3 <- x

linPredPopAve <- matrix(NA, ncol=length(X3), nrow=NN ) #container for predicted values
dim(linPredPopAve)

linPredPopAve2 <- matrix(NA, ncol=length(X3), nrow=NN ) #container for predicted values


mu.A <- out$sims.list$a2
mu.B <- out$sims.list$b2
mu.C <- out$sims.list$delta

mu.A2 <- out2$sims.list$a2
mu.B2 <- out2$sims.list$b2
mu.C2 <- out2$sims.list$delta

for(i in 1:NN){
  for(t in 1:length(X3)){
    linPredPopAve[i,t] <- mu.A[i] + mu.B[i]*X3[t] + ifelse(X3[t] <= kp, 0, mu.C[i]*pmax(0, X3[t] - kp))
    linPredPopAve2[i,t] <- mu.A2[i] + mu.B2[i]*X3[t] + ifelse(X3[t] <= kp2, 0, mu.C2[i]*pmax(0, X3[t] - kp2))
    
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


res <- 10
name_figure <- "hockey_stick_water_chem.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)
def.par <- par(no.readonly = TRUE)

size.labels = 1
size.text = 1

axissize <- 1
x.label = 'Fire disturbance gradient'
y.label = 'Nutrients, sediments, DOC'


nf <- layout(matrix(c(1:1),nrow=1,ncol=1,byrow=TRUE),  TRUE) 
# layout.show(nf)
par(mar=c(0.0,0.1,0.1,0.1),oma=c(3,4,0,1),mai=c(0.0,0.05,0.05,0) )

# Scatter plot

plot(x,yi,  axes=F,ylim=c(min(yi2),max(yi)),xlim=c(min(X3), max(X3)), ylab='', xlab='', type='n')

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

points(X3,yi, cex=2.5, pch=16, col='royalblue4')
points(X3,yi2, cex=2.5, pch=16, col='firebrick')

lines(X3,meanProbPopAve , lwd = 2, col="royalblue4", lty = 1)
lines(X3,meanProbPopAve2 , lwd = 2, col="firebrick", lty = 1)

mtext(y.label, line = 0.8, side = 2, cex = size.text,outer=T)
mtext(x.label, line = 0.8, side = 1, cex = size.text, outer=T)
box()

par(def.par)
dev.off()

