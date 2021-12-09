#### Load libraries
# rm(list=ls())
library(jagsUI)
library(ggplot2)


####### Generate data Ricker model #############
# number of lakes
n = 100
# rate of increase
a <- 3
# degree of decline
b <-0.05

# residual SD
sigma <- 5

# covariate
x <- seq(10, 50, length=n) 

# linear predictor (deterministic part of the model)
mui <- a * x * exp(-b * x)

# plot(mui~x)

# Generate values
yi <- rnorm(n, mean=mui, sd=sigma)

# plot(yi~x)

# MODEL 1
#################################################################
########## BUGS CODE ############################################
#################################################################
sink("ricker1.txt")
cat("
    model{
    for(i in 1:n){
        yoy[i] ~ dnorm(y.hat[i], tau.y)
        y.hat[i] <- a * adult[i] * exp(-b * adult[i])     
    }

    tau.y <- pow(sigma.y,-2)
    sigma.y ~ dunif(0,10)

    # Priors 
    a ~ dnorm(0, 0.0001)
    b ~ dnorm(0, 0.0001)

} # end model
    ",fill=TRUE)
sink()
################




# load data
data <- list(yoy = yi, adult = x,  n = n)

# Initial values

inits <- function(){list(a = a, b = b,
                         sigma.y = sigma ) }

# Parameters monitored
params1 <- c("a", "b", "sigma.y")


# MCMC settings
ni <- 10000
nt <- 3
nb <- 5000
nc <- 1
###########################
out <- jags(data = data, inits = inits, parameters.to.save = params1, 
                     model.file = "ricker1.txt", n.chains = nc, 
                     n.thin = nt, n.iter = ni, n.burnin = nb, parallel = T)


##############################
print(out)

#### Data set 2 ####################################
####### Generate data Ricker model #############
# number of lakes
n = 100
# rate of increase
a <- 3
# degree of decline
b <-0.03

# residual SD
sigma <- 5

# covariate
x <- seq(10, 50, length=n) 

# linear predictor (deterministic part of the model)
mui <- a * x * exp(-b * x)

# plot(mui~x)

# Generate values
yi2 <- rnorm(n, mean=mui, sd=sigma)

# load data
data <- list(yoy = yi2, adult = x,  n = n)

# Initial values

inits <- function(){list(a = a, b = b,
                         sigma.y = sigma ) }

# Parameters monitored
params1 <- c("a", "b", "sigma.y")


# MCMC settings
ni <- 10000
nt <- 3
nb <- 5000
nc <- 1
###########################
out2 <- jags(data = data, inits = inits, parameters.to.save = params1, 
            model.file = "ricker1.txt", n.chains = nc, 
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = T)
print(out2)

#####################################################
########## SCATTER PLOT FOR MODEL FIT#########
X3 <- x
NN <- length(out$sims.list$a)


linPredPopAve <- matrix(NA, ncol=length(X3), nrow=NN ) #container for predicted values
dim(linPredPopAve)

linPredPopAve2 <- matrix(NA, ncol=length(X3), nrow=NN ) #container for predicted values


mu.A <- out$sims.list$a
mu.B <- out$sims.list$b

mu.A2 <- out2$sims.list$a
mu.B2 <- out2$sims.list$b


for(i in 1:NN){
  for(t in 1:length(X3)){
    linPredPopAve[i,t] <- mu.A[i]* X3[t] * exp(-mu.B[i] * X3[t])
    linPredPopAve2[i,t] <- mu.A2[i]* X3[t] * exp(-mu.B2[i] * X3[t])
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
name_figure <- "PP.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)
def.par <- par(no.readonly = TRUE)

size.labels = 1
size.text = 1

axissize <- 1
x.label = 'Fire disturbance gradient'
y.label = 'Algal biomass'


nf <- layout(matrix(c(1:1),nrow=1,ncol=1,byrow=TRUE),  TRUE) 
# layout.show(nf)
par(mar=c(0.0,0.1,0.1,0.1),oma=c(3,4,0,1),mai=c(0.0,0.05,0.05,0) )

# Scatter plot

plot(x,yi,  axes=F,ylim=c(min(yi),max(yi2)),xlim=c(min(X3), max(X3)), ylab='', xlab='', type='n')

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

points(X3,yi, cex=2.5, pch=16, col='firebrick')
points(X3,yi2, cex=2.5, pch=16, col='royalblue4')

lines(X3,meanProbPopAve , lwd = 2, col="firebrick", lty = 1)
lines(X3,meanProbPopAve2 , lwd = 2, col="royalblue4", lty = 1)

mtext(y.label, line = 0.8, side = 2, cex = size.text,outer=T)
mtext(x.label, line = 0.8, side = 1, cex = size.text, outer=T)
box()

par(def.par)
dev.off()

