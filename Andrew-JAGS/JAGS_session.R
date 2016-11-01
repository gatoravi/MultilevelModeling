# Andy Stone
# 10/25/2016

# First, download and install JAGS or OpenBUGS/WinBUGS. I recommend JAGS!
  # JAGS: http://mcmc-jags.sourceforge.net/
  # OpenBUGS: http://www.openbugs.net/w/FrontPage
  # WinBUGS: http://www.mrc-bsu.cam.ac.uk/software/bugs/

# Then, install the rjags and R2jags or rbugs and R2WinBUGS packages
  # JAGS: install.packages("rjags"); install.packages("R2jags")
  # BUGS: install.packages("rbugs"); install.packages("R2WinBUGS")

# Packages we'll need for this example
library(rjags); library(R2jags)

# Our example: the varying-intercept model with a group-level predictor (book page 361, 366)
# setwd("~/Box Sync/Classwork/MLM TA")

# Setting up the data (from Gelman & Hill's website)
srrs2 <- read.table ("srrs2.dat", header=T, sep=",")
mn <- srrs2$state=="MN"
radon <- srrs2$activity[mn]
log.radon <- log (ifelse (radon==0, .1, radon))
floor <- srrs2$floor[mn]       # 0 for basement, 1 for first floor
n <- length(radon)
y <- log.radon
x <- floor

# get county index variable
county.name <- as.vector(srrs2$county[mn])
uniq <- unique(county.name)
J <- length(uniq)
county <- rep (NA, J)
for (i in 1:J){
  county[county.name==uniq[i]] <- i
}

srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
cty <- read.table ("cty.dat", header=T, sep=",")
usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- cty[usa.rows,"Uppm"]
u <- log (uranium)



### The actual JAGS code begins here ###

# Saving the model to an object. This allows us to avoid saving the model to a .bug file and sourcing it in
# Make sure it is within quotations!
the.model <- "model{
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- a[county[i]] + b*x[i]
  }
  b ~ dnorm (0, .0001)
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)
  
  for (j in 1:J){
    a[j] ~ dnorm (a.hat[j], tau.a)
    a.hat[j] <- g.0 + g.1*u[j]
  }
  g.0 ~ dnorm (0, .0001)
  g.1 ~ dnorm (0, .0001)
  tau.a <- pow(sigma.a, -2)
  sigma.a ~ dunif (0, 100)
}"

# Defining the data you will pass into the model -- you *already know* these values
radon.data <- list ("n", "J", "x", "y", "county", "u")

# Defining the initial values that your model's parameters (values you *don't* already know)
radon.inits <- function (){list(a=rnorm(J), 
                                b=rnorm(1), 
                                g.0=rnorm(1), 
                                g.1=rnorm(1),
                                sigma.y=runif(1), 
                                sigma.a=runif(1))}

# Defining which parameters of your model you want JAGS to return to you 
# In the book (page 366), they are missing "g.0" and "g.1"
radon.parameters <- c ("a", "b", "sigma.y", "sigma.a", "g.0", "g.1")

# Now, we can actually run the model with the jags() function
radon.3 <- jags(data=radon.data, 
                inits=radon.inits, 
                parameters.to.save=radon.parameters, 
                model.file=textConnection(the.model), # Note the textConnection() function
                n.chains=3, 
                n.iter=500, 
                DIC=F)

plot(radon.3)
radon.3

# Now, with 1500 iterations (what does this mean?)
radon.3.b <- jags(data=radon.data, 
                inits=radon.inits, 
                parameters.to.save=radon.parameters, 
                model.file=textConnection(the.model), # Note the textConnection() function
                n.chains=3, 
                n.iter=1500, 
                DIC=F)

plot(radon.3.b)
radon.3.b

# If we wanted to run it with the model saved in a .bug file
radon.3.c <- jags(data=radon.data, 
                inits=radon.inits, 
                parameters.to.save=radon.parameters, 
                model.file="radon.3.bug", 
                n.chains=3, 
                n.iter=500, 
                DIC=F)











