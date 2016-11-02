# LOAD LIBRARIES
library(rjags); library(arm); library(coda); library(superdiag); library(lme4)

# LOAD DATA, GO TO http://jgill.wustl.edu/data/asap.rjags.dat AND SAVE THE FILE
source("Article.JPART/asap.rjags.dat")  # READS IN THE LIST VERSION OF THE DATA FOR asap.jags.list
names(asap.jags.list) <- c( "state.id", "contracting", "gov.influence", "leg.influence",
    "elect.board", "years.tenure", "education", "party.ID", "category2", "category3",
    "category4", "category5", "category6", "category7", "category8", "category9",
    "category10", "category11", "category12", "med.time", "medt.contr", "grp.influence",
    "gov.ideology", "lobbyists", "nonprofits", "STATES", "SUBJECTS")

# DEFINE THE MODEL
asap.model2.rjags  <- function()  {
for (i in 1:SUBJECTS) {
    mu[i] <- alpha[state.id[i]]
             + beta[1]*contracting[i] + beta[2]*gov.influence[i] + beta[3]*leg.influence[i] 
             + beta[4]*elect.board[i] + beta[5]*years.tenure[i]  + beta[6]*education[i]
             + beta[7]*party.ID[i]    + beta[8]*category2[i]     + beta[9]*category3[i] 
             + beta[10]*category4[i]  + beta[11]*category5[i]    + beta[12]*category6[i] 
             + beta[13]*category7[i]  + beta[14]*category8[i]    + beta[15]*category9[i] 
             + beta[16]*category10[i] + beta[17]*category11[i]   + beta[18]*category12[i] 
             + beta[19]*med.time[i]   + beta[20]*medt.contr[i]
      grp.influence[i] ~ dnorm(mu[i],tau.y)
    }
    for (j in 1:STATES) {
      eta[j]  <- gamma[1]*gov.ideology[j] + gamma[2]*lobbyists[j] + gamma[3]*nonprofits[j]
      alpha[j] ~ dnorm(eta[j],tau.alpha)
    }
    beta[1]   ~ dnorm(0.070,1);     # PRIOR MEANS FROM KELLEHER AND YACKEE 2009, MODEL 3
    beta[2]   ~ dnorm(-0.054,1);
    beta[3]   ~ dnorm(0.139,1);
    beta[4]   ~ dnorm(0.051,1);      
    beta[5]   ~ dnorm(0.017,1);
    beta[6]   ~ dnorm(0.056,1);
    beta[7]   ~ dnorm(0.039,1);
    beta[8]   ~ dnorm(0.0,1);       # DIFFUSE PRIORS 
    beta[9]   ~ dnorm(0.0,1);      
    beta[10]  ~ dnorm(0.0,1);      
    beta[11]  ~ dnorm(0.0,1);      
    beta[12]  ~ dnorm(0.0,1);      
    beta[13]  ~ dnorm(0.0,1);      
    beta[14]  ~ dnorm(0.0,1);      
    beta[15]  ~ dnorm(0.0,1);       
    beta[16]  ~ dnorm(0.0,1);        
    beta[17]  ~ dnorm(0.0,1);      
    beta[18]  ~ dnorm(0.0,1);    
    beta[19]  ~ dnorm(0.184,1);     # PRIOR MEANS FROM KELLEHER AND YACKEE 2009, MODEL 3
    beta[20]  ~ dnorm(0.156,1);
    gamma[1]  ~ dnorm(0.0,1);       # DIFFUSE PRIORS 
    gamma[2]  ~ dnorm(0.0,1);
    gamma[3]  ~ dnorm(0.0,1);
    tau.y     ~ dgamma(1.0,1);
    tau.alpha ~ dgamma(1.0,1);
}

# SAVE MODEL TO A FILE, NOT NECESSARY
write.model(asap.model2.rjags, "Article.JPART/asap.model2.rjags")  

# RUN THE SAMPLER AND COLLECT coda SAMPLES, NOTE: NUMBER OF DRAWS FOR PEDAGOGICAL, NOT RESEARCH, PURPOSES
asap2.model <- jags.model(file="Article.JPART/asap.model2.rjags", data=asap.jags.list, n.chains=3,  n.adapt=0) 
update(asap2.model, n.iter=500)
asap2.mcmc <- coda.samples(model=asap2.model, variable.names=names(asap.jags.list),n.iter=500)
summary(asap2.mcmc)     # SLOW!

# CHECK CONVERGENCE
# sink("supderdiag.out")
superdiag(as.mcmc.list(asap2.mcmc), burnin=0)
# sink()

# GET THE DEVIANCE AND THE DIC
( asap2.dic <- dic.samples(asap2.model, n.iter=500, type="pD") )

