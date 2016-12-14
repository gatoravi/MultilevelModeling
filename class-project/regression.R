library(rjags); library(R2jags)

try_lm <- function() {
    ann<-read.table("phs000424.v6.pht002743.v6.p1.c1.GTEx_Sample_Attributes.GRU.txt",header=T,sep="\t",quote="",fill=T)
    sra<-read.table("SRARunTable.txt",header=T,sep="\t")
    ann<-merge(ann,sra[,c(9,11)],by.x=2,by.y=2)
    x<-read.table("./memberships_k5.tsv",header=T)
    ann<-ann[which(ann$Run_s %in% x[,1]),]
    x<-x[which(x[,1] %in% ann$Run_s),]
    x<-x[order(x[,1]),]
    ann<-ann[order(ann$Run_s),]
    z<-as.matrix(x[,-1],nrow=7717,ncol=5)
    ann$New<-0
    ann$New[which(ann$SMGEBTCHD %in% c("04/02/2012","05/21/2012","06/02/2012","06/08/2012","08/31/2012","09/02/2012","09/11/2012","09/27/2012","10/09/2013"))]<-1
    drop<-which(x[,4]>0.5)
    x<-x[-drop,]
    ann<-ann[-drop,]
    #WHAT SIGNATURES ARE STRONGLY ASSOCIATED WITH BATCH?
    #HOW DO WE DECIDE? SEEMS LIKE ALL HAVE AT LEAST ONE STRONG BATCH ASSOCIATION
    #DO WE REQUIRE THAT THE LARGEST EFFECT SIZE BE DUE TO SAMPLE TYPE?
    #print(summary(x))
    #print(summary(ann))
    #tmp <- lm(x[,2] ~ ann$SMTS+ann$SMRIN+ann$SMCENTER+ann$SMATSSCR+ann$SMTSPAX+ann$SMNABTCHT)
    #print(summary(tmp))

    #tmp<-lm(x[,6]~ann$SMGEBTCH+ann$SMTS+ann$SMRIN+ann$SMCENTER+ann$SMATSSCR+ann$SMTSPAX+ann$SMNABTCHT+x$n_mut)
    #anova(tmp)

    #tmp<-lm(x[,4]~ann$SMGEBTCH+ann$SMTS+ann$SMRIN+ann$SMCENTER+ann$SMATSSCR+ann$SMTSPAX+ann$SMNABTCHT)
    #summary(tmp)
    #print(2)

    #tmp<-lm(x[,5]~ann$SMGEBTCH+ann$SMTS+ann$SMRIN+ann$SMCENTER+ann$SMATSSCR+ann$SMTSPAX+ann$SMNABTCHT)
    #summary(tmp)
    #print(3)

    #tmp<-lm(x[,6]~ann$SMGEBTCH+ann$SMTS+ann$SMRIN+ann$SMCENTER+ann$SMATSSCR+ann$SMTSPAX+ann$SMNABTCHT)
    #summary(tmp)
    the.model <- "model{
        for (i in 1:n){
            y[i] ~ dbeta (alpha[i], beta[i])
            alpha[i] <- mu[i] * phi
            beta[i] <- (1 - mu[i]) * phi
            logit(mu[i]) <- a[tissue[i]] + b*x[i]
        }
        b ~ dnorm(0, 0.0001)
        tau.y <- pow(sigma.y, -2)
        sigma.y ~ dunif (0, 100)
        for (j in 1:J){
            a[j] ~ dnorm (mu.a, tau.a)
        }
        mu.a ~ dnorm (0, .0001)
        tau.a <- pow(sigma.a, -2)
        sigma.a ~ dunif (0, 100)
        phi ~ dgamma(0.1, 0.1)
    }"

    y <- x[, 2]
    n <- length(x)
    y[y < 1e-8] <- 1e-8 #Truncate
    x <- ann$SMRIN
    tissue <- as.numeric(ann$SMTS)
    J <- length(unique(tissue)) #Number of tissues
    # Defining the data you will pass into the model -- you *already know* these values
    mutation.data <- list ("n", "J", "x", "y", "tissue")

    # Defining the initial values that your model's parameters (values you *don't* already know)
    mutation.inits <- function (){list(a = rnorm(J),
                                       b = rnorm(1),
                                       mu.a  = rnorm(1),
                                       sigma.y=runif(1),
                                       sigma.a=runif(1))}

    mutation.parameters <- c ("a", "b", "sigma.y",
                              "mu.a", "sigma.a")

    mutation.1 <- jags(data=mutation.data,
                    inits=mutation.inits,
                    parameters.to.save=mutation.parameters,
                    model.file=textConnection(the.model),
                    n.chains=3,
                    n.iter=500000,
                    DIC=F)
    plot(mutation.1)
    print(mutation.1)
    save.image("mutation1_varying_intercept.Rdata")
}

try_lm()
