library(foreign)
s.failure <- read.dta("Article.Lasso/Data.State.Failures/sort11v3.dta")
dim(s.failure)
[1] 8580 1231
sum(is.na(s.failure))
[1] 7019387
sum(is.na(s.failure))/prod(dim(s.failure))
[1] 0.66459

random.imp.vec <- function(V)  {
	gone <- is.na(V)
	there <- V[!gone]
	V[gone] <- sample(x=there,size=sum(gone),replace=TRUE)
	return(V)
}
X <- c(1,2,NA,4,5,NA)
random.imp.vec(X)
[1] 1 2 5 4 5 1

dust2.df <- read.table("http://jgill.wustl.edu/data/dust2.asc",header=TRUE)
summary(dust2.df)
      cbr              dust            smoking            expo      
 Min.   :0.0000   Min.   :  0.900   Min.   :0.0000   Min.   : 3.00  
 1st Qu.:0.0000   1st Qu.:  1.945   1st Qu.:0.0000   1st Qu.:16.00  
 Median :0.0000   Median :  5.065   Median :1.0000   Median :25.00  
 Mean   :0.2343   Mean   :  4.822   Mean   :0.7392   Mean   :25.06  
 3rd Qu.:0.0000   3rd Qu.:  6.260   3rd Qu.:1.0000   3rd Qu.:33.00  
 Max.   :1.0000   Max.   : 24.000   Max.   :1.0000   Max.   :66.00  
                  NA's   :578.000                                   

sum(is.na(dust2.df))/prod(dim(dust2.df))
0.1159711

dust2.glm <- glm(cbr ~ dust+smoking+expo, family = binomial(link = logit),
	data=dust2.df,na.action=na.omit)  
summary(dust2.glm)
 
Coefficients:
             Estimate Std. Error z value Pr(>|z|)
(Intercept) -3.442991   0.382028  -9.012  < 2e-16
dust         0.143878   0.037892   3.797 0.000146
smoking      0.835298   0.235169   3.552 0.000382
expo         0.037629   0.008409   4.475 7.64e-06

    Null deviance: 762.07  on 667  degrees of freedom
Residual deviance: 709.61  on 664  degrees of freedom
  (578 observations deleted due to missingness)
AIC: 717.6

dust2.df$dust <- random.imp.vec(dust2.df$dust)
dust2.glm <- glm(cbr ~ dust+smoking+expo, family = binomial(link = logit),
        data=dust2.df,na.action=na.fail)
summary(dust2.glm)

Coefficients:
             Estimate Std. Error z value Pr(>|z|)
(Intercept) -3.154958   0.275542 -11.450  < 2e-16
dust         0.073567   0.027229   2.702 0.006896
smoking      0.674027   0.173820   3.878 0.000105
expo         0.040802   0.006169   6.614 3.74e-11

    Null deviance: 1356.8  on 1245  degrees of freedom
Residual deviance: 1286.4  on 1242  degrees of freedom
AIC: 1294.4

dust.df <- read.table( "http://jgill.wustl.edu/data/dust.asc",header=TRUE)
dust.glm <- glm(cbr ~ dust+smoking+expo, family = binomial(link = logit),
	data=dust.df);  
summary.glm(dust.glm)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -3.04785    0.24813 -12.283  < 2e-16 
dust         0.09189    0.02323   3.956 7.63e-05 
smoking      0.67683    0.17407   3.888 0.000101 
expo         0.04016    0.00620   6.476 9.40e-11 

    Null deviance: 1356.8  on 1245  degrees of freedom
Residual deviance: 1278.3  on 1242  degrees of freedom
AIC: 1286.3

# CASE-WISE DELETION
(Intercept) -3.442991   0.382028  -9.012  < 2e-16
dust         0.143878   0.037892   3.797 0.000146
smoking      0.835298   0.235169   3.552 0.000382
expo         0.037629   0.008409   4.475 7.64e-06

# RANDOM IMPUTATION
(Intercept) -3.154958   0.275542 -11.450  < 2e-16
dust         0.073567   0.027229   2.702 0.006896
smoking      0.674027   0.173820   3.878 0.000105
expo         0.040802   0.006169   6.614 3.74e-11

# FULL DATA
(Intercept) -3.04785    0.24813 -12.283  < 2e-16
dust         0.09189    0.02323   3.956 7.63e-05
smoking      0.67683    0.17407   3.888 0.000101
expo         0.04016    0.00620   6.476 9.40e-11

( nukes.df <- read.table("http://jgill.wustl.edu/data/nukes.full.dat",header=TRUE) )
   US.TEST YEAR US.GDP US.DEBT SOV.TEST       US.TEST YEAR US.GDP US.DEBT SOV.TEST
1       11 1953   0.37    0.49        0    21      24 1973   1.31    1.90       17
2        6 1954   0.38    0.51        0    22      23 1974   1.44    2.07       21
3       18 1955   0.40    0.55        0    23      22 1975   1.55    2.26       19
4       18 1956   0.43    0.58        0    24      21 1976   1.73    2.51       21
5       32 1957   0.45    0.60        0    25      20 1977   1.97    2.83       24
6       77 1958   0.46    0.64        0    26      21 1978   2.21    3.21       31
7        0 1959   0.49    0.69        0    27      16 1979   2.50    3.60       31
8        0 1960   0.52    0.72        0    28      17 1980   2.72    3.96       24
9       10 1961   0.53    0.77        1    29      17 1981   3.05    4.35       21
10      98 1962   0.57    0.82        1    30      19 1982   3.21    4.77       19

   US.TEST YEAR US.GDP US.DEBT SOV.TEST       US.TEST YEAR US.GDP US.DEBT SOV.TEST
11      47 1963   0.60    0.88        0    31      19 1983   3.42    5.34       25
12      47 1964   0.64    0.94        9    32      20 1984   3.81    6.12       27
13      39 1965   0.69    1.01       14    33      18 1985   4.10    7.09       10
14      48 1966   0.75    1.07       18    34      15 1986   4.37    7.95        0
15      42 1967   0.81    1.15       17    35      15 1987   4.61    8.67       23
16      56 1968   0.87    1.24       17    36      15 1988   4.95    9.44       16
17      46 1969   0.95    1.33       19    37      12 1989   5.35   10.18        7
18      39 1970   1.01    1.42       16    38       9 1990   5.68   10.87        1
19      24 1971   1.08    1.56       23    39       8 1991   5.86   11.35        0
20      27 1972   1.18    1.71       24    40       6 1992   6.14   11.90        0

nukes2.df <- nukes.df[10:40,]
nukes.lm1 <- lm(US.TEST ~ YEAR + US.GDP + US.DEBT + SOV.TEST,
                data=nukes2.df);  summary(nukes.lm1)
Residuals:
    Min      1Q  Median      3Q     Max 
    -15.608  -3.940  -0.472   2.425  30.788 

    Coefficients:
             Estimate Std. Error t value Pr(>|t|)
(Intercept)  1.01e+04   2.43e+03    4.17  0.00030
YEAR        -5.14e+00   1.24e+00   -4.14  0.00032
US.GDP       3.10e+01   1.45e+01    2.14  0.04168
US.DEBT     -6.84e+00   5.54e+00   -1.23  0.22810
SOV.TEST    -1.95e-02   3.07e-01   -0.06  0.94986

Residual standard error: 8.84 on 26 degrees of freedom
Multiple R-squared: 0.807, Adjusted R-squared: 0.778 
F-statistic: 27.2 on 4 and 26 DF,  p-value: 5.84e-09 

nukes2.missing.df <- nukes2.df
nukes2.missing.df[27,5] <- nukes2.missing.df[26,5] <- NA
sum(is.na(nukes2.missing.df))/prod(dim(nukes2.missing.df))        
[1] 0.012903
na.action: a function which indicates what should happen when the data contain 
           `NA's.  The default is set by the `na.action' setting of `options', 
           and is `na.fail' if that is unset. The ``factory-fresh'' default is 
           `na.omit'.

nukes.lm2 <- lm(US.TEST ~ YEAR + US.GDP + US.DEBT + SOV.TEST,
                data=nukes2.missing.df, na.action=na.omit)
summary(nukes.lm2)

Residuals:
    Min      1Q  Median      3Q     Max 
-17.532  -2.690  -0.139   2.145  29.298 

             Estimate Std. Error t value Pr(>|t|)
 (Intercept) 9610.662   2522.174    3.81  0.00085
 YEAR          -4.869      1.287   -3.78  0.00091
 US.GDP        35.470     15.261    2.32  0.02890
 US.DEBT      -10.184      6.407   -1.59  0.12502
 SOV.TEST      -0.246      0.375   -0.65  0.51891

Residual standard error: 8.98 on 24 degrees of freedom
  (2 observations deleted due to missingness)
Multiple R-squared: 0.81, Adjusted R-squared: 0.779 
F-statistic: 25.7 on 4 and 24 DF,  p-value: 2.31e-08 
\end{VM}

(Intercept)  1.01e+04   2.43e+03    4.17  0.00030
YEAR        -5.14e+00   1.24e+00   -4.14  0.00032
US.GDP       3.10e+01   1.45e+01    2.14  0.04168
US.DEBT     -6.84e+00   5.54e+00   -1.23  0.22810
SOV.TEST    -1.95e-02   3.07e-01   -0.06  0.94986

(Intercept) 9610.662   2522.174    3.81  0.00085
YEAR          -4.869      1.287   -3.78  0.00091
US.GDP        35.470     15.261    2.32  0.02890
US.DEBT      -10.184      6.407   -1.59  0.12502
SOV.TEST      -0.246      0.375   -0.65  0.51891

library(mice)
m <- 10
nukes2.imp <- mice(nukes2.missing.df,m)
nukes2.imp.array <- array(NA,c(dim(nukes2.missing.df),m))
for (i in 1:m)  nukes2.imp.array[,,i] <- as.matrix(complete(nukes2.imp,i))

nukes2.mids <- lm.mids(US.TEST ~ YEAR + US.GDP + US.DEBT + SOV.TEST,
	                   data=nukes2.imp)
pool(nukes2.mids)

Pooled coefficients:
(Intercept)        YEAR      US.GDP     US.DEBT    SOV.TEST 
 9175.85979    -4.64738    32.76891    -9.28270    -0.28472 

Fraction of information about the coefficients missing due to nonresponse: 
(Intercept)        YEAR      US.GDP     US.DEBT    SOV.TEST 
   0.076928    0.076943    0.075321    0.081546    0.090584 

lm.mids.vals <- function(obj,param) { 
    out.mat <- NULL
    for (i in 1:length(obj$analyses))
        out.mat <- rbind(out.mat,summary.lm(obj$analyses[[i]])$coef[,param])
    out.mat
	
( impute.coef.vec <- apply(lm.mids.vals(nukes2.mids,1),2,mean) )
(Intercept)        YEAR      US.GDP     US.DEBT    SOV.TEST 
 9175.85979    -4.64738    32.76891    -9.28270    -0.28472 
( impute.se.vec <- apply(lm.mids.vals(nukes2.mids,2),2,mean) )
(Intercept)        YEAR      US.GDP     US.DEBT    SOV.TEST 
  2335.4789      1.1914     14.3976      6.0947      0.3473 

( between.var <- apply(lm.mids.vals(nukes2.mids,1),2,var) )
(Intercept)        YEAR      US.GDP     US.DEBT    SOV.TEST 
 1.7137e+04  4.4789e-03  3.4437e-01  2.7547e-01  1.9153e-03 

( within.var <- apply(lm.mids.vals(nukes2.mids,2)^2,2,mean) )
(Intercept)        YEAR      US.GDP     US.DEBT    SOV.TEST 
 5.4557e+06  1.4197e+00  2.0729e+02  3.7153e+01  1.2064e-01 
 
m <- 10
( impute.se.vec <- sqrt(within.var + ((m+1)/m)*between.var) )
(Intercept)        YEAR      US.GDP     US.DEBT    SOV.TEST 
 2339.77166     1.19358    14.41072     6.12012     0.35035 

( impute.df <- (m-1)*(1 + (m/(m+1)) * within.var/between.var)^2 )
(Intercept)        YEAR      US.GDP     US.DEBT    SOV.TEST 
     759034      752532     2704815      137515       30550 
            
gamma <- (m/(m+1)) * between.var/(within.var + ((m+1)/m)*between.var)
full.df <- nrow(nukes2.df) - 1
adj.full.df <- ((full.df+1)/(full.df+3)) * full.df*(1-gamma)
( adj.imp.df <- 1/(1/impute.df + 1/adj.full.df) )
(Intercept)        YEAR      US.GDP     US.DEBT    SOV.TEST 
     28.101      28.100      28.139      27.988      27.757 

out.table <- round( cbind( impute.coef.vec,impute.se.vec,
        impute.coef.vec/impute.se.vec, 
        1-pt(abs(impute.coef.vec/impute.se.vec),adj.imp.df) ),6 )
dimnames(out.table) <- list( c("(Intercept)","YEAR","US.GDP","US.DEBT",
        "SOV.TEST"), c("Estimate","Std. Error","t value","Pr(>|t|)") ) 
out.table

              Estimate Std. Error  t value Pr(>|t|)
(Intercept) 9175.85979 2339.77166  3.92169 0.000258
YEAR          -4.64738    1.19358 -3.89366 0.000278
US.GDP        32.76891   14.41072  2.27393 0.015397
US.DEBT       -9.28270    6.12012 -1.51675 0.070272
SOV.TEST      -0.28472    0.35035 -0.81266 0.211664

summary(pool(nukes2.mids))
                   est         se        t     df   Pr(>|t|)     lo 95       hi 95
(Intercept) 9175.85979 2339.77166  3.92169 24.123 0.00063735 4348.1087 14003.61086
YEAR          -4.64738    1.19358 -3.89366 24.122 0.00068411   -7.1101    -2.18462
US.GDP        32.76891   14.41072  2.27393 24.163 0.03213637    3.0372    62.50059
US.DEBT       -9.28270    6.12012 -1.51675 24.007 0.14238792  -21.9138     3.34841
SOV.TEST      -0.28472    0.35035 -0.81266 23.773 0.42447557   -1.0082     0.43874
            nmis      fmi    lambda
(Intercept)   NA 0.076928 0.0034434
YEAR           0 0.076943 0.0034583
US.GDP         0 0.075321 0.0018241
US.DEBT        0 0.081546 0.0080899
SOV.TEST       2 0.090584 0.0171639

dust2.df <- read.table( "http://jgill.wustl.edu/data/dust2.asc",header=TRUE)
library(mice);  m <- 5;  imp.dust2 <- mice(dust2.df,m)
dust3.glm <- glm.mids(cbr ~ dust+smoking+expo, family = binomial(link = logit),
                      data=imp.dust2)
summary(pool(dust3.glm))
                    est          se         t         df     Pr(>|t|)       lo 95
(Intercept) -3.37686445 0.349777612 -9.654318  168.55060 0.000000e+00 -4.06737387
dust         0.12613589 0.051039923  2.471318   51.20283 1.682791e-02  0.02367882
smoking      0.64561295 0.180901959  3.568855 1014.44146 3.753238e-04  0.29062809
expo         0.04004765 0.006517554  6.144583  931.29865 1.186852e-09  0.02725686
                  hi 95 missing        fmi
(Intercept) -2.68635503      NA 0.36815695
dust         0.22859295     578 0.70018227
smoking      1.00059781       0 0.06132988
expo         0.05283845       0 0.07686594

dust.df <- read.table( "http://jgill.wustl.edu/data/dust.asc",header=TRUE)
dust.glm <- glm(cbr ~ dust+smoking+expo, family = binomial(link = logit),
        data=dust.df);
summary.glm(dust.glm)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)
(Intercept) -3.04785    0.24813 -12.283  < 2e-16
dust         0.09189    0.02323   3.956 7.63e-05
smoking      0.67683    0.17407   3.888 0.000101
expo         0.04016    0.00620   6.476 9.40e-11

    Null deviance: 1356.8  on 1245  degrees of freedom
Residual deviance: 1278.3  on 1242  degrees of freedom
AIC: 1286.3

harr <- read.table("http://jgill.wustl.edu/data/harrison4.txt",header=TRUE)
apply(harr[,-1],2,table)

$NumberKilled
 0  1  2  3  5  6  7  8  9 11 15 17 19 21 23 24 30 
44 13  9  8  3  2  3  2  2  3  4  3  1  3  1  1  1 

$NumberInjured
  0   1   2   3   4   5   6   8   9  11  13  14  16  17  20  21  22  26  27  30 
 28   1   5   4   4   3   1   2   2   2   1   1   1   1   3   1   1   1   1   5 
 40  42  47  50  52  57  58  59  60  65  69  86  90 100 102 120 130 150 188 
  3   1   1   7   1   1   1   2   5   1   1   1   1   3   1   1   1   2   1 

$TotalCasualties
  0   1   2   3   4   5   6   8   9  10  12  13  15  17  20  21  26  27  29  30 
 22   5   6   4   3   3   2   2   1   1   2   2   1   1   2   1   1   2   1   1 
 31  32  35  38  45  49  50  51  52  53  57  58  59  61  62  63  65  67  71  75 
  1   1   1   1   1   2   1   1   1   1   2   1   1   2   1   1   2   2   3   2 
 81  91  93 105 106 123 126 141 145 151 180 199 
  1   1   1   1   1   1   1   1   1   1   1   1 

$ResponsibleHamas                       $ResponsibleisMartyrs
 0  1                                    0  1 
59 44                                   78 25 

$ResponsibleisPIJ                       $ResponsibleisOther
 0  1                                    0  1 
79 24                                   99  4 

$TargetisMilitary                       $TargetisCivilian
 0  1                                    0  1 
76 10                                   10 76 

$TargetisBus                            $TargetisCafe
 0  1                                    0  1 
89 14                                   89 14 

$TargetisCheckpoint                     $TargetisResidence
 0  1                                     0   1 
87 16                                   102   1 

$TargetisOffshore                       $TargetisStore
  0   1                                   0  1 
101   2                                  96  7 

$TargetisStreet                         $TargetisTravelstop
 0  1                                     0  1 
71 32                                    88 15 

$DeviceisCar                            $DeviceisBoat
 0  1                                     0   1 
89 14                                   101   2 

$AttackisPrevented                      $AttackerisChallenged
  0   1                                  0  1 
101   2                                 63 40 

$FirstAttackerisMale                    $FirstAttackerisFemale
 0  1                                    0  1 
 7 92                                   92  7 

$AgeofFirstAttacker
16 17 18 19 20 21 22 23 24 25 26 27 29 31 43 45 48 
 1  8  7 10 15 11 10 12  2  3  2  1  3  1  1  1  1 

harr <- read.table("http://jgill.wustl.edu/data/harrison3.txt",header=TRUE)
apply(apply(harr,2,is.na),2,sum)
                    0                     0                     0 
      TotalCasualties      ResponsibleHamas  ResponsibleisMartyrs 
                    0                     0                     0 
     ResponsibleisPIJ    ResponsibleisOther      TargetisMilitary 
                    0                     0                    17 
     TargetisCivilian           TargetisBus          TargetisCafe 
                   17                     0                     0 
   TargetisCheckpoint     TargetisResidence      TargetisOffshore 
                    0                     0                     0 
        TargetisStore        TargetisStreet    TargetisTravelstop 
                    0                     0                     0 
          DeviceisCar          DeviceisBoat     AttackisPrevented 
                    0                     0                     0 
 AttackerisChallenged    AgeofFirstAttacker   FirstAttackerisMale 
                    0                    14                     4 
FirstAttackerisFemale 
                    4 

harr.glm <- glm(NumberKilled ~ log(AgeofFirstAttacker) + log(as.numeric(Date)) +
                AttackerisChallenged + FirstAttackerisFemale + DeviceisCar + 
		TargetisCafe + TargetisMilitary + ResponsibleHamas, data=harr,
		na.action=na.omit)

summary(harr.glm)
                        Estimate Std. Error t value Pr(>|t|)
(Intercept)               12.195     12.793    0.95   0.3438
log(AgeofFirstAttacker)   -3.369      4.170   -0.81   0.4219
log(as.numeric(Date))      0.582      0.864    0.67   0.5026
AttackerisChallenged      -3.700      1.604   -2.31   0.0241
FirstAttackerisFemale      3.202      3.040    1.05   0.2960
DeviceisCar               -2.018      2.453   -0.82   0.4137
TargetisCafe               3.967      2.013    1.97   0.0527
TargetisMilitary          -5.444      2.495   -2.18   0.0325
ResponsibleHamas           5.363      1.649    3.25   0.0018

(Dispersion parameter for gaussian family taken to be 40.519)

    Null deviance: 4077.5  on 77  degrees of freedom
Residual deviance: 2795.8  on 69  degrees of freedom
  (25 observations deleted due to missingness)
AIC: 520.5

library(mice)

attach(harr)
harr2 <- cbind(NumberKilled, NumberInjured, AgeofFirstAttacker, Date, 
	       ResponsibleisMartyrs, AttackerisChallenged, FirstAttackerisFemale, 
	       ResponsibleisPIJ, TargetisBus, TargetisCheckpoint, DeviceisCar, 
	       TargetisCafe, TargetisMilitary, ResponsibleHamas)
detach(harr)

imp.harr <- mice(harr2,m=10)

harr.mids <- glm.mids(NumberKilled ~ log(AgeofFirstAttacker) + 
		      log(as.numeric(Date)) + AttackerisChallenged + 
		      FirstAttackerisFemale + DeviceisCar + TargetisCafe + 
		      TargetisMilitary + ResponsibleHamas, data=imp.harr)

cbind(summary(harr.glm)$coef[,1:2], summary(pool(harr.mids))[,1:2])

                        Estimate Std. Error             Estimate Std. Error
(Intercept)             12.19545   12.79263             9.873602    9.49898
log(AgeofFirstAttacker) -3.36921    4.16985            -2.061098    3.09964
log(as.numeric(Date))    0.58241    0.86409             0.067666    0.63617
AttackerisChallenged    -3.69990    1.60370            -3.131864    1.23538
FirstAttackerisFemale    3.20166    3.04025             2.815036    2.37085
DeviceisCar             -2.01778    2.45341            -0.177107    1.75987
TargetisCafe             3.96730    2.01286             4.893596    1.72607
TargetisMilitary        -5.44396    2.49457            -3.888428    1.48603
ResponsibleHamas         5.36309    1.64870             3.933507    1.26970

library(mgcv)
harr.gam <- gam(NumberKilled ~ te(log(AgeofFirstAttacker),log(Date),k=3) +
                AttackerisChallenged + FirstAttackerisFemale +
                DeviceisCar + TargetisCafe + TargetisMilitary +
                ResponsibleHamas, data=harr)

summary(harr.gam)

Parametric coefficients:
                      Estimate Std. Error t value Pr(>|t|)
(Intercept)              4.377      1.070   4.091  9.3e-05
AttackerisChallenged    -4.059      1.144  -3.549 0.000616
FirstAttackerisFemale    1.286      2.255   0.571 0.569737
DeviceisCar              1.204      1.677   0.718 0.474763
TargetisCafe             3.824      1.638   2.335 0.021752
TargetisMilitary        -4.772      1.384  -3.448 0.000860
ResponsibleHamas         4.027      1.184   3.400 0.001003

Approximate significance of smooth terms:
                                        edf Ref.df     F p-value
te(log(AgeofFirstAttacker),log(Date)) 5.613  5.613 3.794 0.00255


mice.output <- function(mean.mat,se.mat,var.names=NULL)  {
    # ASSUMES THAT THE INPUT MATRICES ARE k COEFFICIENTS TIMES m IMPUTATIONS
    m <- ncol(mean.mat)
    mean.vec <- apply(mean.mat,1,mean)
    between.var <- apply(mean.mat,1,var)
    within.var <- apply(se.mat^2,1,mean)
    se.vec <- sqrt(within.var + ((m+1)/m)*between.var)
    impute.df <- (m-1)*(1 + (1/(m+1)) * within.var/between.var)^2 
    out.table <- round( cbind( mean.vec, se.vec, mean.vec/se.vec, 
        1-pt(abs(mean.vec/se.vec),impute.df) ),5 )
    dimnames(out.table) <- list(var.names,
        c("Estimate","Std. Error","t value","Pr(>|t|)"))
    return(out.table)
}

X <- cbind(rpois(10000,3), rnorm(10000,3,2), rgamma(10000,2,2), rbeta(10000,5,1))
missing <- sample(40000,10000,replace=FALSE)
X[missing] <- NA
X.imp <- mice(X)
X.complete <- complete(X.imp,1)
table(X.complete[,1])
   0    1    2    3    4    5    6    7    8    9   10   11 
 427 1596 2224 2152 1502  883  450  662   65   29    9    1 
min(X.complete[,3])
[1] 0.006727825
range(X.complete[,4])
[1] 0.1406161 0.9999696

lapply(c("lme4","mice"),library, character.only=TRUE)
mouse <- read.table("/Users/jgill/Grant.TREC/CompiledMouseData.csv",header=TRUE)
m <- 10
mouse.imp <- mice(mouse,m)
mouse.array <- array(NA,c(dim(mouse),m))
for (i in 1:m) mouse.array[,,i] <- as.matrix(complete(mouse.imp,i))

lmer.out.mean <- lmer.out.se <- NULL
for (i in 1:m)  {
    current.mouse.dat <- data.frame(mouse.array[,,i])
    names(current.mouse.dat) <- names(mouse)
    M1 <- lmer (Number_Hyperproliferative ~ 
        Age_when_used                    
        + Body_weight                       
        + UGS_weight                       
        + Prostate_Weight                   
        + Time_Parents_on_diet_before_birth
        + (1 | Diet_Treatment), family="poisson", data=current.mouse.dat)
    lmer.out.mean <- cbind(lmer.out.mean, summary(M1)$coef[,1])
    lmer.out.se   <- cbind(lmer.out.se,   summary(M1)$coef[,2])
}

mice.output(lmer.out.mean,lmer.out.se,var.names=names(fixef(M1)))

                                  Estimate Std. Error  t value Pr(>|t|)
(Intercept)                        6.24279    5.93493  1.05187  0.15513
Age_when_used                     -0.10440    0.04853 -2.15109  0.02534
Body_weight                        0.28757    0.11858  2.42509  0.01077
UGS_weight                        -2.61970    1.97876 -1.32391  0.09605
Prostate_Weight                   48.52982   48.98844  0.99064  0.17132
Time_Parents_on_diet_before_birth -0.00283    0.01081 -0.26198  0.39709

  AIC  BIC logLik deviance
 27.5 33.7  -6.73     13.5

Random effects:
Groups         Name        Variance   Std.Dev. 
Diet_Treatment (Intercept) 1.7293e-18 1.315e-09
Number of obs: 18, groups: Diet_Treatment, 2
\end{VM}

params <- c("mu", "mu35", "mu40", "alpha", "beta", "gamma")
dugong.out <- jags(data = dugong, parameters.to.save = params,
                 model.file = "Class.Multilevel/dugong.linear.jags",
                 n.iter = 100000, n.burnin = 50000, n.thin = 1)
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%
  |**************************************************| 100%

dugong.out
Inference for Bugs model at "Class.Multilevel/dugong.linear.jags", fit using jags,
 3 chains, each with 1e+05 iterations (first 50000 discarded)
 n.sims = 150000 iterations saved
          mu.vect sd.vect    2.5%     25%     50%     75%   97.5%  Rhat n.eff
alpha      2.654   0.109   2.493   2.585   2.640   2.705   2.895 1.006   3500
beta       0.988   0.117   0.786   0.916   0.982   1.051   1.218 1.001   7900
gamma      0.855   0.050   0.738   0.832   0.863   0.888   0.930 1.002  14000
mu[1]      1.810   0.074   1.659   1.763   1.811   1.858   1.949 1.001  42000
mu[2]      1.872   0.059   1.755   1.833   1.872   1.911   1.989 1.001  92000
mu[3]      1.872   0.059   1.755   1.833   1.872   1.911   1.989 1.001  92000
mu[4]      1.872   0.059   1.755   1.833   1.872   1.911   1.989 1.001  92000
mu[5]      1.982   0.046   1.894   1.952   1.981   2.012   2.075 1.001 110000
mu[6]      2.116   0.045   2.036   2.086   2.113   2.141   2.214 1.001  37000
mu[7]      2.188   0.046   2.108   2.158   2.184   2.214   2.291 1.001  31000
mu[8]      2.188   0.046   2.108   2.158   2.184   2.214   2.291 1.001  31000
mu[9]      2.302   0.044   2.222   2.272   2.299   2.329   2.397 1.001  31000
mu[10]     2.347   0.041   2.269   2.319   2.346   2.373   2.433 1.001  35000
mu[11]     2.367   0.040   2.290   2.340   2.366   2.393   2.448 1.001  38000
mu[12]     2.386   0.039   2.310   2.360   2.385   2.411   2.463 1.001  43000
mu[13]     2.403   0.037   2.329   2.378   2.403   2.427   2.476 1.001  50000
mu[14]     2.403   0.037   2.329   2.378   2.403   2.427   2.476 1.001  50000
mu[15]     2.419   0.036   2.347   2.395   2.419   2.443   2.489 1.001  61000
mu[16]     2.472   0.032   2.408   2.452   2.473   2.494   2.535 1.001 150000
mu[17]     2.472   0.032   2.408   2.452   2.473   2.494   2.535 1.001 150000
mu[18]     2.494   0.032   2.431   2.474   2.494   2.515   2.555 1.001 150000
mu[19]     2.494   0.032   2.431   2.474   2.494   2.515   2.555 1.001 150000
mu[20]     2.521   0.033   2.455   2.500   2.522   2.543   2.585 1.001 130000
mu[21]     2.537   0.035   2.466   2.514   2.537   2.559   2.603 1.001  58000
mu[22]     2.537   0.035   2.466   2.514   2.537   2.559   2.603 1.001  58000
mu[23]     2.550   0.037   2.474   2.526   2.551   2.574   2.621 1.001  36000
mu[24]     2.556   0.039   2.476   2.531   2.557   2.581   2.629 1.001  30000
mu[25]     2.602   0.056   2.489   2.565   2.602   2.639   2.710 1.001  12000
mu[26]     2.627   0.072   2.493   2.579   2.625   2.674   2.776 1.001   8500
mu[27]     2.633   0.077   2.493   2.581   2.629   2.681   2.796 1.001   7800
mu[28]     2.639   0.082   2.493   2.583   2.633   2.689   2.817 1.001   7000
mu[29]     2.644   0.088   2.493   2.584   2.636   2.696   2.840 1.001   6200
mu35       2.639   0.082   2.493   2.583   2.633   2.689   2.817 1.001   7000
mu40       2.644   0.088   2.493   2.584   2.636   2.696   2.840 1.001   6200
deviance -43.985   4.962 -51.334 -47.607 -44.728 -41.229 -32.168 1.001  18000

For each parameter, n.eff is a crude measure of effective sample size,
and Rhat is the potential scale reduction factor (at convergence, Rhat=1).

DIC info (using the rule, pD = var(deviance)/2)
pD = 12.3 and DIC = -31.7
DIC is an estimate of expected predictive error (lower deviance is better).

library(mcmcplots)
caterplot(dugong.out, parms=c("mu", "mu35", "mu40"))

dugong <- list(age = c(1.0,  1.5,  1.5,  1.5,  2.5,  4.0,  5.0, 5.0,   7.0, 8.0,  
                       8.5,  9.0,  9.5,  9.5, 10.0, 12.0, 12.0, 13.0, 13.0, 14.5, 
                      15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5, 35.0, 40.0),
               length = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47, 
                          2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43, 
                          2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57,   
                          NA,   NA), 
               NUM.DUGONGS = 29)
lapply(c("rjags","R2jags","arm","coda","superdiag","R2WinBUGS"),library, 
       character.only=TRUE)

dugongs.linear <- function() {
    for(i in 1:NUM.DUGONGS) {
        length[i] ~ dnorm(mu[i], tau)
        mu[i] <- alpha - beta * pow(gamma,age[i])
    }   
    mu35 <- alpha-beta*pow(gamma,35) # 35 IS AGE OF MISSING 1
    mu40 <- alpha-beta*pow(gamma,40) # 40 IS AGE OF MISSING 2

    alpha  ~ dunif(0, 10)
    beta   ~ dunif(0, 10)
    tau    ~ dgamma(1, 0.1)
    sigma  <- 1/sqrt(tau)
    gamma  ~ dunif(0.0, 1.0)
    miss35 ~ dnorm (mu35, tau) # PRIOR ON MISSING 1
    miss40 ~ dnorm (mu40, tau) # PRIOR ON MISSING 2
}

params <- c("mu", "mu35", "mu40", "alpha", "beta", "gamma")
dugong.out <- jags(data = dugong, parameters.to.save = params,
                 model.file = "Class.Multilevel/dugong.linear.jags",
                 n.iter = 100000, n.burnin = 50000, n.thin = 1)
superdiag(dugong.out)

dugong.out
Inference for Bugs model at "Class.Multilevel/dugong.linear.jags", fit using jags,
 3 chains, each with 1e+05 iterations (first 50000 discarded)
 n.sims = 150000 iterations saved
          mu.vect sd.vect    2.5%     25%     50%     75%   97.5%  Rhat n.eff
alpha      2.654   0.109   2.493   2.585   2.640   2.705   2.895 1.006   3500
beta       0.988   0.117   0.786   0.916   0.982   1.051   1.218 1.001   7900
gamma      0.855   0.050   0.738   0.832   0.863   0.888   0.930 1.002  14000
mu[1]      1.810   0.074   1.659   1.763   1.811   1.858   1.949 1.001  42000
mu[2]      1.872   0.059   1.755   1.833   1.872   1.911   1.989 1.001  92000
:

