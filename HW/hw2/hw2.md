#HW2 - Avinash Ramu

##Question3.4
```R
     library(foreign)
     rb <- read.dta("pollution.dta")
```
##Question4.4

```R
     library(foreign)
     p1 <- read.dta("dat/pollution.dta")

```
####A

```R
ggplot(p1) + geom_point(aes(x = nox, y = mort))
ggsave("pollution_scatterplot.png")
m1 <- lm(mort ~ nox, data = p1)a
Call:
lm(formula = mort ~ nox, data = p1)

Residuals:
     Min       1Q   Median       3Q      Max
     -148.654  -43.710    1.751   41.663  172.211

     Coefficients:
                 Estimate Std. Error t value Pr(>|t|)
                 (Intercept) 942.7115     9.0034 104.706   <2e-16 ***
                 nox          -0.1039     0.1758  -0.591    0.557
                 ---
                 Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

                 Residual standard error: 62.55 on 58 degrees of freedom
                 Multiple R-squared:  0.005987,  Adjusted R-squared:  -0.01115
                 F-statistic: 0.3494 on 1 and 58 DF,  p-value: 0.5568
```

![](pollution_scatterplot.png)
![](p1_residuals.png)

From the scatter plot mort and nox don't follow a simple linear relationship.
Most of the mortality is concentrated on a very small range of nox, some sort of transformation is needed on the predictors.


