options(warn = -1)
library(dplyr)
library(broom)
library(aod)
library(knitr)
library(rmarkdown)
library(tidyr)
library(epitools)
library(gmodels)
library(epiDisplay)
library(lmtest)
library(olsrr)
library(AICcmodavg)
library(logistf)
library(arm)
library(ResourceSelection)
library(LogisticDx)
library(DescTools)
library(performance)

library(DHARMa)



data_glow <- read.csv("glow.csv")
attach(data_glow)


raterisk1 <- replicate(500,0)
raterisk2 <- replicate(500,0)
raterisk3 <- replicate(500,0)


for (i in 1:500) {
  if (raterisk[i] == 1) {
    raterisk1[i] = 1
  }
  else if (raterisk[i] ==2) {
    raterisk2[i] = 1
  }
  else if (raterisk[i] ==3 ) {
    raterisk3[i] = 1
  }
}



data_glow <- cbind(data_glow,raterisk1,raterisk2,raterisk3)




fit_glow <- glm(fracture ~ age+priorfrac+momfrac+armassist+raterisk3+age*priorfrac + momfrac*armassist, family="binomial",data=data_glow)


# summary(fit_glow)






 # data_glow_agg <- data_glow %>% group_by(fracture)
# fit_glow2 <- glm(fracture ~ age+priorfrac+momfrac+armassist+raterisk3+age*priorfrac + momfrac*armassist, family="binomial",data=data_glow_agg)
# summary(fit_glow2)









glow_gof <- LogisticDx::gof(fit_glow,plotROC=FALSE)
glow_gof$chiSq
# Pearson residuals calculated by indivdual :        PrI 499.5959 492 0.3966070
# Pearson residuals calculated by covariate group:    PrG 190.2831 220 0.9270099

# Deviance residuals calculated by indvidual:         drI 507.2385 492 0.3078128
# Deviance residuals calculated by covariate group:   drG 241.5342 220 0.1523431








## Getting dataframe of diagnostics using covariate groups and indviduals cases.
diagnostics_glow_bycov  <- LogisticDx::dx(fit_glow,byCov=TRUE)
diagnostics_glow <- LogisticDx::dx(fit_glow, byCov=FALSE)


# Looking at chi sq deletion difference values:
glow_dChisq <- diagnostics_glow$dChisq
glow_dChisq_byCov <- diagnostics_glow_bycov$dChisq



summary(glow_dChisq)
var(glow_dChisq)
sd(glow_dChisq)




summary(glow_dChisq_byCov)
var(glow_dChisq_byCov)
sd(glow_dChisq_byCov)






par(mar=c(3,3,3,3))

# The dataframe diagnostics_glow is filtered by rows with the last row being the most influential cases hence we will see deviations to the right on the graph.

plot(glow_dChisq, type="l", col="black", xlab="Case (ordered with most influential to the right)", ylab=" Delta ChiSq Statistic for Case i Removed")
title("Chi-Squared Stat. Calculated by Individuals for Observation i Removed vs. Observation Number")


plot(glow_dChisq_byCov, type = "l", col="blue", xlab=" Case (ordered with most influential to the right)", ylab=" Delta ChiSq Statistic for Case i Removed", )
title("Chi-Squared Stat. Calculated by Individuals for Observation i Removed vs. Observation Number")



plot(glow_dChisq, type="p", pch =23, col="black", xlab="", ylab=" Delta ChiSq Statistic for Case i Removed")

plot(glow_dChisq_byCov,pch=2, type = "p",col="blue", xlab="Case (ordered with most influential to the right)", ylab=" Delta ChiSq Statistic for Case i Removed")





max(glow_dChisq)
max(glow_dChisq_byCov)



################################################v


scaled <- glow_dChisq_byCov/100
plot(scaled, type="p", pch =23, col="black", xlab="", ylab=" DChiSq_{i}")
par(new=TRUE)
curve(dchisq(x, df = 493), ,col="red", from = 0, to = 500, xlab="", ylab="", axes=FALSE)
par(new=TRUE)
curve(dchisq(x, df = 470), col="blue",from = 0, to = 500, xlab="", ylab="", axes=FALSE)
par(new=TRUE)
curve(dchisq(x, df = 300), col= "yellow", from = 0, to = 500, xlab="", ylab="", axes=FALSE)





scaled2 <- glow_dChisq/100


plot(scaled2, type="p", pch =23, col="black", xlab="", ylab=" DChiSq_{i}")
par(new=TRUE)
curve(dchisq(x, df = 493), ,col="red", from = 0, to = 500, xlab="", ylab="", axes=FALSE)
par(new=TRUE)
curve(dchisq(x, df = 470), col="blue",from = 0, to = 500, xlab="", ylab="", axes=FALSE)
par(new=TRUE)
curve(dchisq(x, df = 455), col= "yellow", from = 0, to = 500, xlab="", ylab="", axes=FALSE)






# n is "Number of observations with these covariates."
# P is the Probability of this covariate pattern.
diagnostics_glow_bycov$n
# diagnostics_glow_bycov$P





detach(data_glow)





##### rmarkdown::render("p3code.R")