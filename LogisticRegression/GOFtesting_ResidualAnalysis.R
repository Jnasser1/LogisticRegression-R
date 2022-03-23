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

R_MAX_NUM_DLLS = 500


newdat <- read.csv("newdat.csv")

attach(newdat)



model_p1 <- fit


# Getting predictions from each dataset.
predict_icu <- predict(model_p1, newdata=data,type="response")
predict_newdat <- predict(model_p1, newdata=newdat,type="response")





plot(predict_icu, col="red", xlab=" ", ylab = "",y=0:1)
par(new=TRUE)
plot(predict_newdat, col="blue", xlab=" ",y=0:1)






# Error rate /  misclassifcation rate
predicted_classes_icu <- ifelse(predict_icu > 0.5, "1", "0")
mean(predicted_classes_icu == data$sta)       # 88% or 84.5% for 0.7 threshold

1-mean(predicted_classes_icu == data$sta)



predicted_classes_newdat <- ifelse(predict_newdat> 0.5, "1", "0")
mean(predicted_classes_newdat == data$sta)

1-mean(predicted_classes_newdat == data$sta)



######



# Brier Scores
# Brier score for icu data 0.0929296
# Brier score for newdat 0.2362692
# The lower the Brier score is for a set of predictions, the better the predictions are calibrated.

response <- sta
DescTools::BrierScore(response,predict_icu)
DescTools::BrierScore(response,predict_newdat)






# Creating a model on the newdat

model_newdat <- glm(sta ~ age+ sys +age*sys+can+type+ph+pco+nloc, family="binomial",data=newdat)


par("mar")
par(mar=c(4,4,4,4))




# R^{2} and GOF statistics, AUC from p1 and of newdat


gof1$R2
gof1$auc

# "Usually, the larger the R2, the better the regression model fits your observations."
# We note that R^2 values for newdat model are larger by about 0.2 
# then the ICU model and by about 0.1 greater than the model with removed cases.

gof1$R2



gof_newdat  <- LogisticDx::gof(model_newdat,plotROC=FALSE)
gof_newdat$auc
gof_newdat$gof
gof_newdat$R2





# " Tjur's R squared value approaching 1 indicates that there is clear separation 
#   between the predicted values for the 0s and 1s "
r2_tjur(model_p1)   # 0.4165465
r2_tjur(model_newdat)    # 0.6166857 



################################################################ 
# Trying to use DHARMa



par(mar=c(2,2,2,2))

# Left: a qq-plot to detect overall deviations from the expected distribution
# Right: a plot of the residuals against the rank-transformed model predictions
sims_newdat = simulateResiduals(model_newdat,n=1000)
plot(sims_newdat, quantreg=FALSE)





sims_icu = simulateResiduals(model_p1,n=1000)
plot(sims_icu, quantreg=FALSE)




# sta ~ age+ sys +age*sys+can+type+ph+pco+nloc
# plotting residuals against model variables

simulationOutput <- sims_newdat

simulationOutput2 <- sims_icu


par(mar=c(4,4,4,4))
plotResiduals(simulationOutput, newdat$age, xlab = "standariz age", main=NULL)
plotResiduals(simulationOutput, newdat$sys, xlab = "sys (newdat)", main=NULL)


plotResiduals(simulationOutput2, data$age, xlab = "standariz age", main=NULL)
plotResiduals(simulationOutput2, data$sys, xlab = "sys (icu)", main=NULL)












# plot(model_newdat)
# plot(model_p1)









################################################################ EC
y <- response

#HL tests for ICU model

hoslem.test(y,predict_icu,g=5)
hoslem.test(y,predict_icu,g=10)
 hoslem.test(y,predict_icu,g=20)
hoslem.test(y,predict_icu,g=50)




# HL tests for newdat Model
 hoslem.test(y,predict_newdat,g=5)
hoslem.test(y,predict_newdat,g=10)
 hoslem.test(y,predict_newdat,g=20)
hoslem.test(y,predict_newdat,g=50)






detach(newdat)



#### rmarkdown::render("p2code.R")































