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


datas <- read.csv("myopia14.csv")

# Replaced DADMY with each variable and noted p value.
model0 <- glm(MYOPIC~DADMY, family = "binomial", data = datas)
summary(model0)




# significance level as high as 0.20 or 0.25 as a screening criterion for initial variable selection

# p-values from univariate model summary
#  AGE:      0.6453
#  "GENDER": 0.127   *
#  "AL":     0.348
#  "ACD":    0.007627
#   "LT":    0.256   ~*
#   "VCD":   0.768
# "SPORTHR": 0.0153  *
#   "READHR":  0.0726 *
#   "COMPHR": 0.521
#   "STUDYHR": 0.429 
#   "TVHR" :  0.92
#   "MOMMY" :  0.00108    *
#    "DADMY" :  0.000276   *




# Initial testing shows variables to consider: DADMY,MOMY,READHR,GENDER,LT




##############################################################################################
##############################################################################################
# Backward selection manual




model <- glm(MYOPIC~., family = "binomial", data = datas)
summary(model)


# At each step removing the predictor with the largest p-value over 0.05

model <- update(model, .~.-AL )
summary(model)

model <- update(model, .~.-LT )
summary

model <- update(model, .~.-VCD )
summary(model)

model <- update(model, .~.-COMPHR )
summary(model)

model <- update(model, .~.-TVHR )
summary(model)

model <- update(model, .~.-AGE )
summary(model)

model <- update(model, .~.-STUDYHR  )
summary(model)
# Made READHR not sig

model <- update(model, .~.-READHR  )
summary(model)

model <- update(model, .~.-SPORTHR  )
summary(model)

# All p -val <0.05.





##############################################################################################
##############################################################################################
############ Variable Selection,


fullmodel <- glm(MYOPIC~., family = "binomial", data = datas)


# Backwards selection

backwards = step(fullmodel)

# AIC=448.17
# MYOPIC ~ GENDER + ACD + SPORTHR + READHR + MOMMY + DADMY




summary(backwards)

# Maybe drop GENDER (p val = 0.064118)




backwards2 <- glm(MYOPIC ~ ACD + SPORTHR + READHR + MOMMY + DADMY, family="binomial", data=datas)
summary(backwards2)

# AIC goes up 1.5..


backwards$deviance    # smaller
backwards2$deviance


##############################################################################################
##############################################################################################










##############################################################################################
##############################################################################################
# Forwards selection: # AIC 448.17


attach(datas)

nothing <- glm(MYOPIC ~ 1, family=binomial)

summary(nothing)



forwards = step(nothing,
                scope=list(lower=formula(nothing),upper=formula(fullmodel)), direction="forward")


forwards

# AIC 448.17


# Var Selection both ways



bothways <- step(nothing, list(lower=formula(nothing),upper=formula(fullmodel)), direction="both",trace=0)


bothways  # AIC 448.2



detach(datas)




##############################################################################################
##############################################################################################



##############################################################################################
##############################################################################################
# Quadratic terms comparison.
## MYOPIC ~ GENDER + ACD + SPORTHR + READHR + MOMMY + DADMY



# Modify data in Excel, 6 columns of significant variables,
# Add columns of their quadratic terms, and perform progressive regression to test interaction sign
# Squared of binary variables is same, so GENDER^2, MOMMY^2, DADMY^2 same.
# So we omit those variable colummns.



datasQuadTerms <- read.csv("myopiaQuadTerms.csv")




Regression_Model <- backwards
Regression_Model_Quadratic <- backwards

attach(datasQuadTerms)


Regression_Model_Quadratic <- glm(MYOPIC~., family = "binomial", data = datasQuadTerms)



summary(Regression_Model_Quadratic)


forwardsQuadMod = step(nothing,
                       scope=list(lower=formula(nothing),upper=formula(Regression_Model_Quadratic)), direction="forward")


summary(forwardsQuadMod)
# AIC: 443.96 for forwards regression on linear, quadratic model
# MYOPIC ~ DADMY + MOMMY + SPORTHRSQ + SPORTHR + READHR + GENDER




LLT_linear_fullQ <- lrtest(Regression_Model, Regression_Model_Quadratic)
LLT_linear_fullQ



LLT_Q_forwardsQ <- lrtest(Regression_Model_Quadratic, forwardsQuadMod)
LLT_Q_forwardsQ



########## ******** p value 2.2*10^{-16}
LLT_linear_forwardsQ <- lrtest(Regression_Model, forwardsQuadMod)
LLT_linear_forwardsQ



detach(datasQuadTerms)






##############################################################################################
##############################################################################################

# Trying poly to add interactions to the model. 
# Running into perfect seperation/glm issue, tried bayesglm and did not get
# good results. logistf wouldn't converge with 100000 iterations and 
# regular glm won't converge.







dataQC <- read.csv("myopiaQuadTerms.csv")



attach(dataQC)



z = as.matrix(dataQC[, 2:10])
z <- z[, colnames(z) != "GENDER"]

# copying the binary cols
M <- dataQC["MOMMY"]
D <- dataQC["DADMY"]

# Removing them for poly
z <- z[, !colnames(z) %in% c("MOMMY", "DADMY")]


# Use poly() to form all 2-way interactions and 2nd order terms

z2 = poly(z, degree = 2, raw = TRUE)

# Re add M, D cols for model
new_z <- cbind(z2, M,D)
new_z

# Resave as a data frame, adding the response var
dataframeQC <- as.data.frame(cbind(new_z, y = dataQC$MYOPIC))



# Add terms ACD, SPORTHR, READHR
dataframeQC$ACD <- dataQC$ACD
dataframeQC$SPORTHR <- dataQC$SPORTHR
dataframeQC$READHR <- dataQC$READHR



colnames(dataframeQC)[30] <- "MYOPIC"





#  Bayes analysis from arm package
fit_bayes <- bayesglm(MYOPIC ~., data=dataframeQC, family="binomial")
display(fit_bayes)
# display showed 13 covariates that have a coefficient of 0. we remove these



# (SPORTHR^2), anything*SPORTHR^2,READHRSQ^2*anything  have coef of 0
dataframeQC2 <- dataframeQC[, !colnames(dataframeQC) %in% 
                             c("0.0.2.0.0.0.0", "0.1.0.0.0.1.0",
                               "0.0.1.0.0.1.0", "0.0.0.1.0.1.0",
                               "0.0.0.0.1.1.0", "0.0.0.0.0.2.0",
                               "0.0.0.0.0.0.1", "0.1.0.0.0.0.1",
                               "0.0.1.0.0.0.1", "0.0.0.1.0.0.1",
                               "0.0.0.0.1.0.1", "0.0.0.0.0.1.1",
                               "0.0.0.0.0.0.2"
                               )
                           ]


fit_bayes2<- bayesglm(MYOPIC ~., data=dataframeQC2, family="binomial")
display(fit_bayes2)




# Not converging, tried to increase the number of iterations:
logistf::logistf.control(maxit=100000)
# fit <- logistf::logistf(MYOPIC ~., data = dataframeQC)
# summary(fit)




# Doesn't converge, p-vals all equal 1.
model_Qinteraction <- glm(MYOPIC ~ ., family = "binomial", data = dataframeQC)
summary(model_Qinteraction)




detach(dataQC)





##############################################################################################
##############################################################################################

# iterartions manually



dataQman<- read.csv("myopiaQuadTerms.csv")
attach(dataQman)


# dataQman <- dataQman[, !colnames(dataQman) %in% c("ACDSQ","SPORTHRSQ", "READHRSQ")]



# Fit model
model_QintManual <- glm(MYOPIC ~ DADMY + MOMMY  + SPORTHR + READHR + GENDER
                        + SPORTHRSQ + READHRSQ + ACDSQ +
                          SPORTHR*READHR+
                          ACD*SPORTHR+
                          READHR*ACD +
                          SPORTHRSQ*ACD+
                          ACDSQ*SPORTHR +
                          READHRSQ*SPORTHR,
                          family ="binomial", data = dataQman)




# Added iteractions between quadratic terms and others resulted in AIC went down found SPORTHR*READHR^2 sig,
# READHRSQ became very sig, 


summary(model_QintManual)


# Performing backward selection:

model_QintMbs <- step(model_QintManual)

summary(model_QintMbs)     # AIC: 439.34








detach(dataQman)

##############################################################################################
##############################################################################################





##############################################################################################
##############################################################################################
################# AIC testing




models <- list(Regression_Model, Regression_Model_Quadratic, forwardsQuadMod, model_QintMbs)
model.names <- c('Reduced Model LinearTerms', 'Reduced Model QuadraticTerms', 'FS QuadraticTerms Model',
                'BS Quad Interactions Model Manually')
aictab(cand.set = models, modnames = model.names)






##############################################################################################
##############################################################################################
#############      GOF TESTS for Model (model with linear, quadratic terms and interactions.)


# using lowest AIC value for model, had lowest p-value for HL test



# model_final <- model_QintMbs
# model_final <- Regression_Model_Quadratic      # X-squared = 5.7505, df = 8, p-value = 0.6752
# model_final <- forwardsQuadMod                 # X-squared = 4.599, df = 8, p-value = 0.7994
 model_final <-Regression_Model
predictions <- predict(model_final, type="response")



y <- dataQman$MYOPIC


hoslem.test(y, predictions)















p1 <- hist(predictions)
library('zoo')
breaks_cdf <- pgamma(p1$breaks, shape=10, scale=3)
null.probs <- rollapply(breaks_cdf, 2, function(x) x[2]-x[1])
a <- chisq.test(p1$counts, p=null.probs, rescale.p=TRUE, simulate.p.value=TRUE)
##                                                # X-squared = 1.2623e+12, df = NA, p-value = 0.0004998




OR_PearsonT <- function(p,observed) {
  sum = 0
  for (i in 1:618)  {
    temp <- ((observed[i] - p[i])^2 / (p[i]*(1-p[i])))
    sum <- sum + temp
                    }  
  return(sum)
 }


vec <- dataQman$MYOPIC
vec1 <- predictions
OR_PearsonT(vec1,vec)

# X^2 = 625.6129




##############################################################################################
##############################################################################################


 result <- LogisticDx::gof(model_final, g=2)


result$auc

result$gof






 ### rmarkdown::render("p1code.R")



















