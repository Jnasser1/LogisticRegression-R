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



df <- read.csv("icu.csv")



########################################## (a)






gmodels::CrossTable(df$sta,df$inf)





########################################## (b)

# LogReg STA=y on INF=x:

#(b) (1)


model1 <- glm(sta ~ inf, family = "binomial", data = df)




summary(model1)








###################3
sta_predictions <- predict(model1 ,type="response")

sta_predictions


# Getting the inv values to put into a dataframe just for viewing.
inf_values = dplyr::select(df,9)


x2 <- data.frame(inf_values,sta_predictions)

# Shows the predicted values:
x2





########################################## (c)

# Multiple Logistic Regression / Testing for Significance of variable race.
attach(df)




race.f <- factor(race, labels=c(1,2,3))

race.f

is.factor(race.f)
df <- cbind(df,race.f)


model_MLR <- glm(sta ~ age+can+cpr+inf+race, family = "binomial", data = df)
summary(model_MLR)





Logit <- function(x1,x2,x3,x4,x5) {
  ans <- 3.52209285 - 0.02790116*x1 - 0.19664592*x2 -1.64139513*x3  -0.69893597*x4 + 0.06527064*x5 
  return(ans)
}


## 2. OR estimate for level 2 verses level 1



Estimate = Logit(0,0,0,0,2) - Logit(0,0,0,0,1) 
Estimate




  
z <- qnorm(.975)
beta5 <- coef(model_MLR)[6]
SE_beta5 <- coef(summary(model_MLR))[, 2][6]     # SE for race




# CI:
right_endpt = exp(beta5 + z*SE_beta5)
left_endpt = exp(beta5 - z*SE_beta5)

right_endpt
left_endpt



## 3. OR estimate for level 2 verses level 3

Estimate2 = Logit(0,0,0,0,3) - Logit(0,0,0,0,2) 
Estimate2




### Find a profile likelihood CI
exp( confint(model_MLR)  )







library(MASS)
exp(cbind(coef(model_MLR), confint(model_MLR))) 





















  
########################################## (d)

# 'Reduced Model'


model_reduced <- glm(sta ~ age+cpr+inf, family = "binomial", data = df)
summary(model_reduced)

# Log-likelihood test
 LLT <- lrtest(model_MLR,model_reduced)

 
 LLT
 
 
G1 = -2* (LLT$LogLik[2]-LLT$LogLik[1])
G1

G2 =  (model_reduced$deviance - model_MLR$deviance)
G2




# Wald test for model and reduced model


waldtest(model_MLR,model_reduced, test ="Chisq")


waldtest(model_MLR,model_reduced, test ="F")



 # rmarkdown::render("p2code.R")


