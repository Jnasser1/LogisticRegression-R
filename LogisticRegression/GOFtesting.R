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
library(rms)




library(LogisticDx)





data <- read.csv("hw3p2.csv")
attach(data)



## y1 model
modely1 <- glm(y1~x1+x2, family="binomial", data=data)
summary(modely1)





## y2 model

modely2 <- glm(y2~x1+x2, family="binomial", data=data)
summary(modely2)






## GOF model y1

result1 <- LogisticDx::gof(modely1)

result1$R2

result1$auc  

result1$gof







## GOF model y2

result2 <- LogisticDx::gof(modely2)


result2$R2

result2$auc  

result2$gof 
########################### (a)
####### H-L tests


predictions_y1 <- predict(modely1, type="response")

predictions_y2 <- predict(modely2, type="response")




# H-L tests with different group sizes.

# y1 model
hoslem.test(y1,predictions_y1,g=5)
hoslem.test(y1,predictions_y1,g=10)
hoslem.test(y1,predictions_y1,g=11)
hoslem.test(y1,predictions_y1,g=20)
hoslem.test(y1,predictions_y1,g=50)



# y2 model
hoslem.test(y2,predictions_y2,g=5)
hoslem.test(y2,predictions_y2,g=10)
hoslem.test(y2,predictions_y2,g=11)
hoslem.test(y2,predictions_y2,g=20)
hoslem.test(y2,predictions_y2,g=50)









detach(data)







### rmarkdown::render("p2code.R")