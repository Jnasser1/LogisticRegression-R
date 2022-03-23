
  
library(dplyr)
library(broom)
library(aod)
library(knitr)
library(rmarkdown)


  


# Same as icu.txt just imported to Excel. Columns 1,2,3 are id, sta, age, respectively.
df <- read.csv("icu.csv")


#scatter plot for 1(b)


par(mar=c(4,4,4,4))
plot(df$age,df$sta, main="Response STA ~ Covariate AGE ", xlab="Age", ylab="Binary Vital Status", pch=4)


# Uses dplyr package to get mean of sta for ages filtered to a certain range 




df %>% filter( age >= 15 & age <= 24 )  %>%
  summarise(mean=mean(sta))




df %>% filter( age >= 25 & age <= 34 )  %>%
  summarise(mean=mean(sta))
  


  df %>% filter( age >= 35 & age <= 44 )  %>%
  summarise(mean=mean(sta))
  
  df %>% filter( age >= 45 & age <= 54 )  %>%
  summarise(mean=mean(sta))
  
  
  
  
  
  df %>% filter( age >= 55 & age <= 64 )  %>%
  summarise(mean=mean(sta))
  
  
df %>% filter( age >= 65 & age <= 74 )  %>%
  summarise(mean=mean(sta))
  
  
  



    
df %>% filter( age >= 75 & age <= 84 )  %>%
  summarise(mean=mean(sta))
  
  
  
    
df %>% filter( age >= 85 & age <= 94 )  %>%
  summarise(mean=mean(sta))



  


# Want to plot midpoints vs Values on same plot
                                                        
STAValues = c(0.9230769, 1, 0.8181818, 0.8, 0.7948718, 0.82, 0.7, 0.5454545)
midpoints = c(19.5, 29.5, 39.5, 49.5, 59.5, 69.5, 79.5,89.5)



#1(c)
plot(midpoints,STAValues, main="Mean values for AGE vs Vital Status ", xlab="Age Midpoints", ylab="STA Means", pch=4)


# Want logistic regression of STA on AGE (b):
model <- glm(sta ~ age, family = "binomial", data = df)

# provides table of MLE parameter estimates 
summary(model)

# MLE Coefficients

coef(model)


# Obtain a 95% profile confidence interval (d):
confint(model)


# Wald Confidence Interval:

confint.default(model)







# computes predicted probabilities of STA=1 for the subjects based on the data age.

predict(model, type="response")
probabilities <- predict(model,type="response")



#plot the predicted probabilities vs AGE

plot(df$age, probabilities, main="Predicited probabilities vs AGE", xlab="Age", ylab="Predcited probability of STA=1")



# Want to superimpose the above plot on the one obtained in (a)

plot(midpoints,STAValues,xlab=" ", ylab= " ", main="Superimposed Plots")

par(new=TRUE)

plot(df$age,probabilities, xaxt = "n", yaxt = "n",xlab=" ", ylab= " ", col="red", pch=5)






vcov(model)

coef(model)

beta_0=coef(model)[1]
beta_1=coef(model)[2]



beta0_var <- 0.484529087
beta1_var <- 0.0001116015
beta0_beta1_cov <- -0.0071029945

z <- qnorm(.975)




# Part (f)

eta <- function(age) {
  STA <- beta_0 + beta_1 * age
  return(STA)
}


pi_prob <- function(age) {
  prob <- exp(eta(age)) /(1+exp(eta(age)))
  return(prob)
}


SE_logit <- function(beta0var,beta1var,beta0beta1cov,x_i) {
  SE <- beta0var + x_i*x_i*beta1var + 2*x_i*beta0beta1cov
  SE <- sqrt(SE)
  return(SE)
  
}



# For an 86-year old subject, predictor, probability, and SE:

logit86 <- eta(86)
logit86



probability86 <- pi_prob(86)
probability86




StandardError86 <- SE_logit(beta0_var,beta1_var,beta0_beta1_cov,86)


# Confidence Intervals:


L <- logit86-z*StandardError86
R <- logit86+z*StandardError86


L
R






F_L <- exp(L) /(1+exp(L))
F_R <- exp(R)/ (1+exp(R))


F_L
F_R




# rmarkdown::render("p1code.R")






df2 <- read.csv("loan_data.csv")
mapping <- c("Not Graduate" = 0, "Graduate" = 1)
df2$Education.r <- mapping[df$Education]
model_amount <- glm(df2$Education.r ~ df$LoanAmount, family = "binomial", data = df2)
model_income <- glm(d2f$Education.r ~ df$ApplicantIncome, family = "binomial", data = df2)
model_credit <- glm(df2$Credit_History ~ df2$ApplicantIncome, family = "binomial", data = df2)

summary(model_credit)
summary(model_amount)

summary(model_income)





predict(model_credit, type="response")

probabilities2 <- predict(model_credit,type="response")

par(mar=c(4,4,4,1))

plot(probabilities2, df2$ApplicantIncome, main="Predicited probabilities vs Applicant Income", xlab="Predictied Probabilities", ylab="Applicant Income", pch=4)












