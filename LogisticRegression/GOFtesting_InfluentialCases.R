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


R_MAX_NUM_DLLS = 500



data <- read.csv("icu.csv")

attach(data)

# Creating the nloc column. nloc = 0 if loc = 0, and nloc = 1 if loc = 1 or loc =2
X <- replicate(200,0)






for (j in 1:200) {
  if (loc[j] == 0) {
    X[j]  =0
  }
  else if (loc[j] ==1 || loc[j] ==2) {
    X[j] = 1
  }
}
  


# Appending the column to the data
nloc <- X
data<- cbind(data,nloc)



fit <- glm(sta ~ age+ sys +age*sys+can+type+ph+pco+nloc, family="binomial",data=data)


# summary(fit)
# plots <- plot(fit)




# This is a data table with rows sorted by ?????^i (dBhat)


diagnostics  <- LogisticDx::dx(fit)

# diagnostics[180:200]
 
# Looked at  diagnostics[n,N] for n=,10, N=n+10,; n=20,30...,100,..etc looking at statistics.
#  age sys can  dChisq                       dBhat              Pr             dr               row #         
#  51 134   0    dChisq =4.4868982                          -2.1072582   -1.8405857             165
#  49 140   0    dChisq = 4.7695996                          -2.1716176 -1.8672203              171
#  41,140   0    dChisq = 5.9785293                             -2.4276798    -1.9649925         182
#  69, 170   0    dChisq = 4.9038084                             -2.1895714    -1.8744927        184         
#  19, 140   0   dChisq = 11.1480430                           -3.2984779         -2.2247933         189
#  20, 148   0    dChisq = 8.5532756                         -2.8743403            -2.1099298          191
#  40, 86   0   dChisq = 11.0711786                         -3.2683307          -2.2172271             194
#  53, 148   0    dChisq = 7.0373530                            -2.5500518           -2.0076212       196
#  65, 104   0    dChisq = 5.7052272                          -2.2742006          -1.9078894            197
#  75, 130   1    dChisq = 5.8244698                               -2.2976444    -1.9168921            198  
#  70, 168   0     dChisq = 90.2202976       0.88981195        -9.4519403        -3.0011902           199  **
#  50, 256   0    dChisq = 5.2919429         1.01715575     -2.1068397       -1.8404095             200  **
    








GetID <- function(x,y) {
 z <- dplyr::filter(data, data$age == x & data$sys == y )$id
}

detach(data)




# Did it manually
# print(GetID(50, 256 ))


# IDs:
# 412   d
# 671   d
#238    d
#  --         not identf
#127    d
# 380   d
# 285   d
# 154    d
# 666     d
#  202     d        can =1 identified
#   208     d 
#   921     d






# deleted those rows and now reading back in data to fit.




data_mod <- read.csv("icu_mod.csv")
attach(data_mod)
X <- replicate(189,0)

for (j in 1:189) {
  if (loc[j] == 0) {
    X[j]  =0
  }
  else if (loc[j] ==1 || loc[j] ==2) {
    X[j] = 1
  }
}
# Appending the column to the modified data
nloc <- X
data_mod <- cbind(data_mod,nloc)



fit_mod <- glm(sta ~ age+ sys +age*sys+can+type+ph+pco+nloc, family="binomial", data_mod)










# Model with influential cases deleted : fit_mod
# Full model : fit


summary(fit_mod)

summary(fit)




# we can see the AIC decreased from 140.23 to 92.91
# Null/Res deviance decreased from 200.16/ 122.23 to 162.02 / 74.91






detach(data_mod)






########################################################## (a).


par(mar=c(2,2,2,2))
gof1<- LogisticDx::gof(fit, g=10,plotROC=FALSE)

gof1$auc
gof1$gof
gof1$R2




gof_mod<- LogisticDx::gof(fit_mod,g=9,plotROC=FALSE)
gof_mod$auc
gof_mod$gof
gof_mod$R2
























### rmarkdown::render("p1code.R")













