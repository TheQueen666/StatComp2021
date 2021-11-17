setwd("C:/Users/Gabriele/Documents/GitHub/StatComp2021")
load("StatComp2021.RData")

# Vogliamo capire cosa spiega la frequenza delle bevute, drink days
dati<-read.csv("https://raw.githubusercontent.com/F041/StatComp2021/main/Merged_Unique_Names_V2.csv")
head(data)

### Librerie ----
library(forecast)
library(MASS)
library(car)
library(gvlma)
library(ggplot2)
library(tidyverse)
library(mctest)
library("skimr")
library(gvlma)
library(lmtest)
library(sandwich)
options(scipen=999)

### Descriptives ----
summary(dati) # ordine differente, vedo gli NAs con più fatica
descr<-skim(dati) #non userò le covariate con più missing >2000
descr
# num colonna: 5,6,10:14, 16, 18:24 ....


# posso automatizzare?
library(dplyr)
selezione<-drop_na(dati) #significa che tutte le righe hanno almeno un missing...

hist(dati$drinks_day) #esponenziale negativissima
plot(ecdf(dati$drinks_day))#Funzione di ripartizione. Un outilier a 80, poi 60
cor(dati[,c("income","age","drinks_day")]) #NAs give issues
pairs((dati[,c("income","age","drinks_day","bmi")]),panel = panel.smooth)

# Cosa succede se metto dentro cose con tanti NA?
pairs((dati[,c("military_insur","drinks_day")]),panel = panel.smooth)
cor(dati[,c("military_insur","drinks_day")])

cor(dati[,4:30])

cor(dati[,c("gen_health","iron")]) #controllo per gen_health, iron 
# troppi NA?

### Modifico variabili ----
dati$gender<-dati$gender%>%as.factor
dati$education<-dati$education%>%as.factor
dati$marital<-dati$marital%>%as.factor
dati$gen_health<-dati$gen_health%>%as.factor

### First model ----
# dalle descrittive osserviamo che Y non si distribuisce come una normale
# trasformeremo dopo
formula <- paste(colnames(dati), collapse="','")
lm1<-lm(drinks_day~diabetes+as.factor(depression)
        +age+gender+race+grip_strength  
        +education+bmi+marital+income+as.factor(household_size)
        +insurance
        #+private_insur+medicare+medicaid
        +gen_health+iron, data=dati) 
summary(lm1)

par(mfrow=c(2,2)) 
plot(lm1)
par(mfrow=c(1,1)) 

bptest(lm1) #presenta eteros. serve sistemare l'inferenza.
coeftest(lm1, vcov=vcovHC(lm1)) 


p<-predict(lm1,dati) #serve dopo

### Controllo collinearità con TOL sotto 0,3 vengolo tolte -----
target=dati[,c("drinks_day")]
covariate=dati[,c(2,4:6)]
covariate=as.matrix(covariate)
library(mctest)
imcdiag(covariate,target) # non funziona
imcdiag(lm1) # funziona 


### Second model ----
# trasformo Y tramite log, tolgo variabili non significative
# tolgo intercetta, non ha senso
hist(dati$drinks_day) #esponenziale negativissima
lm2<-lm(log(drinks_day+1)~0
        +age+gender
        +education+bmi+marital
        #+insurance+private_insur+medicare+medicaid
        +gen_health+iron, data=dati) 
summary(lm2)

par(mfrow=c(2,2)) 
plot(lm2)
par(mfrow=c(1,1)) 

bptest(lm2) #presenta eteros. serve sistemare l'inferenza.
coeftest(lm2, vcov=vcovHC(lm2)) 

imcdiag(lm2)


p<-predict(lm1,dati)
plot(p, dati$drinks_day) #brutto

car::ncvTest(lm2) # permane eteroschedasticità


### Osservazioni influenti? Codice lovaglio -----
# outliers
library(car)
influencePlot(lm2,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

cooksd <- cooks.distance(lm2)
cooksda=data.frame(cooksd)

# cutoff of cookD  4/(n-k).. NB n should be n used in the model!!!

n_used=length(lm2$residuals)
n_used # be careful!!! 

cutoff <- 4/(n_used-length(lm2$coefficients)-2)
cutoff
Noinflu=data.frame(dati[cooksd < cutoff, ])  # influential row numbers

lminf = lm(log(drinks_day+1)~0
           +age+gender
           +education+bmi+marital
           #+insurance+private_insur+medicare+medicaid
           +gen_health+iron, data=dati)
summary(lminf)

par(mfrow=c(2,2)) 
plot(lminf)
par(mfrow=c(1,1)) 

bptest(lminf) #presenta eteros. serve sistemare l'inferenza.
# Togliere osservazioni non migliora il modello

### Distanze di cook valori influenti: versione Pennoni ----
require(faraway)
cook <- cooks.distance(lm1)
halfnorm(cook, ylab = "distanza di Cook") # 1,3
### Punti di leva ----
halfnorm(hatvalues(lm1), main="Punti di leva") #2318,2110

### Modello senza certe variabili -----
lmr<-lm(log(drinks_day+1)~0
        +age+gender+ 
          +education+bmi+marital
        #+insurance+private_insur+medicare+medicaid
        +gen_health+iron, data=dati[,c(-1,-3,-2318,-2110)]) 
summary(lmr)

par(mfrow=c(2,2)) 
plot(lmr)
par(mfrow=c(1,1)) 

anova(lm2,lmr) # non conviene usare l'ultimo modello 

###  Inferenza con stand err robusti: da rivedere ----



coeftest(lm2, vcov=vcovHC(lm2)) 

### Box Cox----
library(MASS)
boxcoxreg1<-boxcox(lm2)
title("Lambda")
lambda=boxcoxreg1$x[which.max(boxcoxreg1$y)]
lambda # suggerisce trasformata y -0.3434343

dati$drinks_day_modificati<-dati$drinks_day^-0.3434343
lmc<-lm(drinks_day_modificati~0
        +age+gender+ 
                +education+bmi+marital
        #+insurance+private_insur+medicare+medicaid
        +gen_health+iron, data=dati) 
summary(lmc) #netto miglioramento

par(mfrow=c(2,2)) 
plot(lmc)
par(mfrow=c(1,1)) 

bptest(lmc) #permane eteros


### Modello con trasformate ----
library(gam)
gam1<-gam(drinks_day_modificati~0
          +s(age)+gender+ 
                  +education+s(bmi)+marital
          +gen_health+s(iron), data=dati)
summary(gam1)
par(mfrow=c(2,2)) 
plot(gam1)
par(mfrow=c(1,1)) 
# dubbio su age

lmg<-lm(drinks_day_modificati~0
        +I(age)^3+gender+ 
                +education+bmi+marital
        #+insurance+private_insur+medicare+medicaid
        +gen_health+iron, data=dati) 
summary(lmg) 

par(mfrow=c(2,2)) 
plot(lmg)
par(mfrow=c(1,1)) 

bptest(lmg) #permane eteros
#Inutili trasformate sulle x

### model selection da rivedere ----
library(MASS)
step <- stepAIC(lmc, direction="both")


### Ultimo modello: metto factor delle variabili e uso GAM---
lmf<-lm(log(drinks_day+1)~0
        +age+factor(gender)+as.factor(race)
        +income
        +factor(education)+bmi+factor(marital)
        #+insurance+private_insur+medicare+medicaid
        +gen_health+iron, data=dati)
summary(lmf)

shapiro.test(lmf$residuals) #accetta normalità dei residui

# guadagno risibile rispetto al secondo modello: conviene usare lm2

par(mfrow=c(2,2)) 
plot(lmf)
par(mfrow=c(1,1)) 

car::ncvTest(lmf) #permane eteros. In ogni caso uso lm2

# Controllo ipotesi

#Prima
bptest(lm1)
gvlma(lm1)

#Dopo
bptest(lmf)
gvlma(lmf) 

### Confronto modello iniziale-finale ----
pf<-predict(lmf,dati)
par(mfrow=c(1,2)) 
plot(p,dati$drinks_day)
plot(pf, dati$drinks_day)


### logistico: drinkdays >= 52 come soglia critica ----
table(dati$drinks_day); median(dati$drinks_day)
drink_dangerous<-ifelse((dati$drinks_day)>11,1,0);
glm1<-glm(drink_dangerous~0
          +age+gender
          #+ race^2
          +education^3+bmi^3+marital+exp(income)
          #+insurance+private_insur+medicare+medicaid
          +gen_health+iron, data=dati)

summary(glm1)
exp(glm1$coefficients)

r2=1-(1305.0/2978.4) # null dev / resid
r2

library(coefplot)
coefplot(fit, intercept=FALSE)

# PREDICTION
good_covariates$predicted_p <- predict(fit, good_covariates, type="response") 
tail(good_covariates)

# predicted target
good_covariates$predicted_y <- ifelse(good_covariates$predicted_p > 0.5,1,0)

table(observed=good_covariates$yes, predicted=good_covariates$predicted_y)/nrow(good_covariates)

accuracy=0.315+0.4819
accuracy



