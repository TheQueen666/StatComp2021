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
colnames(dati)
lm1<-lm(drinks_day~diabetes+as.factor(depression)
        +age+gender+race+grip_strength  
        +education+bmi+marital+income+household_size
        +insurance
        #+private_insur+medicare+medicaid
        +gen_health+iron, data=dati) 
summary(lm1)

par(mfrow=c(2,2)) 
plot(lm1)
par(mfrow=c(1,1)) 

p<-predict(lm1,dati)

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
hist(log(dati$drinks_day+1)) #esponenziale negativissima
lm2<-lm(log(drinks_day+1)~0
        +age+gender
        +education+bmi+marital
        #+insurance+private_insur+medicare+medicaid
        +gen_health+iron, data=dati) 
summary(lm2)

par(mfrow=c(2,2)) 
plot(lm2)
par(mfrow=c(1,1)) 

imcdiag(lm2)


p<-predict(lm1,dati)
plot(p, dati$drinks_day) #brutto

car::ncvTest(lm2) # permane eteroschedasticità


### Osservazioni influenti? -----
### Distanze di cook valori influenti----
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

### Modello con trasformate ----
library(gam)
gam1<-gam(drinks_day~0+
          +s(age)+gender+s(race)+s(grip_strength)  
          +s(education)+s(bmi)+s(marital)+s(household_size)
          +insurance
          #+private_insur+medicare+medicaid
          +s(gen_health)+s(iron), data=dati)
summary(gam1)
# race  vuole una parabola probabilmente
# education vuole un terzo grado o parabola inferzo, bmi forse una parabola o una terza
# income una esponenziale

### Modello con trasformate ma modificato----
gam2<-gam(log(drinks_day+1)~0
                  +s(age)+gender+s(race) 
          +s(education)+s(bmi)+s(marital)+s(household_size)
          +insurance
          #+private_insur+medicare+medicaid
          +s(gen_health)+s(iron), data=dati)
summary(gam2)


par(mfrow=c(2,2)) 
plot(gam2)
par(mfrow=c(1,1)) 
# cosa passa da age?
# race una parabola
# Education una parabola

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
library(gvlma)
library(lmtest)
#Prima
bptest(lm1)
gvlma(lm1)

#Dopo
bptest(lmf)
gvlma(lmf) 


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

### Confronto modello iniziale-finale ----
pf<-predict(lmf,dati)
par(mfrow=c(1,2)) 
plot(p,dati$drinks_day)
plot(pf, dati$drinks_day)
