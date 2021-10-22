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

### Controllo collinearità -----
target=dati[,c("drinks_day")]
covariate=dati[,c(2,4:6)]
library(mctest)
imcdiag(covariate,target) #Tutto risulta collineare

### First model ----
# dalle descrittive osserviamo che Y non si distribuisce come una normale
# trasformeremo dopo
colnames(dati)
lm1<-lm(drinks_day~diabetes+depression
        +age+gender+race+grip_strength  
        +education+bmi+marital+income+household_size
        +insurance
        #+private_insur+medicare+medicaid
        +gen_health+iron, data=dati) 
summary(lm1)

par(mfrow=c(2,2)) 
plot(lm1)
par(mfrow=c(1,1)) 


### Second model ----
# trasformo Y tramite log, tolgo variabili non significative
# tolgo intercetta, non ha senso
hist(log(dati$drinks_day+1)) #esponenziale negativissima
lm2<-lm(log(drinks_day+1)~0
        +age+gender+ 
        +education+bmi+marital
        #+insurance+private_insur+medicare+medicaid
        +gen_health+iron, data=dati) 
summary(lm2)

par(mfrow=c(2,2)) 
plot(lm2)
par(mfrow=c(1,1)) 


p<-predict(lm1,dati)
plot(p, dati$drinks_day)


### Osservazioni influenti? -----
### Distanze di cook valori influenti----
require(faraway)
cook <- cooks.distance(lm1)
halfnorm(cook, ylab = "distanza di Cook") # 1,3
### Punti di leva ----
halfnorm(hatvalues(lm1), main="Punti di leva") #2318,2110


# Osservazione influenti? Da rifare
influencePlot(lm1, main="Influence Plot")
stzed <- rstudent(lm1)
lever <- hat(lm1.matrix(lm1))
dffits1 <- dffits(lm1)
cooksd <- cooks.distance(lm1);cooksd
cutoff <- 4/((nrow(dati)-length(lm1$coefficients)-2))
plot(lm1, which=4, cook.levels=cutoff)
abline(h=cutoff)
influential <- dati[cooksd >= cutoff,];influential 
influ = dati[influential, ];influ                  
filtered <- dati[cooksd < cutoff, ]  ;filtered    

### Modello senza certe variabili
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
