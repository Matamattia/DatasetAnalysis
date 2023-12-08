#ANALISI DI UN DATASET

data=read.csv("Dataset_AH_gruppo12.csv")

#CONTROLLO DATI

#Controllo se ci sono entry nulle 
table(is.na(data)) 
#Nel nostro caso la tabella è popolata 

#Controlliamo che i dati siano numerici 
str(data) 

#Visualizziamo il dataset 
View(data)

#suddividiamo il dataset in training set(70%) e test set(30%) 
training=data[31:100,]
test=data[1:30,]

#STATISTICA DESCRITTIVA

# moda, mediana, media ,min, 1 quartile,3 quartile
summary(training) #Attraverso il summary notiamo le caratteristiche principali di tutti le variabili

# escursione campionaria
diff(range(training$y_ImageQuality))
diff(range(training$x1_ISO)) 
diff(range(training$x2_FRatio)) 
diff(range(training$x3_TIME))
diff(range(training$x4_MP))
diff(range(training$x5_CROP))
diff(range(training$x6_FOCAL))
diff(range(training$x7_PixDensity))
#indici di dispersione
apply(training,2,var)
apply(training, 2, sd)


#Istogrammi
hist(training$x1_ISO,las=1,  freq=F,xlab="Sensibilità del sensore",ylab="Frequenza relativa", main="Istogramma ISO",col="green")#las=1 mette i numeri in orizzontale
lines(density(training$x1_ISO),lwd=2,col="red")
curve(expr = dnorm(x = t,mean = mean(training$x1_ISO), sd = sd(training$x1_ISO)),from = min(training$x1_ISO),to = max(training$x1_ISO),n = 500,add = TRUE,xname = "t",col = "blue", lwd=2)

hist(training$x2_FRatio,las=1,  freq=F,xlab="Rapporto focale",ylab="Frequenza relativa", main="Istogramma FRatio",col="green")
lines(density(training$x2_FRatio),lwd=2,col="red")
curve(expr = dnorm(x = t,mean = mean(training$x2_FRatio), sd = sd(training$x2_FRatio)),from = min(training$x2_FRatio),to = max(training$x2_FRatio),n = 500,add = TRUE,xname = "t",col = "blue", lwd=2)

hist(training$x3_TIME,las=1,  freq=F,xlab="Tempo di esposizione",ylab="Frequenza relativa", main="Istogramma TIME",col="green")
lines(density(training$x3_TIME),lwd=2,col="red")
curve(expr = dnorm(x = t,mean = mean(training$x3_TIME), sd = sd(training$x3_TIME)),from = min(training$x3_TIME),to = max(training$x3_TIME),n = 500,add = TRUE,xname = "t",col = "blue", lwd=2)

hist(training$x4_MP,las=1,  freq=F,xlab="Mega Pixel del sensore",ylab="Frequenza relativa", main="Istogramma MP",col="green")
lines(density(training$x4_MP),lwd=2,col="red")
curve(expr = dnorm(x = t,mean = mean(training$x4_MP), sd = sd(training$x4_MP)),from = min(training$x4_MP),to = max(training$x4_MP),n = 500,add = TRUE,xname = "t",col = "blue", lwd=2)

hist(training$x5_CROP,las=1,  freq=F,xlab="Fattore di CROP",ylab="Frequenza relativa", main="Istogramma CROP",col="green")
lines(density(training$x5_CROP),lwd=2,col="red")
curve(expr = dnorm(x = t,mean = mean(training$x5_CROP), sd = sd(training$x5_CROP)),from = min(training$x5_CROP),to = max(training$x5_CROP),n = 500,add = TRUE,xname = "t",col = "blue", lwd=2)

hist(training$x6_FOCAL,las=1,  freq=F,xlab="Focale",ylab="Frequenza relativa", main="Istogramma FOCAL",col="green")
lines(density(training$x6_FOCAL),lwd=2,col="red")
curve(expr = dnorm(x = t,mean = mean(training$x6_FOCAL), sd = sd(training$x6_FOCAL)),from = min(training$x6_FOCAL),to = max(training$x6_FOCAL),n = 500,add = TRUE,xname = "t",col = "blue", lwd=2)

hist(training$x7_PixDensity,las=1,  freq=F,xlab="Densità di pixel",ylab="Frequenza relativa", main="Istogramma PixDensity",col="green")
lines(density(training$x7_PixDensity),lwd=2,col="red")
curve(expr = dnorm(x = t,mean = mean(training$x7_PixDensity), sd = sd(training$x7_PixDensity)),from = min(training$x7_PixDensity),to = max(training$x7_PixDensity),n = 500,add = TRUE,xname = "t",col = "blue", lwd=2)

hist(training$y_ImageQuality,las=0,  freq=F,xlab="Qualità dell'immagine percepita",ylab="Frequenza relativa", main="Istogramma ImageQuality",col="green")
lines(density(training$y_ImageQuality),lwd=2,col="red")
curve(expr = dnorm(x = t,mean = mean(training$y_ImageQuality), sd = sd(training$y_ImageQuality)),from = min(training$y_ImageQuality),to = max(training$y_ImageQuality),n = 500,add = TRUE,xname = "t",col = "blue", lwd=2)

#BoxPlot 

out0 = boxplot(training$y_ImageQuality,main="Boxplot variabile dipendente"); out0$out #1 outlier, -8.682078, riga 80
out1 = boxplot(training$x1_ISO,main="Boxplot variabile x1"); out1$out
out2 = boxplot(training$x2_FRatio,main="Boxplot variabile x2"); out2$out
out3 = boxplot(training$x3_TIME,main="Boxplot variabile x3"); out3$out
out4 = boxplot(training$x4_MP,main="Boxplot variabile x4"); out4$out
out5 = boxplot(training$x5_CROP,main="Boxplot variabile x5"); out5$out
out6 = boxplot(training$x6_FOCAL,main="Boxplot variabile x6"); out6$out
out7 = boxplot(training$x7_PixDensity,main="Boxplot variabile x7"); out7$out
training=training[-50,] # rimuoviamo outlier

# TEST DI NORMALITA' (SHAPIRO TEST E QQPLOT)

# la variabile dipendente deve rispettare ipotesi di normalità per inferenza sull'errore

shapiro.test(training$y_ImageQuality) # p-value = 0.6115, non rifiuto H0 -> normale
qqnorm(training$y_ImageQuality)
qqline(training$y_ImageQuality)
shapiro.test(training$x1_ISO) # p-value = 0.08316, non rifiuto H0 -> normale
shapiro.test(training$x2_FRatio) # p-value = 0.002954, rifiuto H0 -> non normale
qqnorm(training$x2_FRatio)
qqline(training$x2_FRatio) 
shapiro.test(training$x3_TIME) # p-value = 0.005979, rifiuto H0 -> non normale
qqnorm(training$x3_TIME)
qqline(training$x3_TIME)
shapiro.test(training$x4_MP) # p-value = 0.09268, non rifiuto H0 -> normale 
shapiro.test(training$x5_CROP) # p-value = 5.81e-05, rifiuto H0 -> non normale
qqnorm(training$x5_CROP)
qqline(training$x5_CROP)
shapiro.test(training$x6_FOCAL) # p-value = 0.008652, rifiuto H0 -> non normale
qqnorm(training$x6_FOCAL)
qqline(training$x6_FOCAL)
shapiro.test(training$x7_PixDensity) # p-value = 0.628, non rifiuto H0 -> normale

#scatter plot tra tutte le combinazioni di variabili

pairs(training[,c(1,2,3,4,5,6,7,8)],
      col = "red",                # cambia colore
      pch = 5,                   # cambia la forma dei punti
      labels = c("Y","x1","x2","x3","x4","x5","x6","x7"), # ; con questo esempio dice come customizzare i grafici
      #main = aggiunge un titolo principale
)


library(corrplot)
training_new=training[,1:8]
cor1 = round(cor(training_new), digits = 2); cor1#matrice di correlazione
corrplot.mixed(cor(training_new),number.cex=0.8,tl.cex=0.8)#rappresenta graficamente  la matrice di correlazione

library(ggplot2)
library("GGally")

#grafici senza la y, per vedere solo se sono linearmente dipendenti i regressori così da non inserirli insieme nel nostro modello di regressione multipla
ggpairs(training[,c(8,7,6,5,4,3,2)])#questo permette di unire i grafici. Rappresenta scatter plot e pdf dei regressori;
#correlazione alta tra x4 e x7
ggpairs(training[,c(8,5)])

#REGRESSIONE POLINOMIALE

#ora faccio i grafici tra la y e ogni singola x per vedere la regressione polinomiale
ggpairs(training[,c(8,1)]) # correlazione assente
ggpairs(training[,c(7,1)]) # correlazione assente
ggpairs(training[,c(6,1)]) # correlazione medio-alta negativa (-0.611)
ggpairs(training[,c(5,1)]) # correlazione assente
ggpairs(training[,c(4,1)]) # correlazione bassa (0.274)
ggpairs(training[,c(3,1)]) # correlazione medio-bassa negativa (-0.376)
ggpairs(training[,c(2,1)]) # correlazione medio-bassa negativa (-0.382)

# creazione dei modelli polinomiali con stepwise forward
step_1= step(lm(y_ImageQuality ~ 1 , data=training),scope = ~ x1_ISO + I(x1_ISO^2) + I(x1_ISO^3),direction="forward",trace=1)
summary(step_1)
# modello ottimo (AIC): y = x^3

step_2= step(lm(y_ImageQuality ~ 1, data=training),scope = ~ x2_FRatio + I(x2_FRatio^2) + I(x2_FRatio^3),direction="forward",trace=1)
summary(step_2)
# modello ottimo (AIC): y = x + x^2

step_3=step(lm(y_ImageQuality ~ 1 , data=training),scope = ~ x3_TIME + I(x3_TIME^2) + I(x3_TIME^3),direction="forward",trace=1)
summary(step_3)
# modello ottimo (AIC): y = x

step_4=step(lm(y_ImageQuality ~ 1 , data=training),scope = ~ x4_MP + I(x4_MP^2) + I(x4_MP^3),direction="forward",trace=1)
summary(step_4)
# modello ottimo (AIC): y = x^2

step_5=step(lm(y_ImageQuality ~ 1 , data=training),scope = ~ x5_CROP + I(x5_CROP^2) + I(x5_CROP^3),direction="forward",trace=1)
summary(step_5)
# modello ottimo (AIC): y = x + x^2

step_6=step(lm(y_ImageQuality ~ 1 , data=training),scope = ~ x6_FOCAL + I(x6_FOCAL^2) + I(x6_FOCAL^3),direction="forward",trace=1)
summary(step_6)
# modello ottimo (AIC): y = x

step_7=step(lm(y_ImageQuality ~ 1 , data=training),scope = ~ x7_PixDensity + I(x7_PixDensity^2) + I(x7_PixDensity^3),direction="forward",trace=1)
summary(step_7)
# modello ottimo (AIC): y = 1
# considerando la correlazione tra x4 e x7 e questo conviene non inserire x7 nei modelli multipli


# REGRESSIONE MULTIPLA

# tutti i regressori lineari
lin_mult1 = lm(y_ImageQuality ~ x1_ISO + x2_FRatio + x3_TIME + x4_MP + x5_CROP + x6_FOCAL + x7_PixDensity, data = training)
summary(lin_mult1)  # T-Test rimuove x4, x6, x7; R^2=0.7815
# rimuovo x4, x6 e x7 (p-values: 0.0852, 0.3697, 0.7315)
lin_mult2 = lm(y_ImageQuality ~ x1_ISO + x2_FRatio + x3_TIME + x5_CROP, data = training)
summary(lin_mult2)# R^2=0.7562

# aggiungo i termini polinomiali suggeriti dalle regressioni polinomiali
lin_mult3 = lm(y_ImageQuality ~ 
                 x1_ISO + I(x1_ISO^3) + 
                 x2_FRatio + I(x2_FRatio^2) + 
                 x3_TIME + 
                 x4_MP + I(x4_MP^2) + 
                 x5_CROP + I(x5_CROP^2) + 
                 x6_FOCAL,
               data = training)
summary(lin_mult3) # T-Test rimuove x1, x4^2, x5^2, x6; R^2=0.8805
# rimuovo regressori con p-value maggiore di 0.05
lin_mult4 = lm(y_ImageQuality ~ 
                 I(x1_ISO^3) + 
                 x2_FRatio + I(x2_FRatio^2) + 
                 x3_TIME +
                 x4_MP +
                 x5_CROP, 
               data = training)
summary(lin_mult4) # T-Test tutti accettati; R^2=0.8707

# VALORE PARAMETRI E INTERVALLI DI CONFIDENZA
print(lin_mult4$coefficients) # stima ai minimi quadrati dei parametri del modello
confint(lin_mult4) #intervalli di confidenza di tutti i parametri del modello

# calcolo manuale per modello lineare semplice: step_6 <- y=x6
confint(step_6) #Intervalli di confidenza: Intercept=[71.13973,83.355227]; x6_FOCAL=[-11.65512,1.456842]
b0 = step_6$coefficients[1]; b0
b1 = step_6$coefficients[2]; b1
y.fitted=step_6$fitted.values # Y_cappello
y=training$y_ImageQuality
x=training$x6_FOCAL
n=length(y) # data-size
sqe=sum((y - y.fitted)^2); sqe #SQE
msqe=sqe/(n-2); msqe #MSQE
S=sqrt(msqe); S # Stimatore della deviazione standard
t.val = qt(0.975, n - 2) #Quantile T-Student livello 1-alpha/2  (alpha=0.05)
#Intervallo di confidenza con livello di confidenza al 95% per b0
conf_int_b0=t.val*S*sqrt(1/n+( (mean(x))^2 / (sum(x^2)-n*(mean(x))^2 ) ) )
L_b0=b0-conf_int_b0;L_b0
U_b0=b0+conf_int_b0;U_b0
#Intervallo di confidenza con livello di confidenza al 95% per b1
conf_int_b1=t.val*S*sqrt( (1 / (sum(x^2)-n*(mean(x))^2 ) ) )
L_b1=b1-conf_int_b1;L_b1
U_b1=b1+conf_int_b1;U_b1
# Intervallo di confidenza con livello di confidenza al 95% di E[Y|X=x]
xx <- seq(min(x),max(x),along.with = x)  # i valori vanno messi ordinati (sia asse x che asse y) per plottare la retta di regressione
fhat_of_y = (cbind(1, xx)%*%step_6$coefficients) ## b0+b1*xx
low_conf_int_ev=fhat_of_y - t.val*S*sqrt(1/n+( (xx-mean(x))^2  / (sum(x^2)-n*(mean(x))^2 ) ) )
up_conf_int_ev=fhat_of_y + t.val*S*sqrt(1/n+( (xx-mean(x))^2  / (sum(x^2)-n*(mean(x))^2 ) ) )
low_pred_int=fhat_of_y - t.val*S*sqrt(1+1/n+( (xx-mean(x))^2  / (sum(x^2)-n*(mean(x))^2 ) ) )
up_pred_int=fhat_of_y + t.val*S*sqrt(1+1/n+( (xx-mean(x))^2  / (sum(x^2)-n*(mean(x))^2 ) ) )
# plot di intervallo confidenza di E[Y|X=x] e intervallo di predizione
dev.new(width = 550, height = 330, unit = "px")
plot(x, y,col="red",type="p",pch=19,xlab = "x6_FOCAL",ylab="y_ImageQuality",xlim=c(min(training$x6_FOCAL),max(training$x6_FOCAL)),ylim=c(min(training$y_ImageQuality),max(training$y_ImageQuality)))
lines(xx, fhat_of_y,lty=1, lwd=2, col="black")
lines(xx, low_conf_int_ev,lty=3, lwd=2, col="blue")
lines(xx, up_conf_int_ev,lty=3, lwd=2, col="blue")
lines(xx, low_pred_int,lty=3, lwd=2, col="green")
lines(xx, up_pred_int,lty=3, lwd=2, col="green")
legend('topright', c('dati', 'linea di regressione', 'intervallo di confidenza','','intervallo di predizione',''), lty=c(NA,1,3,NA,3,NA), lwd=c(NA,2,2,NA,2,NA), col=c('red', 'black', 'blue', 'blue', 'green', 'green'), pch=c(19,NA,NA,NA,NA,NA),cex=0.95)

# DIAGNOSTICA DEI MODELLI

#lin_mult4
plot(lin_mult4)
# Residuals vs Fitted: i residui si distribuiscono intorno all'asse delle ascisse con varianza abbastanza omogenea -> buon modello, possiamo assumere l'omoschedasticità
# QQ-Plot: tranne che per le code i quantili teorici e quelli del modello sono allineati lungo la bisettrice, quindi possiamo assumere la normalità dei campioni
# Scale-Location: nessun campione va oltre il limite (valore assoluto maggiore di 3), quindi non ci sono outlier
# Residuals vs Leverage: nessun campione va oltre la distanza di Cook, quindi non ci sono punti di leva
library(lmtest)
# test di omoschedasticità
bptest(lin_mult4) # p-value = 0.8999 > 0.05 -> non rifiuto H0 -> omoschedasticità
# BoxPlot dei residui
boxplot(lin_mult4$residuals)
# Mediana poco minore di 0, Q1 vicino a -5, Q3 vicino a 7, Q1-1.5*IQR vicino a -15, Q3+1.5*IQR vicino a 20
# test di normalità dei residui (assunzione di errore normale)
shapiro.test(lin_mult4$residuals) # p-value = 0.5182, non rifiutiamo H0 -> residui rispettano normalità

# MODELLO MIGLIORE TRAMITE STEPWISE

# Parametro: AIC
best1 = step(lm(y_ImageQuality ~ 1 , data=training),scope = ~ x1_ISO + I(x1_ISO^2) + I(x1_ISO^3) +
               x2_FRatio + I(x2_FRatio^2) + I(x2_FRatio^3) + 
               x3_TIME + I(x3_TIME^2) + I(x3_TIME^3) +
               x4_MP + I(x4_MP^2) + I(x4_MP^3) +
               x5_CROP + I(x5_CROP^2) + I(x5_CROP^3) +
               x6_FOCAL + I(x6_FOCAL^2) + I(x6_FOCAL^3) +
               x7_PixDensity + I(x7_PixDensity^2) + I(x7_PixDensity^3)
             ,direction="forward",trace=1)
summary(best1) # y = x1^2 + x1^3 + x2 + x2^2 + x3 + x3^2 + x4 + x4^3 + x5 + x6 + x7^2; AIC=295.90, R^2=0.9165
best2 = step(lm(y_ImageQuality ~ 
                  I(x1_ISO^3) + 
                  x2_FRatio + I(x2_FRatio^2) + 
                  x3_TIME +
                  x4_MP +
                  x5_CROP, data=training),
             scope = ~ x1_ISO + I(x1_ISO^2) + I(x1_ISO^3) +
               x2_FRatio + I(x2_FRatio^2) + I(x2_FRatio^3) + 
               x3_TIME + I(x3_TIME^2) + I(x3_TIME^3) +
               x4_MP + I(x4_MP^2) + I(x4_MP^3) +
               x5_CROP + I(x5_CROP^2) + I(x5_CROP^3) +
               x6_FOCAL + I(x6_FOCAL^2) + I(x6_FOCAL^3) +
               x7_PixDensity + I(x7_PixDensity^2) + I(x7_PixDensity^3)
             ,direction="both",trace=1) 
summary(best2) # uguale a best1
# PARAMETRO BIC
best3 = step(lm(y_ImageQuality ~ 1 , data=training),scope = ~ x1_ISO + I(x1_ISO^2) + I(x1_ISO^3) +
               x2_FRatio + I(x2_FRatio^2) + I(x2_FRatio^3) + 
               x3_TIME + I(x3_TIME^2) + I(x3_TIME^3) +
               x4_MP + I(x4_MP^2) + I(x4_MP^3) +
               x5_CROP + I(x5_CROP^2) + I(x5_CROP^3) +
               x6_FOCAL + I(x6_FOCAL^2) + I(x6_FOCAL^3) +
               x7_PixDensity + I(x7_PixDensity^2) + I(x7_PixDensity^3)
             ,trace=1, direction = "forward", k=log(n))
summary(best3) # y = x1^2 + x1^3 + x2 + x2^2 + x3 + x4 + x5; BIC = 318.11; R^2=0.9002
best4 = step(lm(y_ImageQuality ~ 
                  I(x1_ISO^2) + I(x1_ISO^3) + 
                  x2_FRatio + I(x2_FRatio^2) + 
                  x3_TIME +
                  x5_CROP +
                  I(x7_PixDensity^2), data=training),
             scope = ~ x1_ISO + I(x1_ISO^2) + I(x1_ISO^3) +
               x2_FRatio + I(x2_FRatio^2) + I(x2_FRatio^3) + 
               x3_TIME + I(x3_TIME^2) + I(x3_TIME^3) +
               x4_MP + I(x4_MP^2) + I(x4_MP^3) +
               x5_CROP + I(x5_CROP^2) + I(x5_CROP^3) +
               x6_FOCAL + I(x6_FOCAL^2) + I(x6_FOCAL^3) +
               x7_PixDensity + I(x7_PixDensity^2) + I(x7_PixDensity^3)
             ,direction="both",trace=1, k=log(n)) 
summary(best4) # uguale a best3


#calcolo SQE e MSQE( sul modello best3)
y.fitted= best3$fitted.values

#MSQE è uno stimatore non distorto della varianza dell' errore
sqe=sum((y - y.fitted)^2)
sqe #4244.986
msqe=sqe/(n-8)
msqe #69.6
#stima dev standard
S=sqrt(msqe)
S #8.34


# SCEGLIAMO BEST3 (PARAMETRO BIC) - DIAGNOSTICA

print(best3$coefficients)
confint(best3)
plot(best3)
# Residuals vs Fitted: i residui si distribuiscono intorno all'asse delle ascisse con varianza abbastanza omogenea -> buon modello, possiamo assumere l'omoschedasticità
# QQ-Plot: tranne che per le code (con distanza minore) i quantili teorici e quelli del modello sono allineati lungo la bisettrice, quindi possiamo assumere la normalità dei campioni
# Scale-Location: nessun campione va oltre il limite (valore assoluto maggiore di 3), quindi non ci sono outlier
# Residuals vs Leverage: nessun campione va oltre la distanza di Cook, quindi non ci sono punti di leva


library(lmtest)
# test di omoschedasticità

bptest(best3) # p-value = 0.2226 > 0.05 -> non rifiuto H0 -> omoschedasticità
shapiro.test(best3$residuals)
# p-value = 0.3069 > 0.05 -> non rifiuto H0 -> normalità dei residui

# TEST SET

predictions = predict(best3,newdata=test); predictions
testing_error = mean((predictions - test$y_ImageQuality)^2); testing_error # 104.8651

predictions2 = predict(lin_mult4,newdata=test); predictions2
testing_error2 = mean((predictions2 - test$y_ImageQuality)^2); testing_error2 # 157.0269

predictions3 = predict(best1,newdata=test); predictions3
testing_error3 = mean((predictions3 - test$y_ImageQuality)^2); testing_error3 # 103.9134
rtesting_error3=sqrt(testing_error3)

mse_func=function(actual,predicted) 
{
  mean( (actual-predicted)^2 ) 
}
mse_best3 = mse_func(training$y_ImageQuality, best3$fitted.values); mse_best3 # 61.52153

rmse_best3= sqrt(mse_best3)

