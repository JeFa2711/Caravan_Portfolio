#<<<---------- Bemerkung Korrektur --------------->>>
# -------------
# Ergebnis:
# -------------
# Aufgabe 1: /3
# Aufgabe 2: /4
# Aufgabe 3: /5
# Aufgabe 4: /6
# Aufgabe 5: /6
# Aufgabe 6: /6
# -------------
# Summe: /30
#--------------
#<<<---------------------------------------------->>>

#
# Template für die Bearbeitung der Übungsaufgaben 7
#        -- Analyse des BostonHousing-Datensatz -- 
#

#-----------------
# Bitte ausfüllen:
#-----------------
# Gruppe: Anakin
# Namen und Matrikelnummern:
# Jonas Lang, 7904543
# Simon Baader, 9879031
# Nina Heyer, 8804482
# Jessica Fander, 2525677


#-------------------------
# notwendinge Bibliotheken
#-------------------------

library(mlbench) #Datensatz
library(glmnet)  #Ridge/Lasso-Regression

data("BostonHousing")

#------------------------------------
# Aufgabe 1: Trainings- und Testdaten
#------------------------------------

seed <- 15
# Damit alle Seed-Funktionen den gleichen Seed haben. Für alle Zahlen hier wurde der seed15 verwendet
set.seed(seed) # Für bessere Nachvollziehbarkeit wird ein Seed gesetzt.
laenge <- length(BostonHousing$crim) # Anzahl der Datensätze
aufteilung <- sample(laenge,0.8*laenge) # Aufteilung für Test und Trainingsdaten
BostonHousing.trainingsdaten <- BostonHousing[aufteilung,] # Trainingsdaten
BostonHousing.testdaten<- BostonHousing[-aufteilung,] # Testdaten

#------------------------------------
# Aufgabe 2: Lineare Regression
#------------------------------------

# Lineares Modell 
lm.fit.all <- lm(
  formula = medv ~ .,
  data = BostonHousing.trainingsdaten
)
# Funktion für MQA für Trainingsdaten
mqaTraining <- function(modell){
  return( 
    mean(
      (BostonHousing.trainingsdaten$medv - predict(modell,BostonHousing.trainingsdaten ))^2)
    )
}

# Funktion für MQA für Trainingsdaten
mqaTest <- function(modell){
  return( 
    mean(
      (BostonHousing.testdaten$medv - predict(modell,BostonHousing.testdaten ))^2)
  )
}

# Summary für bessere Übersicht
summary(lm.fit.all)

#Trainingsfehler
training.mqa.lm <- mqaTraining(lm.fit.all)
training.mqa.lm
# [1] 22.34304

#Testfehler
test.mqa.lm <- mqaTest(lm.fit.all)
test.mqa.lm 
# [1] 20.73189

#------------------------------------
# Aufgabe 3: Bias-Variance
#------------------------------------



summary(lm.fit.all)
# Relevante Ergebnisse des summaries:
# Residual standard error: 4.811 on 390 degrees of freedom
# Multiple R-squared:  0.7282,	Adjusted R-squared:  0.7191 
# F-statistic: 80.36 on 13 and 390 DF,  p-value: < 2.2e-16

# R-Squared zeigt an, wie viel der beobachteten Varianz durch das Regressionsmodell erklärt wird 
# Ein wert von 0.74 bedeuete, dass 74% der Varianz durch das Regeressionsmodell erklärt werden kann 
# Dies bedeuet, dass das Modell 26% der Varianz im Datensatz nicht erklären kann. Dies ist also kein extrem schlechter Wert, aber auch kein guter Wert
# Dies geschieht größtenteils durch die Angepasstheit des Modells an die Daten. Im fall des OVerfitting (Variance muss reduziert werden, weil sich das Modell zu sehr an das zufällige Rauschen anpasst), passt sich das Modell zu sehr an
# Wenn es Probleme mit dem Bias gibt (Underfitting), passt sich das Modell nicht genug an die daten an 

# Over-/Underfitting 
plot(BostonHousing$medv)
abline(reg= lm.fit.all, col ="blue")

# Im Plot erkennt man eindeutig, dass eine lineares Modell nicht flexibel genug ist, um die Daten zu erklären.
# Deshalb liegt hier ein Fall von Underfitting vor. 
# Das ist hier der Fall, da der Zusammenhang, wie man auf dem Plot sehen kann, nicht linear ist, aber mit einem linearen Modell
# erklärt werden soll. Das PRoblem liegt hierbei somit nicht in einer großen Varianz, da sich das Modell auf Grund der geringen flexibilität nicht gut an die Trainigsdaten anpassen kann 
# Im Fall von einem Variance Problem (overfitting), würde man außerdem erwarten, dass der Testfehler signifikant größer als der Trainingsfehler ist. 

#------------------------------------
# Aufgabe 4: Ridge Regression
#------------------------------------

library(glmnet)
set.seed(seed)
# Liste von Lambda-Werten, die das Modell ausprobieren soll (aus Aufgabenstellung)
lambda <- 10^seq( from = 5, to = -3, length = 100)

#Trainingsdatenmatrizen für die unabhängigen (x) und abhängigen Variablen (y) erstellen
BostonHousing.trainingsdaten.x <- data.matrix(subset(BostonHousing.trainingsdaten, select = -c(medv) ))
BostonHousing.trainingsdaten.y <- BostonHousing.trainingsdaten[, "medv"]

# Passende Daten für den Test zur Bestimmung des Testfehlers
BostonHousing.testdaten.x <- data.matrix(subset(BostonHousing.testdaten, select = -c(medv) ))
BostonHousing.testdaten.y <- BostonHousing.testdaten[, "medv"]

# Ridge Regressionsmodell
ridge.fit.cv <- cv.glmnet(x= BostonHousing.trainingsdaten.x,y= BostonHousing.trainingsdaten$medv,alpha = 0, lambda =lambda)

# Optimales Lambda finden
bestes_lambda.ridge <- ridge.fit.cv$lambda.min

# Modell mit optimalem Lambda
ridge.fit <- glmnet(x= BostonHousing.trainingsdaten.x,y= BostonHousing.trainingsdaten$medv,alpha = 0, lambda =bestes_lambda.ridge)

# Testfehler
test.mqa.ridge<- mean(
  (BostonHousing.testdaten.y - predict(ridge.fit, BostonHousing.testdaten.x))^2
)
test.mqa.ridge

#------------------------------------
# Aufgabe 5: Lasso Regression
#------------------------------------

# Lasso Regressionsmodell
lasso.fit.cv <- cv.glmnet(x= BostonHousing.trainingsdaten.x,y= BostonHousing.trainingsdaten$medv,alpha = 1, lambda =lambda)

# Optimales Lambda finden
bestes_lambda.lasso <- lasso.fit.cv$lambda.min

# Modell mit optimalem Lambda
lasso.fit <- glmnet(x= BostonHousing.trainingsdaten.x,y= BostonHousing.trainingsdaten$medv,alpha = 1, lambda =bestes_lambda.lasso)

# Testfehler
test.mqa.lasso<- mean(
  (BostonHousing.testdaten.y - predict(lasso.fit, BostonHousing.testdaten.x))^2
)
test.mqa.lasso

# Feature Selection
coef(lasso.fit)

# Ergebnis:
# 14 x 1 sparse Matrix of class "dgCMatrix"
# s0
# (Intercept)  34.644681413
# crim         -0.130062865
# zn            0.044980411
# indus         0.024424487
# chas          2.361136092
# nox         -17.032723292
# rm            3.699164991
# age           0.010078506
# dis          -1.380068472
# rad           0.329302198
# tax          -0.012334912
# ptratio      -0.984386569
# b             0.008114339
# lstat        -0.547459892

# Keine Variable wird auf Null geschätzt
# Im lasso wird dies jedoch bei anderen Seed(z.B. seed=18, bei dem indus und age auf null geschätzt werden)  ein Punkt angezeigt, welcher für eine null schätzung  steht.
# Im ridge werden die werte zwa r sehr klein, aber (praktisch) nie null

#------------------------------------
# Aufgabe 6: Vergleich der Ergebnisse
#------------------------------------

test.mqa.lm
test.mqa.lasso
test.mqa.ridge

#Berechnugn der Varianz 
testfehler <- c(test.mqa.lasso, test.mqa.lm, test.mqa.ridge)
var(testfehler)
# Wert = [1] 0.014206383
# Die Varianz ist ein Weg, um zu messen, wie verteilt die Datenwerte um den Mittelwert liegen.

# der sehr geringe Wert der Varianz zeigt, dass die Regularisierung zu keinem Großem Unterschied im Testfehler geführt hat
#, der Testfehler aber bei den Regularisierten Modellen bei diesem seed leicht kleiner war(in anderen seeds varriert dies jedoch)
# Dies lässt sich dadurch begründen, dass die Regularisierung Overfitting (anhand des Strafterms Lambda) bestrafen soll.
# Damit reduziert die Regularisierung die Varianz, aber nicht die Bias
# Durch das lineare Modell liegt hier allerdings Underfitting vor, auf Grund der hohen Bias der linearen Regression, da das Modell nicht die nötige Flexibilität hat, um sich an den Datensatz anzupassen
# (siehe Aufgabe 3), weshalb Regularisierungsmethoden zur Vermeidung der Überanpassung
# an dieser Stelle keinen Sinn ergeben und auch den Testfehler somit nicht reduzieren können. 
