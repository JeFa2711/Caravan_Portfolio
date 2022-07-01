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

seed <- seed # Damit alle Seed-Funktionen den gleichen Seed haben.
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

#Testfehler
test.mqa.lm <- mqaTest(lm.fit.all)
test.mqa.lm 


#------------------------------------
# Aufgabe 3: Bias-Variance
#------------------------------------

# Over-/Underfitting 
plot(BostonHousing$medv)
abline(reg= lm.fit.all, col ="blue")

# Im Plot erkennt man eindeutig, dass eine lineares Modell die Daten nicht erklären kann.
# Deshalb liegt hier ein Fall von Underfitting vor. 

# Der Bias ist hoch, wenn ein Modell zu allgemein ist, um genaue Prognosen zu treffen. 
# Das ist hier der Fall, da der Zusammenhang, wie man auf dem Plot sehen kann, nicht linear ist, aber mit einem linearen Modell
# erklärt werden soll. Die Varianz ist niedrig. 

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
#14 x 1 sparse Matrix of class "dgCMatrix"
#s0
#(Intercept)  32.739054210
#crim         -0.107451038
#zn            0.035628104
#indus         .          
#chas          2.529593970
#nox         -17.426837901
#rm            3.940486249
# age           .          
# dis          -1.436922259
# rad           0.292110037
# tax          -0.009911046
# ptratio      -0.993739448
# b             0.010684410
# lstat        -0.533948281

# Die "indus" und "age" Variablen werden im finalen Modell mit Null geschätzt, 
# Deshalb wird statt einer Zahl (die im Ridge zwar klein, aber nicht direkt 0 wird) ein Punkt angezeigt, welcher für 0 steht.

#------------------------------------
# Aufgabe 6: Vergleich der Ergebnisse
#------------------------------------

test.mqa.lm
test.mqa.lasso
test.mqa.ridge

# Der Testfehler ist bei allen vergleichbar, weil die Regularisierung Overfitting (anhand des Strafterms Lambda) bestrafen soll.
# Durch das lineare Modell liegt hier allerdings Underfitting vor (siehe Aufgabe 3), weshalb Regularisierungsmethoden
# an dieser Stelle keinen Sinn ergeben und auch den Testfehler somit nicht reduzieren. 
