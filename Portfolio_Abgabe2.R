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
# Nina Heyer, 
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

#    ...Platz für Ihren Code...
seed <- seed #damit alle seed funktionen den gleichen seed haben
set.seed(seed) # Für bessere Nachvollziehbarkeit wird ein Seed gesetzt.
laenge <- length(BostonHousing$crim) # Anzahl der Datensätze
aufteilung <- sample(laenge,0.8*laenge) # Aufteilung für Test und Trainingsdaten
BostonHousing.trainingsdaten <- BostonHousing[aufteilung,] # Trainingsdaten
BostonHousing.testdaten<- BostonHousing[-aufteilung,] # Testdaten


#------------------------------------
# Aufgabe 2: Lineare Regression
#------------------------------------
lm.fit.all <- lm(
  formula = medv ~ .,
  data = BostonHousing.trainingsdaten
)

mqaTraining <- function(modell){
  return( 
    mean(
      (BostonHousing.trainingsdaten$medv - predict(modell,BostonHousing.trainingsdaten ))^2)
    )
}

mqaTest <- function(modell){
  return( 
    mean(
      (BostonHousing.testdaten$medv - predict(modell,BostonHousing.testdaten ))^2)
  )
}


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

#over/underfitting 
plot(BostonHousing$medv)
abline(reg= lm.fit.all, col ="blue")

#    ...Platz für Ihre Begründung...

#TODO TEXTT_ wie im plot erkenntlich, eine gerade kann es nicht wiederspiegeln, deswegen ein offensichtlicher Fall des underfitting
#Hier viel besser


#------------------------------------
# Aufgabe 4: Ridge Regression
#------------------------------------

#    ...Platz für Ihren Code...
# Kommentar: ich habe ehrlich gesagt keine Ahnung ob das richtig ist 
library(glmnet)
set.seed(seed)
lambda <- 10^seq( from = 5, to = -3, length = 100)

#TODO Beschreibung
BostonHousing.trainingsdaten.x <- data.matrix(subset(BostonHousing.trainingsdaten, select = -c(medv) ))
BostonHousing.trainingsdaten.y <- BostonHousing.trainingsdaten[, "medv"]

#passende Daten für den Test zut bestimmung des Testfehlers
BostonHousing.testdaten.x <- data.matrix(subset(BostonHousing.testdaten, select = -c(medv) ))
BostonHousing.testdaten.y <- BostonHousing.testdaten[, "medv"]

ridge.fit.cv <- cv.glmnet(x= BostonHousing.trainingsdaten.x,y= BostonHousing.trainingsdaten$medv,alpha = 0, lambda =lambda)
bestes_lambda.ridge <- ridge.fit.cv$lambda.min
ridge.fit <- glmnet(x= BostonHousing.trainingsdaten.x,y= BostonHousing.trainingsdaten$medv,alpha = 0, lambda =bestes_lambda.ridge)


test.mqa.ridge<- mean(
  (BostonHousing.testdaten.y - predict(ridge.fit, BostonHousing.testdaten.x))^2
)
test.mqa.ridge
#------------------------------------
# Aufgabe 5: Lasso Regression
#------------------------------------

#    ...Platz für Ihren Code...
lasso.fit.cv <- cv.glmnet(x= BostonHousing.trainingsdaten.x,y= BostonHousing.trainingsdaten$medv,alpha = 1, lambda =lambda)
bestes_lambda.lasso <- lasso.fit.cv$lambda.min
lasso.fit <- glmnet(x= BostonHousing.trainingsdaten.x,y= BostonHousing.trainingsdaten$medv,alpha = 1, lambda =bestes_lambda.lasso)



test.mqa.lasso<- mean(
  (BostonHousing.testdaten.y - predict(lasso.fit, BostonHousing.testdaten.x))^2
)

test.mqa.lasso

#Feature_selection
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

#Die indus und age Variablen werden im Finalen Modell mit Null geschätzt, 
#deshalb wird statt einer Zahl ( die im ridge zwar klein aber nicht direkt 0 wird) ein punkt angezeigt
#==> steht für null

#------------------------------------
# Aufgabe 6: Vergleich der Ergebnisse
#------------------------------------
<<<<<<< Updated upstream
test.mqa.lm
#Ergebnis: [1] 28.45997
test.mqa.lasso
#Ergebnis: [1] 28.50109
test.mqa.ridge
#Ergebnis: 1] 28.66372

#  Der Testfehler ist bei allen vergleichbar (sogar leicht höher als in der Lineraren Regression), weil die Regualrisierung 
# Regularisierung bestraft overfitting mit dem Strafterm lambda, um so die Varianz zu refuzieren. Da hier jedoch underfitting vorliegt, keine Positiven aspekte 
=======

#    ...Platz für Ihren Code und Begründung...

>>>>>>> Stashed changes
