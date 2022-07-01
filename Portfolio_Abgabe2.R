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
# Jonas Lang, 
# Simon Baader, 
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
set.seed(28) # Für bessere Nachvollziehbarkeit wird ein Seed gesetzt.
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
lambda <- 10^seq( from = 5, to = -3, length = 100)

BostonHousing.trainingsdaten.x <- data.matrix(subset(BostonHousing.trainingsdaten, select = -c(medv) ))
#y
BostonHousing.trainingsdaten.y <- BostonHousing.trainingsdaten[, "medv"]

ridge.fit <- cv.glmnet(x= BostonHousing.trainingsdaten.x,y= BostonHousing.trainingsdaten$medv,alpha = 0, lambda =lambda)

#training.mqa.ridge <- mqaTraining(ridge.fit)
#training.mqa.ridge

#test.mqa.ridge <- mqaTest(ridge.fit)
#test.mqa.ridge

mean(
  (BostonHousing.trainingsdaten.y - predict(ridge.fit, BostonHousing.trainingsdaten.x))^2
  )
#------------------------------------
# Aufgabe 5: Lasso Regression
#------------------------------------

#    ...Platz für Ihren Code...


#------------------------------------
# Aufgabe 6: Vergleich der Ergebnisse
#------------------------------------

#    ...Platz für Ihren Code und Begründung...