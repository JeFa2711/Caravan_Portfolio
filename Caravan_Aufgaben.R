#
# Template für die Bearbeitung der Übungsaufgabe 5
#        -- Analyse des Caravan-Datensatz -- 
#

# Gruppenname: Anakin
# Gruppenteilnehmer: Nina Heyer, Jonas Lang, Simon Baader, Jessica Fander



#-------------------------
# notwendinge Bibliotheken
#-------------------------
library(ISLR) #Datensatz
## Übersicht
summary(Caravan)
plot(Caravan$Purchase)

#------------------------------------
# Aufgabe 1: Evaluierungsmetrik
#------------------------------------
# In der Aufgabenstellung ist beschrieben, dass die Faktoren Recall 
# (tats. interessierten Kunden) und Precision (Minimierung der False Positives) für den 
# Sachverhalt relevant sind.
# Aus der gegebenen Aufgabe lässt sich das Kostenverhältnis zwischen nicht-identifizierten 
# Kunden und verschwendeter Zeit der Mitarbeiter in uninteressierte Kunden nicht schließen. 
# Daher wird als Evaluierungsmetrik keine Kostenfunktion, sondern die F1-Metrik als 
# harmonisches Mittel von Recall und Precision verwendet, um den optimalsten Mittelwert 
# zwischen beiden Metriken festzustellen.
# Mit konkreteren Informationen zum gegebenen Sachverhalt könnte sich eine sinnvolle 
# Kostenfunktion ableiten lassen, mit der sich die Effiktivität der Versicherung 
# (besser als mit der F1-Metrik) optimieren ließe.

#------------------------------------
# Aufgabe 2: Trainings- und Testdaten
#------------------------------------
# Der Caravan Datensatz hat 5822 Datensätze und 86 Attribute
# 80% (Trainingsdaten) von 5822 sind ~4658 
# Entsprechend dazu 1164 Datensätze für die Testdaten 

set.seed(28)
laenge <- length(Caravan$Purchase)
anzahlSpalten <- length(Caravan)
aufteilung <- sample(laenge,0.8*laenge)
Caravan.trainingsdaten <- Caravan[aufteilung,]
Caravan.testdaten <- Caravan[-aufteilung,]

#------------------------------------
# Aufgabe 3: Logistische Regression
#------------------------------------
# Logistische Regression
glm.fit <- glm(
  formula = Purchase ~ . ,
  family = binomial,
  data = Caravan.trainingsdaten
)

glm.fit

vorhergeseheneWerte <- predict( 
  object  = glm.fit, 
  newdata = Caravan.testdaten, 
  type    = "response"
)

# Aus diesen vorhergesehenen Werten, werden nun cutoffPoints definiert. Um sinnvolle Punkte festzustellen,
# werden die Vorhergesagten Werte (transparent) geplottet, um ihre Verteilung festzustellen

plot(vorhergeseheneWerte, col = adjustcolor("blue", alpha = 0.35))

# Aus diesen Werten wurden nun,nach dem eigenem ermessen der Mitarbeiter Cut-Off points definiert 
# Der Fokus liegt hierbei im Bereich 0-01-0.1, da der Plot aufzeigt, dass hier die meisten Werte zu finden sind.

t <- c(0.01,0.05,0.07, 0.1, 0.2, 0.3)
wahrheitsMatrix <- list()

for ( i in 1:length(t)) {
  test.vorhersage <- ifelse(vorhergeseheneWerte > t[i], "Yes", "No") 
  wahrheitsMatrix[[i]] <- table(test.vorhersage,Caravan.testdaten$Purchase)
  
  print(wahrheitsMatrix[[i]])
}


#------------------------------------
# Aufgabe 4: KNN
#------------------------------------
library(class)

#Für die Verwendung in KNN, müssen die Testdaten in data.frames umgewandelt werden. 
Caravan.scaled = subset(Caravan, select = -c(Purchase) )

scale(Caravan.scaled)
Caravan.scaled.trainingsdaten <- as.data.frame(Caravan.scaled[aufteilung,])
Caravan.scaled.testdaten <- as.data.frame(Caravan.scaled[-aufteilung,])
Caravan.scaled.trainingsdaten.output <- Caravan[aufteilung, "Purchase"]
Caravan.scaled.testdaten.output <- Caravan[-aufteilung, "Purchase"]
set.seed(28)



#Fertige funktion
knnBerechnen <- function(k){
  return( knn(
    train = Caravan.scaled.trainingsdaten,         ## Input-Variablen der Trainingsdaten      
    test  = Caravan.scaled.testdaten,              ## Input-Variablen der Testdaten
    cl    = Caravan.scaled.trainingsdaten.output,  ## Class-Label der Trainingsdaten
    k     = k,
    prob = FALSE
  ) )
}

knnListeBerechnen <- function(vektor){
  
  knnModellListe <- list()
  knnWahrheitsmatrixListe <- list()
  for(i in 1:length(vektor)){
    knnModellListe[[i]] <- knnBerechnen(vektor[i])
  }
  
  for(i in 1:length(knnModellListe)){
    knnWahrheitsmatrixListe[[i]] <- table(knnModellListe[[i]],Caravan.scaled.testdaten.output)
  }
  return(knnWahrheitsmatrixListe)
}

#zu verwendene K werte , WErte höher als 7 können nicht verewndet
kWerte <- c(1,2,3,4,5,6)
knnWahrheitsmatrixListe <- knnListeBerechnen(kWerte)

#------------------------------------
# Aufgabe 5: Modellauswahl
#------------------------------------

# Ziel laut Aufgabenstellung ist es, möglichst wenige false positives zu erhalten,
# um die Arbeitszeit nicht zu verschwenden. Andererseits soll es auch möglichst viele Personen erkennen,
# die Interesse an einer Police haben, um diese zu verkaufen.
# Zusammenfassend die Übersicht: 
# Logistische Regression: 

# Jonas's Backlog
precision <-  confusion.table.k4[[4]]/(confusion.table.k4[[4]] + confusion.table.k4[[3]])
recall <- confusion.table.k4[[4]]/(confusion.table.k4[[4]]+confusion.table.k4[[2]])
f1.score <- 2*(recall*precision)/(recall+precision)

errechneF1score <- function(matrix){
  recall <- errechneRecall(matrix= matrix)
  precision <- errechnePrecision(matrix=matrix)
  return ( 2*(recall*precision)/(recall+precision))
}

errechnePrecision <- function(matrix){
  return(
    matrix[[4]]/(matrix[[4]] + matrix[[3]])
  )
}

errechneRecall <- function(matrix){
  return(
    matrix[[4]]/(matrix[[4]] + matrix[[2]])
  )
}
#funktioniert nur für Werte bis 6 bei Knn
bestimmeBestenf1Wert <- function(matrixListe){
  f1Werte <- vector()# Vektor, in dem alle Werte gespeichert werden
  
  for ( i in 1:length(matrixListe) ){
    f1Werte <- append(f1Werte,  errechneF1score(matrixListe[[i]]))
  }
  for(i in 1:length(f1Werte)){
    if(f1Werte[i]==   max(f1Werte)){
      cat("Der Index des Maximalen Wertes ist:")
      print(i)
      cat("Der Maximale Wert ist: ")
      print(max(f1Werte))
      print("Das  Modell mit dem besten F1 Wert ist")
      
      return(matrixListe[[i]])
    }
  }
  
}
#Möglichkeit wenn ich Lust habe: automatisch bestimmen, zu wem der zwei es gehört 
# Verbinden beider Listen 
kombinierteListe <- append(wahrheitsMatrix, knnWahrheitsmatrixListe)
bestimmeBestenf1Wert(kombinierteListe)
#TODO ERgebnisse hier in TExt kopieren
#Das BEste Ergebnis ist 0.2 bei erstem X-........



