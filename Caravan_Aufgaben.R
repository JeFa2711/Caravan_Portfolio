#
# Template für die Bearbeitung der Übungsaufgabe 5
#        -- Analyse des Caravan-Datensatz -- 
#

# Gruppenname: Anakin
# Gruppenteilnehmer: Nina Heyer, Jonas Lang, Simon Baader, Jessica Fander



#-------------------------
# notwendinge Bibliotheken
#-------------------------
#Datensatz
library(ISLR) 
library(class)

## Übersicht
summary(Caravan)
plot(Caravan$Purchase) # Plotten von Yes und No 
anzahlYes <- nrow(Caravan[Caravan$Purchase == "Yes",])
anzahlYes # 348
# Anzahl No: 5474
# 348 von 5822 haben eine Versicherung (ca. 6%)

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

set.seed(28) # Für bessere Nachvollziehbarkeit wird ein Seed gesetzt.
laenge <- length(Caravan$Purchase) # Anzahl der Datensätze
anzahlSpalten <- length(Caravan) # Anzahl der Spalten
aufteilung <- sample(laenge,0.8*laenge) # Aufteilung für Test und Trainingsdaten
Caravan.trainingsdaten <- Caravan[aufteilung,] # Trainingsdaten
Caravan.testdaten <- Caravan[-aufteilung,] # Testdaten

#------------------------------------
# Aufgabe 3: Logistische Regression
#------------------------------------
# Logistische Regression
glm.fit <- glm(
  formula = Purchase ~ . ,
  family = binomial,
  data = Caravan.trainingsdaten
)

glm.fit # zur Ausgabe

vorhergeseheneWerte <- predict( 
  object  = glm.fit, 
  newdata = Caravan.testdaten, 
  type    = "response"
)

# Aus diesen vorhergesehenen Werten, werden nun Cutoff Points definiert. Um sinnvolle Punkte festzustellen,
# werden die Vorhergesagten Werte (transparent) geplottet, damit die Verteilung festgestellt werden kann.

plot(vorhergeseheneWerte, col = adjustcolor("blue", alpha = 0.35))

# Aus diesen Werten wurden nun,nach dem eigenem Ermessen der Gruppenmitglieder Cutoff Points definiert.
# Der Fokus liegt hierbei im Bereich 0-01-0.1, da der Plot aufzeigt, dass hier die meisten Werte zu finden sind.

t <- c(0.01,0.05,0.07, 0.1, 0.2, 0.3)
wahrheitsMatrix <- list()

for ( i in 1:length(t)) {
  test.vorhersage <- ifelse(vorhergeseheneWerte > t[i], "Yes", "No") 
  wahrheitsMatrix[[i]] <- table(test.vorhersage,Caravan.testdaten$Purchase)
  
  print(wahrheitsMatrix[[i]])
}

# t = 0.01: 69 von 1017 (ca. 6%) Yes werden auch als Yes erkannt und alle (100%) 148 No's werden auch korrekt als No erkannt. (Keine False Positives) 
# t = 0.05: 54 von 459 (ca. 11%) Yes werden auch als Yes erkannt und 691 von 706 werden als korrekt negativ erkannt. (15 False Positives)
# t = 0.07: 46 von 332 (ca. 13,8%) werden als korrekt positiv erkannt. 23 False Positives. 
# t = 0.1 : 35 von 210 (ca. 16,6%) werden als korrekt positiv erkannt. 34 False Positives.
# t = 0.2 : 16 von 40 (ca. 40%) werden als korrekt positiv erkannt. 53 False Positives.
# t = 0.3 : 3 von 14 (ca. 21%) werden als korrekt positiv erkannt. 66 False Positives.

#------------------------------------
# Aufgabe 4: KNN
#------------------------------------

# Um den Wert skalieren zu könnnen, muss ein Datensatz verwendet werden, aus dem die nicht numerische Purchase Output variable entfernt wurde
Caravan.scaled = subset(Caravan, select = -c(Purchase) )
scale(Caravan.scaled)

#Für die Verwendung in KNN, müssen die Testdaten in data.frames umgewandelt werden.
Caravan.scaled.trainingsdaten <- as.data.frame(Caravan.scaled[aufteilung,])
Caravan.scaled.testdaten <- as.data.frame(Caravan.scaled[-aufteilung,])

#Abspeichern der purchase Variablen, die aus Caravan.scaled entfernt wurden in einer eigenen Variable für die spätere Verwendung
Caravan.scaled.trainingsdaten.output <- Caravan[aufteilung, "Purchase"]
Caravan.scaled.testdaten.output <- Caravan[-aufteilung, "Purchase"]
set.seed(28)

#Fertige Funktion
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

# Zu verwendene K-Werte , Werte höher als 7 werden nicht verwendet
kWerte <- c(1,2,3,4,5,6)
knnWahrheitsmatrixListe <- knnListeBerechnen(kWerte)

#------------------------------------
# Aufgabe 5: Modellauswahl
#------------------------------------

# Ziel laut Aufgabenstellung ist es, möglichst wenige False Positives zu erhalten,
# um die Arbeitszeit nicht zu verschwenden. Andererseits soll es auch möglichst viele Personen erkennen,
# die Interesse an einer Police haben, um diese zu verkaufen.

#Funktion zur Berechnung des F1-Scores. Sie greift für eine bessere Übersichtlichkeit auf Methoden zur Berechnung des Recalls und der Precision zu

errechneF1score <- function(matrix){
  recall <- errechneRecall(matrix= matrix)
  precision <- errechnePrecision(matrix=matrix)
  return ( 2*(recall*precision)/(recall+precision))
}

#Für die werte werden die Positionen innerhalb der Wahrheitsmatrix verwendet
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
# Funktioniert nur für Werte bis 6 bei KNN
bestimmeBestenf1Wert <- function(matrixListe){
  f1Werte <- vector() # Vektor, in dem alle Werte gespeichert werden
  
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
# Beide Ergebnisse (Logistische Reg. und KNN werden anschließend zusammengeführt)
kombinierteListe <- append(wahrheitsMatrix, knnWahrheitsmatrixListe)
bestimmeBestenf1Wert(kombinierteListe)

# Die Ergebnisse: 
# Durch die Funktion bestimmeBestenf1Wert, wird das Ergebnis ausgegeben 

# Der Maximale Wert ist: 0.256
# Das  Modell mit dem besten F1 Wert ist:
# test.vorhersage   
#                   No     Yes
#           No      1056   53
#           Yes     40     16
# Der index des maximalen Wertes in der kombinierten Liste ist 5 
# Damit lässt sich oben nachlesen, dass es handelt es sich um ein Modell der logistischen Regression mit t = 0.2



