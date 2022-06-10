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


#------------------------------------
# Aufgabe 2: Trainings- und Testdaten
#------------------------------------
# Der Caravan Datensatz hat 5822 Datensätze und 86 Attribute
# 80% (Trainingsdaten) von 5822 sind ~4658 
# Entsprechend dazu 1164 Datensätze für die Testdaten 

set.seed(28)
laenge <- nrow(Caravan)
aufteilung <- sample(laenge,0.8*laenge)
trainingsdaten <- Caravan[aufteilung,]
testdaten <- Caravan[-aufteilung,]

#------------------------------------
# Aufgabe 3: Logistische Regression
#------------------------------------

#    ...Platz für Ihren Code...

#------------------------------------
# Aufgabe 4: KNN
#------------------------------------

#    ...Platz für Ihren Code...

#------------------------------------
# Aufgabe 5: Modellauswahl
#------------------------------------

#    ...Platz für Ihren Code, Auswahl und Begründung...


