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
laenge <- length(Caravan$Purchase)
aufteilung <- sample(laenge,0.8*laenge)
Caravan.trainingsdaten <- Caravan[aufteilung,]
Caravan.testdaten <- Caravan[-aufteilung,]

#------------------------------------
# Aufgabe 3: Logistische Regression
#------------------------------------

#    ...Platz für Ihren Code...
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
#Aus diesen vorhergesehenen Werten, werden nun cutoffPoints definiert. Um sinnvolle Punkte festzustellen,
# werden die Vorhergesagten Werte (transparent) geplottet, um ihre Verteilung festzustellen

plot(vorhergeseheneWerte, col = adjustcolor("blue", alpha = 0.35))

# Aus diesen Werten wurden nun,nach dem eigenem ermessen der Mitarbeiter Cut-Off points definiert 
# Der Fokus liegt hierbei im Bereich 0-01-0.1, da der Plot aufzeigt, dass hier die meisten Werte zu finden sind 
t <- c(0.01,0.05,0.07,0.1, 0.2)
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

#Für die Verwendung in KNN, müssen die Testdaten in data.frames umgewandelt werden 
Caravan.scaled = subset(Caravan, select = -c(Purchase) )

scale(Caravan.scaled)
Caravan.scaled.trainingsdaten <- as.data.frame(Caravan.scaled[aufteilung,])
Caravan.scaled.testdaten <- as.data.frame(Caravan.scaled[-aufteilung,])
Caravan.scaled.trainingsdaten.output <- Caravan[aufteilung, "Purchase"]
Caravan.scaled.testdaten.output <- Caravan[-aufteilung, "Purchase"]
set.seed(28)
# K = 3
knn.pred.k3 <- knn(
  train = Caravan.scaled.trainingsdaten,         ## Input-Variablen der Trainingsdaten      
  test  = Caravan.scaled.testdaten,          ## Input-Variablen der Testdaten
  cl    = Caravan.scaled.trainingsdaten.output, ## Class-Label der Trainingsdaten
  k     = 3,
  prob = FALSE
) 

confusion.table.k3 <- table(knn.pred.k3,Caravan.scaled.testdaten.output)

#    ...Platz für Ihren Code...

#------------------------------------
# Aufgabe 5: Modellauswahl
#------------------------------------

#    ...Platz für Ihren Code, Auswahl und Begründung...


