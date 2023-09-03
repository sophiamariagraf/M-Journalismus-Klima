#Deskriptive Statistik zu den Kommentaren
library('dplyr')
library('psych')

dataCom <- read.csv2("Journalismus&Klima_Kommentare_Mastersheet.csv")
summary(dataCom)
describe(dataCom)

##Anzahl_Kommentare (SD, Schiefe, AM, Histogramm, Boxplott)
### Mean: 281.49 
### SD: 208.17 
### min: 2 max: 719



## Problematisierung
### zur Kontrolle: --> alle (415) mit Problematisierung des Klimawandels
table(dataCom$Problematisierung)

## Lösungsansätze             16 414      3.36    1.09      4      3.57   0.00      1      4
ComLösung <- table(dataCom$Lösungsansätze)
print(ComLösung)
rel_ComLösung <- round(prop.table(ComLösung), 3)
print(rel_ComLösung)
### A:   1: 0.12   2: 0.11   3: 0.05    4:0.72

## Haltung_Lösungsansätze
ComHaltungLösung <- table(dataCom$Haltung_Lösungsansätze)
rel_ComHaltungLösung <- round(prop.table(ComHaltungLösung), 3)
print(rel_ComHaltungLösung)

Haltung_Lösung <- cbind(ComHaltungLösung, rel_ComHaltungLösung)

## Haltung_Klimakrise
ComHaltungKlima <- table(dataCom$Haltung_Klimakrise)
rel_ComHaltungKlima <- round(prop.table(ComHaltungKlima), 3)
print(rel_ComHaltungKlima)

Haltung_Klima <- cbind(ComHaltungKlima, rel_ComHaltungKlima)
round(Adressat_in, digits = 3)

## Adressat_in
ComAdress <- table(dataCom$Adressat_in)
print(ComAdress)
rel_ComAdress <- round(prop.table(ComAdress), 3)
print(rel_ComAdress)

Adressat_in <- cbind(ComAdress, rel_ComAdress)
round(Adressat_in, digits = 3)


### Kreisdiagramm (schön machen lohnt nur, wenn wir es nutzen wollen)
install.packages("ggplot2")
library("ggplot2")
pie(rel_ComAdress, labels = )

## Lob
ComLob <- table(dataCom$Lob)
print(ComLob)
rel_ComLob <- round(prop.table(ComLob), 3)
print(rel_ComLob)

Lob <- cbind(ComLob, rel_ComLob)

## Kritik
ComKritik <- table(dataCom$Kritik)
print(ComKritik)
rel_ComKritik <- round(prop.table(ComKritik), 3)
print(rel_ComKritik)

Kritik <- cbind(ComKritik, rel_ComKritik)

## Beleidigung_Inzivilität
ComBelInz <- table(dataCom$Beleidigung_Inzivilität)
print(ComBelInz)
rel_ComBelInz <- round(prop.table(ComBelInz), 3)
print(rel_ComBelInz)

Beleidigung_Inzivilität <- cbind(ComBelInz, rel_ComBelInz)

# Kreuztabellen

xtabLös <- table(dataCom$Lösungsvorschlag, dataCom$Lösungsansätze)
print(xtabLös)
addmargins(round(prop.table(xtabLös, margin = 1), digits = 3))



