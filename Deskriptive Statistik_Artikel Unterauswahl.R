# Deskriptive Statistik zur Unterauswahl
library('dplyr')
library('psych')

DataArtUA <- read.csv2("Journalismus&Klima_Artikel_Unterauswahl.csv")
summary(DataArtUA)
describe(DataArtUA)

## Histogramm Datum
### Häufigkeit

ArtUADatum <- table(DataArtUA$Datum)
print(ArtUADatum) ## --> Zwei Mal 2 Artikel, am 09.07.22 & 06.11.22

###(Skalierung müsste bearbeitet werden)
barplot(table(DataArtUA$Datum),
        xlab = "Datum", ylab = "Anzahl Artikel",
        main = "Verteilung der Unterauswahl im Erhebungszeitraum",
        cex.axis = 1, cex.names = 1,
        col = "lightblue",
        las = 2)

## Anzahl Kommentare
AnzahlKomm <- describe(DataArtUA$Anzahl_Kommentare)


## Lösungsvorschlag
Lös <- table(DataArtUA$Lösungsvorschlag)
print(Lös)

## Anzahl Lösungsvorschläge
AnzahlLös <- describe(DataArtUA$Anzahl_Lösungsvorschläge)
  
table(DataArtUA$Anzahl_Lösungsvorschläge)

## Ausführlichkeit I
AusführlI <- table(DataArtUA$Ausführlichkeit_I)
print(AusführlI)

## Ausführlichkeit II
### Häufigkeiten
UAAusführlII <- table(DataArtUA$S_II_Ausführlichkeit)
print (UAAusführlII)
AusführlichkeitII <- cbind(UAAusführlII)

## Konstruktive Wortwahl
KonstrukWoWa <- table(DataArtUA$Konstruktive.Wortwahl)
print(KonstrukWoWa)

## Mitigation/ Adaption
### Häufigkeiten
UAMitAd <- table(DataArtUA$S_Mitigation.Adaption)
print(UAMitAd)

rel_UAMitAd <- round(prop.table(UAMitAd), digits = 3)
print(rel_UAMitAd)

Mitigation_Adaption <- cbind(UAMitAd, rel_UAMitAd)

## Bürger:innen
### Häufigkeiten
UABürg <- table(DataArtUA$S_Bürger.innen)
print (UABürg)


