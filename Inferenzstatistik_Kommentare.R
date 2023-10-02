# Inferenzstatistik Kommentare

install.packages("rcompanion")
library("rcompanion")
install.packages("esc")
library("esc")

# H1: Die konstruktiven Elemente in den Artikeln haben keine Auswirkung 
#### auf die Anzahl der Kommentare der Nutzer:innen.

# Voraussetzung t-Test: Shapiro-Wilk-Test

shapiro.test(Journalismus_Klima_Codingsheet_Artikel$`Anzahl Kommentare`)
shapiro.test(Journalismus_Klima_Codingsheet_Unterauswahl_Artikel$`Anzahl Kommentare`)

# Ergebnisse nicht signifikant -> Nullhypothese muss beibehalten werden -> keine Normalverteilung gegeben

# Durchführung Mann-Whitney-U-Test

wilcox.test(Journalismus_Klima_Codingsheet_Artikel$`Anzahl Kommentare`~Journalismus_Klima_Codingsheet_Artikel$Lösungsvorschlag)
wilcox.test(Journalismus_Klima_Artikel_Unterauswahl$Anzahl_Kommentare~Journalismus_Klima_Artikel_Unterauswahl$Lösungsvorschlag)

# Ergebnisse nicht signifikant -> Nullhypothese kann beibehalten werden -> Hypothese kann angenommen werden

_________

# Metrische Daten
table(DataArtUA$Anzahl_Kommentare)

# Normaverteilung ## -> nicht gegeben?
x = DataArtUA
x2 <- seq(min(x), max(x), length = 10)
fun <- dnorm(x2, mean = mean(x), sd = sd(x))

hist(x, prob = TRUE,
     main = "Histogramm Social Media Nutzung", 
     xlab = "Social Media Nutzung in Minuten", ylab = "Density",
     col = "white", 
     breaks = 15, xaxt='n')
lines(x2, fun, col = 2, lwd = 2) 

shapiro.test(x) 

# Verianzhomogenität

## Nichtparametrischer Test für unverbundene Strichproben
library("psych")
wilcox.test(DataArtUA$Anzahl_Kommentare~DataArtUA$Lösungsvorschlag) 

### Wilcoxon rank sum test with continuity correction
### data:  DataArtUA$Anzahl_Kommentare by DataArtUA$Lösungsvorschlag
### W = 54, p-value = 0.1291
### alternative hypothesis: true location shift is not equal to 0

# Warning message:
## In wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...) :
## cannot compute exact p-value with ties

_____________


# H2: Die konstruktiven Elemente in den Artikeln sorgen dafür, dass 
#### Lösungsansätze in den Kommentaren häufiger thematisiert werden.

xtabLös <- xtabs(~ dataCom$Lösungsvorschlag + dataCom$Lösungsansätze)
print(KreuztabLös)

xtabLös <- table(dataCom$Lösungsvorschlag, dataCom$Lösungsansätze)
addmargins(xtabLös)

## Dichotome Variable Ja/ Nein

library("dplyr")
install.packages("sjmisc")
library("sjmisc")

dataCom <- dataCom |>
  mutate(Lösungsansätze_JN = rec(Lösungsansätze,
                                 rec = "1,2,3 = 1 [JA]; 4 = 2 [NEIN]"))
dataCom |> sjmisc::frq(Lösungsansätze_JN)

## Kreuztabelle
xtabLösJN <- xtabs(~ dataCom$Lösungsvorschlag + dataCom$Lösungsansätze_JN)
print(xtabLösJN)

install.packages('xfun')
library('xfun')
install.packages('sjPlot')
library('sjPlot')

sjt.xtab(var.col = dataCom$Lösungsvorschlag, var.row = dataCom$Lösungsansätze_JN,
         var.labels = c("Lösungsansatz im Kommentar", "Art des Artikels"),
         show.col.prc = TRUE,
         show.exp = TRUE,
         show.legend = TRUE,
         file = "KreuztabelleJN.html",
         use.viewer = TRUE)
  
## Chisq-Test

chisq.test(dataCom$Lösungsvorschlag, dataCom$Lösungsansätze_JN) #X-squared = 61.551, df = 1, p-value = 4.314e-15

## alternative Packages? 
install.packages("stats")
library("stats")
install.packages("vcd")
library("vcd")
install.packages("grid")
library("grid")

cramerV(xtabLösJN) # Cramer V  0.3905

# H3: Auf Artikel, die vorwiegend Mitigation als Lösungsansatz thematisieren (H3a) 
#### und dabei die Bürger:innen adressieren (H3b), 
#### reagieren die Nutzer:innen in den Kommentaren eher mit Kritik am Medium.

## AV Kritik am Medium
## Zunächst Unterschied zw. Konstruk. & Problemzentrierten Artikeln? 
xtabKonstrKrit <- xtabs(~ dataCom$Lösungsvorschlag + dataCom$Kritik)
print(xtabKonstrKrit)

sjt.xtab(var.col = dataCom$Lösungsvorschlag, var.row = dataCom$Kritik,
         var.labels = c("Kritik am Medium", "Art des Artikels"),
         show.col.prc = TRUE,
         show.exp = TRUE,
         show.legend = TRUE,
         file = "xtab_ArtArtikelxKritik.html",
         use.viewer = TRUE)

## Nur konstruktive Artikel: Unterschied zwischen Mitigation / Adaption
dataCom$Mitigation.Adaption[dataCom$Mitigation.Adaption == 99] <- NA
table(is.na(dataCom$Mitigation.Adaption))

dataCom$Mitigation.Adaption[dataCom$Mitigation.Adaption == 9] <- NA
table(is.na(dataCom$Mitigation.Adaption))

xtabMitKrit <- xtabs(~  dataCom$Mitigation.Adaption + dataCom$Kritik)
print(xtabMitKrit)

fisher.test(dataCom$Mitigation.Adaption, dataCom$Kritik) ## p = 0.344

sjt.xtab(var.col = dataCom$Mitigation.Adaption, var.row = dataCom$Kritik,
         var.labels = c("Kritik", "Art des Lösungsansatzes"),
         show.col.prc = TRUE,
         show.exp = TRUE,
         show.legend = TRUE,
         file = "xtab_ArtLösungxKritik.html",
         use.viewer = TRUE)

## Adressieren der Bürger:innen
dataCom$Bürger_innen[dataCom$Bürger_innen == 99] <- NA
table(is.na(dataCom$Bürger_innen))

sjt.xtab(var.col = dataCom$Bürger_innen, var.row = dataCom$Kritik,
         var.labels = c("Kritik", "Buerger:innen adressiert"),
         show.col.prc = TRUE,
         show.exp = TRUE,
         show.legend = TRUE,
         file = "xtab_BuergxKritik.html",
         use.viewer = TRUE)

## Zusätzlich: Zusammenhang mit Lob?
xtabKonstrLob <- xtabs(~ dataCom$Lösungsvorschlag + dataCom$Lob)
print(xtabKonstrLob)

sjt.xtab(var.col = dataCom$Lösungsvorschlag, var.row = dataCom$Lob,
         var.labels = c("Lob Medium", "Art des Artikels"),
         show.col.prc = TRUE,
         show.exp = TRUE,
         show.legend = TRUE,
         file = "xtab_ArtArtikelxLob.html",
         use.viewer = TRUE)
## p < 0.05 X2 = 3.646 --> signifikanter Zusammenhang mit schwachem Effekt

xtabMitLob <- xtabs(~  dataCom$Mitigation.Adaption + dataCom$Lob)
print(xtabMitLob)

fisher.test(dataCom$Mitigation.Adaption, dataCom$Lob) ## p = 0.344

sjt.xtab(var.col = dataCom$Mitigation.Adaption, var.row = dataCom$Lob,
         var.labels = c("Lob", "Art des Lösungsansatzes"),
         show.col.prc = TRUE,
         show.exp = TRUE,
         show.legend = TRUE,
         file = "xtab_ArtLösungxLob.html",
         use.viewer = TRUE)
## kein Zusammenhang

# FF1: Hat die Verwendung konstruktiver Elementen in den Artikeln 
#### eine Auswirkung auf die Beleidigungen/Inzivilität der Kommentare?

xtabBelInz <- xtabs(~ dataCom$Beleidigung_Inzivilität + dataCom$Lösungsvorschlag)
round(
  addmargins(
    prop.table(xtabBelInz, margin = 2)), 
  digits = 3)



# Exakter Fisher-Test
fisher.test(dataCom$Lösungsvorschlag, dataCom$Beleidigung_Inzivilität)
cramerV(xtabBelInz) # Cramer V 0.118

sjt.xtab(var.col = dataCom$Lösungsvorschlag, var.row = dataCom$Beleidigung_Inzivilität,
         var.labels = c("Beleidung / Inzivilität", "Art des Artikels"),
         show.col.prc = TRUE,
         show.exp = TRUE,
         show.legend = TRUE,
         use.viewer = TRUE)

## --> Knapp nicht signifikant. Evtl. mit Regression o.ä. nochmal genauer checken.

## Beleidigung & Inzivilität zusammenfassen

dataCom <- dataCom |>
  mutate(BelInz_JN = rec(Beleidigung_Inzivilität,
                                 rec = "1,2 = 1 [JA]; 3 = 0 [NEIN]"))
dataCom |> sjmisc::frq(BelInz_JN)
table(dataCom$Lösungsvorschlag)

## Logistische Regressionsanalyse
# Definition des Modells 
moBelInz <- as.formula("BelInz_JN ~ Lösungsvorschlag + Adressat_in")

logit <- glm(moBelInz, family = binomial, data = dataCom) # Ausgabe der Schätzergebnisse 
summary(logit)

moBelInzII <- as.formula("BelInz_JN ~ Bürger_innen + Mitigation.Adaption")
logitII <- glm(moBelInzII, family = binomial, data = dataCom) # Ausgabe der Schätzergebnisse 
summary(logitII)
# Problem: singularities, Multikollinearität

# FF2: Hat die Verwendung konstruktiver Elemente einen Effekt auf 
#### die geäußerte Haltung zur Klimakrise?

## Kreuztabelle

xtabff2 <- xtabs(~ Journalismus_Klima_Kommentare_Mastersheet$Lösungsvorschlag + Journalismus_Klima_Kommentare_Mastersheet$Haltung_Klimakrise)
print(xtabff2)


install.packages("xfun")
library(xfun)
install.packages("sjPlot")
library(sjPlot)

sjt.xtab(var.col = Journalismus_Klima_Kommentare_Mastersheet$Lösungsvorschlag, var.row = Journalismus_Klima_Kommentare_Mastersheet$Haltung_Klimakrise,
         var.labels = c("Haltung zur Klimakrise", "Konstruktiver Artikel"),
         show.col.prc = TRUE,
         show.exp = TRUE,
         show.legend = TRUE,
         file = "KreuztabelleFF2.html",
         use.viewer = TRUE)


## Chisq-Test

chisq.test(Journalismus_Klima_Kommentare_Mastersheet$Lösungsvorschlag, Journalismus_Klima_Kommentare_Mastersheet$Haltung_Klimakrise) 
cohenW(Journalismus_Klima_Kommentare_Mastersheet$Lösungsvorschlag, Journalismus_Klima_Kommentare_Mastersheet$Haltung_Klimakrise) 

cramerV(xtabff2)

esc_chisq(p = 0.05707, totaln = 412)
esc_chisq(chisq = 7.519, totaln = 412)



# FF3: Hat die Art des Lösungsansatzes (Mitigation/Adaption) einen Einfluss
#### auf die geäußerte Haltung zu dem Lösungsansatz?


## Kreuztabelle

xtabff3 <- xtabs(~ Journalismus_Klima_Kommentare_Mastersheet$`Mitigation/Adaption` + Journalismus_Klima_Kommentare_Mastersheet$Haltung_Lösungsansätze)
print(xtabff3)

sjt.xtab(var.col = Journalismus_Klima_Kommentare_Mastersheet$`Mitigation/Adaption`, var.row = Journalismus_Klima_Kommentare_Mastersheet$Haltung_Lösungsansätze,
         var.labels = c("Haltung zum Lösungsansatz", "Art des Lösungsansatzes"),
         show.col.prc = TRUE,
         show.exp = TRUE,
         show.legend = TRUE,
         file = "KreuztabelleFF3.html",
         use.viewer = TRUE)


## Chisq-Test

chisq.test(Journalismus_Klima_Kommentare_Mastersheet$`Mitigation/Adaption`, Journalismus_Klima_Kommentare_Mastersheet$Haltung_Lösungsansätze) 
cohenW(Journalismus_Klima_Kommentare_Mastersheet$`Mitigation/Adaption`, Journalismus_Klima_Kommentare_Mastersheet$Haltung_Lösungsansätze) 

cramerV(xtabff3)

esc_chisq(p = 2.2e-16, totaln = 415)
esc_chisq(chisq = 488.44, totaln = 415)
