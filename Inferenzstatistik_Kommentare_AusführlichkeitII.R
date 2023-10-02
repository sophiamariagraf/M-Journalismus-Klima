# Inferenzstatistik Kommentare
# Diesmal wir die Variable Ausführlichkeit II berücksichtigt (engere Definition konstruktiver Artikel)

## Subset bilden
dataCom_subset <- dataCom[(dataCom$II_Ausführlichkeit == "1" 
                           | dataCom$II_Ausführlichkeit == "99"),]


# H1: Die konstruktiven Elemente in den Artikeln haben keine Auswirkung 
#### auf die Anzahl der Kommentare der Nutzer:innen.

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

# H2: Die konstruktiven Elemente in den Artikeln sorgen dafür, dass 
#### Lösungsansätze in den Kommentaren häufiger thematisiert werden.

xtabLösII <- xtabs(~ dataCom_subset$Lösungsvorschlag + dataCom_subset$Lösungsansätze)
print(xtabLösII)

xtabLös <- table(dataCom$Lösungsvorschlag, dataCom$Lösungsansätze)
addmargins(xtabLös)

## Dichotome Variable Ja/ Nein

library("dplyr")
install.packages("sjmisc")
library("sjmisc")

dataCom_subset <- dataCom_subset |>
  mutate(Lösungsansätze_JN = rec(Lösungsansätze,
                                 rec = "1,2,3 = 1 [JA]; 4 = 2 [NEIN]"))
dataCom_subset |> sjmisc::frq(Lösungsansätze_JN)

## Kreuztabelle
xtabLösJNII <- xtabs(~ dataCom_subset$Lösungsvorschlag + dataCom_subset$Lösungsansätze_JN)
print(xtabLösJNII)

install.packages('xfun')
library('xfun')
install.packages('sjPlot')
library('sjPlot')

sjt.xtab(var.col = dataCom_subset$Lösungsvorschlag, var.row = dataCom_subset$Lösungsansätze_JN,
         var.labels = c("Lösungsansatz im Kommentar", "Art des Artikels"),
         show.col.prc = TRUE,
         show.exp = TRUE,
         show.legend = TRUE,
         file = "xtab_LösungJNII.html",
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

xtabBelInzII <- xtabs(~ dataCom_subset$Beleidigung_Inzivilität + dataCom_subset$Lösungsvorschlag)


# Exakter Fisher-Test
fisher.test(dataCom_subset$Lösungsvorschlag, dataCom_subset$Beleidigung_Inzivilität)
cramerV(xtabBelInzII) # 

sjt.xtab(var.col = dataCom$Lösungsvorschlag, var.row = dataCom$Beleidigung_Inzivilität,
         var.labels = c("Beleidung / Inzivilität", "Art des Artikels"),
         show.col.prc = TRUE,
         show.exp = TRUE,
         show.legend = TRUE,
         file = "xtab_ArtArtikelxBelInz.html",
         use.viewer = TRUE)

## --> Knapp nicht signifikant. Evtl. mit Regression o.ä. nochmal genauer checken.