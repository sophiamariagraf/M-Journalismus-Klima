# Inferenzstatistik Kommentare

install.packages("rcompanion")
library("rcompanion")
install.packages("esc")
library("esc")

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
         file = "KreuztabelleJN.jpg",
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

xtabMitKrit <- xtabs(~  dataCom$Mitigation.Adaption + dataCom$Kritik)
print(xtabMitKrit)

chisq.test(dataCom$Mitigation.Adaption, dataCom$Kritik)
cramerV

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
