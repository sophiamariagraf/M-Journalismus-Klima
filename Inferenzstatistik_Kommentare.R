# Inferenzstatistik Kommentare

install.packages("rcompanion")
library("rcompanion")
install.packages("esc")
library("esc")

# H1: Die konstruktiven Elemente in den Artikeln haben keine Auswirkung 
#### auf die Anzahl der Kommentare der Nutzer:innen.

# s. Scrpit zum Datensatz Unterauswahl_Artikel

# H2: Die konstruktiven Elemente in den Artikeln sorgen dafür, dass 
#### Lösungsansätze in den Kommentaren häufiger thematisiert werden.

KreuztabLös <- xtabs(~ dataCom$Lösungsvorschlag + dataCom$Lösungsansätze)
print(KreuztabLös)

## Achtung wsl falsch/ mit bestehender Variable

chisq.test(dataCom$Lösungsvorschlag, dataCom$Lösungsansätze_JN) # X-squared = 105.72, df = 3, p-value < 2.2e-16
fisher.test(dataCom$Lösungsvorschlag, dataCom$Lösungsansätze) # p-value < 2.2e-16 alternative hypothesis: two.sided

cohenW(dataCom$Lösungsvorschlag, dataCom$Lösungsansätze) # Cohen w 0.5053 

## Dichotome Variable Ja/ Nein

library("dplyr")
install.packages("sjmisc")
library("sjmisc")

DATEN_IA |> sjmisc::frq(Medium_Q)

dataCom <- dataCom |>
  mutate(Lösungsansätze_JN = rec(Lösungsansätze,
                                 rec = "1,2,3 = 1 [JA]; 4 = 2 [NEIN]"))
dataCom |> sjmisc::frq(Lösungsansätze_JN)

## Kreuztabelle
xtabLösJN <- xtabs(~ dataCom$Lösungsvorschlag + dataCom$Lösungsansätze_JN)
print(xtabLösJN)

## Chisq-Test

chisq.test(dataCom$Lösungsvorschlag, dataCom$Lösungsansätze_JN) # X-squared = 62.179, df = 1, p-value = 3.137e-15
cohenW(dataCom$Lösungsvorschlag, dataCom$Lösungsansätze_JN) 

esc_chisq(p = 3.137e-15, totaln = 413)
esc_chisq(chisq = 62.179, totaln = 413)

###Effect Size Calculation for Meta Analysis
#Conversion: chi-squared-value to effect size d
#Effect Size:   0.8420
#Standard Error:   0.1068
#Variance:   0.0114
#Lower CI:   0.6327
#Upper CI:   1.0513
#Weight:  87.7053

## alternative Packages? 
install.packages("stats")
library("stats")
install.packages("vcd")
library("vcd")
install.packages("grid")
library("grid")

cramerV(xtabLösJN) # Cramer V  0.3929  --> stimmt das ???

# H3: Auf Artikel, die vorwiegend Mitigation als Lösungsansatz thematisieren (H3a) 
#### und dabei die Bürger:innen adressieren (H3b), 
#### reagieren die Nutzer:innen in den Kommentaren eher mit Kritik am Medium.



# FF1: Hat die Verwendung konstruktiver Elementen in den Artikeln 
#### eine Auswirkung auf die Beleidigungen/Inzivilität der Kommentare?

xtabBelInz <- xtabs(~ dataCom$Lösungsvorschlag + dataCom$Beleidigung_Inzivilität)
print(xtabBelInz)

chisq.test(dataCom$Lösungsvorschlag, dataCom$Beleidigung_Inzivilität) # X-squared = 5.765, df = 2, p-value = 0.05599
cramerV(xtabBelInz) # Cramer V 0.118 

## --> Knapp nicht signifikant. Evtl. mit Regression o.ä. nochmal genauer checken.
