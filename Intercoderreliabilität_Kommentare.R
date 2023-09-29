# Cohens's Kappa

# Paket installieren

install.packages("DescTools")
library(DescTools)

# Grundlage: Kreuztabelle

ratertab_loesungsansaetze <- xtabs (~ Codingsheet_Kommentare_Rel$C_Lösungsansätze + Codingsheet_Kommentare_Rel$S_Lösungsansätze)
ratertab_loesungsansaetze

ratertab_haltung_loesungsansaetze <- xtabs (~ Codingsheet_Kommentare_Rel$C_Haltung_Lösungsansätze + Codingsheet_Kommentare_Rel$S_Haltung_Lösungsansätze)
ratertab_haltung_loesungsansaetze

ratertab_haltung_klimakrise <- xtabs (~ Codingsheet_Kommentare_Rel$C_Haltung_Klimakrise + Codingsheet_Kommentare_Rel$S_Haltung_Klimakrise)
ratertab_haltung_klimakrise

ratertab_adressat_in <- xtabs (~ Codingsheet_Kommentare_Rel$C_Adressat_in + Codingsheet_Kommentare_Rel$S_Adressat_in)
ratertab_adressat_in

ratertab_lob <- xtabs (~ Codingsheet_Kommentare_Rel$C_Lob + Codingsheet_Kommentare_Rel$S_Lob)
ratertab_lob

ratertab_kritik <- xtabs (~ Codingsheet_Kommentare_Rel$C_Kritik + Codingsheet_Kommentare_Rel$S_Kritik)
ratertab_kritik

ratertab_beleidigung_inzivilitaet <- xtabs (~ Codingsheet_Kommentare_Rel$C_Beleidigung_Inzivilität + Codingsheet_Kommentare_Rel$S_Beleidigung_Inzivilität)
ratertab_beleidigung_inzivilitaet

# Berechnung Cohen's Kappa

CohenKappa(ratertab_loesungsansaetze)
CohenKappa(ratertab_haltung_loesungsansaetze)
CohenKappa(ratertab_haltung_klimakrise)
CohenKappa(ratertab_adressat_in)
CohenKappa(ratertab_lob)
CohenKappa(ratertab_kritik)
CohenKappa(ratertab_beleidigung_inzivilitaet)

# Ergebnisse auf zwei Nachkommastellen gerundet

# Lösungsansätze: 0.60
# Haltung Lösungsansätze: 0.81
# Haltung Klimakrise: 0.37
# Adressat_in: 0.69
# Lob: 0.74
# Kritik: 0.53
# Beleidigung_Inzivilität: 0.80



# Krippendorff's Alpha

# Subsets aus Datensatz ziehen

data_loesungsansaetze <- subset(Codingsheet_Kommentare_Rel, select = c(C_Lösungsansätze, S_Lösungsansätze))
data_haltung_loesungsansaetze <- subset(Codingsheet_Kommentare_Rel, select = c(C_Haltung_Lösungsansätze, S_Haltung_Lösungsansätze))
data_haltung_klimakrise <- subset(Codingsheet_Kommentare_Rel, select = c(C_Haltung_Klimakrise, S_Haltung_Klimakrise))
data_adressat_in <- subset(Codingsheet_Kommentare_Rel, select = c(C_Adressat_in, S_Adressat_in))
data_lob <- subset(Codingsheet_Kommentare_Rel, select = c(C_Lob, S_Lob))
data_kritik <- subset(Codingsheet_Kommentare_Rel, select = c(C_Kritik, S_Kritik))
data_beleidigung_inzivilitaet <- subset(Codingsheet_Kommentare_Rel, select = c(C_Beleidigung_Inzivilität, S_Beleidigung_Inzivilität))

# Umwandlung Dataframe in Matrix 

data_loesungsansaetze2 <- as.matrix(data_loesungsansaetze)
data_haltung_loesungsansaetze2 <- as.matrix(data_haltung_loesungsansaetze)
data_haltung_klimakrise2 <- as.matrix(data_haltung_klimakrise)
data_adressat_in2 <- as.matrix(data_adressat_in)
data_lob2 <- as.matrix(data_lob)
data_kritik2 <- as.matrix(data_kritik)
data_beleidigung_inzivilitaet2 <- as.matrix(data_beleidigung_inzivilitaet)

# Zeilen und Spalten tauschen 

data_loesungsansaetze3 <- t(data_loesungsansaetze2)
data_haltung_loesungsansaetze3 <- t(data_haltung_loesungsansaetze2)
data_haltung_klimakrise3 <- t(data_haltung_klimakrise2)
data_adressat_in3 <- t(data_adressat_in2)
data_lob3 <- t(data_lob2)
data_kritik3 <- t(data_kritik2)
data_beleidigung_inzivilitaet3 <- t(data_beleidigung_inzivilitaet2)

# Berechnung Krippendorff's Alpha

KrippAlpha(data_loesungsansaetze3, method = "nominal")
KrippAlpha(data_haltung_loesungsansaetze3, method = "nominal")
KrippAlpha(data_haltung_klimakrise3, method = "nominal")
KrippAlpha(data_adressat_in3, method = "nominal")
KrippAlpha(data_lob3, method = "nominal")
KrippAlpha(data_kritik3, method = "nominal")
KrippAlpha(data_beleidigung_inzivilitaet3, method = "nominal")

# Ergebnisse auf zwei Nachkommastellen gerundet

# Lösungsansätze: 0.60
# Haltung Lösungsansätze: 0.81
# Haltung Klimakrise: 0.34
# Adressat:in: 0.69
# Lob: 0.74
# Kritik: 0.53
# Beleidigung_Inzivilität: 0.80

