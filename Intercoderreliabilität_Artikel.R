# Cohens's Kappa

# Paket installieren

install.packages("DescTools")
library(DescTools)

# Grundlage: Kreuztabelle

ratertab_problematisierung <- xtabs (~ Codingsheet_Artikel_Rel$C_Problematisierung + Codingsheet_Artikel_Rel$S_Problematisierung)
ratertab_problematisierung

ratertab_loesungsvorschlag <- xtabs (~ Codingsheet_Artikel_Rel$C_Lösungsvorschlag + Codingsheet_Artikel_Rel$S_Lösungsvorschlag)
ratertab_loesungsvorschlag

ratertab_anzahl_loesungsvorschlaege <- xtabs (~ Codingsheet_Artikel_Rel$C_Anzahl_Lösungsvorschläge + Codingsheet_Artikel_Rel$S_Anzahl_Lösungsvorschläge)
ratertab_anzahl_loesungsvorschlaege

ratertab_ausfuehrlichkeit <- xtabs (~ Codingsheet_Artikel_Rel$C_Ausführlichkeit + Codingsheet_Artikel_Rel$S_Ausführlichkeit)
ratertab_ausfuehrlichkeit

ratertab_konstruktive_wortwahl <- xtabs (~ Codingsheet_Artikel_Rel$C_Konstruktive_Wortwahl + Codingsheet_Artikel_Rel$S_Konstruktive_Wortwahl)
ratertab_konstruktive_wortwahl

# Berechnung Cohen's Kappa

CohenKappa(ratertab_problematisierung)
CohenKappa(ratertab_loesungsvorschlag)
CohenKappa(ratertab_anzahl_loesungsvorschlaege)
CohenKappa(ratertab_ausfuehrlichkeit)
CohenKappa(ratertab_konstruktive_wortwahl)

# Ergebnisse auf zwei Nachkommastellen gerundet

# Problematisierung: 0.79
# Lösungsvorschlag: 0.64
# Anzahl Lösungsvorschläge: 0.51
# Ausführlichkeit: 0.58
# Konstruktive Wortwahl: 



# Krippendorff's Alpha

# Subsets aus Datensatz ziehen
data_problematisierung <- subset(Codingsheet_Artikel_Rel, select = c(C_Problematisierung, S_Problematisierung))
data_loesungsvorschlag <- subset(Codingsheet_Artikel_Rel, select = c(C_Lösungsvorschlag, S_Lösungsvorschlag))
data_anzahl_loesungsvorschlaege <- subset(Codingsheet_Artikel_Rel, select = c(C_Anzahl_Lösungsvorschläge, S_Anzahl_Lösungsvorschläge))
data_ausfuehrlichkeit <- subset(Codingsheet_Artikel_Rel, select = c(C_Ausführlichkeit, S_Ausführlichkeit))
data_konstruktive_wortwahl <- subset(Codingsheet_Artikel_Rel, select = c(C_Konstruktive_Wortwahl, S_Konstruktive_Wortwahl))

# Umwandlung Dataframe in Matrix 
data_problematisierung2 <- as.matrix(data_problematisierung)
data_loesungsvorschlag2 <- as.matrix(data_loesungsvorschlag)
data_anzahl_loesungsvorschlaege2 <- as.matrix(data_anzahl_loesungsvorschlaege)
data_ausfuehrlichkeit2 <- as.matrix(data_ausfuehrlichkeit)
data_konstruktive_wortwahl2 <- as.matrix(data_konstruktive_wortwahl)

# Zeilen und Spalten tauschen 
data_problematisierung3 <- t(data_problematisierung2)
data_loesungsvorschlag3 <- t(data_loesungsvorschlag2)
data_anzahl_loesungsvorschlaege3 <- t(data_anzahl_loesungsvorschlaege2)
data_ausfuehrlichkeit3 <- t(data_ausfuehrlichkeit2)
data_konstruktive_wortwahl3 <- t(data_konstruktive_wortwahl2)

# Berechnung Krippendorff's Alpha

KrippAlpha(data_problematisierung3, method = "nominal")
KrippAlpha(data_loesungsvorschlag3, method = "nominal")
KrippAlpha(data_anzahl_loesungsvorschlaege3, method = "ratio")
KrippAlpha(data_ausfuehrlichkeit3, method = "nominal")
KrippAlpha(data_konstruktive_wortwahl3, method = "nominal")

# Ergebnisse auf zwei Nachkommastellen gerundet

# Problematisierung: 0.78
# Lösungsvorschlag: 0.64
# Anzahl Lösungsvorschläge: 0.60
# Ausführlichkeit: 0.58
# Konstruktive Wortwahl: 
