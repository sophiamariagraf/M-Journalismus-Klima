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

ratertab_art_loesungsansatz <- xtabs (~ Codingsheet_Unterauswahl_Artikel_Rel$C_Mitigation_Adaption + Codingsheet_Unterauswahl_Artikel_Rel$S_Mitigation_Adaption)
ratertab_art_loesungsansatz

ratertab_buergerinnen <- xtabs (~ Codingsheet_Unterauswahl_Artikel_Rel$C_Bürger_innen + Codingsheet_Unterauswahl_Artikel_Rel$S_Bürger_innen)
ratertab_buergerinnen

# Berechnung Cohen's Kappa

CohenKappa(ratertab_problematisierung)
CohenKappa(ratertab_loesungsvorschlag)
CohenKappa(ratertab_anzahl_loesungsvorschlaege)
CohenKappa(ratertab_ausfuehrlichkeit)
CohenKappa(ratertab_konstruktive_wortwahl)
CohenKappa(ratertab_art_loesungsansatz)
CohenKappa(ratertab_buergerinnen)

# Ergebnisse auf zwei Nachkommastellen gerundet

# Problematisierung: 0.79
# Lösungsvorschlag: 0.64
# Anzahl Lösungsvorschläge: 0.51
# Ausführlichkeit: 0.58
# Konstruktive Wortwahl: 0.50
# Art des Lösungsansatzes: 1.0
# Bürger:innen als Adressat: 1.0



# Krippendorff's Alpha

# Subsets aus Datensatz ziehen

data_problematisierung <- subset(Codingsheet_Artikel_Rel, select = c(C_Problematisierung, S_Problematisierung))
data_loesungsvorschlag <- subset(Codingsheet_Artikel_Rel, select = c(C_Lösungsvorschlag, S_Lösungsvorschlag))
data_anzahl_loesungsvorschlaege <- subset(Codingsheet_Artikel_Rel, select = c(C_Anzahl_Lösungsvorschläge, S_Anzahl_Lösungsvorschläge))
data_ausfuehrlichkeit <- subset(Codingsheet_Artikel_Rel, select = c(C_Ausführlichkeit, S_Ausführlichkeit))
data_konstruktive_wortwahl <- subset(Codingsheet_Artikel_Rel, select = c(C_Konstruktive_Wortwahl, S_Konstruktive_Wortwahl))
data_art_loesungsansatz <- subset(Codingsheet_Unterauswahl_Artikel_Rel, select = c(C_Mitigation_Adaption, S_Mitigation_Adaption))
data_buergerinnen <- subset(Codingsheet_Unterauswahl_Artikel_Rel, select = c(C_Bürger_innen, S_Bürger_innen))

# Umwandlung Dataframe in Matrix 

data_problematisierung2 <- as.matrix(data_problematisierung)
data_loesungsvorschlag2 <- as.matrix(data_loesungsvorschlag)
data_anzahl_loesungsvorschlaege2 <- as.matrix(data_anzahl_loesungsvorschlaege)
data_ausfuehrlichkeit2 <- as.matrix(data_ausfuehrlichkeit)
data_konstruktive_wortwahl2 <- as.matrix(data_konstruktive_wortwahl)
data_art_loesungsansatz2 <- as.matrix(data_art_loesungsansatz)
data_buergerinnen2 <- as.matrix(data_buergerinnen)

# Zeilen und Spalten tauschen 

data_problematisierung3 <- t(data_problematisierung2)
data_loesungsvorschlag3 <- t(data_loesungsvorschlag2)
data_anzahl_loesungsvorschlaege3 <- t(data_anzahl_loesungsvorschlaege2)
data_ausfuehrlichkeit3 <- t(data_ausfuehrlichkeit2)
data_konstruktive_wortwahl3 <- t(data_konstruktive_wortwahl2)
data_art_loesungsansatz3 <- t(data_art_loesungsansatz2)
data_buergerinnen3 <- t(data_buergerinnen2)

# Berechnung Krippendorff's Alpha

KrippAlpha(data_problematisierung3, method = "nominal")
KrippAlpha(data_loesungsvorschlag3, method = "nominal")
KrippAlpha(data_anzahl_loesungsvorschlaege3, method = "ratio")
KrippAlpha(data_ausfuehrlichkeit3, method = "nominal")
KrippAlpha(data_konstruktive_wortwahl3, method = "nominal")
KrippAlpha(data_art_loesungsansatz3, method = "nominal")
KrippAlpha(data_buergerinnen3, method = "nominal")

# Ergebnisse auf zwei Nachkommastellen gerundet

# Problematisierung: 0.78
# Lösungsvorschlag: 0.64
# Anzahl Lösungsvorschläge: 0.60
# Ausführlichkeit: 0.58
# Konstruktive Wortwahl: 0.50
# Art des Lösungsansatzes: 1.0
# Bürger:innen als Adressat: 1.0

