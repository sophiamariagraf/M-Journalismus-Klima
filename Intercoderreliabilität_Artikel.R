# Cohens's Kappa

# Paket installieren

install.packages("DescTools")
library(DescTools)

# Grundlage: Kreuztabelle

ratertab_problematisierung <- xtabs (~ Codingsheet_Artikel_Rel$C_Problematisierung + Codingsheet_Artikel_Rel$S_Problematisierung)
ratertab_problematisierung

ratertab_loesungsvorschlag <- xtabs (~ Codingsheet_Artikel_Rel$C_Lösungsvorschlag + Codingsheet_Artikel_Rel$S_Lösungsvorschlag)
ratertab_loesungsvorschlag

ratertab_anzahl_loesungsvorschlaege <- xtabs (~ Codingsheet_Artikel_Rel$`C_Anzahl Lösungsvorschläge` + Codingsheet_Artikel_Rel$`S_Anzahl Lösungsvorschläge`)
ratertab_anzahl_loesungsvorschlaege

ratertab_ausfuehrlichkeit <- xtabs (~ Codingsheet_Artikel_Rel$C_Ausführlichkeit + Codingsheet_Artikel_Rel$S_Ausführlichkeit)
ratertab_ausfuehrlichkeit

ratertab_konstruktive_wortwahl <- xtabs (~ Codingsheet_Artikel_Rel$`C_Konstruktive Wortwahl` + Codingsheet_Artikel_Rel$`S_Konstruktive Wortwahl`)
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
