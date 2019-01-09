# benötigte Pakete installieren und laden ####

for (pack in c("rstudioapi", "tidyverse", "gesis")) {
  if(!(pack %in% installed.packages()[,1])) {
    install.packages(pack)
  }
  library(pack, character.only = T)
}

# Arbeitsverzeichnis = Ort, an dem dieses Skript gespeichert ist (funktioniert nur in RStudio) ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# DBK-Session mit eigenen Login-Daten initialisieren und verfügbare Studiengruppen explorieren ####
# username = E-Mail-Adresse, die bei der DBK-Anmeldung verwendet wurde
s <- login(username = "", password = "")

get_study_groups()

# ALLBUS ####
