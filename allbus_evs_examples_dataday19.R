# benötigte Pakete installieren und laden ####

for (pack in c("rstudioapi", "tidyverse", "gesis", "haven", "ggmap", "ggthemes")) {
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

# Welche ALLBUS-Datensätze gibt es?
get_datasets("0007") %>% 
  View("Verfuegbare ALLBUS-Datensaetze")

# ALLBUScompact - Kumulation 1980-2016 herunterladen (Studiennr. = ZA4587)
# Dateityp (filetype) = SPSS, Verwendungszweck (purpose) = wiss. Forschung
download_dataset(s, doi = "4587", path = "./data", filetype = ".sav", purpose = 1)

# Mit dem folgenden Befehl kann man auch das Codebuch für die Studie herunterladen
# download_codebook(doi = "4587", path = "./data")

# ALLBUS 2016 herunterladen
download_dataset(s, doi = "5250", path = "./data", filetype = ".sav", purpose = 1)

# Datei muss in diesem Fall noch entpackt werden
unzip("./data/ZA5250_v2-1-0.sav.zip", exdir = "./data")

# Mit dem folgenden Befehl kann man auch das Codebuch für die Studie herunterladen
# download_codebook(doi = "5250", path = "./data")

# Daten einlesen und explorieren
allbus_kum <- read_sav("data/ZA4587_v1-0-0.sav")
dim(allbus_kum) # Anzahl der Befragten und Variablen
names(allbus_kum) # Variablennamen (Codebuch zum Verständnis benötigt)

allbus_2016 <- read_sav("data/ZA5250_v2-1-0.sav")
dim(allbus_2016) # Anzahl der Befragten und Variablen
names(allbus_2016) # Variablennamen (Codebuch zum Verständnis benötigt)

# Variablen von Interesse auswählen und bearbeiten
contact <- allbus_kum %>% 
  select(year, eastwest, starts_with("mc"), wghtpt, wghtptew) %>% 
  mutate(eastwest = as_factor(eastwest)) %>% 
  mutate(eastwest = recode(eastwest,
                           "ALTE BUNDESLAENDER" = "Westdeutschland",
                           "NEUE BUNDESLAENDER" = "Ostdeutschland"))

migration_att <- allbus_2016 %>% 
  select(eastwest, land, ma09, mp03) %>%
  mutate(eastwest = as_factor(eastwest)) %>% 
  mutate(land = as_factor(land)) %>% 
  mutate(land = fct_recode(land, "ST" = "SACHSEN-ANHALT",
                           "SN" = "SACHSEN",
                           "NI" = "NIEDERSACHSEN",
                           "BW" = "BADEN-WUERTTEMBERG",
                           "HE" = "HESSEN",
                           "MV" = "MECKLENB.-VORPOMMERN",
                           "BE" = "EHEM. BERLIN-OST",
                           "BE" = "EHEM. BERLIN-WEST",
                           "BB" = "BRANDENBURG",
                           "RP" = "RHEINLAND-PFALZ",
                           "SL" = "SAARLAND",
                           "TH" = "THUERINGEN",
                           "BY" = "BAYERN",
                           "NW" = "NORDRHEIN-WESTFALEN",
                           "SH" = "SCHLESWIG-HOLSTEIN",
                           "HB" = "BREMEN",
                           "HH" = "HAMBURG"))

# Summary Statistics für Variablen berechnen
fremd_land <- migration_att %>%
  group_by(land) %>%
  summarise(n=n(),
    mean=mean(ma09, na.rm = T),
    sd=sd(ma09, na.rm = T)) %>%
  mutate(se=sd/sqrt(n)) %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))

fremd_land

ggplot(fremd_land, aes(x = land, y = mean)) + 
  geom_point(aes(size = n)) +
  geom_errorbar(aes(ymin = mean-ic, ymax = mean+ic), width = .1) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(1,7)) +
  labs(x = "",
       y = "",
       title = "Durch die vielen Ausländer in Deutschland fühlt man sich zunehmend \nals Fremder im eigenen Land.",
       subtitle = "1 = Stimme überhaupt nicht zu, 7 = Stimme voll und ganz zu",
       caption = "Quelle: ALLBUS 2016. GESIS Datenarchiv, Köln. ZA5250 Datenfile Version 2.1.0, doi:10.4232/1.12796") +
  theme_calc()

bereicher_land <- migration_att %>%
  group_by(land) %>%
  summarise(n=n(),
            mean=mean(mp03, na.rm = T),
            sd=sd(mp03, na.rm = T)) %>%
  mutate(se=sd/sqrt(n)) %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))

bereicher_land

ggplot(bereicher_land, aes(x = land, y = mean)) + 
  geom_point(aes(size = n)) +
  geom_errorbar(aes(ymin = mean-ic, ymax = mean+ic), width = .1) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(1,7)) +
  labs(x = "",
       y = "",
       title = "Durch die vielen Ausländer in Deutschland fühlt man sich zunehmend \nals Fremder im eigenen Land.",
       subtitle = "1 = Stimme überhaupt nicht zu, 7 = Stimme voll und ganz zu",
       caption = "Quelle: ALLBUS 2016. GESIS Datenarchiv, Köln. ZA5250 Datenfile Version 2.1.0, doi:10.4232/1.12796") + 
theme_calc()
  

# Variablenwerte rekodieren (0 = "nein")
contact <- contact %>%
  mutate(mc01 = recode(as.numeric(mc01), "2"= 0)) %>% 
  mutate(mc02 = recode(as.numeric(mc02), "2"= 0)) %>% 
  mutate(mc03 = recode(as.numeric(mc03), "2"= 0)) %>% 
  mutate(mc04 = recode(as.numeric(mc04), "2"= 0))

# Variablen gewichten: Variablenvektor mit Gewichtungsvektor multiplizieren
# Aussagen über Personen im Ost-West-Vgl., daher (nur) Personengewicht
contact <- contact %>% 
  mutate(mc01_w = mc01 * wghtpt) %>% 
  mutate(mc02_w = mc02 * wghtpt) %>% 
  mutate(mc03_w = mc03 * wghtpt) %>% 
  mutate(mc04_w = mc04 * wghtpt)

# Daten fürs Plotten umstrukturieren
contact_long <- contact %>% 
  select(year, eastwest, mc01_w:mc04_w) %>% 
  gather(key = type_of_contact, value = yes_no, -year, -eastwest) %>%
  mutate(type_of_contact = recode(type_of_contact,
                                  "mc01_w" = "In der Familie",
                                  "mc02_w" = "Am Arbeitsplatz",
                                  "mc03_w" = "In der Nachbarschaft",
                                  "mc04_w" = "Im Freundeskreis")) %>%
  filter(!is.na(yes_no)) %>% 
  group_by(year, eastwest, type_of_contact) %>% 
  summarise(sum = sum(yes_no), n = n()) %>% 
  mutate (Anteil = (sum/n)*100)

# In welchen Jahren wurden die entsprechenden Fragen gestellt?
unique(contact_long$year)

# Zeitreihen im Ost-West-Vgl. plotten
contact_long %>% 
  ggplot(aes(x = year, y = Anteil, color = eastwest, shape = eastwest)) +
  geom_point(size = 2) +
  geom_line() +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 100, 10),
                     limits = c(0,100),
                     labels = c("0%", "10%", "20%", "30%", "40%" , "50%", "60%", "70%", "80%", "90%", "100%")) +
  ylab("Anteil der Befragten mit Kontakten") +
  scale_x_continuous(breaks = c(1980, 1984, 1988, 1990, 1994, 1996, 2000, 2002, 2006, 2010, 2012, 2016)) +
  theme(axis.text.x = element_text (angle = 90, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank()) +
  xlab(NULL) +
  facet_wrap(~type_of_contact, scales = "free") +
  ggtitle("Kontakte zwischen Deutschen und Ausländern",
          subtitle = "Quelle: ALLBUScompact - Kumulation 1980-2016.GESIS Datenarchiv, Köln.\nZA4587 Datenfile Version 1.0.0, doi:10.4232/1.13048")

# Plot exportieren
ggsave("./plots/Kontakte_mit_Auslaendern_ALLBUS_1980-2016.png")

# EVS ####
get_datasets("0009") %>% 
  View("Verfuegbare EVS-Datensaetze")

# Pre-Release des integrierten EVS-Datensatzes 2017 herunterladen (Daten aus 16 Ländern)
download_dataset(s, doi = "7500", path = "./data", filetype = ".sav", purpose = 1)
# Datei muss in diesem Fall noch entpackt werden
unzip("./data/ZA7500_v1-0-0.sav.zip", exdir = "./data")

# Mit dem folgenden Befehl kann man auch das Codebuch für die Studie herunterladen
# download_codebook(doi = "7500", path = "./data")

# EVS-2017-Pre-Release-Daten einlesen und explorieren

evs_2017 <- read_sav("data/ZA7500_v1-0-0.sav")
dim(evs_2017) # Anzahl der Befragten und Variablen
names(evs_2017) # Variablennamen (Codebuch zum Verständnis benötigt)

# Variablen von Interesse auswählen und bearbeiten
pol_evs_2017 <- evs_2017 %>% 
  select(country, v124, v131, v142, v144) %>% 
  mutate(country = as_factor(country),
         v124 = recode(as.numeric(v124), "1" = 4,
                       "2" = 3,
                       "3" = 2,
                       "4" = 1),
         v131 = recode(as.numeric(v131), "1" = 4,
                       "2" = 3,
                       "3" = 2,
                       "4" = 1))

# Weltkartendaten laden
world <- map_data("world")

# Daten auf Länderebene aggregieren
pol_country_evs_2017 <- pol_evs_2017 %>% 
  group_by(country) %>% 
  summarize_all(mean, na.rm = T)

# Karten- und EVS-Daten verbinden
evs_map_2017 <- world %>% 
  left_join(pol_country_evs_2017, by = c("region" = "country"))

# Karte zu Vertrauen in EU plotten
ggplot(data = evs_map_2017, aes(x = long, y = lat, group = group, fill = v124)) + 
  geom_polygon(color = "white") + 
  xlim(-25, 47.0) + ylim(35, 70) +
  theme_map() +
  theme(plot.title = element_text(face="bold", size = 16)) +
  labs(title = "Vertrauen in die EU 2017",
       subtitle = "1 = Überhaupt kein Vertrauen, 4 = Sehr viel Vertrauen",
       fill = "",
       caption = "Quelle: European Values Study 2017: Integrated Dataset (EVS 2017). GESIS Datenarchiv, Köln. ZA7500 Datenfile Version 1.0.0, doi:10.4232/1.13090") +
  scale_fill_distiller(breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4), palette = "RdYlGn", direction = 1, limits = c(1,4))

# Plot exportieren
ggsave("./plots/vertrauen_eu_evs_2017.png")

# Karte zu Vertrauen in Regierung plotten
ggplot(data = evs_map_2017, aes(x = long, y = lat, group = group, fill = v131)) + 
  geom_polygon(color = "white") + 
  xlim(-25, 47.0) + ylim(35, 70) +
  theme_map() +
  theme(plot.title = element_text(face="bold", size = 16)) +
  labs(title = "Vertrauen in die Regierung 2017",
       subtitle = "1 = Überhaupt kein Vertrauen, 4 = Sehr viel Vertrauen",
       fill = "",
       caption = "Quelle: European Values Study 2017: Integrated Dataset (EVS 2017). GESIS Datenarchiv, Köln. ZA7500 Datenfile Version 1.0.0, doi:10.4232/1.13090") +
  scale_fill_distiller(breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4), palette = "RdYlGn", direction = 1, limits = c(1,4))

# Plot exportieren
ggsave("./plots/vertrauen_regierung_evs_2017.png")

# Karte zu Wichtigkeit von Demokratie plotten
ggplot(data = evs_map_2017, aes(x = long, y = lat, group = group, fill = v142)) + 
  geom_polygon(color = "white") + 
  xlim(-25, 47.0) + ylim(35, 70) +
  theme_map() +
  theme(plot.title = element_text(face="bold", size = 14)) +
  labs(title = "Wie wichtig ist es für Sie, in einem Land zu leben, das demokratisch regiert wird?",
       subtitle = "1 = Überhaupt nicht wichtig, 10 = Absolut wichtig",
       fill = "",
       caption = "Quelle: European Values Study 2017: Integrated Dataset (EVS 2017). GESIS Datenarchiv, Köln. ZA7500 Datenfile Version 1.0.0, doi:10.4232/1.13090") +
  scale_fill_distiller(palette = "RdYlGn", direction = 1)

# Plot exportieren
ggsave("./plots/wichtig_demokratie_evs_2017.png")

# Karte zu Zufriedenheit mit pol. System plotten
ggplot(data = evs_map_2017, aes(x = long, y = lat, group = group, fill = v144)) + 
  geom_polygon(color = "white") + 
  xlim(-25, 47.0) + ylim(35, 70) +
  theme_map() +
  theme(plot.title = element_text(face="bold", size = 13)) +
  labs(title = "Wie zufrieden sind Sie damit, wie das politische System in Ihrem Land heutzutage funktioniert?",
       subtitle = "1 = Überhaupt nicht zufrieden, 10 = Voll und ganz zufrieden",
       fill = "",
       caption = "Quelle: European Values Study 2017: Integrated Dataset (EVS 2017). GESIS Datenarchiv, Köln. ZA7500 Datenfile Version 1.0.0, doi:10.4232/1.13090") +
  scale_fill_distiller(palette = "RdYlGn", direction = 1)

# Plot exportieren
ggsave("./plots/zufrieden_system_evs_2017.png")

# EVS 2008 ####
# Alle Länder + Vgl. mit 2017
# Pre-Release des integrierten EVS-Datensatzes 2008 herunterladen (Daten aus 16 Ländern)
download_dataset(s, doi = "4800", path = "./data", filetype = ".sav", purpose = 1)
# Datei muss in diesem Fall noch entpackt werden
unzip("./data/ZA4800_v4-0-0.sav.zip", exdir = "./data")

# Mit dem folgenden Befehl kann man auch das Codebuch für die Studie herunterladen
# download_codebook(doi = "4800", path = "./data")

# EVS-2008-Pre-Release-Daten einlesen und explorieren

evs_2008 <- read_sav("data/ZA4800_v4-0-0.sav")
dim(evs_2008) # Anzahl der Befragten und Variablen
names(evs_2008) # Variablennamen (Codebuch zum Verständnis benötigt)

# Variablen von Interesse auswählen und bearbeiten
# Wichtigkeit von Demokratie und Zufriedenheit mit pol. System wurden im EVS 2008 nicht abgefragt
pol_evs_2008 <- evs_2008 %>% 
  select(country, v214, v222) %>% 
  mutate(country = as_factor(country),
         v214 = recode(as.numeric(v214), "1" = 4,
                       "2" = 3,
                       "3" = 2,
                       "4" = 1),
         v222 = recode(as.numeric(v222), "1" = 4,
                       "2" = 3,
                       "3" = 2,
                       "4" = 1))

# Weltkartendaten laden (falls nicht bereits für EVS 2017 geschehen)
# world <- map_data("world")

# Daten auf Länderebene aggregieren
pol_country_evs_2008 <- pol_evs_2008 %>% 
  group_by(country) %>% 
  summarize_all(mean, na.rm = T)

# Welche Ländernamen unterscheiden sich zwischen den Karten- und EVS-Daten?
pol_country_evs_2008 %>% anti_join(world, by = c("country" = "region"))

View(unique(world$region))

pol_country_evs_2008 <- pol_country_evs_2008 %>% 
  mutate(country = recode(country, `Bosnia Herzegovina` = 'Bosnia and Herzegovina',
                          `Slovak Republic` = 'Slovakia',
                          `Russian Federation` = 'Russia',
                          `Great Britain` = 'UK'))

world <- world %>%
  mutate(region = ifelse(region == "UK" & subregion == "Northern Ireland", "Northern Ireland", region))

# Karten- und EVS-Daten verbinden
evs_map_2008 <- world %>% 
  left_join(pol_country_evs_2008, by = c("region" = "country"))

# Karte zu Vertrauen in EU plotten
ggplot(data = evs_map_2008, aes(x = long, y = lat, group = group, fill = v214)) + 
  geom_polygon(color = "white") + 
  xlim(-25, 47.0) + ylim(35, 70) +
  theme_map() +
  theme(plot.title = element_text(face="bold", size = 16)) +
  labs(title = "Vertrauen in die EU 2008",
       subtitle = "1 = Überhaupt kein Vertrauen, 4 = Sehr viel Vertrauen",
       fill = "",
       caption = "Quelle: European Values Study 2008: Integrated Dataset (EVS 2008). GESIS Datenarchiv, Köln. ZA4800 Datenfile Version 4.0.0, doi:10.4232/1.12458") +
  scale_fill_distiller(breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4), palette = "RdYlGn", direction = 1, limits = c(1,4))

# Plot exportieren
ggsave("./plots/vertrauen_eu_evs_2008.png")

# Karte zu Vertrauen in Regierung plotten
ggplot(data = evs_map_2008, aes(x = long, y = lat, group = group, fill = v222)) + 
  geom_polygon(color = "white") + 
  xlim(-25, 47.0) + ylim(35, 70) +
  theme_map() +
  theme(plot.title = element_text(face="bold", size = 16)) +
  labs(title = "Vertrauen in die Regierung 2008",
       subtitle = "1 = Überhaupt kein Vertrauen, 4 = Sehr viel Vertrauen",
       fill = "",
       caption = "Quelle:European Values Study 2008: Integrated Dataset (EVS 2008). GESIS Datenarchiv, Köln. ZA4800 Datenfile Version 4.0.0, doi:10.4232/1.12458") +
  scale_fill_distiller(breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4), palette = "RdYlGn", direction = 1, limits = c(1,4))

# Plot exportieren
ggsave("./plots/vertrauen_regierung_evs_2017.png")
