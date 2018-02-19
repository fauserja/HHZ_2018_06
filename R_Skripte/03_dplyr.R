################################################################################
# Datenmanagement mit `dplyr`                                                  #
# 20./21. Februar 2018                                                         #
################################################################################

# 00: Löschen des Workspace ----------------------------------------------------
rm(list ) ls(all.names = TRUE)

## 01a: Laden zusätzlicher Pakete ----------------------------------------------
library(dplyr)

# 01: Einlesen der Dateien -----------------------------------------------------
dat <- read.csv(file = "Data/Analytics.csv")

# 02: Auswahl einzelner Spalten ------------------------------------------------
select(dat, date, users, userGender)

# 03: Auswahl einzelner Zeilen -------------------------------------------------
slice(dat, 1:10)
filter(dat, userGender == "female", users > 15)

# 04: Sortieren des Datensatzes ------------------------------------------------
slice(arrange(dat, desc(users)), 1:10)

# 05: Erzeugung neuer Variablen (zeilenweise pro U.E.) -------------------------
mutate(dat, avgSessionDuration = sessionDuration/sessions)

# 06: Aggregationsfunktion -----------------------------------------------------
summarise(dat, meanSessions = mean(sessions))

# 07: Chaining -----------------------------------------------------------------
dat %>% 
  select(date, users, userGender) %>% 
  slice(1:10) %>% 
  arrange(desc(users)) %>% 
  group_by(userGender) %>% 
  summarise(meanUsers = mean(users))

# 08: Gruppierung und gruppenspezifische Berechnungen --------------------------
dat %>% 
  group_by(userGender) %>%
  mutate(meanUsers = mean(users),
         usersDeMeaned = users - meanUsers) %>% 
  select(date, users, userGender, usersDeMeaned, meanUsers) %>% 
  slice(1:5) %>% 
  View
  