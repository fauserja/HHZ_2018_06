################################################################################
# Datenmanagement in R                                                         #
# Juni 2018                                                                    #
# Autor: Steffen Wagner                                                        #
# Email: steffen.wagner@inwt-statistics.de                                     #
################################################################################
rm(list = ls(all.names = TRUE))

# 01: Laden der benötigten Pakete ----------------------------------------------
library(dplyr)
library(tidyr)
library(readr)


#02: Laden der Daten ----------------------------------------------------------
readLines("Data/AnalyticsRaw.csv", n = 5)
dat <- read.table(file = "Data/AnalyticsRaw.csv", 
                  header = TRUE, 
                  sep = ",",
                  dec = ".")
View(dat)
str(dat)

#03: Datenmanagement mi `dplyr` ------------------------------------------------

#03a: Auswahl bestimmter Spalten -----------------------------------------------
select(dat, date, users, gender)      # dplyr syntax
dat[ , c("date", "users", "gender")]  # klassisches Subsetting

# 03b: Auswahl bestimmer Zeilen: -----------------------------------------------
slice(dat, 1:10)  # dplyr syntax
dat[1:10, ]       # klassisches positional Subsetting
filter(dat, gender == 0)  # dplyr: logical Subsetting

# 03c: Erzeugung neuer Variablen -----------------------------------------------
mutate(dat, 
       date = as.Date(as.character(date), 
                            format = "%Y%m%d"),
       gender = factor(gender, levels = 0:1, labels = c("m", "w"))
       )

# 03d: Aggregation: summarise --------------------------------------------------
summarise(dat, meanBounces = mean(bounces))

# 03e: Sortierung --------------------------------------------------------------
dat %>% arrange(sessions) %>% slice(1:10) # aufsteigend
dat %>% arrange(desc(sessions)) %>% slice(1:10) # absteigend

# 03f: Chain Operator %>% ------------------------------------------------------
datFinal <- dat %>% 
  select(date, users, gender) %>% 
  mutate(gender =  factor(gender, levels = 0:1, labels = c("m", "w")),
         date = as.Date(as.character(date), 
                        format = "%Y%m%d")) %>% 
  filter(date > as.Date("2014-09-17")) %>% 
  group_by(gender) %>% 
  summarise(n = n(),
            meanUsers = mean(users))


#04: Umstrukturierung von Datensätzen (Reshaping) ------------------------------

# Laden der Daten
preg <- read_csv("Data/preg.csv")
preg

# Die nachfolgend verwendeten Funktion sind aus dem Paket `tidyr`, das bereits 
# im Abschnitt `01: Laden der Pakete` geladen wurde.

#04a: von wide -> long-Format (gather) -----------------------------------------
preg %>% 
  gather(key = "Treatment", 
         value = "Value", treatmenta, treatmentb) %>% 
  mutate(Treatment = gsub(pattern = "treatment", 
                          replacement = "", 
                          Treatment))

#04b: von long -> wide (spread) ------------------------------------------------
weather2 <- read_csv("Data/weather2.csv")
head(weather2)
tail(weather2)
weather2 %>% slice(1:6)

# Anlegen einer zusätzlichen Ausprägung in Spalte `element`, so dass wide Format
# drei Spalten mit Messwerten enthält
weather2$element[1] <- "tmean"
head(weather2)

weather2 <- weather2 %>%
  spread(key = element, value = value) %>%
  # filter(!is.na(tmean)) %>%
  mutate(day = gsub("d", "", day),
         datum = paste(year, month, day, sep = "-"),
         datum = as.Date(datum))

head(weather2)



#04c: Trennung von multipler Information in einer Spalte -----------------------
tb2 <- read_csv("Data/tb2.csv")
table(tb2$demo)  # Häufigkeitstabelle

tb2 %>% 
  separate(demo, c("geschl", "age"), 1) %>% 
  head



#04d: Separation von unterschiedlichen Informationen ---------------------------
billboard2 <- read_csv("Data/billboard2.csv")

# Die Datei enthält Stammdaten und Zeitreihendaten, die eigentlich getrennt
# organisiert sein sollten.

# Stammdaten
songinfo <- billboard2 %>% 
  select(artist, track, time, date) %>% 
  distinct()  # Verdichtung, so dass jede Kombination genau
              # einmal auftritt

# Anlegen einer ID-Variablen
songinfo <- songinfo %>% 
  mutate(songID = 1:n())
songinfo$songID[1:20]

# Erstellen der Billboard-Rankings (Zeitreihe)
ranking <- billboard2 %>% 
  select(artist, track, week, rank) %>% 
  arrange(artist, track, week)
head(ranking)


# Zusammenführen der unterschiedlichen Datensätze, um ID-Info in Zeitreihen
# anzulegen (Funktion `left_join` aus Paket `dplyr`)
left_join(ranking, 
          songinfo %>% 
            select(track, artist, songID),
          by = c("track", "artist")) %>% 
  head

# Beispiel: Join bei unterschiedlichen Spaltennamen:
left_join(ranking %>% 
            rename(trackNew = track, artistNew = artist) , 
          songinfo %>% 
            select(track, artist, songID),
          by = c("trackNew" = "track", "artistNew" = "artist")) %>%
  select(songID, week, rank) %>% 
  head


# Die Funktion `rename` findet sich ebefnalls im Paket `dplyr`
ranking %>% 
  rename(trackNew = track, artistNew = artist) %>% 
  head










