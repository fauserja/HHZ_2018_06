################################################################################
# Datenmanagement in R                                                         #
# 20.2.2018                                                                    #
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

#03: Auswahl bestimmter Spalten --------------------------
select(dat, date, users, gender)
dat[ , c("date", "users", "gender")]

# 04: Auswahl bestimmer Zeilen:
slice(dat, 1:10)
dat[1:10, ]
filter(dat, gender == 0)

# 05: Erzeugung neuer Variablen
mutate(dat, 
       date = as.Date(as.character(date), 
                            format = "%Y%m%d"),
       gender = factor(gender, levels = 0:1, labels = c("m", "w"))
       )

# 06: Zusammenfassen: summarise
summarise(dat, meanBounces = mean(bounces))

# 07: Sortierung
dat %>% arrange(sessions) %>% slice(1:10) # aufsteigend
dat %>% arrange(desc(sessions)) %>% slice(1:10) # absteigend

# 08: Chain Operator %>% 
datFinal <- dat %>% 
  select(date, users, gender) %>% 
  mutate(gender =  factor(gender, levels = 0:1, labels = c("m", "w")),
         date = as.Date(as.character(date), 
                        format = "%Y%m%d")) %>% 
  filter(date > as.Date("2014-09-17")) %>% 
  group_by(gender) %>% 
  summarise(n = n(),
            meanUsers = mean(users))


#09: Reshape von Datensätzen ---------------------------------------------------
preg <- read_csv("Data/preg.csv")
preg

# gather: von wide -> long-Format 
preg %>% 
  gather(key = "Treatment", 
         value = "Value", treatmenta, treatmentb) %>% 
  mutate(Treatment = gsub(pattern = "treatment", 
                          replacement = "", 
                          Treatment))

# spread: von long -> wide
head(weather2)
tail(weather2)
weather2 %>% slice(1:6)

weather2$element[1] <- "tmean"
head(weather2)

weather2 <- weather2 %>%
  spread(key = element, value = value) %>%
  # filter(!is.na(tmean)) %>%
  mutate(day = gsub("d", "", day),
         datum = paste(year, month, day, sep = "-"),
         datum = as.Date(datum))


as.Date()


# TRennung von INfomration in derselben Spalten
tb2 <- read_csv("Data/tb2.csv")
table(tb2$demo)

tb2 %>% 
  separate(demo, c("geschl", "age"), 1) %>% 
  head



# Separation von unterschiedlichen Informationen
billboard2 <- read_csv("Data/billboard2.csv")

songinfo <- billboard2 %>% 
  select(artist, track, time, date) %>% 
  distinct()  # Verdichtung, so dass jede Kombination genau
              # einmal auftritt

# Anlegen einer ID-Variablen
songinfo <- songinfo %>% 
  mutate(songID = 1:n())
songinfo$songID[1:20]

# Erstellen der Billboard-Rankings
ranking <- billboard2 %>% 
  select(artist, track, week, rank) %>% 
  arrange(artist, track, week)
head(ranking)

left_join(ranking, 
          songinfo %>% 
            select(track, artist, songID),
          by = c("track", "artist")) %>% 
  head


left_join(ranking %>% 
            rename(trackNew = track, artistNew = artist) , 
          songinfo %>% 
            select(track, artist, songID),
          by = c("trackNew" = "track", "artistNew" = "artist")) %>%
  select(songID, week, rank) %>% 
  head

ranking %>% 
  rename(trackNew = track, artistNew = artist) %>% 
  head










