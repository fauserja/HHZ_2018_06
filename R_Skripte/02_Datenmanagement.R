################################################################################
# Datenmagagement - Schulung am HHZ                                            #
# Datum: 4.6.18                                                                #
#                                                                              #
# Autor: Steffen Wagner                                                        #
# Email: steffen.wagner@inwt-statistics.de                                     #
################################################################################
rm(list = ls(all.names = TRUE))


# 01: Laden der benötigten Pakete ----------------------------------------------
library(dplyr)
library(readr)
library(tidyr)
search()

# 02: Laden der Daten ----------------------------------------------------------
readLines("Data/AnalyticsRaw.csv", n = 5)
dat <- read.table("Data/AnalyticsRaw.csv", 
                  header = TRUE, 
                  sep = ",", 
                  dec = ".")
View(dat)


# 03: Datenmanagement mit dplyr ------------------------------------------------

# 03a: Auswahl bestimmter Spalten: ---------------------------------------------
View(dat[ , c("date", "gender", "pageviews")])
select(dat, date, gender, pageviews)

# 03b: Auswahl bestimmter Zeilen -----------------------------------------------
# positional
dat[ 1:10, ]
slice(dat, 1:10)

# logisches
dat$gender == 0
dat[dat$gender == 0, ]
filter(dat, gender == 0)

# 03c: Erzeugung neuer Metriken ------------------------------------------------
head(dat)
tail(dat)
mutate(dat, 
       date = as.Date(as.character(date), format = "%Y%m%d"),
       gender = factor(gender, levels = 0:2, labels = c("female", "male", "neutral")),
       newUserClass = cut(newUsers, breaks = c(0, 10, 20, Inf)))

# Exkurs: Anlegen von Date-Objekte
as.Date(as.character(dat$date), format = "%Y%m%d")
# Hilfe zu den Zeit-Formaten: ?strptime


# 03d: Aggregatsfunktion -------------------------------------------------------
summarise(dat, meanBounce = mean(bounces))

# 03e: Sortierung --------------------------------------------------------------
arrange(dat, bounces) # aufsteigend sortiert
arrange(dat, desc(bounces))


# Syntac der dplyr-Vokabeln: 
# - funktion(datensatz, argumente für die jeweilige Vokabel)
# - Rückgabewert: immer data.frame


# 03f: Chain Operator: %>% 
# x %>% f(y, ...)  = f(x, y, ...)
dat %>% 
  select(sessionDuration, newUsers) %>% 
  arrange(newUsers) %>% 
  slice(1:10)

# 03g: Gruppierung -------------------------------------------------------------
# Gruppenweise Filtern
dat %>% 
  mutate(gender = factor(gender, 
                         levels = 0:2, 
                         labels = c("female", "male", "neutral"))) %>% 
  group_by(gender) %>% 
  arrange(desc(bounces)) %>% 
  slice(1:5) %>% 
  View

# Gruppenspezifische Aggregate
dat %>% 
  mutate(gender = factor(gender, 
                         levels = 0:2, 
                         labels = c("female", "male", "neutral"))) %>% 
  group_by(gender) %>% 
  summarise(meanBounces = mean(bounces))

# Gruppenweise neue Metriken:
dat <- dat %>% 
  mutate(gender = factor(gender, 
                         levels = 0:2, 
                         labels = c("female", "male", "neutral"))) %>% 
  group_by(gender) %>% 
  mutate(sessionsDeMeaned = sessions - mean(sessions)) %>% 
  ungroup

slice(select(dat, sessionDuration, newUsers), 1:10)

# Beispiel: Pipe-Operator außerhalb Datenkontexts ------------------------------
round(
  prop.table(
    table(dat$gender)
  ), 
  digits = 3)


dat$gender %>%
  table() %>% 
  prop.table() %>% 
  round(digits = 3)



# 04: Umstrukturierung von Datensätzen -----------------------------------------

# 04a: Problem Tabelle im 'wide' soll ins 'long' trasformiert werden -----------
preg <- read_csv("Data/preg.csv")
preg %>% 
  gather(key = "Treatment",
         value = "Messwert",
         treatmenta, treatmentb) %>% 
  mutate(Treatment = gsub("treatment", "", Treatment))
          
# 04b: Problem Tabelle im 'long' soll ins 'wide' trasformiert werden -----------
weather2 <- read_csv("Data/weather2.csv")
head(weather2)
weather2$element[3] <- "tmean"
head(weather2)

weather2 %>% 
  spread(key = "element", value = "value", fill = 0) %>% 
  mutate(date = paste(year, month, day, sep = "-"),
         date = as.Date(date, format = "%Y-%m-d%d"))


# 04c: Separation von unterschiedlichen Informationen --------------------------
billboard2 <- read_csv("Data/billboard2.csv")

# Problem: Vermischung von Stammdaten mit Zeitreihendatren
# Lösung: Trennung der Informationen + gezielte Zusammenführung

# Stammdaten
songInfo <- billboard2 %>% 
  select(artist, track, time, date) %>% 
  distinct() %>% 
  mutate(ID = 1:n())

# Zeitreihen
ranking <- billboard2 %>% 
  left_join(songInfo,
            by = c("artist" = "artist", "track", "time", "date")) %>% 
  select(ID, week, rank)



