################################################################################
# Tidy Data                                                                    #
# Umstrukturierung nicht-idealer Datenformate                                  #
################################################################################

# 00: Aufräumen ----------------------------------------------------------------
rm(list = ls(all.names = TRUE))

# 01: Laden der zusätzlichen Pakete --------------------------------------------
library(tidyr)
library(dplyr)
search()

# 02: Laden der Daten ----------------------------------------------------------
dat <- read.csv("Data/preg.csv")
dat


# 03: Vom Wide zum Long-Format (gather) ----------------------------------------
datTidy <- dat %>% 
  gather(key = Behandlung, value = Anzahl, treatmenta, treatmentb) %>% 
  mutate(Behandlung = gsub("treatment", "", Behandlung))
datTidy

# 04: Vom Long- zum Wide-Format (spread) ---------------------------------------
weather <- read.csv("Data/weather2.csv")
head(weather)

weather %>% 
  spread(key = element, value = value) %>% 
  head

# 05: Problem mehrerer Informationen in einer Zelle ----------------------------
tb <- read.csv("Data/tb2.csv")

tail(tb)
head(tb)
table(tb$demo)

tbTidy <- tb %>% 
  separate(demo, c("geschl", "alter"), 1)
head(tbTidy)

# 06: Trennung von Informationen unterschiedlicher Granularität ----------------
billboard <- read.csv("Data/billboard2.csv")
head(billboard)
View(billboard)

songInfo <- billboard %>% 
  select(artist, track, time, date) %>% 
  distinct  # alternative: unique

rankInfo <- billboard %>% 
  select(artist, track, week, rank) %>% 
  arrange(artist, track, week)
head(rankInfo)


songInfo2 <- billboard %>% 
  select(artist, track, time, date) %>% 
  unique %>% # alternative: dplyr::distinct
  mutate(songId = 1:n())  

rankInfo2 <- billboard %>% 
  left_join(songInfo2, 
            by = c("artist", "track", "time", "date")) %>% 
  select(songId, week, rank)
  
head(rankInfo2)
