################################################################################
# Datenimport - Schulung am HHZ                                                #
# Datum: 5.6.18                                                                #
#                                                                              #
# Autor: Steffen Wagner                                                        #
# Email: steffen.wagner@inwt-statistics.de                                     #
################################################################################
rm(list = ls(all.names = TRUE))

# 01: Laden der Libraries ------------------------------------------------------
library(readr)
library(dplyr)
library(readxl)
library(DBI)


# 02: csv/txt ------------------------------------------------------------------
readLines("Data/Analytics.csv", n = 3)
# read.table klassische Alternative zu dplyr::read_delim
analytics <- read_delim("Data/Analytics.csv", delim = ",")

analytics <- read_delim("Data/Analytics.csv", 
                        delim = ",",
                        col_types = cols(
                          date = col_date("%Y%m%d"),
                          users = col_integer(),
                          newUsers = col_integer(),
                          sessions = col_integer(),
                          bounces = col_integer(),
                          sessionDuration = col_integer(),
                          pageviews = col_integer(),
                          avgTimeOnPage = col_double(),
                          userGender = col_factor(levels = c("female", "male")),
                          bouncesHigh = col_factor(levels = c(">=10 bounces",
                                                              "<10 bounces"))
                        ))

# Warum ist die explizite Typ-Zuweisung notwendig?
readLines("Data/longInteger.txt")
longIntegers <- read_delim("Data/longInteger.txt", 
                           delim = ";",
                           col_types = cols(ckid = col_double(),
                                           ckidChar = col_character()))
longIntegers
longIntegers %>% 
  mutate(ckid = as.character(ckid))

# Learning: Vorsicht bei langen Integer 

# Einlesen ausgewählter Spalten (beschleunigt Import-Prozess)
# Ersetzung von Funktion 'cols' durch `cols_only`
analytics2 <- read_delim("Data/Analytics.csv", 
                        delim = ",",
                        col_types = cols_only(
                          date = col_date("%Y%m%d"),
                          users = col_integer(),
                          # newUsers = col_integer(),
                          # sessions = col_integer(),
                          # bounces = col_integer(),
                          # sessionDuration = col_integer(),
                          # pageviews = col_integer(),
                          avgTimeOnPage = col_double(),
                          userGender = col_factor(levels = c("female", "male")),
                          bouncesHigh = col_factor(levels = c(">=10 bounces",
                                                              "<10 bounces"))
                        ))

# Erstellug eigener Metrik 'usersHigh'
analytics2$users %>% 
  table %>% 
  prop.table() %>% 
  cumsum


analytics2 <- analytics2 %>% 
  mutate(usersHigh = factor(users >= 13, 
                            levels = c(TRUE, FALSE),
                            labels = c(">=13 users",
                                       "<13 users")))

analytics2 <- analytics2 %>% 
  mutate(users = pmin(users, 20),
         usersHigh = users >= 13,
         usersHigh = factor(usersHigh,
                            levels = c(TRUE, FALSE),
                            labels = c(">=13 users",
                                       "<13 users")))

# Analoge Spezifikation bei `read.table` über Argument 'colClasses' möglich


# Encoding: 
readLines("Data/encodingLatin1.txt")
read_delim("Data/encodingLatin1.txt", 
           delim = "\t",
           locale = locale(encoding = "latin1"))
# read.table(fileEncoding = )

# 03: Excel-Dateien ------------------------------------------------------------
analyticsXLS <- read_excel("Data/analytics.xlsx",
                           sheet = 1,
                           col_types = c("text", 
                                         "text",
                                         rep("numeric", 8)))



# 04: SQL Verbindung -----------------------------------------------------------
# MS T-SQL -> ODBC -> Paket `RODBC`
# MySQL -> Paket `RMySql`
# Basis: Paket 'DBI'


# Aufbau der Datenverbindung:
con <- dbConnect(RSQLite::SQLite(), "")
sqlStatement <- "CREATE TABLE Analytics
                   (id INTEGER PRIMARY KEY, -- Autoincrement
                   date TEXT,
                   userGender VARCHAR(10),
                   users INT,
                  newUSers INT,
                  sessions INT,
                  bounces INT,
                  sessionDuration INT,
                  pageviews INT,
                  avgTimeOnPage DOUBLE,
                  bouncesHigh VARCHAR(15))"
dbExecute(con, sqlStatement)
dbListTables(con)
dbListFields(con, "Analytics")
dbReadTable(con, "Analytics")

# Befüllen der Tabelle:
dbWriteTable(con, "Analytics", analytics %>% rename(newUSers = newUsers), 
             append = TRUE, 
             overwrite = FALSE, 
             row.names = FALSE)

dbReadTable(con, "Analytics")

dbGetQuery(con, "SELECT * FROM Analytics WHERE pageviews < 15")

# Dynamisch angepasstes Query:
genderOfInterest <- "female" 
query <- paste("SELECT *, CAST(ckid as CHAR) as ckidChar FROM Analytics WHERE userGender = '", 
               genderOfInterest, "'", sep = "")
query
dbGetQuery(con, query)

tmp <- dbSendQuery(con, query)
fetch(tmp, n = 10)
dbClearResult(tmp)

# Robustifizierung gegenüber SQL Injections
sqlQuery <- "SELECT * FROM Analytics where userGender = :x"
dbGetQuery(con, 
           sqlQuery,
           param = list(x = genderOfInterest))

# mehrere Bedingungen:
sqlQuery <- "SELECT * FROM Analytics where userGender = :x AND bounces >= :y"
dbGetQuery(con, 
           sqlQuery,
           param = list(x = genderOfInterest,
                        y = 10))

# mehrere Sets an Bedingungen:
genderOfInterest <- c("female", "male")
bounceKriterium <- c(10, 10)  # rep(lenth(genderOfINterest), 10)
sqlQuery <- "SELECT * FROM Analytics where userGender = :x AND bounces >= :y"
dat <- dbGetQuery(con, 
           sqlQuery,
           param = list(x = genderOfInterest,
                        y = bounceKriterium))
table(dat$userGender, dat$bounces)

dbDisconnect(con)
