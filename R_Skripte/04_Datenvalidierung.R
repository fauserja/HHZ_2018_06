################################################################################
# Datenvalidierung - Schulung am HHZ                                           #
# Datum: 5.6.18                                                                #
#                                                                              #
# Autor: Steffen Wagner                                                        #
# Email: steffen.wagner@inwt-statistics.de                                     #
################################################################################
rm(list = ls(all.names = TRUE))


# 01: Laden der Libraries ------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

# 02: Einlesen der Daten -------------------------------------------------------
dat <- read.csv("Data/bookSales.csv", stringsAsFactors = FALSE)
str(dat)
table(dat$store)

# 03: Datenaufbereitung --------------------------------------------------------
dat <- dat %>% 
  mutate(store = factor(store),
         date = as.Date(date, format = "%Y-%m-%d"),
         weekDay = factor(weekDay,
                          levels = c(1:6, 0),
                          labels = c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")),
         holidayState = factor(holidayState, levels = 0:1, labels = c("nein", "ja")),
         holidaySchool = factor(holidaySchool, levels = 0:1, labels = c("nein", "ja")),
         open = factor(open, levels = 0:1, labels = c("nein", "ja")),
         promo = factor(promo, levels = 0:1, labels = c("nein", "ja")))

# 04: Deskr. Analyse -----------------------------------------------------------
# 04a: kategoriale Variablen ---------------------------------------------------
summary(dat)

# Kategoriale Variablen: Häufigkeiten
table(dat$holidayState)
table(dat$holidayState)

# Bivariater Zusammenhang zwiwchen kat. Variablen
table(dat$holidayState, dat$holidaySchool, dnn = c("staatl.", "schul."))

# relative Häufigkeiten: prop.table
table(dat$holidayState, 
      dat$holidaySchool, 
      dnn = c("staatl.", "schul.")) %>% 
  prop.table() %>% 
  round(digits = 3)

# relative Häufigkeiten: zeile- oder spaltenweise
table(dat$holidayState, 
      dat$holidaySchool, 
      dnn = c("staatl.", "schul.")) %>% 
  prop.table(margin = 1) %>% 
  round(digits = 3)

# 04b: metrischen Variablen ----------------------------------------------------
# z.B.: customer
# Lageparameter
summary(dat$customer)
min(dat$customer)
max(dat$customer)
mean()
median()
quantile(dat$customer, probs = 0:10/10)


# 05: Visualiserung ------------------------------------------------------------
# zur Auswahl geeigneter Diagramm-Typen: 
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

# customers
ggplot(dat, aes(x = customer)) +
  geom_histogram(bins = 40)

ggplot(dat, aes(x = customer, col = open, fill = open)) +
  geom_histogram(bins = 40)

ggplot(dat, aes(x = customer, col = open, fill = open)) +
  geom_histogram(bins = 40) +
  facet_grid(open ~ .)

ggplot(dat, aes(x = customer, col = open, fill = open)) +
  geom_histogram(bins = 40) +
  facet_grid(open ~ holidayState)

# sales 
dat %>% 
  ggplot(aes(x = sales)) +
  geom_histogram() +
  facet_grid(open ~ (customer > 0))

# Validierungsschritt: Identifikation unplausibler Datenpunkte
# - geschäft zu, aber Umsätze
# - geschäft zu, aber kunden

datUnplausibel <- dat %>% 
  filter(open == "nein" & (sales > 0 | customer > 0))

# Bereinigung des Datensatzes um unplausible Daten
dat <- dat %>% 
  anti_join(datUnplausibel,
            by = c("store", "date"))

# Zusammenhang zwischen zwei metrischen Variablen
# - deskriptiv: Korrelation
cor(dat$sales, dat$customer, method = "spearman")

# Scatterplot: nur bei überschaubarer Anzahl von Beobachtungen sinnvoll:
# dat %>% 
#   ggplot(aes(x = customer, y = sales)) +
#   geom_point()

dat %>% 
  ggplot(aes(x = customer, y = sales)) +
  geom_bin2d()

# Zusammenhang: kategorial vs metrisch
dat %>% 
  ggplot(aes(x = weekDay, y = customer, fill = weekDay)) +
  geom_boxplot() +
  facet_grid(open ~ .)

# Grafik: Kunden bei geschlossenem Geschäft ist irrelevant -> raus
dat %>% 
  filter(open == "ja") %>% 
  ggplot(aes(x = weekDay, y = customer, fill = weekDay)) +
  geom_boxplot() +
  facet_grid(open ~ .)

# abgeleitete Größe: mittl. Umsatz/Kunde/WTag
dat %>% 
  filter(open == "ja") %>% 
  group_by(weekDay) %>% 
  summarise(sales = sum(sales),
            customer = sum(customer)) %>% 
  ungroup %>% 
  mutate(salesPerCustomer = sales/customer)
  
# Promo: welchen Einfluss hat die Promo?
dat %>% 
  filter(customer > 0) %>%  # UM warnings() zu vermeiden
  mutate(salesPerCustomer = sales/customer) %>% 
  ggplot(aes(x = promo, y = salesPerCustomer, fill = weekDay)) +
  geom_boxplot() +
  facet_grid(. ~ weekDay) +
  ylim(0, 7)

dat %>% 
  filter(customer > 0) %>%  # UM warnings() zu vermeiden
  mutate(salesPerCustomer = sales/customer) %>% 
  ggplot(aes(x = promo, y = salesPerCustomer, fill = weekDay)) +
  geom_boxplot() +
  facet_grid(. ~ weekDay) +
  coord_cartesian(ylim = c(0, 7))
  

# Zeitreihen-Visualisierung 
dat %>% 
  group_by(date) %>% 
  summarise(sales = sum(sales),
            customer = sum(customer)) %>% 
  ggplot(aes(x = date, y = sales)) +
  geom_line()

# Grafik ist viel zu granular
dat %>% 
  mutate(jahr = format(date, "%Y"),
         monat = format(date, "%m"),
         jahrMonat = format(date, "%Y-%m")) %>% 
  group_by(jahr, monat) %>% 
  summarise(sales = sum(sales),
            customer = sum(customer)) %>% 
  ggplot(aes(x = monat, y = sales, group = jahr, col = jahr)) +
  geom_line()


dat %>% 
  mutate(jahr = format(date, "%Y"),
         monat = format(date, "%m"),
         # jahrMonat muss in Datum umgewandelt werden, damit zeitreihen-Plot funktioniert
         jahrMonat = format(date, "%Y-%m-01") %>% as.Date) %>% 
  group_by(jahrMonat) %>% 
  summarise(sales = sum(sales),
            customer = sum(customer)) %>% 
  ggplot(aes(x = jahrMonat, y = sales)) +
  geom_line()


dat %>% 
  mutate(jahr = format(date, "%Y"),
         monat = format(date, "%m"),
         # jahrMonat muss in Datum umgewandelt werden, damit zeitreihen-Plot funktioniert
         jahrMonat = format(date, "%Y-%m-01") %>% as.Date) %>% 
  group_by(jahrMonat) %>% 
  summarise(sales = sum(sales),
            customer = sum(customer)) %>% 
  gather(key = "Typ", value = "Summe", -jahrMonat) %>% 
  ggplot(aes(x = jahrMonat, y = Summe, col = Typ)) +
  geom_line()

# 06: Exportieren --------------------------------------------------------------
pdf(file = "Grafiken/UmsaetzeMonatlich.pdf",
    width = 7, height = 5)
dat %>% 
  mutate(jahr = format(date, "%Y"),
         monat = format(date, "%m"),
         # jahrMonat muss in Datum umgewandelt werden, damit zeitreihen-Plot funktioniert
         jahrMonat = format(date, "%Y-%m-01") %>% as.Date) %>% 
  group_by(jahrMonat) %>% 
  summarise(sales = sum(sales),
            customer = sum(customer)) %>% 
  ggplot(aes(x = jahrMonat, y = sales)) +
  geom_line()
dev.off()

# weitere Grafikformate
?png  # und die in der Hilfe genannten Funktionen: bitmap
?windows # windows Mteafile.


