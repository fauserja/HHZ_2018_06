################################################################################
# Deskriptive Datenanlyse / Datenvalidierung                                   #
# 21.2.18                                                                      #
################################################################################
rm(list = ls(all.names = TRUE))

# 00: Laden der benötigten Pakete ----------------------------------------------
library(dplyr)
library(ggplot2)


# 01: Einlesen der Daten -------------------------------------------------------
dat <- read.csv("Data/bookSales.csv", stringsAsFactors = FALSE)
str(dat)
dat$customer[1:10]

# 02: Datenaufbereitung --------------------------------------------------------
dat <- dat %>% 
  mutate(store = factor(store),
         date = as.Date(date),
         weekDay = factor(weekDay,
                          levels = c(1:6, 0),
                          labels = c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")),
         holidayState = factor(holidayState, 
                               levels = 0:1,
                               labels = c("nein", "ja")),
         holidaySchool = factor(holidaySchool, 
                                levels = 0:1,
                                labels = c("nein", "ja")),
         open = factor(open, 
                       levels = 0:1,
                       labels = c("nein", "ja")),
         promo = factor(promo, 
                        levels = 0:1,
                        labels = c("nein", "ja")),
         customer = as.integer(customer))
  
  
# 03: Deskriptive Analyse ------------------------------------------------------
summary(dat)

# Häufigkeitstabelle bei kategorialen Variablen
table(dat$holidayState)
table(dat$holidaySchool)
# bivariate Betrachtung
table(dat$holidayState, 
      dat$holidaySchool, 
      dnn = c("state", "school"))

# relative Häufigkeiten
table(dat$holidayState, 
      dat$holidaySchool, 
      dnn = c("state", "school")) %>% 
  prop.table() %>% 
  round(digits = 2)

# relative Häufigkeiten; spalten- oder zeilenweise
table(dat$holidayState, 
      dat$holidaySchool, 
      dnn = c("state", "school")) %>% 
  prop.table(margin = 1) %>% 
  round(digits = 2)


table(dat$holidayState, 
      dat$holidaySchool, 
      dnn = c("state", "school")) %>% 
  prop.table() %>% 
  addmargins() %>% 
  round(digits = 2)

# Metrische Variable
summary(dat$customer)
min(dat$customer)
max(dat$customer)
range(dat$customer)

median(dat$customer)
mean(dat$customer)

quantile(dat$customer)
quantile(dat$customer, probs = 0:10/10)

# Visualisierung einer mezttrischen Variablen:
ggplot(dat, aes(x = 1, y = customer)) +
  geom_boxplot()

ggplot(dat, aes(x = customer)) +
  geom_histogram() +
  facet_grid(open ~ promo)


ggplot(dat, aes(x = sales)) +
  geom_histogram() +
  facet_grid(open ~ promo)

ggplot(dat, aes(x = sales)) +
  geom_histogram() +
  facet_grid(open ~ (customer > 0))




# Entfernen der unplausiblen Datenpunkte 
datUnplausibel <- dat %>% 
  filter(open == "nein" & (sales > 0 | customer > 0))

nrow(dat)  # Fallzahl vor dem `anti_join`

dat <- dat %>% 
  anti_join(datUnplausibel,
            by = c("store", "date"))

# Fallzahlen nach dem 'ant_join'
nrow(dat) + nrow(datUnplausibel)

# Visualisierung zweier metrischer Variablen:
# prinzipiell: Scatterplot geom_point
# aber: bei großen Fallzahlen ungeeignet
dat %>%
  ggplot(aes(x = customer, y = sales)) +
  geom_bin2d() +
  facet_grid(open ~ .)


# Zusammenhang kategoriale vs Metrische Variable
dat %>% 
  ggplot(aes(x = weekDay, y = customer, fill = weekDay)) +
  geom_boxplot() +
  facet_grid(open ~ .)

dat %>% 
  filter(open == "ja") %>% 
  ggplot(aes(x = weekDay, y = customer, fill = weekDay)) +
  geom_boxplot() 

# abgeleitete Metrik: mittlerer UMsatz/Kunde/Wochentag
dat %>% 
  group_by(weekDay) %>% 
  summarise(sales = sum(sales),
            customer = sum(customer)) %>% 
  mutate(salesPerCustomer = sales/customer)


# Einfluss der Promotion
dat %>% 
  mutate(salesPerCustomer = sales/customer) %>% 
  ggplot(aes(x = promo, y = salesPerCustomer, fill = weekDay)) +
  geom_boxplot() +
  facet_grid(. ~ weekDay)


# Zeitliche Betrachtung der Sales
dat %>% 
  group_by(date) %>% 
  summarise(sales = sum(sales),
            customer = sum(customer)) %>% 
  ggplot(aes(x = date, y = sales)) +
  geom_line()

dat %>% 
  mutate(monat = format(date, format = "%m"),
         jahr = format(date, format = "%Y")) %>% 
  group_by(jahr, monat) %>% 
  summarise(sales = sum(sales),
            customer = sum(customer)) %>% 
  ggplot(aes(x = monat, y = sales, col = jahr, group = jahr)) +
  geom_line() +
  facet_grid(jahr ~ .)



dat %>% 
  group_by(weekDay, open) %>% 
  summarise(n = n()) %>% 
  group_by(weekDay) %>% 
  mutate(n_ges = sum(n)) %>% 
  ungroup %>% 
  mutate(ratio = n/n_ges * 100) %>% 
  ggplot(aes(x = weekDay, y = ratio,  fill = open)) +
  geom_bar(stat = "identity")



table(dat$weekDay, dat$open, dnn = c("weekDay", "open")) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame()












