################################################################################
# Beispiel: Deskriptive Analyse                                                #
# 20./21. Februar 2018                                                         #
################################################################################

# 00: Leeren des Workspace -----------------------------------------------------
rm(list = ls(all.names = TRUE))

# 00a: Laden der Pakete --------------------------------------------------------
library(dplyr)
library(ggplot2)


# 01: Einlesen der Daten -------------------------------------------------------
dat <- read.csv("Data/bookSales.csv", stringsAsFactors = FALSE)

# 02: Datenaufbereitung --------------------------------------------------------
# dat$date <- as.Date(dat$date)
# dat$weekDay <- factor(dat$weekDay, 
#                       levels = 0:6,
#                       labels = c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa"))
# dat$id <- factor(dat$id)
                      
dat <- dat %>% 
  mutate(date = as.Date(date),  
         weekDay = factor(weekDay, 
                          levels = 0:6,
                          labels = c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa")),
         store = factor(store))

str(dat)
# mean, median, min, max, range
summary(dat)


# 03: Deskriptive Statistiken --------------------------------------------------

# Häufigkeitstabellen und Kreuztabellen
table(dat$holidayState)
table(dat$holidaySchool)

table(dat$holidaySchool, dat$holidayState)
table(dat$holidaySchool, dat$holidayState, dnn =c("school", "state"))
with(dat, table(holidaySchool, holidayState, dnn =c("school", "state")))

table(dat$holidaySchool, dat$holidayState, dnn =c("school", "state")) %>% 
  prop.table(margin = 1)

table(dat$holidaySchool, dat$holidayState, dnn =c("school", "state")) %>% 
  prop.table(margin = 2)

# 04: Histogramm bei metrischen Variablen --------------------------------------

# Betrachtung der Verteilung der Kunden
dat %>% 
  ggplot(aes(x = customer)) +
  geom_histogram() +
  coord_cartesian(xlim = c(1, 5000)) +
  xlim(1, 5000)

dat %>% 
  ggplot(aes(x = customer)) +
  geom_histogram() +
  facet_grid(open ~ .)

ggplot(dat,
       aes(x = customer)) +
  geom_histogram(bins = 100) +
  coord_cartesian(xlim = c(1, 5000))

ggplot(dat,
       aes(x = customer)) +
  geom_histogram(bins = 100) +
  xlim(1, 5000)

p <- ggplot(dat,
       aes(x = customer)) +
  geom_histogram(bins = 100)

ggplot(dat,
       aes(x = customer, fill = factor(promo))) +
  geom_histogram(bins = 100) +
  facet_grid(open ~ .)

# Betrachtung der Verteilung der Sales
ggplot(dat,
       aes(x = sales)) +
  geom_histogram(bins = 100) +
  facet_grid(open ~ .)

# 05: Identifikation der auffälligen Beobachtungen -----------------------------
datSpooky <- dat %>% 
  filter((open == 1 & (sales == 0 | customer == 0)) |
         (open == 0 & (sales > 0 | customer > 0)))


# Bereinigung des ursprünglichen Datensatzes um unplausible Einträge
datKorr <- dat %>%
  anti_join(datSpooky,
            by = c("date", "store"))
# Check: Sind die Mengen disjunkt und vollständig?
stopifnot(nrow(dat) == nrow(datSpooky) + nrow(datKorr))

# 06: Zusammenhangsstruktur zwischen metrischen Variablen ----------------------
cor(datKorr$sales, datKorr$customer)
mean(datKorr$sales/datKorr$customer, na.rm = TRUE)
summary(datKorr)

# Visualisierung bei hoher Anzahl von Daten: Dichteplot
# sonst scatter-Plot über Funktion `geom_points`
ggplot(datKorr,
       aes(x = customer, y = sales)) +
  geom_point()

ggplot(datKorr,
       aes(x = customer, y = sales)) +
  geom_bin2d()

# 07: Zusammenhang kategoriale vs metrische Variable ---------------------------
# Einfluss zwischen Wochentag und Umsatz
# Visualiserung des Zusammenhangs zwischen kategorialen und metrischen Variablen

ggplot(datKorr,
       aes(x = weekDay, y = customer)) +
  geom_boxplot()


ggplot(datKorr %>% 
         filter(open == 1),
       aes(x = weekDay, y = customer, fill = weekDay)) +
  geom_boxplot()

ggplot(datKorr %>% 
         filter(open == 1),
       aes(x = weekDay, y = sales, fill = weekDay)) +
  geom_boxplot()

# Anzahl der geöffneten Geschäfte nach Wochentag
table(datKorr$open, datKorr$weekDay) %>% 
  prop.table(margin = 2) %>% 
  round(3)

datDaily <- datKorr %>% 
  group_by(weekDay) %>% 
  summarise(salesSum = sum(sales),
            customerSum = sum(customer)) %>% 
  ungroup %>% 
  mutate(salesPerCustomer = salesSum/customerSum)
datDaily

ggplot(datKorr %>% 
         mutate(salesPerCustomer = sales/customer),
       aes(x = weekDay, y = salesPerCustomer, col = weekDay)) +
  geom_boxplot() +
  facet_grid(. ~ promo)


datKorr %>% 
  mutate(salesPerCustomer = sales/customer) %>% 
  filter(salesPerCustomer > 10)


ggplot(datKorr %>% 
         mutate(salesPerCustomer = sales/customer),
       aes(x = weekDay, y = salesPerCustomer, fill = weekDay)) +
  geom_boxplot() +
  facet_grid(. ~ promo)

ggplot(datKorr %>% 
         mutate(salesPerCustomer = sales/customer,
                weekDayIA = interaction(promo, weekDay)),
       aes(x = weekDayIA, y = salesPerCustomer, col = weekDay)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(5, 10))

# 08: Zeitreihen-Grafik --------------------------------------------------------
# Zeitliche Entwicklung der Umsätze - Saisonalitäten

datKorr %>% 
  group_by(date) %>% 
  summarise(sales = sum(sales),
             customer = sum(customer)) %>% 
  # ungroup %>% 
  ggplot(aes(x = date, y = sales)) +
  geom_line()

format(datKorr$date[1:10], "%Y-%U")

datKorr %>% 
  mutate(year = format(date, "%Y"),
         month = format(date, "%m")) %>% 
  group_by(year, month) %>% 
  summarise(sales = sum(sales),
            customer = sum(customer)) %>%
  ungroup %>% 
  ggplot(aes(x = month, y = sales, color = year, group = year)) +
  geom_point() +
  geom_line() +
  facet_grid(year ~ .)



