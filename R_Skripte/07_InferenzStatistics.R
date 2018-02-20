################################################################################
# Beispiel: Inferenz- Statistik                                                #
# 20./21. Februar 2018                                                         #
################################################################################

# 00: Leeren des Workspace -----------------------------------------------------
rm(list = ls(all.names = TRUE))

# 00a: Laden der Pakete --------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)

# 01: Einlesen der Daten -------------------------------------------------------
dat <- read.csv("Data/Analytics.csv", stringsAsFactors = FALSE)

# 02: Einstichproben-Test (Lageparameter) --------------------------------------
# Zielgröße: `sessionDuration`
# Visualisierung der zu untersuchenden Größe
ggplot(dat,
       aes(x = sessionDuration)) +
  geom_density() + 
  geom_vline(xintercept = 2100) +
  geom_vline(xintercept = c(mean(dat$sessionDuration),
                             median(dat$sessionDuration)), col = "red")
  
# Q: Welche Hypothese (Forschungsebene) soll geprüft werden?
# A: Behauptung: Durchschnittliche `sessionDuration` > 2100s 

# Da nicht normal-verteilt nicht-parametrischer Vorzeichentest
# getesteter Lageparameter: Median

# H0: sessionDuration <= 2100s
# H1: sessionDuration > 2100s

# Ergebnis aus den Daten:

sum(dat$sessionDuration > 2100)  # Fälle mit `Erfolg`
nrow(dat)                        # ANzahl der UE

binom.test(x = sum(dat$sessionDuration > 2100),
           n = nrow(dat),
           alternative = "greater")


# 03: Ein-Stichproben t-Test----------------------------------------------------
# Visualisierung der zu untersuchenden Größe
ggplot(dat,
       aes(x = avgTimeOnPage)) +
  geom_density() +
  geom_vline(xintercept = 180)

qqnorm(dat$avgTimeOnPage)
qqline(dat$avgTimeOnPage) 

# Q: durchschnittliche TimeOnPage ungleich 180s

# Zweiseitiger Test
# H0: mean(avgTimeOnpage) = 180s
# H1: mean(avgTimeOnpage) != 180s
t.test(dat$avgTimeOnPage, 
       mu = 180)

# Einseitiger Test
# H0: mean(avgTimeOnpage) >= 180s
# H1: mean(avgTimeOnpage) < 180s
t.test(dat$avgTimeOnPage, 
       mu = 180,
       alternative = "less")

# 04: Zwei-Stichproben-Tests ---------------------------------------------------

# Test der Lageparameter
# Visualisierung der zu untersuchenden Größe
ggplot(dat,
       aes(x = avgTimeOnPage, col = userGender)) +
  geom_density() 

dat %>% 
  group_by(userGender) %>% 
  summarise(mittelwert = mean(avgTimeOnPage),
            varianz = sd(avgTimeOnPage))

# Test auf Varianzgleichheit: levene-Test
library(car) # Enthält Funktion für Levene-Test
# Das Laden des Paketes gehört eigentlich in Abschnitt `00a`

leveneTest(dat$avgTimeOnPage, dat$userGender)
# Varianzengleichheit kann nicht verworfen werden

# H0: Gleichheit der durchschnittl. Aufenthaltsdauern
# H1: UNtershciede
t.test(avgTimeOnPage ~ userGender, data = dat, var.equal = TRUE)


# Falls Welch-Test bei ungeichen Varianzen
t.test(avgTimeOnPage ~ userGender, data = dat)

# Test auf Normalverteilung
shapiro.test(dat$avgTimeOnPage) 

# Gruppenweiser Test auf Normalverteilung 
dat %>% 
  group_by(userGender) %>% 
  summarise(pWertShapiro = shapiro.test(avgTimeOnPage)$p.value)

# Ergebnis keine Normalverteilung (auch nicht in Untergruppen)
# => Wilcoxon/Mann-Whitney-U
wilcox.test(avgTimeOnPage ~ userGender, data = dat)

# Aufräumen des Workspace
rm(list = ls(all.names = TRUE))
  
# Gepaarte STichprobe:
dat <- read.table("https://raw.githubusercontent.com/INWT/RKI_170925/master/Data/orderVal.txt",
                  header = TRUE, sep = ";")
str(dat)
head(dat)

dat <- dat %>% 
  select(-clicks) %>% 
  mutate(jId = factor(jId, levels = 1:2, labels =c("Kauf1", "Kauf2"))) %>% 
  spread(jId, orderValue) %>% 
  mutate(delta = Kauf1 - Kauf2)

t.test(dat$delta)
t.test(dat$Kauf1, dat$Kauf2, paired = TRUE)

# ausgespart: Überprüfung der Normalverteilungsannahme
# falls nicht normal-verteilt: Wilcoxon 
# Umsetzung ganz analog


# 05: ANOVA --------------------------------------------------------------------
# Beispieldatensatz
data(mtcars)
str(mtcars)

table(mtcars$cyl)

ggplot(mtcars %>% 
         mutate(cyl = factor(cyl)),
       aes(x = cyl, y = hp)) +
  geom_boxplot()

mtcars %>% str
mtcars$cyl <- factor(mtcars$cyl)

anova <- aov(hp ~ cyl, data = mtcars)
anova
summary(anova)

TukeyHSD(anova)

# Falls normalverteilte Untergruppen nicht gegeben sind:
kruskal.test(hp ~ cyl, data = mtcars)

# Paarweise Vergleiche analog zu `TukeyHSD` sind mittels der Funktion
pairwise.wilcox.test()
# möglich. Die Hilfe listet die Möglichkeiten zur Korrektur der alpha-Fehler-
# Inflation auf.




