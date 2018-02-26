################################################################################
# Inferenz-Statistik                                                           #
# 21.2.18                                                                      #
################################################################################
rm(list = ls(all.names = TRUE))

# 00: Laden der Pakete ---------------------------------------------------------
library(ggplot2)
library(dplyr)
library(car)
library(tidyr)

# 01: Einlesen der Daten -------------------------------------------------------
dat <- read.csv("Data/Analytics.csv")
str(dat)

# 02: Einstichprobentest - Lageparameter
# Untersuchung der Sessionduration
dat %>% 
  ggplot(aes(sessionDuration)) +
  geom_density() +
  geom_vline(xintercept = 2100, linetype = "dotted")

# Forschungshypothese
# Die durchschnittliche Zeit auf Homepage 'sessiuonDuration' > 2100s

# Statistische Hypothese H0:
# H0: sessionDuration <= 2100s
# H1: sessionDuration > 2100s

# Normalverteilungsannahme abgelehnt
# Test auf Mittelwert nicht möglich, aber: Test auf Median
# Median = 50% Punkt -> Test auf Anteilswerte `binom.test`

nSuccess <- sum(dat$sessionDuration > 2100)
n <- nrow(dat)

binom.test(nSuccess, n, p = 0.5, alternative = "greater")

# Ergebnis: H0 kann nicht verworfen werden.


# 03: Ein-Stichproben t-Test ---------------------------------------------------
dat %>% 
  ggplot(aes(x = avgTimeOnPage)) +
  geom_density() +
  geom_vline(xintercept = 180, linetype = "dotted")

# Prüfung auf Normalverteilung:
# - grafische Überprüfung
dat %>% 
  ggplot(aes(x = avgTimeOnPage)) +
  stat_ecdf()
qqnorm(dat$avgTimeOnPage)
qqline(dat$avgTimeOnPage, col = "red")

# - Überprüfung mittels Stat. Test
# H0: Daten sind normalverteilt
# Daten sind nicht normalverteilt
shapiro.test(dat$avgTimeOnPage)
# Ergebnis: H0 verwerfen; NOrmalverteilungsannahme nicht wahrscheinlich

# TRotz nicht-gültiger Normal-Vert.-Annahme: t-test

# H0: avgTimeOnSite == 180
# H1: avgTimeOnSite != 180

t.test(dat$avgTimeOnPage, mu = 180)
t.test
# Ergebnis: H0 verwerfen


# 04: Zwei-Stichproben-Test ----------------------------------------------------
dat %>% 
  ggplot(aes(x = avgTimeOnPage, col = userGender)) +
  geom_density()

dat %>% 
  group_by(userGender) %>% 
  summarise(sd = sd(avgTimeOnPage),
            avgTimeOnPage = mean(avgTimeOnPage))

# Test auf Normalverteilung:
# Prüfung: Normalverteilung in den einzelnen Gruppen

qqnorm(dat$avgTimeOnPage[dat$userGender == "male"])
qqline(dat$avgTimeOnPage[dat$userGender == "male"])

qqnorm(dat$avgTimeOnPage[dat$userGender == "female"])
qqline(dat$avgTimeOnPage[dat$userGender == "female"])

# Shapiro-Wilk-Test: gruppenweise
# H0: Normalverteilung gegeben
dat %>% 
  group_by(userGender) %>% 
  summarise(pWertShapiro = shapiro.test(avgTimeOnPage)$p.value)

# Test auf Varianz-Homogenität: Levene-Test (Paket: car)

# H0: Varianzen sind gleich
leveneTest(avgTimeOnPage ~ userGender, data = dat)
leveneTest(dat$avgTimeOnPage, dat$userGender)

# Zwei-Stichproben: t-test (gleiche Varianzen)
# H0: Gruppenmittel identisch
t.test(avgTimeOnPage ~ userGender, data = dat, var.equal = TRUE)

# Zwei-Stichproben: t-test (unterschiedliche Varianzen)
# H0: Gruppenmittel identisch
t.test(avgTimeOnPage ~ userGender, data = dat, var.equal = FALSE)


# NOrmalverteilungsannahme nicht gegeben: Wilcoxon/Whitney-U-Test
wilcox.test(avgTimeOnPage ~ userGender, data = dat)


# 05: Gepaarte Stichproben:
rm(list = ls(all.names = TRUE))
dat <- read.table("Data/orderVal.txt", header = TRUE, sep = ";")
str(dat)
head(dat)

dat <- dat %>% 
  select(-clicks) %>% 
  mutate(jId = ifelse(jId == 1, "Kauf1", "Kauf2")) %>% 
  spread(key = jId, value = orderValue) %>% 
  mutate(delta = Kauf2 - Kauf1)
    

# Vebundene STichproben
t.test(dat$Kauf2, dat$Kauf1, paired = TRUE)
t.test(dat$delta)

# EXkurs:
# gezieltter Zugriff auf Funktionen, die evtl. maskiert sind
# dplyr::recode



# 05: Mehr als zwei Gruppen ----------------------------------------------------

# normalverteilte Beobachtungen (innerhalb der Gruppen)
# ANOVA
data("mtcars")
mtcars
str(mtcars)
table(mtcars$cyl)
mtcars %>% 
  mutate(cyl = factor(cyl)) %>% 
  ggplot(aes(x = cyl, y = hp)) +
  geom_boxplot()


anova <- aov(hp ~ factor(cyl), data = mtcars)
anova
summary(anova)

TukeyHSD(anova)


# keine Normalverteilung
# Kruskal-Wallis
kt <- kruskal.test(hp ~ factor(cyl), data = mtcars)
kt
pairwise.wilcox.test(mtcars$hp, mtcars$cyl)












