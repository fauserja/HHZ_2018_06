################################################################################
# Recap                                                                        #
# 21. Februar 2018                                                             #
################################################################################

# 00: Leeren des Workspace -----------------------------------------------------
rm(list = ls(all.names = TRUE))

# 01: Datentypen ---------------------------------------------------------------

# logical
s <- c(TRUE, FALSE)

# numeric Vector
x <- c(3, 5:7, 13)

# character Vector
y <- c("Dies", "ist", "ein", "Vektor")

# 02: Coercion (Zwangsumwandlung) ----------------------------------------------
z <- c(x, y)
class(z)

# 03: Subsetting ---------------------------------------------------------------
y[3:4]
x[x > 5]
names(x) <- letters[1:length(x)]
x[names(x) > "c"]
x[c("a", "c", "e")]
x[["a"]]


# Länge
length(x)

# 04: Kombination von Vektoren -------------------------------------------------
# cbind / rbind
u <- 1:3
k <- 10:12

cbind(u, k)
mat <- rbind(u, k)
attributes(mat)

# 05: Listen! ------------------------------------------------------------------
niceList <- list(eine = 1, 
                 kleine = "nette", 
                 liste = TRUE)

# Subsetting mittels Name
niceList$eine

# 06: Dataframe ----------------------------------------------------------------
mydf <- data.frame(Name = c("Max Mendez", "Louisa Lumiere", "Otto Oberman"),
                   Place = c(2, 1, 3),
                   Points = c(138, NA, 99))
mydf
mean(mydf$Points, na.rm = TRUE)


# 07: Fehlende Werte (NA) ------------------------------------------------------
mydf$Points == NA
is.na(mydf)
complete.cases(mydf)
na.omit(mydf)


# 08: Datenmanagement ----------------------------------------------------------
library(tidyr)
library(dplyr)

dat <- read.table(file = "Data/anscombe.txt", header = TRUE)
View(dat)


dat <- dat %>% 
  mutate(id = 1:n()) %>% 
  gather(key = dim, value = value, -id) %>%  
  mutate(group = gsub("x|y|\\.", "", dim),
         group = ifelse(group == "", 0, group),
         dim = gsub("\\.[0-9]", "", dim)) %>% 
  select(id, group, dim, value) 

dat <- dat %>% 
  spread(dim, value) %>% 
  arrange(group, id)

dat %>% 
  group_by(group) %>% 
  summarise(cor = cor(x, y),
            meanX = mean(x),
            meanY = mean(y),
            varX = var(x),
            varY = var(y),
            alpha = lm(y ~ x)$coef[1],
            beta = lm(y ~ x)$coef[2],
            R2 = summary(lm(y~x))$r.squared) %>% 
  View


library(ggplot2)
ggplot(dat, 
       aes(x = x, y = y, col = group)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ group, nrow = 2)
