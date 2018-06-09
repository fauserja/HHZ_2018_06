################################################################################
# R Grundlagen - Schulung am HHZ                                               #
# Datum: 4.6.18                                                                #
#                                                                              #
# Autor: Steffen Wagner                                                        #
# Email: steffen.wagner@inwt-statistics.de                                     #
################################################################################
rm(list = ls(all.names = TRUE))

# 01: R als Taschenrechner -----------------------------------------------------
8 * 4

# 02: Objekte ------------------------------------------------------------------
a <- 8 * 4
a

# Schlechter Stil:
# bitte nicht: c <- 3 *18 -> b
d = 8 * 5
# Entferne nicht mehr benötigte Objekte
rm(d)

# Rechnen mit Objekten
b <- a + 5

# 03: Objekteigenschaften ------------------------------------------------------
class(a)
c <- 7L   # explizite Erzeugung einer ganzen Zahl 'integer'
class(c)
# character Objekt erzeugen:
d <- "Montag"
class(d)
"Dienstag"
Dienstag

# logische Elemente: boolesch
TRUE  # schlechter Stil T
FALSE # schlechter Stil F
e <- TRUE


# 5 atomic class: complexe Zahlen

# 04: Vektoren -----------------------------------------------------------------
vec1 <- c(1, 7, 13, 19) 
vec2 <- 1:100
vec3 <- c("Heute", "ist", "Montag.", "Morgen ist Dienstag.")
vec4 <- c(FALSE, TRUE, e)

# Besonderheit: Umwandlung - coercion
c(vec3, vec4)
c(7L, 3.14)
c("a", 3.14)
c(TRUE, 1, 3.14)
c(TRUE, 1L)

# wichtig: Vektoren in R können nur Elemente der selben Klasse enthalten.
as.integer(TRUE)
as.integer("a")
as.character(3.14)

# 05: nützliche Funtionen zum Erzeugen Vektoren --------------------------------
1:5 # sequence operator
-1:5
-(1:5)
rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,5))  
?seq
help(data.frame)

# 06: vektorisierte Operationen ------------------------------------------------
vec5 <- 1:10
vec6 <- rep(1:5, each = 2)

vec5/vec6
vec5/3
3/vec5
vec5*vec1 # warning erhalten wg. nicht passender Vektorlängen
vec2*vec1 # kein warning, weil längerer Vektor ganzzahliges Vielfaches

stopifnot(length(vec5) == length(vec1))
stop()


# 07: Attribute von Vektoren ---------------------------------------------------
class(vec1)
length(vec1)
names(vec1)
names(vec1) <- c("a", "b", "c", "d")
vec1
vec5
letters
LETTERS

# Benamung vec5
# Namen: Element1, Element2, ...
length(vec5)
names(vec5) <- paste("Element", 1:length(vec5), sep = "")

# Benamten Vektor erzeugen:
vec7 <- c(a = 1, b = 2, c = 3, d = 5)
vec7

# 08: Faktoren -----------------------------------------------------------------
geschl <- c("m", "w", "w", "m", "w", "w", "m")
geschl <- factor(geschl)
geschl
attributes(geschl)
unclass(geschl)

geschl <- factor(geschl, levels = c("w", "m"))
geschl

# Anhängen eines weiteren Elements
geschl[8] <- "m"
geschl
geschl[9] <- "neutral"
geschl
levels(geschl) <- c("w", "m", "neutral")
geschl
geschl[9] <- "neutral"
geschl


# 09: Generische Vektoren: Listen ----------------------------------------------
l1 <- list("a", 1:5, c(TRUE, FALSE), pi)
# vs c("a", 1:5, c(TRUE, FALSE), pi)
class(l1)

l2 <- list(text = c("Das", "ist", "text"),
           ganzeZahlen = 1:10,
           liste = l1)
l2
names(l2) <- c("name1", "name2", "name3")
l2

# 10: data.frames --------------------------------------------------------------
# = Liste + jedes Listenelement besitzt die selbe Anzhal an Einträgen
d <- data.frame(nachname = c("Müller", "Schmidt", "Meier"),
                vornamen = c("Julia", "Mia", "Leon"),
                alter = c(29, 4, NA))
d
d$nachname <- as.character(d$nachname)

d <- data.frame(nachname = c("Müller", "Schmidt", "Meier"),
                vornamen = c("Julia", "Mia", "Leon"),
                alter = c(29, 4, NA),
                stringsAsFactors = FALSE)
str(d)

# 11: Subsetting ---------------------------------------------------------------
# 3 Arten von Subsetting:
# - positional
# - by name
# - logische Subsetting

# Generelle anmerkungen: [,[[ bezeichnen immer sub-setting
# positional Subsetting: über Index = integr-Wert (beginnend 1)
vec5
vec5[5]
vec5[c(2, 4, 7)]
vec5[2:8]
vec5[rep(2, 5)]
vec5[0]
vec5[11]

# Subsetting by name:
names(vec5)
vec5["Element7"]
vec5[["Element7"]]

# Subsetting 'by name' robust gegenüber geänderter Reihenfolge
set.seed(1)
vec5 <- sample(vec5)
vec5[3:7]
vec5[paste("Element", 3:7, sep = "")]


# logischen Subsetting
vec5[ c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)]
vec5[vec5 >= 4]

# 11a: Subsetting bei data.frames ----------------------------------------------
# positional subsetting
# d[zeile, spalte]
d[2, 2] # kein data.frame mehr, trotz subsetting mittels einfacher Klammer
d[ , 2] # kein data.frame mehr, trotz subsetting mittels einfacher Klammer

d[2, 2, drop = FALSE]
d[ , 2, drop = FALSE]

d[1, ]
d[ , 3, drop = FALSE]

# mehrere Einträge
d[1:2, 2:3]

# subsetting by name
# zusätzliche Möglichkeit: $
d$nachname
d[["nachname"]]
d["nachname"]


# logisches Subsetting:
d$nachname %in% c("Müller", "Meier")
d[d$nachname %in% c("Müller", "Meier"), ]

# komplexer Filter:
nachnameFilter <- d$nachname %in% c("Müller", "Meier")
vornameFilter <- d$vornamen %in% c("Leon", "Matthias")

d[nachnameFilter & vornameFilter, ] 
