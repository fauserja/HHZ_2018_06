################################################################################
# R Grundlagen                                                                 #
# 20.2.18                                                                      #
################################################################################
rm(list = ls(all.names = TRUE))


# 01: R als Taschenrechner -----------------------------------------------------
2 * 4  # Strg + Enter schickt Berechnung an Console

# 02: Objekte ------------------------------------------------------------------
a <- 2 * 4
a
b <- a + 5

# 03: Objekteigenschaften ------------------------------------------------------
# IN R gibt es 5 basis (atomic) Klassen
class(a)  # numeric: Dezimalzahl mit Nachkommastellen
class(5)

class(5L)  # integer: ganze Zahlen

x <- "Dienstag"  # character, Text
class(x)
Dienstag  # Fehler, da Objekt Dienstag nicht vorhanden
satz <- "Das ist ein ganzer Satz."

TRUE # wahr
FALSE # falsch

y <- TRUE
class(y)

# 5. Klasse: komplexe Zahlen


# 04: Vektoren -----------------------------------------------------------------
vec1 <- c(1, 2, 3, 7, 8)
vec2 <- 10:20
vec3 <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
vec4 <- c("Das", "ist", "ein", "Satz", ".")

# Besonderheit: Vektoren können nur Elemente einer Klasse enthalten
# Falls unterschieliche Klassen zusammengfügt werden, werden die 
# Elemente umgewandelt (coersion)
vec5 <- c("Alter:", 40)
c("text", TRUE)
c(5.1, TRUE)
class(c(5, TRUE))
class(c(1:10, FALSE))
c(1:10, FALSE)

# 05: Nützliche Funktionen zum Erzeugen von Vektoren -------------------
1:20 # Sequence OPerator ':'
rep(2, 10)
rep(1:4, 3)
rep(1:4, each = 3)
?rep  # alternativ: help(rep) odxer F1

rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,2))     # same as second.
rep(1:4, c(2,1,2,1))
rep(1:4, each = 2, len = 4)    # first 4 only.
# https://cran.r-project.org/doc/contrib/Short-refcard.pdf

# 06: Vektorsierte Operationen -------------------------------------------------
vec2
vec2/2  # Kombination aus Vektor und Skalar

vec1 * vec3  # Kombination aus Vektoren gleicher Länge
# Implizite Umwandlung von vec3 'logical' -> 'numeric'

vec1/1:4
rep(2, 10)/vec1

# Explizite Prüfung, ob Vektoren die selbe Länge haben
length(vec1)                             # Gibt Länge (Anzahl Elemente) eines Vektors zurück
length(vec1) == length(vec2)             # Prüfung auf Gleicheit mit '==' Operater
stopifnot(length(vec1) == length(vec2))  # 'stopifnot' erzeugt Fehler, falls kein TRUE eingeht


# 07: Attribute von  Vektoren --------------------------------------------------
length(vec1)  # Länge oder Anzahl Elemente
class(vec1)   # Klasse s.o.

names(vec1)   # Besitzen die Elemente des Vektors Namen?
# names(vec) <- c("element1", "element2", ....)
names(vec1) <- paste("element", 1:length(vec1), sep = "")
vec1

# dynamische Erzeugung der Elementnamen
vec1 <- c(vec1, 99)
names(vec1) <- paste("element", 1:length(vec1), sep = "")
vec1

# Direkte Erezeugung eines Vektors mit Namen:
vec6 <- c(a = 1, b= 45, c= 12, d = 17)
vec6
names(vec6)

attributes(vec6)
vec6 <- c(vec6, 51)
attributes(vec6)

# 08: Faktoren -----------------------------------------------------------------
geschl <- c("m", "w", "m", "m", "w", "w")  # Text-Vektor
geschl <- factor(geschl)
attributes(geschl)

# Reihenfolge der Faktorstufen durch 'levels'-Argument festlegen
attributes(                                # attributes-Fkt. zur Ansicht der 
                                           # internen Kodierung
  factor(c("m", "w", "m", "m", "w", "w"),
         levels = c("w", "m"))
)

geschl2 <- factor(rep(0:1, each = 5),
                  levels = 0:1,
                  labels = c("m", "w"))
geschl2[11] <- "neutral"
geschl2
levels(geschl2) <- c(levels(geschl2), "neutral")
geschl2[11] <- "neutral"
as.integer(geschl2) - 1
factor(geschl2,
       levels = c("m", "w", "neutral"),
       labels = c("0", "1", "2"))

# 09: generische Vektoren: Listen, data.frames ---------------------------------
c("Alter:", 43)
l <- list("Alter", 43)
l
l2 <- list(vorname = c("hans", "martin", "rolf"),
           nachname = c("wagner", "meier", "schmidt"),
           alter = c(43, 12))
l2


d <- data.frame(vorname = c("hans", "martin", "rolf"),
                nachname = c("wagner", "meier", "schmidt"),
                alter = c(43, 12))

d <- data.frame(vorname = c("hans", "martin", "rolf"),
                nachname = c("wagner", "meier", "schmidt"),
                alter = c(43, NA, 12),
                stringsAsFactors = FALSE)

# 10: Subsetting ---------------------------------------------------------------
# - positional subsetting
# - logical subsetting
# - subsetting by names

# Subsetting bei (atomic) Vektoren
vec1
vec1[3]
vec1[c(1, 4, 5)]
vec1[3] <- 17
vec1[7]       # Element nicht vorhanden
vec[7] <- 23  # aber schreibbar
vec1[100] <- 100  # Elemente bis Element 100, die nicht existieren werden mit NA befüllt
vec1 <- vec1[1:5]

# - Logical Subsetting
vec1[c(TRUE, FALSE, TRUE, TRUE, FALSE)]  # für jedes einzelne Elemnte Auswahl ja/nein

# Dynamische Erzeugung des logischen Vektors
vec1 < 8  
vec1[vec1 < 8]

# - Subsetting bei Name
vec1
vec1["element4"]
vec1[c("element4", "element1")]
vec1[c("element4", "element4")]


# Subsetting bei Listen
l2

# - positional Subsetting
l2[2]
l2[[2]]
l2$nachname

# Subsetting: data.frames
d
d[1]
d[[1]]
d$vorname
d$nachname

2018 - d$alter 
2018 - d[3]

# Werte vor dem Komma: Auswahl der Zeilen; 
# Werte nach dem Komma: AUswahl der Spalten
d[ 1:2, 2:3]        
d[ 1:2, "alter"]
d[ 1:2, "alter", drop = FALSE]

d[ d$alter > 15,]
d$alter > 15
