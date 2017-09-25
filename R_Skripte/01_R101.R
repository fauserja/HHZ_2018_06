################################################################################
# R Grundlagen                                                                 #
# 25.9.2017                                                                    #
################################################################################

# 01: R als Taschenrechner  ----------------------------------------------------
8 * 4
8 * 4 + 5

# 02: R Objekt -----------------------------------------------------------------
a <- 8 * 4
class(a)
print(a)
a
a + 5

# 03: Atomic Classes (Basis Objekt-Klassen)
a  # numerisch (Gleitkommazahlen)
class(32.5)

32L # ganze Zahlen (integer)
class(32L)

text <- "Montag" # Text (character)
class(text)

bool1 <- TRUE # boolsche Variablen (logical) 
bool2 <- FALSE
class(bool1)

# darüber hinaus: komplexe Zahlen

# 03: Vektoren -----------------------------------------------------------------
vec1 <- c(12, 34, 17, 18, 99, 7)

vec2 <- c("Hans", "Martin", "Rolf")

c("Hans", 43)
c(2L, FALSE)

# 04: Nützliche Funktionen zum Erzeugen von Vektoren ---------------------------
vec3 <- c(1, 2, 3, 4)
vec4 <- 1:4  # sequence operator 
vec5 <- 2:500
vec6 <- rep(vec2, times = 3)
vec6
rep(vec2, each = 3)
# vgl. R Reference Card

rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,2))     # same as second.
rep(1:4, c(2,1,2,1))
rep(1:4, each = 2, len = 4)    # first 4 only.
rep(1:4, each = 2, len = 10)   # 8 integers plus two recycled 1's.
rep(1:4, each = 2, times = 3)  # length 24, 3 complete replications

# 05: Mathematische Operationen mit Vektoren -----------------------------------
vec1 / 60
zeitAufessen <- vec1
anzahlBlaetter <- c(1, 3, 1, 4, 10, 1)
zeitAufessen / anzahlBlaetter

# Achtung: Vektoren unterschiedlicher Längen
anzahlBlaetter
anzahlBlaetter / 1:3
anzahlBlaetter / 1:5

# 06: weitere Operationen ------------------------------------------------------
anzahlBlaetter + zeitAufessen  # Summe
anzahlBlaetter * zeitAufessen  # Produkt
anzahlBlaetter^zeitAufessen    # Exponent

# Kombination von Opertaionen:
(zeitAufessen * 60)/(anzahlBlaetter + 2)


# 07: Eigenschaften (Attribute) von Objekten -----------------------------------
length(zeitAufessen)  # Attribut: Länge

# BSp: Dynamische Anpassung auf Basis des Längenattributs
zeitAufessen <- c(zeitAufessen, 50)
zeitAufessen / 1:length(zeitAufessen)

# Benamte Vektoren
namedVector <- c(a = 5L, b = 7L, d = 1L)
namedVector
attributes(namedVector)
names(namedVector)
class(names(namedVector))
length(names(namedVector))

# 08: Matrizen ----------------------------------------------------------------- 
dim(namedVector)
x <- 1:25
dim(x)
matA <- matrix(x, nrow = 5)
matB <- matrix(x, nrow = 5, byrow = TRUE)
matB

dim(matA)  # Frage: Was sind die Spalten, was die Zeilen?

matC <- matrix(x, nrow = 7)
matC
dim(matC)
attributes(matC)

# Spaltennamen
colnames(matC) <- c("Spalte 1", 
                    "Spalte 2", 
                    "Spalte 3", 
                    "Spalte 4")
attributes(matC)
colnames(matC)

# Die `paste`-Funktion
rownames(matC) <- paste("Reihe", 1:nrow(matC), sep = "_")
matC

# Matrix-Operationen: Matrix-Alegebra
matA
matA * 2  # Multiplikation mit Skalar
matB

matA * matB   # Multiplikation paarweise
matA %*% matB # Echte Matrix-Multiplikation


matC



# 08: Listen (generische Vektoren)


