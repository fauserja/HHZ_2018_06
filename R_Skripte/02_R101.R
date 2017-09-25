################################################################################
# R Grundlagen - Listen                                                        #  
################################################################################

# 00: Aufräumen des Workspace --------------------------------------------------
rm(list = ls(all.names = TRUE))

# 01: Bsp.: einfache Liste -----------------------------------------------------
vec <- c(name = "Hans", alter = 33)
vec
attributes(vec)

l <- list(name = "Hans", alter = 33)
l
attributes(l)

l2 <- list(vornamen = c("Rolf", "Hans", "Christian"),
           nachnamen = c("Meier", "Müller"),
           alter = c(33, 44, 45, 67))
l2

# 02: data.frames --------------------------------------------------------------
d <- data.frame(vornamen = c("Rolf", "Hans", "Christian"),
                nachnamen = c("Meier", "Müller", "Schmid"),
                alter = c(33, 44, 45))


# 03: Faktoren
x <- factor(c("yes", "no", "yes", "yes", "no", "yes"))
x
table(x)

x <- factor(c("yes", "no", "yes", "yes", "no", "yes"),
            levels = c("yes", "no"))
table(x)


# Bsp: Kodierungsschema enthält die Null, z.B (0 = weiblich, 1 = männlich)
x2 <- factor(c(0, 1, 1, 0, 1, 0),
             levels = c(0, 1, 2),
             labels = c("w", "m", "neutral"))
attributes(x2)
unclass(x2)
table(x2)

x2[7] <- "m"
x2
x2[8] <- "mm"
x2

# Hinzufügen neuer Faktorstufen
levels(x2) <- c(levels(x2), "mm")
x2
x2[8] <- "mm"
x2

d2 <- data.frame(vornamen = c("Rolf", "Hans", "Christian"),
                 nachnamen = c("Meier", "Müller", "Schmid"),
                 alter = c(33, 44, 45),
                 stringsAsFactors = FALSE)

# 03: Subsetting ---------------------------------------------------------------

# - Name
# - Positionen
# - logischen Selektor

x <- c("a", "b", "c", "d", "a", "b", "c")

# Position:
x[2]
x[c(1, 3, 5)]

# Alle Elemente die im Alphabet hinter "b" stehen
"a" > "b"
x > "b"
x[x > "b"]

names(x) <- LETTERS[1:length(x)]
letters
LETTERS

x
x["A"]
class(LETTERS)

# Subsetting `[[``
x[["A"]]

l2["alter"]
class(l2["alter"])
l2[["alter"]]
class(l2[["alter"]])


# Subsetting bei Listen und data.frames über Namen
d2$vornamen
d2[1]


d2$vornamen[d2$alter > 40]

d2[1:2, 1]

d2[d2$alter > 40, c("vornamen", "nachnamen")]
d2[1:2, "vornamen"]
d2$vornamen[1:2]


for(spalte in names(d2)){
  print(d2[[spalte]])
}


# Welche Spalte ist von INteresse?
colOfInterest <- "nachnamen"
d2[[colOfInterest]]


