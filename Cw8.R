install.packages("MASS")
library(MASS)

str(whiteside)
plot(whiteside)


plot(whiteside$Temp, whiteside$Gas,
     type = "p", pch=17, col="springgreen", xlab = "Outside temperature",
     ylab = "Heating gas consumption")
title("Scatter plot")
abline(a=7, b=-0.5, lty=2, col="red")


par(mfrow=c(2,2))
plot(whiteside$Temp, col="#EE9A49")
title("First graph")
boxplot(whiteside$Temp, col="orange")
title("Second graph")
plot(whiteside$Gas, col="#EE90AA")
title("Third graph")
boxplot(whiteside$Gas, col="red")
title("Fourth graph")



# Zadanie 1
# a) Dane Cars93 (Pakiet MASS).
# Wykonaj wykres rozrzutu zmiennej Price(oś X) i Max.Price (oś Y) za pomocą czer-wonych litych trójkątów, dodaj tytuł „Price relationship”
# Dodaj do wykresu w postaci punktów zależność między Price (oś X) i Min.Price (oś Y) funkcja points(), kolor niebieski, punkty okręgi.

data(Cars93)
plot(Cars93$Price, Cars93$Max.Price, pch=17, col="red", xlab="Price", ylab="Max.price" )
title("Price relationship")
points(Cars93$Price, Cars93$Min.Price, pch=16, col="blue")


# b) Dane Cars93 (Pakiet MASS). Wykonaj wykres wyglądający jak przykład poniżej. 
# Zastosuj logarytmowanie osi X i Y drugiego wykresu: log="xy"

par(mfrow=c(2,2))
plot(Cars93$Luggage.room, Cars93$RPM, pch=4, col="grey",
     xlab="Luggage room", ylab="RPM")
title("Original reprezentation")
plot(Cars93$Luggage.room, Cars93$RPM, pch=16, col="green",
     log="xy", xlab="Luggage room", ylab="RPM")
title("Log-log plot")


# c)Dane Cars93 (Pakiet MASS). Umieść 3 wykresy w jednej ramce:
# • wykres kołowy (pie) liczby cylindrów. Przydatna będzie funkcja table()
# • wykres słupkowy (barplot) z prostopadłymi pomniejszonymi o 50% etykie-tami. Przydatne będą parametry las(), cex.names()
# • histogram (hist) dla Price.
# Zmodyfikuj kolory


par(mfrow=c(1,3))
pie(table(Cars93$Cylinders))
title("Pie chart - Cylinders")
barplot(Cars93$EngineSize, las=2, cex.names = 0.5)
hist(Cars93$Price, xlab="Price", main="Price histogram" )



#######################################################################################################

install.packages("corrplot")


library(corrplot)
var<-UScereal[, c(2:10)]
corr_matrix<-cor(var)
corrplot(corr_matrix, method = "color", type="upper")


plot(mtcars$hp, mtcars$mpg, type = "n",
     xlab = "Horsepower", ylab = "Gas mileage")
points(mtcars$hp, mtcars$mpg, pch = mtcars$cyl)
text(x = mtcars$hp,
     y = mtcars$mpg,
     labels = mtcars$am, adj = -1)


# a) Dane Cars93 (Pakiet MASS).
# Wykonaj 2 histogramy zmiennej Horsepower w jednej ramce:
# Z wykorzystaniem funkcji hist()
# Z wykorzystaniem funkcji truehist() z pakietu MASS
# Dodaj: lines(density(Cars93$Horsepower))
# Nadaj tytuły wykresom
library(MASS)

par(mfrow=c(1,2))
hist( Cars93$Horsepower, main = "Horse Power", xlab="Horsepower", ylab="Frequency" )
lines(density(Cars93$Horsepower))
truehist( Cars93$Horsepower, main = "True Horse Power", xlab="Horsepower", ylab="Frequency" )
lines(density(Cars93$Horsepower), col = "red")

# b)Wykonaj wykres odpowiadający wzorowi poniżej:
#   - uruchom pakiet aplpack
install.packages("aplpack")
library(aplpack)

Boston

par(mfrow=c(3,1))
plot(Boston$zn, Boston$rad, pch=5, col=15, main="scatterplot" )
sunflowerplot(Boston$zn, Boston$rad, main="sunflower plot" )
boxplot(Boston$crim~Boston$rad, log='y', col="purple",main="boxplot" )
bagplot(Boston$zn, Boston$rad, cex=1.5, main="bagplot" )


# c) Dane Cars93.
# Utwórz pusty wykres. Na osi X umieść Horsepower, na osi Y MPG.city. Nadaj tytuły osi X: „Horsepower” i osi Y: „Gas mileage”.
# Dodaj wykres punktowy Horsepower względem MPG.city, z pch=as.charac-ter(Cars93$cyl)

plot( Cars93$Horsepower, Cars93$MPG.city, type='n', xlab = "Horsepower", ylab="Gas mileage")
points(Cars93$Horsepower, Cars93$MPG.city, pch=as.character(Cars93$cyl))



# d) Dane Cars93.
# Utwórz indeks wskazujący na auta z 3 cylindrami.
# Utwórz wykres rozrzutu Horsepower (oś X) oraz MPG.city (oś Y), znaczniki pełne kwadraty.
# Dodaj tekst znajdujący się koło punktów (opisujący auta 3 cylindrowe), większa czcionka, bold.

library("dplyr")
indeks <- Cars93 %>%
  filter( Cylinders ==  3 )
plot( Cars93$Horsepower, Cars93$MPG.city, pch=15 )
text(x = indeks$Horsepower,
     y = indeks$MPG.city,
     labels = indeks$Cylinders, adj = -0.1, font = 2, cex = 2)
points(indeks$Horsepower, indeks$MPG.city, pch = 15, col = 'red')
  
  
  
  
  
  
  
  
  
  




