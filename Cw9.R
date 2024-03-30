install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggthemes")

library(tidyverse)
library(ggplot2)
library(ggthemes)

ggplot(data=mtcars, aes(x=cyl, y=mpg))+
  geom_point(shape=1, size=4)


ggplot(data=mtcars, aes(x=wt, y=mpg, color = disp, size=disp))+
  geom_point(alpha=0.8)+
  geom_smooth()

pl<-ggplot(mtcars, aes(wt, mpg))

pl+geom_point(aes(alpha=cyl))

pl+geom_point(aes(shape=as.factor(cyl)))+
  scale_x_log10()

pl+geom_text(aes(label=cyl))
pl+geom_point(aes(size= hp/wt))
dgreen<-"#013220"
pl+geom_point(color=dgreen, alpha=0.6, size=10, shape=1)
pl+ geom_text(label=rownames(mtcars))

m_colors <- c("#378AB1", "#E2111C")
ggplot(mtcars, aes(as.factor(cyl), fill = as.factor(am))) +
  geom_bar(position="dodge") +
  labs(x = "Number of Cylinders", y = "Count")
scale_fill_manual("Type", values = m_colors)

m_colors <- c("#378AB1", "#E2111C")
ggplot(mtcars, aes(as.factor(cyl), fill = as.factor(am))) +
  geom_bar(position="dodge") +
  labs(x = "Number of Cylinders", y = "Count") +
scale_fill_manual("Type", values = m_colors)




# Zadanie 1
# a) Zobacz jakie zmienne znajdują się w df diamonds.
# Następnie wykonaj wykres roz-rzutu: na osi X umieść carat,
# na osi Y price. Dodatkowo dopasuj do danych krzywą obrazującą przebieg danych.
data("diamonds")
print(diamonds)
ggplot( diamonds, aes( x=carat, y=price ) ) +
  geom_point() +
  geom_smooth()


  # b)Zmodyfikuj wykres z podpunktu a: dodaj atrybut clarity jako kolor 
# do wykresu oraz zmień punkty na półprzeźroczyste o wielkości 3 pikseli.

ggplot( diamonds, aes( x=carat, y=price, color=clarity ) ) +
  geom_point( alpha=0.5, size=3 ) +
  geom_smooth()


# c) Wykonaj wykres pokazany poniżej:

ggplot( diamonds, aes( x=carat, y=price, color=clarity, size=as.factor(cut))) +
  geom_point( alpha=0.5, size=3 ) +
  geom_smooth() +
  scale_colour_manual( values=rainbow(8))



# d) Wykonaj wykres punktowy danych diamonds. Na osi X carat, 
# na osi Y price. Ustaw punkty jako półprzeźroczyste oraz obie osie 
# na skali logarytmicznej.

ggplot( diamonds, aes( x=carat, y=price, color=clarity, size=as.factor(cut))) +
  geom_point( alpha=0.5, size=3 ) +
  geom_smooth() +
  scale_x_log10() +
  scale_y_log10()


# e) Wykonaj wykres kolumnowy dla atrybutu cut. Jako wypełnienie
# kolorem (fill) ustaw atrybut clarity. Wykonaj wykres w dwóch pozycjach:
#   standardowej oraz dodge

ggplot(diamonds, aes( x=  cut, fill = clarity)) +
  geom_bar( position="dodge" ) +
  geom_bar( position="stack",alpha=0.5)
  





# Zadanie 2
# a) Wykonaj wykres opdowiadający wzorowi:

ggplot( diamonds, aes( x=carat, y=price, color=as.factor(cut))) +
  geom_point( size=1 ) +
  facet_wrap(~clarity)


# b)Wykonaj wykres rozrzutu danych diamonds. Na osi X umieść 
# atrybut carat, na osi Y price, dodaj tytuł „Diamonds scatterplot”
# i tytuł osi x „Weight carats”


ggplot( diamonds, aes( x=carat, y=price)) +
  geom_point() +
labs(title = "Diamonds scatterplot", x = "Weight carats", y="Price" ) +
ggtitle("Tekst")



# c) Wykonaj macierzowe histogramy atrybutu price dla df diamonds. 
# Ustaw szerokość przedziałów na 200. Zmienna grupująca clarity

ggplot( diamonds, aes( x = price )) +
  geom_histogram( binwidth = 200 ) +
  facet_wrap(~clarity)


# d) Wykonaj wykres odpowiadający wzorowi poniżej (geom_density)

ggplot( diamonds, aes( x = price, color = cut ) ) +
  geom_density()


# e) Wykonaj wykres ramka wąsy (boxplot) dla atrybutu na osi X color,
# Y price dla da-nych diamonds. Oś Y- logarytmiczna.

ggplot( diamonds, aes( x = color, y = price ) ) +
  geom_boxplot() +
  scale_y_log10()



# f) Zapisz wykres rozrzutu carat (oś X), price (oś Y) do zmiennej m_plot.
# Następnie za-pisz do plików mp_plot.png oraz mj_plot.jpeg

m_plot <- ggplot( diamonds, aes( x = carat, y = price ) ) +
  geom_point()
 
ggsave(filename="mp_plot.png", m_plot)
ggsave( filename="mj_plot.jpeg", m_plot )



# g) Wykonaj wykres cut (oś X), clarity (oś Y) z dodanym szumem do
# punktów (geom_jitter), ustaw przeźroczystość punktów na 0.03

ggplot( diamonds, aes( x = cut, y = clarity ) ) +
  geom_jitter( alpha = 0.03 )


# Zadanie 3
# a) Wykonaj wykres punktowy dla danych iris. Na osi X umieść Sepal.
# Length, na osi Y Sepal.Width, jako kolor- Species. Jako atrybuty 
# geometrii ustaw przeźroczystość na 0.5, pozycja-jitter o 
# parametrze width=0.2 i kształcie punktów 12.

data(iris)

print(iris)
ggplot( iris, aes( x = Sepal.Length, y = Sepal.Width, color = Species ) ) +
  geom_point( alpha = 0.5 ) +
  geom_jitter( width = 0.2, shape = 12 )


# b)Wykonaj histogram dla atrybutu Sepal.Length z iris. Użyj w 
# aes dodatkowo wyraże-nie: ..density..
# Szerokość przedziałów histogramów ustaw na 1, kolor histogramu: 
#   ciemno niebieski.

ggplot( iris, aes( x = Sepal.Length, ..density..  ) ) +
  geom_histogram( width = 1, color = "darkblue" )



# c) Wykonaj histogram dla atrybutu Sepal.Width, z fill=Species.
# Ustaw szerokość prze-działu histogramu na 0.5, pozycję na identity
# i przeźroczystość na 0.2.
# Jaki efekt uzyskamy zmieniając pozycję na fill?

ggplot( iris, aes( x = Sepal.Width, fill = Species ) ) +
  geom_histogram( width = 0.5, position = "identity", alpha = 0.2 )


ggplot( iris, aes( x = Sepal.Width, fill = Species ) ) +
  geom_histogram( width = 0.5, alpha = 0.2, position = "fill" )


# fill przeskalowuje na całą wysokość od 0 do 1 może to ułątwiać porównywanie wartości 



  









