install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("plotly")


library(tidyverse)
library(ggplot2)
library(ggthemes)
library(plotly)


ir_pl<-ggplot(iris, aes(Sepal.Width, fill = Species)) +
geom_bar(position = "fill")+
  scale_fill_brewer()
ir_pl+
  theme(legend.position=c(0.8, 0.7))



ir_pl+
ggtitle("Iris plot")+
  theme(
    rect = element_rect(fill = "light blue"),
    legend.key = element_rect(color = NA),
    axis.text=element_text(color="dark blue"),
    plot.title=element_text(size=16, face="italic", color="dark blue"),
    legend.margin=margin(10, 30, 20, 20, "pt")
  )


ir_pl+
  theme_dark()

global_mean<-mean(iris$Sepal.Width)
ir_pl+
  geom_vline(xintercept=global_mean, color="black", linetype=4)



ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species))+
   geom_jitter() +
   geom_smooth(method = "lm", se = FALSE) +
   coord_fixed(ratio=1)



# a) Przypisz do zmiennej mtcars_plot wykres rozrzutu dla danych mtcars.
# Na osi X umieść wt, na osi Y mpg, jako kolor cylindry jako zmienna jakościowa.
# Do wykresu dodaj tytuł „Scatterplot”.
# Następnie zmodyfikuj wygląd wykresu:
#   Tytuł wykresu- kolor czerwony, rozmiar 14, czcionka pogrubiona kursywa.
# Tytuł osi X- kolor niebieski, rozmiar 14, czcionka pogrubiona.
# Tytuł osi Y- kolor #993333, rozmiar 14, czcionka pogrubiona.
# Tło wykresu- jasnoniebieski
# Panel wykresu (panel.background)- jasno żółty, kontur szary.
library(MASS)
data(Cars93)

mtcars_plot <- ggplot( mtcars, aes( x = wt, y = mpg, col = as.factor(cyl) ) ) +
  geom_point() +
  ggtitle( "Scatterplot" ) +
  theme(
    plot.title = element_text( color = "red", size = 14, face = "bold.italic" ),
    axis.title.x = element_text( color = "blue", size = 14, face = "bold" ),
    axis.title.y = element_text( color = "#993333", size = 14, face = "bold" ),
    plot.background = element_rect( fill = "lightblue" ),
    panel.background = element_rect( fill = "lightyellow", color = "grey" )
  )    
 
print( mtcars_plot )



# b) Zmodyfikuj wykres utworzony w podpunkcie a) za pomocą gotowych theme():
mtcars_plot+
  theme_economist_white()




# c) Wykonaj wykres rozrzutu qsec i mpg dla danych mtcars.
# Zaznacz 2 linie na wykresie: me-dianę i wartość średnią z mpg.

ggplot( mtcars, aes( x = qsec, y = mpg ))+
  geom_point()+
  geom_hline( yintercept = median(mtcars$mpg), color = "blue" )+
  geom_hline( yintercept = mean(mtcars$mpg), color = "green" )



# d) Wykonaj wykres z stat_smooth(), lub geom_smooth()

ggplot( mtcars, aes( x = wt, y = mpg, color = as.factor(cyl) ) )+
  geom_point() +
  geom_smooth( se = FALSE, method = "lm" )+
  geom_smooth( se = FALSE )









plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)%>%
  add_markers()





iris%>%
  filter(Species =="setosa")%>%
plot_ly(x = ~Sepal.Width)%>%
  add_histogram(nbinsx=6, color = I("darkgreen"), opacity = 0.5)



plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)%>%
  add_markers(marker = list(size = 12,
                            color = 'rgba(255, 182, 193, .9)',
                            line = list(color = "rgba(100, 20, 20, .5)",
                                        width = 5)))%>%
  layout(title = "Scatterplot")








# Zadanie 2.
# Dane: mtcars. Po wykonaniu każdego wykresu sprawdź jak działa jego interaktywność.
# a) Wybierz z danych wyłącznie auta, które mają 4 cylindry.
# Wykonaj wykres rozrzutu mpg od disp. Jako color ustaw atrybut am.
# Użyj gotowej palety kolorów Set1. Dodaj tytuł wykresu „New colors”.


mtcars%>%
  filter( mtcars$cyl == 4 )%>%
  plot_ly( x = ~mpg, y = ~disp, color = ~as.factor(am), colors = "Set1" )%>%
  add_markers()%>%
  layout( title = "New colors" )
    

  



# b) Wykonaj histogram 2D dla atrybutów disp i mpg. Ustaw liczbę przedziałów histogramu na 3 na osi X i Y

plot_ly( mtcars, x = ~disp, y = ~mpg )%>%
  add_histogram2d( nbinx = 3, nbiny = 3 )
  



# c) Wykonaj wykres ramka-wąsy mpg względem cyl. Dodaj tytuł wykresu „Boxplot”.







