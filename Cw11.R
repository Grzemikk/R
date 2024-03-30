install.packages("dplyr")

library(ggplot2)
library(plotly)
library(dplyr)
library(gapminder)


g_plot<-ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Length, color=Species))+
  geom_point(alpha=0.5)
ggplotly(g_plot)


iris %>%
  plot_ly(x = ~Sepal.Length) %>%
  add_histogram(xbins = list(start=4, end=8, size=1))


iris %>%
  plot_ly(x = ~Sepal.Length, y=~Petal.Length, color=~Species) %>%
  add_markers(marker = list(symbol = "diamond", size = 6))


 
iris %>%
  filter(Sepal.Length>5)%>%
  count(Species) %>%
  plot_ly(x = ~Species, y = ~n, hoverinfo = "y") %>%
  add_bars()


iris %>%
plot_ly(x = ~Sepal.Length, y=~Petal.Length, color=~Species) %>%
  add_markers(marker = list(opacity = 0.5)) %>%
  layout(xaxis = list(title="Sepal Length", showgrid=FALSE),
         yaxis = list(title="Petal Length"), paper_bgcolor="#ababab")



model<-lm(Petal.Length~Sepal.Length, data=iris)
iris%>%
  plot_ly(x = ~Sepal.Length, y=~Petal.Length, color=~Species) %>%
  add_markers(showlegend = FALSE) %>%
  add_lines(y = ~fitted(model))



install.packages("diamonds")
data(diamonds)



# Zadanie 1. Dane: diamonds.
# a)Wykonaj wykres kolumnowy z pakietem ggplot2. Atrybut cut-oś X,
# jako fill ustaw clarity. Następnie zmień wykres na interaktywny
diamonds <- ggplot( diamonds, aes( x = cut, fill = clarity ))+
  geom_bar()
  
ggplotly( diamonds )



# b)Wykonaj histogram dla carat, uwzględniający wyłącznie diamenty
# o kolorach od G do J (na histogramie powinien być widoczny podział
# ze względu na kolor). Ustaw ręcznie zakres histogramu oraz 
# szerokość histogramu aby uzyskać 7 przedziałów.
diamonds %>%
  filter( color %in% ( "G", "H", "I", "J" ))
  plot_ly(x=~carat) %>%
  add_histogram(xbins=list(start=0, end=1.75, size=0.25))




# c)Wykonaj wykres rozrzutu carat względem price. Dodaj linię trendu
# wyliczoną z użyciem funkcji lm(). Tytuły osi X i Y oraz tytuł wykresu,
# popraw wygląd legendy. Ustaw punkty na półprzeźroczyste, zmień paletę 
# kolorów. Linie siatki wykresu nie powinny być widoczne.
# Zmień tło wykresu oraz tło obszaru rysowania.




# d)Utwórz 3 wykresy, zmodyfikuj ich wygląd (tytuły osi, kolory):
#   - wykres ramka-wąsy dla atrybutu carat, uwzględniający kolor
# - histogram atrybutu carat
# - histogram 2D carat względem price
# Umieść te wykresy w jednym oknie graficznym (wykonaj wykre








iris %>%
  plot_ly(x = ~Sepal.Length, y = ~Sepal.Width) %>%
  add_markers(symbol = ~Species, symbols = c("circle-open", "square-open", "star-open"))%>%
  layout(xaxis = list(title = "Sepal Length"),
         yaxis = list(title = "Sepal Width"))



iris %>%
  filter(Sepal.Width > 3.3) %>%
  plot_ly(x = ~Sepal.Length, y = ~Petal.Length) %>%
  add_markers(size = ~Petal.Width,
              color = ~Species,
              marker = list( sizemode = "diameter"))



gapminder%>%
  plot_ly(x = ~gdpPercap,y = ~lifeExp)%>%
  add_markers(size = ~pop, color = ~continent, frame = ~year)%>%
  animation_opts(frame = 1000, transition = 300, easing = "elastic") %>%
  animation_slider(currentvalue = list(prefix = NULL, font = list(color = "blue") ) ) %>%
  layout(xaxis = list(title="Real GDP (millions USD)"), yaxis = list(title="Life expectancy" ))




# Zadanie 2.

# a)Wykonaj wykres kołowy (pie) dla danych gapminder, w którym 
# przedstawione będą wartości średnie gdpPercap dla każdego 
# z kontynentów. Zmodyfikuj kolor poszczególnych wycinków koła.

mean_con <- aggregate(gapminder$gdpPercap, by=list(gapminder$continent), FUN=mean)
mean_con %>%
  plot_ly(labels=~Group.1, values=~x, type="pie", marker=list(colors=c("yellow", "red", "green", "blue", "orange")))



# b)Wykonaj wykres rozrzutu lifeExp względem roku 
# (zestaw danych gapminder). Kształt symboli w zależności od 
# zmiennej continent: "circle-open", "square-open", "star-open",
# "triangle-left", "diamond-tall". Dodaj tytuły osi x i y.
 gapminder %>%
   plot_ly( x = ~year, y = ~lifeExp, symbol = ~continent, symbols = c( "circle-open", "square-open", "star-open",
                                                                        "triangle-left", "diamond-tall") ) %>%
   layout( xaxis = list( title="Year" ), yaxis = list( title = "lifeExp" ) )



# c)Dla danych gapminder wykonaj wykres ramka-wąsy dla lifeExp
# z interakcją (country, continent). Dodaj kolor pudełek
# wykresu w zależności od kontynentu.
 gapminder %>%
   plot_ly( x = ~country, y = ~lifeExp, type="box", color = ~continent )

 
# d)Wykonaj wykres rozrzutu gdpPercap na osi x, lifeExp na osi y,
# wielkość punktów uzależniona od pop. Po najechaniu kursorem na punkt,
# powinna wyświetlać się nazwa kraju. Oś x zlogarytmowana.
# Animacja ustawiona na podstawie kontynentu. Wydłuż czas dla
# pojedynczej klatki wykresu. Zestaw danych gapminder.
gapminder %>%
  plot_ly( x = ~log(gdpPercap), y = ~lifeExp ) %>%
  add_markers(  size = ~pop, frame = ~continent, test = ~country )
  
 

# e)Wykonaj wykres bąbelkowy lifeExp (y) i pop (x) dla lat 1972-2007
# dla Azji. Jako kolor wybierz country, wielkość punktów gdpPercap.
# Animuj na podstawie year. Zestaw danych gapminder

gapminder %>%
  filter( year >= 1972 & year <= 2007 ) %>%
  filter( continent == "Asia" ) %>%
  plot_ly( x = ~pop, y = ~lifeExp ) %>%
  add_markers( color = ~country, size = ~gdpPercap, frame = ~year )












