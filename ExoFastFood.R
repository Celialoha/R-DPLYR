head(iris)
apply(iris[,-5],2,mean)
apply(iris[,-5],2,summary)
fctsup4<-function(vect){
  length(vect[vect>4])
}
tail(iris)

#by(x,indices,fun)
by(iris,iris$Species,summary)
by(iris[,-5],iris$Species,cor)

install.packages("dplyr")
library(dplyr)
tiris<-as_tibble(iris)
select(tiris,Sepal.Length, Sepal.Width, Species)
select(tiris,Sepal.Width:Species)
select(tiris,-Species)
select(tiris,starts_with("Se"))
select(tiris,ends_with("Width"))
select(tiris,contains("al"))
select(tiris,-starts_with("Se"))

filter(tiris, Sepal.Length>=4, Sepal.Length<=7)
filter(tiris, between(Sepal.Length,4,7))
filter(tiris, Species=="setosa")
filter(tiris, Species %in% c("versicolor","setosa"))
filter(tiris, Species=="setosa" | Species=="versicolor")
filter(tiris, Species!="setosa")
filter_all(tiris[,-5],all_vars(.>2))
filter_all(tiris[,-5],any_vars(.>2))

tiris %>% select(Sepal.Length, Petal.Length, Species)
select(tiris, Sepal.Length, Petal.Length, Species) 
tiris %>%
  select (-Species) %>% filter_all(all_vars(.>2))  
tiris %>% arrange(Sepal.Length)
tiris %>% arrange(desc(Sepal.Length))
tiris %>% arrange(Sepal.Length, Sepal.Width)
tiris %>% select(Sepal.Length, Sepal.Width, Species) %>% arrange(Sepal.Length, Sepal.Width)
tiris %>% select(Sepal.Length, Sepal.Width, Species) %>% filter(Species=='setosa')%>% arrange(Sepal.Length, Sepal.Width)

tiris %>% summarise(moyPL<-mean(Petal.Length), minPL<-min(Petal.Length), maxPL<-max(Petal.Length), total<-n())
tiris %>% summarise(moyPL<-mean(Petal.Length), minPL<-min(Petal.Length), maxPL<-max(Petal.Length), moySL<-mean(Sepal.Length), minSL<-min(Sepal.Length), maxSL<-max(Sepal.Length))
tiris %>% summarise_each(funs(mean, min, max), Petal.Length, Sepal.Length)
tiris %>% group_by(Species) %>% summarise_each(funs(mean, min, max), Petal.Length, Sepal.Length)
tiris %>% group_by(Species) %>% filter(Petal.Length>5) %>% summarise(n())
tiris %>% mutate(somPetal=Petal.Length+Petal.Width, somSepal=Sepal.Length+Sepal.Width)
tiris %>% mutate(Sepal.Length=NULL)
tiris %>% mutate(Petal.Length=Petal.Length*2)


# Exercices en R, DPLYR
# Fast food USA
#1
install.packages("readr")
library(readr)
library(tibble)
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
readcsv<-read_csv(file="C:/Users/Dell/Desktop/FastFoodRestaurants.csv")
#2
treadcsv<-as_tibble(readcsv)
#2
treadcsv
#3
treadcsv %>% group_by(city) %>% summarise(FF = n()) %>% arrange(desc(FF)) %>% distinct(city) %>% head(n=5)
q3<-treadcsv %>% group_by(city) %>% summarise(FF = n()) %>% arrange(desc(FF))%>% head(n=5)
q3
#???4
treadcsv%>%select(name,city)%>%filter(city %in% c(pull(q3,city)))%>%group_by(city,name)%>%count%>%arrange(city,desc(n))%>%group_by(city)%>%slice(1)
#5
count(filter(treadcsv, city %in% c(pull(q3,city))))
#???6
treadcsv %>% group_by(city) %>% summarise(FF = n()) %>% arrange(desc(FF)) %>% distinct(city,.keep_all = TRUE) %>% head(n=5)
#@7
treadcsv%>%filter(city %in% c(pull(q3,city)))%>%group_by(name)%>%count%>%arrange(desc(n))%>%head()
#8
q8<-treadcsv %>% group_by(city) %>% summarise(FF = n()) %>% arrange(desc(FF))
q8

treadcsv%>%filter(city%in% c(pull(q8,city)))%>%group_by(name)%>%count%>%arrange(desc(n))%>%head()
#9
treadcsv%>%select(city,name)%>%filter(name=="McDonald's")%>%group_by(city) %>% summarise(FF = n()) %>% arrange(desc(FF)) %>%head()
#10
treadcsv%>%select(city, name)%>%filter(city=="New York")%>%count
#11
treadcsv%>%select(city, name)%>%filter(city=="New York")%>%group_by(name)%>% count()%>%arrange(desc(n))%>%head()

                                     