---
title: "Homework 3"
author: "Freddie Kiessling"
date: "2023-02-14"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

1.
```{r}
set.seed(920840939)

n = 50
set.seed(0)
x = sort(runif(n, min=-2, max=2))
y = x^3 + rnorm(n)
plot(x, y, type="l")
```
1.a
```{r}
plot(x, y, type="l",xlim = c(-1, 1), ylim = c(-5,5),xlab = "Trimmed x", ylab="Trimmed y")
```

1.b
```{r}
x.trimmed = x[x<1 & x>-1]
y.trimmed = y[x<1 & x>-1]
plot(x.trimmed, y.trimmed, type = "l")
```

1.c
```{r}
plot(x,y, pch = c(1,16,1,16), col = c("black","blue","black","red"))

```

2.a
```{r}
image(volcano, col = terrain.colors(25))
```



```{r}
clockwise90 <- function(a) { t(a[nrow(a):1,]) }
```


2.b
```{r}
volcano.rev = volcano[87:1, 61:1]

image(clockwise90(volcano.rev), xlab = "West -> East", ylab = "South -> North",main = "Heatmap of Maungawhau volcano")
```

2.c
```{r}
x = 1:nrow(volcano)
y = 1:ncol(volcano)
persp(x,y,z = volcano)
```


```{r}
pros.data =
read.table("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data")
```

3.a
```{r}
library(tidyverse)

pros.data %>% 
  ggplot(aes(x = age, y = lweight, color = lcavol))+
  geom_point()
```
3.b
```{r}
pros.data %>% 
  ggplot(aes(x = age, y = lweight, color = lcavol))+
  geom_point()+
  facet_wrap(~svi)
```
3.c
```{r}
pros.data %>%
  mutate(age_group = ifelse(age>65,"older","younger"))%>%
  ggplot(aes(x = age_group, y =lcavol))+
  geom_boxplot()+
  facet_wrap(~svi)
```
3.d
```{r}
pros.data %>%
  ggplot(aes(x=lcp))+
  geom_histogram(fill = '#FF0000')
```

4.a
```{r}
library(nycflights13)

flights.weather = inner_join(flights,weather)
head(flights.weather)
```

4.b
```{r}
flights.weather = flights.weather %>% 
  mutate(speed = distance / air_time)


flights.weather %>%
  ggplot(aes(x=precip,y=dep_delay))+
  geom_point()

cor(flights.weather$precip,flights.weather$dep_delay,use="complete.obs")


flights.weather %>%
  ggplot(aes(x=precip,y=speed))+
  geom_point()

cor(flights.weather$precip,flights.weather$speed,use="complete.obs")
```
4.c
```{r}
library(tidyverse)
library(nycflights13)

flights %>%
  inner_join(airlines) %>%
  group_by(carrier,name) %>%
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE))%>%
  arrange(desc(avg_delay))

#Frontier Airlines Inc.	20.215543
```

```

```{r}
F9flights = flights %>% filter(carrier == 'F9') %>%
left_join(planes, by = "tailnum")
F9flights %>% distinct(tailnum) %>% na.omit()




F9flights %>%
distinct(model) %>%
na.omit()




F9flights %>%
group_by(model) %>%
summarize(avg_seats = mean(seats))
```

