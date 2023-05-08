---
title: "Homework_1"
author: "Freddie Kiessling"
date: "2023-02-16"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

set.seed(1)
1.a
```{r}
norm.vector <- rnorm(500,1,sqrt(2))
norm.vector[1:20]
tail(norm.vector,20)
head(norm.vector,20)
```




2.a)
```{r}
m<-matrix(c(1,2,3,4,5),ncol=5,nrow=5)

n<-matrix(c(1,2,3,4,5),ncol=5,nrow=5)

cbind(m,n)

```

```{r}
library(tidyverse)
str_length(c("AB","CD"))
str_c("AB","CD")
str_c("AB","CD" , sep = ",")
str_c(c("AB","CD"), collapse =",")
x<-c("Apple","Banana","Pear")
str_sub(x, 1, 2)
```

