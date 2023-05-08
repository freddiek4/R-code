---
title: "Homework_5"
author: "Freddie Kiessling"
date: "2023-03-10"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

1. (i)
```{r}
#1. 
#Null Hypothesis: There is no relationship between the treatment and the outcome.
#Alternative Hypothesis: There is relationship between the treatment and the outcome.

#2. Calculate the test statistic

mouse.data <- data.frame(Alive = c(67,57),Dead = c(33,43))
rownames(mouse.data)<-c("Bacteria and antiserum","Bacteria only")
chisq.test(mouse.data,p=0.01)

#Our test statistic is 1.719

#3. Our p value is 0.1898
#4. We fail to reject the null hypothesis because the p value is higher than 0.01.

#5. Our real world conclusion is that there is no relationship between the treatment and the outcome.
```
ii)
```{r}
#Yes, X-squared = 1.719
```


2.
```{r}
King_Size <- c(1.1, 1.7, 1.7, 1.1, 1.1, 1.4, 1.1, 1.4, 1.0, 1.2, 1.1, 1.1, 1.1, 1.1, 1.1, 1.8, 1.6, 1.1, 1.2, 1.5, 1.3, 1.1, 1.3, 1.1, 1.1)

Menthol <- c(1.1, 0.8, 1.0, 0.9, 0.8, 0.8, 0.8, 0.8, 0.9, 0.80, .8, 1.2, 0.8, 0.8, 1.3, 0.7, 1.4, 0.2, 0.8, 1.0, 0.8, 0.8, 1.2, 0.6, 0.7)

Filtered <- c(0.4, 1.0, 1.2, 0.8, 0.8, 1.0, 1.1, 1.1, 1.1, 0.8, 0.8, 0.8, 0.8, 1.0, 0.2, 1.1, 1.0, 0.8, 1.0, 0.9, 1.1, 1.1, 0.6, 1.3, 1.1)

cigarette_data <- data.frame(King_Size,Menthol,Filtered)

cigarette_anova<-aov(values~ind,data=stack(cigarette_data))
summary(cigarette_anova)

# We reject the null hypothesis of the anova test(that all the means are the same) because the P value is below 0.05, thus at least one of the means differs from the others. 
```

3. (i)
```{r}
#Hypotheses:
# The null hypothesis is that the correlation between the weight and fuel is 0.
# The alternative hypothesis is that the correlation between the weight and fuel is not 0.

cor.test(mtcars$wt,mtcars$mpg, method = "pearson")

#We reject the null hypothesis because the p-value is below 0.05.

# The real world conclusion to this is that a cars weight and fuels miles per gallon in this data set does have a negative correlation. 
```
(ii)
```{r}
library(tidyverse)
lm1 <- lm(wt~mpg,data = mtcars)
ggplot(data = mtcars, aes(x = mpg,
                          y = wt)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

confint(lm1)
```
(iii)
```{r}
#Null Hypothesis: The null hypothesis is that the slopes are 0. 
lm2 <- lm(wt ~ mpg + cyl:mpg + am:mpg, data = mtcars) 
lm2_anova <- aov(lm2)
summary(lm2_anova)

# We reject the null hypothesis that the slopes are 0. 
```
(iv)
```{r}
lm3 <- lm(mpg~hp+wt+cyl+am,data = mtcars)
summary(lm3)

#The weight variable is significant because it has a p value of less than 0.05.
```
(v)
```{r}
new_data <- data.frame(hp = 250, wt = 4.589, cyl = 4, am =1)
predict(lm3, newdata = new_data)
```

(vi)
```{r}
summary(lm1)
summary(lm3)
```

Based on our measure for evaluating the model, the R-squared indicates that the second model (iv) is better because it has higher R-squared value. 
