---
title: "Homework_4_Stats_2"
author: "Freddie Kiessling"
date: "2023-02-28"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


Problem 1
i)
```{r}
x=c(6.39,6.75,6.60,6.43,6.65,7.05,6.47,6.71)
n=length(x)
df=n-1
x.bar=mean(x)
mu.0 =6.5

t.statistic = (x.bar-mu.0)/(sd(x)/sqrt(n))
t.statistic
p.value = 1-pt(t.statistic,df=df)
p.value

#checking the answer
t.test(x=x,mu = 6.5,alternative = "greater")

```
We fail to reject the null hypothesis based on the p value of 0.06347 being greater than 0.05.  

ii)
```{r}
# one sided confidence interval

x.bar-qt(0.95,df=df)*sd(x)/sqrt(n)
```

$(6.48,\infty)$
The 95% confidence interval is one sided. 

Problem 2.
```{r}
p1 = 375/1000
p2 = 345/1000

## 95 %CI

p1-p2-1.96*sqrt(p1*(1-p1)/1000+p2*(1-p2)/1000)
p1-p2+1.96*sqrt(p2*(1-p1)/1000+p2*(1-p2)/1000)
```

95% CI for $\theta_1-\theta_2$: (-0.01205331,0.07118799).


Problem 3.
```{r}
# 5 steps from class
n1=38
n2=34

x.bar1 = 66.7
x.bar2 = 65.6

s2.1 = 10.5
s2.2 = 12.3

## CI for two sample mean difference

s2_pool =((n1-1)*s2.1+(n2-1)*s2.2)/(n1+n2-2)

#x.bar1-x.bar2-qt(0.975,df=n1+n2-2)*sqrt(s2_pool)*sqrt(1/n1+1/n2)

#x.bar1-x.bar2+qt(0.975,df=n1+n2-2)*sqrt(s2_pool)*sqrt(1/n1+1/n2)

t.statistic <- (x.bar1-x.bar2)/(sqrt(s2_pool)*sqrt(1/n1+1/n2))
t.statistic

df<-n1+n2-2
pvalue <- 2*(1-pt(t.statistic, df=df))
pvalue
```
Since our p value is greater than 0.05 we fail to reject the null hypothesis which means we have insufficient evidence to support the original claim. 

Problem 4
i)
```{r}
#paired t test

before <- c(204,243,253,212,239,241,256,267,231,251,255,260,243,246,210,258,212,208,234,211,235,266,208,243,248,259,270,253,239,238)
after <-c(200,235,256,200,232,210,249,270,233,243,259,253,23,222,206,246,210,200,236,204,220,270,210,246,246,265,256,246,210,230)

df <- data.frame(before,after)
mean(before)
mean(after)
t.test(x=df$before,y=df$after,paired = TRUE, conf.level = 0.99, alternative = "less")



```
It appears to be statistically insignificant.

ii)
I did a paired t test because it ties the differences of before and after to the same patient. 

Problem 5
```{r}
O <- c(529,58,28)
A <- c(220,13,9)
B <- c(95,8,7)
AB <- c(476,26,31)

df <- data.frame(O,A,B,AB)

rownames(df) <- c("Absen","Mild","Severe")

chisq.test(df,p=0.05)
```

We fail to reject the null hypothesis because the p value is over 0.05, which means there is not a significant correlation between blood type and severity of disorder. 



