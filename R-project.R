---
title: "STA 35B Final Project"
author: "Freddie Kiessling (920840939) and Wilson Cam (9208637112)"
date: "2023-03-11" 
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Question 1

a)
```{r}
Children<-as.data.frame(read.csv(file = 'Downloads/children.csv'))

Parents<-as.data.frame(read.csv(file = 'Downloads/parents.csv'))

Cases<-as.data.frame(read.csv(file = 'Downloads/cases.csv'))

Payments<-as.data.frame(read.csv(file = 'Downloads/payments.csv'))


dim(Children)
dim(Parents)
dim(Cases)
dim(Payments)
```

b)
```{r}
mean_child = mean(table(Children$CASE_NUM))
hist(table(Children$CASE_NUM),main='Histogram of children in each case')
points(mean_child,0,col='red',pch = 16)
```

c)
```{r}
max(table(Children$ID))
```

d)
```{r}
library(tidyverse)
payments_parents_pooled = Payments %>%
  left_join(Parents,by='AP_ID')
sum(is.na(payments_parents_pooled$AP_ID))
```

Since ther are 0 NAs we can assume that every absent parent (AP_ID) identified in the payments data has an identifying record in the parents data file.


```{r}
table(Payments$PYMNT_SRC)
```


Question 2
```{r}
Payments <- Payments
pool_categories <- function(x, level) {
  tablePropoprtions <- prop.table(table(x))
  badProportions <- tablePropoprtions[tablePropoprtions < level]
  factor(ifelse(x %in% names(badProportions), 'other', x))
}

Payments$PYMNT_SRC <- pool_categories(Payments$PYMNT_SRC, level = 0.00001)
table(Payments$PYMNT_SRC)

```





Question 3
a)
```{r}
library(ggplot2)
Payments$DATE <- as.Date(Payments$COLLECTION_DT, "%m/%d/%Y")

# i) 
range(Payments$DATE)

# ii)
Before_May_1_2015 <- subset(Payments, DATE < as.Date("2015-05-01"))
(nrow(Before_May_1_2015)/nrow(Payments)) *100
```

b)
```{r}
library(tidyverse)
Payments %>%
  filter(DATE >= as.Date("2015-05-01")) %>%
  count(DATE) %>%
ggplot(aes(x = DATE, y = n))+
  geom_line()

Payments %>%
  filter(DATE >= as.Date("2015-05-01")) %>%
  count(DATE) %>%
  arrange(desc(n)) %>% head()
```

c) 

```{r}
Payments %>% group_by(DATE) %>% summarise(total_num_payments=n()) %>% filter(DATE>='2015-05-01') %>%
  ggplot(aes(x=total_num_payments)) +
  geom_freqpoly() +
  labs(title="Number of Payments",x="Total Number of Payments",y="Days of Occurences")

 PaymentCounts <- Payments %>% filter(DATE>='2015-05-01') %>% count(DATE)
mean(PaymentCounts$n)
```

The bimodal nature of this graph of the marginal distribution of the number of payments over this time period means that, due to the frequency plot above, most days had 2500 payments and the second most occurences were around 6000 payments, each occured on around 70 days and 28 days, respectively. We found that the average number of payments made on a single day is around 3847.706, which helps us explain why there is a bimodal shape to the marginal distribution. It also shows that the peaks are at around 2500 payments per day and 6000 payments per day.




d)
```{r}
Payments %>%
  ggplot(aes(x = PYMNT_AMT)) +
  geom_histogram(binwidth = 100) +
  labs(title="Payments Amounts",x="Payment Amount ($)",y="Days of Occurences")
```
Most the payment amounts were in the 100-300 dollar range. The explanation for the shape is that most of the payments were in that shape and from there the number of people who made the payment got significantly smaller very quickly. The shape of the distribution is similar to an F distribution. 

Question 4
a)
```{r}
children_cases <- left_join(Children,Cases)
children_cases_summary <- children_cases %>%
  count(ID, AP_ID)


children_payments <- left_join(Payments,children_cases_summary) %>%
  group_by(AP_ID) %>%
  summarize(Payments = n(),
            total_amount = sum(PYMNT_AMT),
            n_children = n_distinct(ID)) 

cor(children_payments$n_children,children_payments$Payments, use="complete.obs")
cor(children_payments$n_children,children_payments$total_amount, use="complete.obs")

cor.test(children_payments$n_children,children_payments$Payments, use="complete.obs")
cor.test(children_payments$n_children,children_payments$total_amount, use="complete.obs")
```

First correlation:
Assumptions: Both the number of children per parent and number of payments made are quantitative and the relationship between them is generally linear.
Research Question: Is there a correlation between number of children and number of payments?
Null Hypothesis: H0 : mean = 0
Alternative Hypothesis: Ha : mean != 0
2)Test statistic = 44.965
3) P-value = 2.2e-16
4) Decision: since P-value < 0.05, we reject null hypothesis.
5) Conclusion: There is sufficient evidence to conclude that the correlation between the number of children per parent and the number of payments made is not zero. This means there exists a correlation. 


Second Correlation:
Assumptions: Both the number of children per parent and total payment amount made are quantitative and the relationship between them is generally linear.
Research Question: Is there a correlation between the number of children and total amount of payments?
Null Hypothesis: H0 : mean = 0
Alternative Hypothesis: Ha : mean != 0
2)Test statistic = 86.521
3) P-value = 2.2e-16
4) Decision: since P-value < 0.05, we reject null hypothesis.
5) Conclusion: There is sufficient evidence to conclude that the correlation between the number of children per parent and the total payment amount made not zero therefore there is a correlation.

b)
```{r}
Children$DATE <- as.Date(Children$DATE_OF_BIRTH_DT, "%m/%d/%Y")

Children$AGE <- as.Date("2017-01-01")-Children$DATE

Children$AGE <- as.numeric(Children$AGE)
Children$AGE <- floor(Children$AGE/365)

Parent_Payments <- inner_join(Cases, Payments) %>%
  group_by(AP_ID,CASE_NUM) %>%
  summarize(payment_amount = sum(PYMNT_AMT)) %>%
  inner_join(Children) %>%
  group_by(AP_ID) %>%
  summarize(avg_age = mean(AGE, na.rm=TRUE), total_payments = sum(payment_amount))

cor(Parent_Payments$avg_age, Parent_Payments$total_payments, use="complete.obs") 
cor.test(Parent_Payments$avg_age, Parent_Payments$total_payments, use="complete.obs")
```
There is a slight negative correlation of -0.0884825  but it is statistically significant. 


Steps:
Assumptions: Both the average age of the children of an absent parent and total amount of payments made by the absent parent are quantitative and the relationship between them is generally linear.
Research Question: Is there a correlation between the average age of the children and the total amount of payments made?
Null Hypothesis: H0 : mean = 0
Alternative Hypothesis: Ha : mean != 0
2)Test statistic = -14.85
3) P-value = 2.2e-16
4) Decision: since P-value < 0.05, we reject null hypothesis.
5) Conclusion: There is sufficient evidence to conclude that there is a correlation between the average age of the children of an absent parent and total amount of payments made by the absent parent.

c)
```{r}
Payment_data <- inner_join(Parents, Payments)
Payment_data %>%
  count(AP_ADDR_ZIP) 


Payment_data %>%
  group_by(AP_ADDR_ZIP) %>% 
  summarize(total_payment_amount = sum(PYMNT_AMT))


anova <- aov(PYMNT_AMT~AP_ADDR_ZIP,data=Payment_data)
summary(anova)
```
Yes, it does. Most of the payments and the total payment amounts are coming from 01. (01 is City)
We conducted a one way ANOVA test between the AP_ADDR_ZIP and the PYMNT_AMT giving us a p-value of 2e-16. This implies that there exists a statistically significant difference between the means of the different groups. The parents that live in the city pay the highest number of payments and total payment amount.

d)
```{r}

full_payments <- inner_join(children_payments, Parent_Payments)

lmodel <- lm(total_amount~avg_age*n_children, data = full_payments)
summary(lmodel)

```
Since the p-value of 2.2 e-16 with the average age of the children and the parents being less than 0.05, our variables are therefore significant. This allows us to conclude that the combination of attributes of the parent with average age of the children involved help us predict the total amount of payments made by a parent. However, since our adjusted R-squared value of 0.2083 is low, the model may not be as accurate and further studies should be made to make a better conclusion.

Question 5)

a)
```{r}

daily_payments <- Payments %>%
  group_by(AP_ID, DATE)%>%
  summarize(total_payment_amount = sum(PYMNT_AMT),
            total_payments = n()) %>%
  ungroup() %>%
  group_by(AP_ID)%>%
  summarize(mean_daily_payments = mean(total_payment_amount),
            sd_daily_payments = sd(total_payment_amount),
            number_payments = sum(total_payments),
            sum_amount = sum(total_payment_amount))

daily_payments

cor(daily_payments$sd_daily_payments,daily_payments$mean_daily_payments, use="complete.obs")

```

b)
```{r}
daily_payments$CV <- daily_payments$sd_daily_payments/daily_payments$mean_daily_payments

head(daily_payments$CV)


CV_parents <- Payments %>%
  filter(AP_ID %in% c(1790778,1829063,1802326))%>%
  mutate(CV_level = ifelse(AP_ID==1790778,"low",
ifelse(AP_ID==1829063,"medium","high")))%>%
  group_by(AP_ID, DATE,CV_level) %>%
  summarize(Payment = sum(PYMNT_AMT))
CV_parents$AP_ID <- as.factor(CV_parents$AP_ID)
ggplot(CV_parents, aes(x = DATE,
                       y= Payment,
                       color = AP_ID))+
  geom_line()+
  facet_wrap(~CV_level)



#1790778 - low
#1829063 - medium
#1802326 - high
```
Low is normally around consisten payments around 300-500 dollars, but it shows a consistent record of payment. Low is closer to the average of payments. Medium makes some payments so it shows more of a consistency but still variates greatly. While high pays nothing but then will pay $5000 randomly.

c)
```{r}
cor(daily_payments$CV,daily_payments$sum_amount, use = "complete.obs")
```

There is a very weak slight positive correlation between the CV of payments and  the total $ amount of payments.

d)
```{r}
CV_parents$AP_ID <- as.character(CV_parents$AP_ID)

CV_parents$AP_ID <- as.numeric(CV_parents$AP_ID)

# converting factor back to number: first have to convert it to a character, and then from character to number.

CV_parents_attributes <- Parents %>%
  inner_join(daily_payments)

CV_parents_attributes %>% 
  filter(AP_APPROX_AGE > 14) %>%
  group_by(AP_APPROX_AGE) %>%
  summarize(mean_cv = mean(CV,na.rm = TRUE)) 
```
Age would be an attribute. It seems that the older the people get the more consistently the payments get. This means the older they get the smaller the CV gets.  



