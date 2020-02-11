---
title: "Assignment 2"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, include=FALSE}
##Library's read in

library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(lubridate)
library(tidytext)
library(RColorBrewer)
library(wordcloud)
```


```{r, echo=FALSE}
## load data
data <- read.csv('output.csv')
```

```{r}
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
```

```{r}
year = c()
for (i in 1:192462) {
  a = substrRight(as.character(data$review_date[i]),2)
  year = append(year, a)
}
```

```{r}
for (i in 1:192462) {
  year[i] = paste('20',year[i],sep="")
}
```

```{r}
data <- data %>%
  mutate(Year = year)
```

```{r}
# Rating distribution Over the Years
data %>%
  group_by(Year,rating) %>%
  count(rating) %>%
  arrange(rating) %>%
  ggplot(aes(x=Year,y=n,group=rating,color=rating))+geom_point(size = 2)+
  geom_line(stat = "identity", show.legend = FALSE)+
  theme_bw() +
  labs(title = 'Rating distribution Over the Years',
       x = 'Year',
       y = 'Rating Frequency')
```
```{r}
Item_id = as.character(data$item_id)
data <- data %>%
  mutate(item_id = Item_id)
```

```{r}
data1 <- data %>%
  group_by(item_id) %>%
  count(item_id) %>%
  rename(counts = n) %>%
  arrange(desc(counts))
```

```{r}
table <- data.frame("item_id" = data1$item_id[1:20], "counts" = data1$counts[1:20])

ggplot(table, aes(x=fct_reorder(item_id,counts),y=counts,fill=item_id))+
geom_bar(stat = "identity", show.legend = FALSE) + coord_flip() + 
theme_bw() + theme(legend.position = "none") +
labs(title = 'Top 20 Products Ranking',
     x = 'Product ID',
     y = 'Frequency')
```
```{r}
data$rented.for[data$rented.for == ""] <- "other"
```


```{r}
data %>%
  group_by(rented.for) %>%
  count(rented.for) %>%
  ggplot(aes(x=fct_reorder(rented.for,n),y=n,fill=rented.for))+
  geom_bar(stat = "identity", show.legend = FALSE) + coord_flip() + 
  theme_bw() + theme(legend.position = "none") +
  labs(title = 'Reasons for Renting Distribution',
       x = 'Reason',
       y = 'Frequency')
```

```{r}
data %>%
  group_by(category) %>%
  count(category) %>%
  ggplot(aes(x=fct_reorder(category,n),y=n,fill=category))+
  geom_bar(stat = "identity", show.legend = FALSE) + coord_flip() + 
  theme_bw() + theme(legend.position = "none") +
  theme(axis.text=element_text(size=4,face="bold")) +
  labs(title = 'Category Distribution',
       x = 'Category',
       y = 'Frequency')
```


