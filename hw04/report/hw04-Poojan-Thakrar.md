---
title: "HW04-Poojan-Thakrar"
author: "Poojan Thakrar"
date: "April 12, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(ggplot2)
library(stringr)
```


```{r}
##Question 3##

rm(list = ls())
url <- "https://www.figure-eight.com/wp-content/uploads/2016/07/text_emotion.csv"
emotion_data <- read.csv(url, sep = ",")
head(emotion_data$content)
```
```{r}
#Question 3.1#

char_count <- NULL
for(i in 1:length(emotion_data$content)){
  char_count[i] <- nchar(toString(emotion_data$content[i]))
}

sum(char_count)
#Total characters = 2937515

char_hist <- qplot(char_count, geom = "histogram", binwidth = 5, xlab = "Characters per Tweet", ylab = "Count", main = "Histogram of Characters per Tweet")
char_hist
```

```{r}
#Question 3.2#


##Title: mention_finder
##Description: finds the number of mentions in a list of tweets, designated by @. It uses two helper functinos
##Parameters: list of factors
##Return Output: a list of equal length which has the number of mentions


mention_finder <- function(list){
  mention_counter <- rep(0, length(list))
  
  for(i in 1:length(list)){
    list_of_strings <- splitter(toString(list[i]))
    for(j in 1:length(list_of_strings)){
      if(str_sub(list_of_strings[j], 1, 1) == '@'&length(list_of_strings[j])<16&alpha_num_checker(str_sub(list_of_strings[j], 2))){
        mention_counter[i] = mention_counter[i]+1
      }
      
    }
    
  }
  #list_of_strings
  mention_counter
}

###########Helper########


##Title: splitter
##Description: The function splits a long string into its individual words
##Parameters: A long string, potentially with multiple words
##Return Output: A matrix of individual words


splitter <- function(x){
  split <- str_split(x, pattern = " ")
  list_of_words <- NULL
  i = 1
  while(!is.na(lapply(split, function(x) x[i]))){
    i = i+1
  }
  
   for(i in 1:(i-1)){
    list_of_words = append(list_of_words, lapply(split, function(x) x[i]))
   }
  return(list_of_words)
}

str_sub(splitter(emotion_data$content[1])[19], 1, 1)
########Helper########

##Title: alpha_num_check
##Description: The function makes sure that a word only contains alphanumerics or an underscore
##Parameters: A string
##Return Output: True or False depending on whether all characters are alphanumerics


alpha_num_checker <- function(x){
  logical <- NULL
  for(i in 1:nchar(x)){
    logical[i] = Reduce("|", tolower(str_sub(x,i,i)) == c("q", "w", "e", "r", "t", "y", "u", "i", "o", "p", "a", "s", "d", "f", "g", "h", "j", "k", "l", "z", "x", "c", "v", "b", "n", "m", "_", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"))
  }
  Reduce("&", logical)
}

alpha_num_checker("3124fas_#d")

########Helper########



mention_numbers <-mention_finder(emotion_data$content)


mention_bar_chart <- barplot(table(mention_numbers), main = "Bar Plot for Number of Mentions", xlab = "Number of Mentions", ylab = "Frequency")


#Only one tweet with ten mentions
emotion_data$content[mention_numbers == 10]

```

```{r}
#Question 3.3#

##Title: hashtag
##Description: The function counts the number of hashtags in each tweet in a list
##Parameters: A list of tweets
##Return Output: A list of the same length with the number of hashtags per tweet

hashtag <- function(x){
  counter  = rep(0, length(x))
  for(i in 1:length(x)){
    string = toString(x[i])
    after_amp = str_locate(string, pattern = "&amp")[2]+1
    if(str_detect(string, pattern = "&amp")&is.na(as.numeric(str_sub(string, after_amp, after_amp)))){
      counter[i] <- str_count(string, pattern = "&amp")
    }
  }
  counter
}
hashtag_count <- hashtag(emotion_data$content)

#Frequency Table
table <- table(hashtag_count)
table
#Frequency Barplot
barplot(table(hashtag_count), main = "Count of Hashtags")

#Average Length
sub_st

toString(emotion_data$content[7])

#Mode
table[which.max(table)]
#The most frequent amount of hashtags is 0, with 38988
```




































