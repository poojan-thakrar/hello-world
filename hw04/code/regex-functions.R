---
title: "archive-functions"
author: "Poojan Thakrar"
date: "April 11, 2018"
---
library(stringr)  
##Question 2##
  
#Question 2.1#

##Title: split_chars
##Description: The function takes a word and split it into its individual characters
##Parameters: A string
##Return Output: A list of strings which signify each character

split_chars <- function(string){
  list_of_strings <- substr(string, 1, 1)
  for(i in 2:nchar(string)){
    list_of_strings <- append(list_of_strings, substr(string, i, i))
  }
  list_of_strings
}
split_chars("4fjlsd")

#Question 2.2#

###########Helper########


##Title: letter_counter
##Description: The function finds the number of instances of a letter in a word
##Parameters: Two strings, one that represents the word and one that represents the letter that might be in the word
##Return Output: A number that represents the instances of a letter in a word

letter_counter <- function(input, letter){
 (sum(input == toupper(letter)))+(sum(input == tolower(letter)))
}

###########Helper########

##Title: num_vowels
##Description: The function finds the number of each vowel in a list of strings
##Parameters: A list of strings
##Return Output: A named vector with the number of vowels in the list of strings

num_vowels <- function(string_vector){  
  vowel_list <- NULL
  for(i in 1:length(string_vector)){
    for(j in 1:nchar(string_vector[i])){
      if(Reduce("|", substr(string_vector[i], j, j) == c('a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'))){
        vowel_list = append(vowel_list, substr(string_vector[i], j, j))
      }
    }
  }
  return_object <- c(letter_counter(vowel_list, 'a'), letter_counter(vowel_list, 'e'), letter_counter(vowel_list, 'i'), letter_counter(vowel_list, 'o'), letter_counter(vowel_list, 'u'))
  names(return_object) = c('a', 'e', 'i', 'o', 'u')
  return(return_object)
}
w <- num_vowels(c('e', 'r', 'f', 'e', 'o'))
vec <- c('G', 'O', ' ', 'B', 'e', 'a', 'r', 's', '!')
num_vowels(vec)

#Question 2.3#

##Title: count_vowels
##Description: The function finds the number of instances of a vowels in a string
##Parameters: A string
##Return Output: A named vector with the number of vowels in the string


count_vowels <- function(large_string){
  num_vowels(split_chars(large_string))
}

count_vowels("the quick brown fox jumps over the lazy dog")
count_vowels("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG")

#Question 2.4#

##Title: reverse_chars
##Description: The function reverses the characters in a string
##Parameters: A string
##Return Output: A string with the reversed characters

reverse_chars <- function(string){
  backwords <- ""
  for(i in 1:nchar(string)){
    backwords = paste0(backwords, str_sub(string, -i, -i))
  }
  backwords
}
reverse_chars("gattaca")
reverse_chars("Lumox Maxima")

#Question 2.5#

##Title: reverse_words
##Description: The function reverses the order of words in a long string
##Parameters: A string with potentially multiple words
##Return Output: A string with the words in that long string reversed

reverse_words <-function(string){
  backwards <- NULL
  i = 1
  split <- str_split(string, pattern = " ")
  while(!is.na(lapply(split, function(x) x[i]))){
    i = i+1
  }
  
  
  for(i in (i-1):1){
    backwards = paste(backwards, lapply(split, function(x) x[i]))
  }
  backwards 
}


reverse_words("sentence! this reverse")
reverse_words("string")

