---
title: "archive-functions"
author: "Poojan Thakrar"
date: "April 11, 2018"
---

library(XML)
library(stringr)

#Reading the archive of stringr
tbl_html <- readHTMLTable('http://cran.r-project.org/src/contrib/Archive/stringr')
tbl_html


##Question ##

#Questions 1.1#
read_archive <- function(archived_function){
  readHTMLTable(paste0('http://cran.r-project.org/src/contrib/Archive/', archived_function))
}

read_archive('stringr')

#Question 1.2#

split_names <- function(raw_data){
  str_split(raw_data$NULL.Name, pattern = '_')
}


clean_archive <- function(raw_data){
  raw_table <- as.data.frame(raw_data)
  raw_table <- raw_table[c(1:4)]
  #for(i in 3:length(raw_data$NULL.)-1){
  vector2 <- NULL
  for(i in 3:13){
    vector2[i-2] <- str_replace(lapply(split_names(as.data.frame(read_archive('stringr')))[i], function(x) x[2]), pattern = ".tar.gz", replacement = "")
    vector3[i-2] <- toString(raw_table$NULL.[3:13])
    vector4[i-2] <- 
  }
  vector1 <- vector2
  vector1 <- lapply(split_names(as.data.frame(read_archive('stringr')))[4], function(x) x[1])
  table <- cbind(vector1, vector2, vector3, vector4)
  return(raw_table)
}

r <- read_archive('stringr')
n <- clean_archive(r)
###I recognize this is very far off

sink(file = '../data/stringr-archive.csv')
n
sink()


#Question 1.3
real_table <- cbind(c("stringr", "stringr", "stringr", "stringr", "stringr", "stringr", "stringr", "stringr", "stringr", "stringr", "stringr"), c(6.8, 10, 11, 16, 18, 20, 20, 20, 34, 62, 92))

plot_archive <- function(table){
  table[,1] <- 1:length(table[,1])
  points <- table[,2]
  graph <- ggplot(data = table, x = table[,1], y = table[,2]) + geom_step()
}
plot_archive(real_table)

