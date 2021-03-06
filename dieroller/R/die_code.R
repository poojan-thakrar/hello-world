# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#Question 1

#Die + auxilary functions


#' @title check_prob
#' @description checks if probability is valid
#' @param prob numeric vector
#' @return TRUE is vector is valid
check_prob <- function(prob){
  if (length(prob) != 6 | !is.numeric(prob)) {
    stop("\n'prob' must be a numeric vector of length 6")
  }
  if (any(prob < 0) | any(prob > 1)) {
    stop("\n'prob' values must be between 0 and 1")
  }
  if (sum(prob) != 1) {
    stop("\nelements in 'prob' must add up to 1")
  }
  TRUE
}

#' @title check_sides
#' @description checks if sides is valid
#' @param sides vector of sides
#' @return true if sides is valid
check_sides <- function(sides){
  if (length(sides) != 6){
    stop("\n'sides' must be a vector of length 6")
  }
  TRUE
}

#' @title print.die
#' @description overrides default print for die
#' @param x die object
#' @return printed version of die
print.die <- function(x, ...){
  cat('object "die"\n\n')
  print.data.frame(data.frame(side = x$sides, prob = x$prob))
  invisible(x)
}


die <- function(sides = c(1,2,3,4,5,6), prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)){

  check_prob(prob)
  check_sides(sides)

  res <- list(sides = sides, prob = prob)
  class(res) <- "die"
  return(res)
}

#Testing

fair_die <- die()
fair_die


bad_die <- die(sides = c('a', 'b', 'c', 'd', 'e'))
bad_die <- die(
  sides = c('a', 'b', 'c', 'd', 'e', 'f'),
  prob = c(0.2, 0.1, 0.1, 0.1, 0.5, 0.1))

#Question 2

#Roll


#' @title check_times
#' @description checks if times is valid
#' @param times numeric times
#' @return True if times is valid
check_times <- function(times){
  if ((times%%1 == 0&times != 0)==FALSE){
    stop("\n'times' must be an integer greater than 0")
  }
  TRUE
}

#' @title print.roll
#' @description overrides print function for roll objects
#' @param x roll object
#' @return printed version of roll
print.roll <- function(x, ...){
  cat('object "roll"\n\n')
  print(list(rolls = x$rolls))
  invisible(x)
}

roll <- function(die, times){
  check_times(times)

  rolls <- sample(die$sides, times, replace = TRUE)
  sides <- die$sides
  prob <- die$prob
  total = times
  roll_object <- list(rolls, sides, prob, total)
  names(roll_object) <- c("rolls", "sides", "prob", "total")
  class(roll_object) <- "roll"
  return(roll_object)
}

#Testing roll

set.seed(123)
fair_50rolls <- roll(fair_die, times = 50)
fair_50rolls
names(fair_50rolls)
fair_50rolls$rolls
fair_50rolls$sides
fair_50rolls$prob
fair_50rolls$total

str_die <- die(sides = c('a', 'b', 'c', 'd', 'e', 'f'), prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))
# roll 20 times
set.seed(123)
str_rolls <- roll(str_die, times = 20)
names(str_rolls)
str_rolls

#Question 3

#Summary

#' @title print.summary.roll
#' @description overrides default print for summary objects
#' @param x summary roll object
#' @return printed version of summary object
print.summary.roll <- function(x){
  cat('summary "roll"\n\n')
  summary.roll(x)$freqs
  print(x$freqs)
  invisible(x)
}

summary.roll <- function(x){
  poop <- NULL
  poop$freqs <- data.frame(cbind(x$sides, table(x$rolls), table(x$rolls)/x$total))
  colnames(poop$freqs) <- c("side", "count", "prop")
  class(poop) <- "summary.roll"
  return(poop)
}

#Testing summary

set.seed(123)
fair_50rolls <- roll(fair_die, times = 50)
fair50_sum <- summary(fair_50rolls)
fair50_sum
class(fair50_sum)
names(fair50_sum)
fair50_sum$freqs
`
#Question 4

#Plot

#' @title plot.roll
#' @description overrides default plot for roll
#' @param rolls_object a rolls object
#' @return a default table for rolls objects
plot.roll <- function(rolls_object){
  barplot(table(rolls_object$rolls)/rolls_object$total,
          border = "white",
          main = paste("Frequencies in a series of", rolls_object$total, "die rolls"),
          xlab = "sides of die", ylab = "relative frequencies")
}

plot(fair_50rolls)

#Question 5
#' @title []
#' @description Gets certain number in indexed subscript
#' @param roll_object a roll object
#' @param index the index where the number you want is
#' @return the number at the given index

"[.roll" <- function(roll_object, index) {
  if(index>roll_object$total){
    stop("\n'index' must be smaller than the number of rolls ")
  }
  if(index<0){
    stop("\n'index' must be greater than 0 ")
  }
  roll_object$rolls[index]
}
fair_50rolls[49]

#' @title make_roll
#' @description makes a roll object with given paramenters
#' @param rollobject a rollobject
#' @param rolls_given a vector of rolls
#' @return a different rollobject

make_roll <- function(rollobject, rolls_given){
  rolls <- rolls_given
  sides <- rollobject$sides
  prob <- rollobject$prob
  total = length(rolls_given)

  res <- list(rolls, sides, prob, total)
  names(res) <- c("rolls", "sides", "prob", "total")
  class(res) <- "roll"
  return(res)
}

#' @title [ <-
#' @description replaces number at index with a value
#' @param roll_object a roll object
#' @param index the index you want to replace
#' @param value the value you want to replace with
#' @return the same roll object with one different value

"[<-.roll" <- function(roll_object, index, value) {
  if(index>roll_object$total){
    stop("\n'index' must be smaller than the number of rolls ")
  }
  if(index<0){
    stop("\n'index' must be greater than 0 ")
  }
  roll_object$rolls[index] <- value
  make_roll(roll_object, roll_object$rolls)
}

fair_50rolls <- roll(fair_die, times = 50)
class(fair_50rolls)
fair_50rolls[6]
summary(fair_50rolls)
fair_50rolls[6] <- 4
summary(fair_50rolls)

#' @title +
#' @description adds more rolls to a roll object
#' @param roll_object a roll object
#' @param extra however many more rolls
#' @return a roll object with more rolls

"+.roll" <- function(roll_object, extra){
  if(extra<0|extra%%1!=0){
    stop("\n'extra tosses' must be an integer greater than 0 ")
  }
  extra_rolls <- sample(roll_object$sides, extra, replace = TRUE)
  make_roll(roll_object, c(roll_object$rolls, extra_rolls))
}

fair_150 <- fair_50rolls+100























