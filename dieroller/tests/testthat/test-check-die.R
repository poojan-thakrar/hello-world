context("Check die arguments")



test_that("check_sides with ok vectors", {



  expect_true(check_sides(c(1,2,3,4,5,6)))

  expect_true(check_sides(c("q","w","e","r","t","y")))

})





test_that("check_sides fails with invalid lengths", {



  expect_error(check_sides(c('one', 'two', 'three')))

  expect_error(check_sides(c('one')))

  expect_error(check_sides(1:5))

  expect_error(check_sides(1))

})








test_that("check_prob works with ok vectors", {



  expect_true(check_prob(c(0.5, 0.5,0,0,0,0)))

  expect_true(check_prob(c(0, 1,0,0,0,0)))


})





test_that("check_prob fails with invalid lengths", {



  expect_error(check_prob(1:5))

  expect_error(check_prob(1))

})


test_that("check_time fails with invalid lengths", {



  expect_true(check_prob(1))

  expect_true(check_prob(4))

})

test_that("check die and roll", {



  expect_true(die()$prob == c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))

  expect_true(class(roll(die(), 10)) == 'roll')

})

