context("test-lens-laws.R")

test_that("Index lens works", {
  v <- 1:10
  ix4 <- index(4)
  expect_equal(view(v, ix4), 4)
  expect_equal(over(v, ix4, 5)
           , c(1:3, 5, 5:10))
})

test_that("Lens composition works", {
  modified_frame <-
    setNames(seq_along(LETTERS), LETTERS) %>%
    as.list %>%
    as.data.frame %>%
    over(namel %..%
         indexes(10:15) %..%
         index(3)
       , "hi")

  expect_equal(names(modified_frame)[12], "hi")
})
