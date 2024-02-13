test_that("make groups can work well", {
  friends <- make_groups(c("Anna", "Femke", "Luc", "Stefan"))
  expect_true(ncol(friends) == 2)
})
