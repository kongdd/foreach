context("test-names")


test_that("numeric vector works", {
    x <- setNames(1:4, 1:4)
    y <- foreach(i = x) %do% { i }

    expect_equal(names(x), names(y))
})

test_that("list  works", {
    x <- as.list(setNames(1:4, 1:4))
    y <- foreach(i = x) %do% { i }

    expect_equal(names(x), names(y))
})

test_that("list  works", {
    x <- data.frame(1, 2, 3, 4)
    y <- foreach(i = x) %do% { i }

    expect_equal(names(x), names(y))
})
