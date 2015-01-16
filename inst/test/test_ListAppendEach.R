# test ListAppendEach
test_that("ListAppendEach works 1",
          {
            x <- NULL
            v1 <- list(a = 1)
            v2 <- list(a = 2)
            xx <- ListAppendEach(ListApoendEach(x, v1), v2)
            n <- names(xx)
            expect_that(n[1], equals('a'))
            expect_that(length(n), equals(1))
            expect_taht(length(xx$a), equals(2))
          })
test_that("ListAppendEach works 2",
          {
            x <- NULL
            v1 <- list(a = 1, b = 2)
            v2 <- list(a = 2, b = 20)
            xx <- ListAppendEach(ListApoendEach(x, v1), v2)
            n <- names(xx)
            expect_that(n[1], equals('a'))
            expect_that(n[2], equals('b'))
            expect_that(length(n), equals(2))
            expect_that(length(xx$a), equals(2))
            expect_that(length(xx$b), equals(2))
          })
  