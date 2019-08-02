test_that("generate default n and p", {
  mat <- generate_board_mat()
  expect_true(ncol(mat) == 5 && nrow(mat) == 5 &&
                mat %in% c(0, 1))
})

test_that("generate default p", {
  mat <- generate_board_mat(n = 9)
  expect_true(ncol(mat) == 9 && nrow(mat) == 9 &&
                all(mat %in% c(0, 1)))
})

test_that("generate p = 0", {
  mat <- generate_board_mat(p = 0)
  expect_true(all(mat == 1))
})

test_that("generate p = 1", {
  mat <- generate_board_mat(p = 1)
  expect_true(all(mat == 0))
})

test_that("generate error n", {
  expect_error(generate_board_mat(n = c(1,2)))
  expect_error(generate_board_mat(n = "asdf"))
  expect_error(generate_board_mat(n = -5))
})




test_that("generate is valid error, not square", {
  mat <- matrix(c(0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0), ncol = 5)
  expect_error(is_valid(mat))
})

test_that("generate is valid error, not matrix", {
  mat <- "not a matrix"
  expect_error(is_valid(mat))
})

test_that("generate is valid error, not contains 0, 1, 2", {
  mat <- matrix(c(1, 0, 0,
                  0, 1, 0,
                  2, 0, 3), ncol = 3)
  expect_error(is_valid(mat))
})




test_that("read boards, identical", {
  load(url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata"))
  boards <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test.txt")
  expect_true(identical(boards, board_list))
})

test_that("read lines, test na list", {
  expect_true(is.na(read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test1.txt")))
  expect_true(is.na(read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test2.txt")))
  expect_true(is.na(read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test3.txt")))
  expect_true(is.na(read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test4.txt")))
  expect_true(is.na(read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test5.txt")))
  expect_true(is.na(read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test6.txt")))
})
