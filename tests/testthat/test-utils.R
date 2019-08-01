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





test_that("board input mat, return same structure, n, p", {
  my_matrix <- matrix(c(1, 0, 0,
                        0, 1, 0,
                        1, 0, 0), ncol = 3)
  my_board <- board(my_matrix)
  expect_equivalent(my_board, my_matrix)
  expect_true(attr(my_board, "n") == 3)
  expect_true(attr(my_board, "p") == 2/3)
})

test_that("board input invalid, error", {
  invalid_mat1 <- matrix(c(0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0), ncol = 5)
  invalid_mat2 <- "not a matrix"
  invalid_mat3 <- matrix(c(1, 0, 0,
                           0, 1, 0,
                           2, 0, 3), ncol = 3)
  expect_error(board(mat = invalid_mat1))
  expect_error(board(mat = invalid_mat2))
  expect_error(board(mat = invalid_mat3))
})

test_that("board input n and p, valid", {
  expect_true(is_valid(board(n = 5, p = 0.3)))
  expect_true(is_valid(board(n = 3, p = 1)))
  expect_true(is_valid(board(n = 4, p = 0)))
})



