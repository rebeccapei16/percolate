test_that("board input mat, return same structure, n, p", {
  my_matrix <- matrix(c(1, 0, 0,
                        0, 1, 0,
                        1, 0, 0), ncol = 3)
  my_board <- board(my_matrix)
  expect_equivalent(unclass(my_board), my_matrix)
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
