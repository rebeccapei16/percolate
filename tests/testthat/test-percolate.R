test_that("percolate.board() work with all open sites",{
  my_board <- board(matrix(rep(1, 100), ncol = 10))
  lst_board <- percolate(my_board)
  result_board <- board(matrix(rep(2, 100), ncol = 10))
  expect_true(identical(lst_board$result_board, result_board))
  expect_true(lst_board$result == T)
})

test_that("percolate.board() work with all blocked sites",{
  my_board <- board(matrix(rep(0, 100), ncol = 10))
  lst_board <- percolate(my_board)
  result_board <- board(matrix(rep(0, 100), ncol = 10))
  expect_true(identical(lst_board$result_board, result_board))
  expect_true(lst_board$result == F)
})

test_that("percolate.board() work with first row blocked",{
  mat <- matrix(c(0,0,0,0,1,0,1,0,1,0,
                  0,1,1,1,0,1,1,1,0,0,
                  0,0,1,0,1,0,1,0,0,0,
                  0,1,1,0,0,0,0,0,0,0,
                  0,0,1,1,0,0,0,0,0,0,
                  0,0,1,0,0,0,1,0,0,0,
                  0,1,1,1,1,1,1,1,1,0,
                  0,0,0,0,0,0,1,0,0,0,
                  0,1,1,1,1,0,1,0,0,0,
                  0,0,0,0,0,0,0,0,0,0), ncol = 10)
  my_board <- board(mat)

  exp_board <- matrix(c(0,0,0,0,1,0,1,0,1,0,
                        0,1,1,1,0,1,1,1,0,0,
                        0,0,1,0,1,0,1,0,0,0,
                        0,1,1,0,0,0,0,0,0,0,
                        0,0,1,1,0,0,0,0,0,0,
                        0,0,1,0,0,0,1,0,0,0,
                        0,1,1,1,1,1,1,1,1,0,
                        0,0,0,0,0,0,1,0,0,0,
                        0,1,1,1,1,0,1,0,0,0,
                        0,0,0,0,0,0,0,0,0,0), ncol = 10)
  exp_board <- board(exp_board)

  lst_board <- percolate(my_board)
  expect_true(identical(lst_board$result_board, exp_board))
  expect_true(lst_board$result == F)
})

test_that("percolate.board() work with last row blocked",{
  mat <- matrix(c(0,0,0,0,1,0,1,0,1,0,
                  1,1,1,1,0,1,1,1,0,0,
                  1,0,1,0,1,0,1,0,0,0,
                  1,1,1,0,0,0,0,0,0,0,
                  0,0,1,1,0,0,0,0,0,0,
                  0,0,1,0,0,0,1,0,0,0,
                  1,1,1,1,1,1,1,1,1,0,
                  0,0,0,0,0,0,1,0,0,0,
                  1,1,1,1,1,0,1,0,0,0,
                  0,0,0,0,0,0,0,0,0,0), ncol = 10)
  my_board <- board(mat)

  exp_board <- matrix(c(0,0,0,0,1,0,1,0,1,0,
                        2,2,2,2,0,1,1,1,0,0,
                        2,0,2,0,1,0,1,0,0,0,
                        2,2,2,0,0,0,0,0,0,0,
                        0,0,2,2,0,0,0,0,0,0,
                        0,0,2,0,0,0,2,0,0,0,
                        2,2,2,2,2,2,2,2,2,0,
                        0,0,0,0,0,0,2,0,0,0,
                        2,2,2,2,2,0,2,0,0,0,
                        0,0,0,0,0,0,0,0,0,0), ncol = 10)
  exp_board <- board(exp_board)

  lst_board <- percolate(my_board)
  expect_true(identical(lst_board$result_board, exp_board))
  expect_true(lst_board$result == F)
})

test_that("percolate.board() works with all the test cases",{
  load(url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata"))

  your_result_list <- lapply(board_list, percolate)

  bool_vec <- sapply(1:length(result_list), function(x){
    your_board <- your_result_list[[x]]$result_board
    result_board <- result_list[[x]]$result_board

    identical(your_board, result_board) *
      (your_result_list[[x]]$result == result_list[[x]]$result)
  })
  expect_true(all(bool_vec))
})
