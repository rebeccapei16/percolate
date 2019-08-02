#' generate_board_mat
#'
#' @param n a positive integer denote the size of the board
#' @param p a number between 0 and 1 that denotes the fraction of the `n^2` squares are blocked
#'
#' @return a matrix
#' @export
#'
#' @examples generate_board_mat(n = 10, p = 0.5)
generate_board_mat <- function(n = 5, p = 0.25){
  assert_that (is.numeric(n), msg = "\nn must be numeric")
  assert_that (length(n) == 1, msg = "\nn must be a numeric vector of length 1")
  assert_that (n %% 1 == 0, msg = "\nn must be an integer")
  assert_that (n > 0, msg = "\nn must be positive")
  assert_that (is.numeric(p), msg = "\np must be numeric")
  assert_that (length(p) == 1, msg = "\np must be a numeric vector of length 1")
  assert_that (p >= 0 && p <= 1, msg = "\np must be between 0 and 1")

  return(matrix(sample(c(rep(1, n^2 - floor(p*n^2)),
                         rep(0, floor(p*n^2)))),
                ncol = n))
}

#' is_valid
#'
#' @param mat a matrix
#'
#' @return TRUE of throw an error
#' @export
#'
#' @examples is_valid(my_matrix)
is_valid <- function(mat) {
  assert_that(is.matrix(mat), msg = "\nmat must be a matrix")
  assert_that(is.numeric(mat), msg = "\nmat must be numeric")
  assert_that(ncol(mat) == nrow(mat), msg = "\n mat must be a square matrix")
  assert_that(all(mat %in% c(0, 1, 2)), msg = "\n mat only contains value 0, 1, 2")
  TRUE
}
