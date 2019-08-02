#' board
#'
#' @param mat matrix into a board
#' @param n if there is no matrix, n by n matrix
#' @param p if there is no matrix, p is the percentage of the squared that are blocked
#'
#' @return a matrix object, inherited from the matrix class, attr n and p
#' @export
#'
#' @examples
board <- function(mat = NULL, n = 5, p = 0.25){
  if(is.null(mat)){
    mat <- generate_board_mat(n, p)
  } else {
    is_valid(mat)
    n <- ncol(mat)
    p <- sum(mat == 0) / n^2
  }
  object <- mat
  attr(object, "n") <- n
  attr(object, "p") <- p
  class(object) <- c("board", "matrix")
  object
}
