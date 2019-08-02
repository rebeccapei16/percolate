#' helper function for percolate board, get the locations that are filled
#'
#' @param board a board
#'
#' @return row and column
#' @export
#'
#' @examples
get_filled_location <- function(board){
  index <- which(board == 2)
  n <- attr(board, "n")
  return(cbind(col = (index-1) %/% n + 1,
               row = (index-1) %% n + 1))
}

#' fill, helper function for percolate board
#'
#' @param board board
#' @param filled location that are already filled
#'
#' @return
#' @export
#'
#' @examples
fill <- function(board, filled){
  result_board <- board
  for(i in 1:nrow(filled)){
    row <- filled[i,2]
    col <- filled[i,1]
    if(row - 1 >= 1 && result_board[row - 1, col] == 1) {
      result_board[row - 1, col] <- 2
    }
    if(row + 1 <= nrow(result_board) && result_board[row + 1, col] == 1) {
      result_board[row + 1, col] <- 2
    }
    if(col - 1 >= 1 && result_board[row, col - 1] == 1) {
      result_board[row, col - 1] <- 2
    }
    if(col + 1 <= nrow(result_board) && result_board[row, col + 1] == 1) {
      result_board[row, col + 1] <- 2
    }
  }
  return(result_board)
}

percolate <- function(x,...){ UseMethod("percolate") }


#' percolate.board
#'
#' @param board a board object
#'
#' @return a list, result_board is the percolated board, result is True if last row is filled
#' @export
#'
#' @examples
percolate.board <- function(board){
  b <- board
  n <- attr(b, "n")
  assert_that(all(board %in% c(0, 1)))
  new_b <- b
  new_b[1,] <- ifelse(new_b[1,] == 1, 2, new_b[1,])
  changed <- ifelse(all.equal(new_b, b) == T, F, T)
  while(changed){
    b <- new_b
    location <- get_filled_location(b)
    new_b <- fill(b, location)
    changed <- ifelse(all.equal(new_b, b) == T, F, T)
  }
  return(list(result_board = new_b,
              result = any(new_b[n,] == 2)))
}
