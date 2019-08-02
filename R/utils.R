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
#' @examples
is_valid <- function(mat) {
  assert_that(is.matrix(mat), msg = "\nmat must be a matrix")
  assert_that(is.numeric(mat), msg = "\nmat must be numeric")
  assert_that(ncol(mat) == nrow(mat), msg = "\n mat must be a square matrix")
  assert_that(all(mat %in% c(0, 1, 2)), msg = "\n mat only contains value 0, 1, 2")
  TRUE
}




#' read_boards
#'
#' @param link a url link
#'
#' @return boards list of boards
#' @export
#'
#' @examples read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test.txt")
read_boards <- function(link){
  raw <- readLines(link)
  raw <- trimws(raw[raw != ""])
  all_lines <- paste(raw, collapse = " ")
  all_lines <- unlist(strsplit(all_lines, split="----"))
  all_lines <- all_lines[all_lines!=""]
  all_lines <- strsplit(all_lines, split=" ")
  if(length(all_lines) == 1) return(lines_to_board(all_lines[[1]]))
  boards <- sapply(all_lines, lines_to_board)
  return(boards)
}


#' lines_to_board, helper function for read_boards
#'
#' @param line turn the lines into boards
#'
#' @return turn the lines into boards
#' @export
#'
#' @examples
lines_to_board <- function(line){
  line <- line[line != ""]
  n <- as.numeric(line[1])
  mat <- line[2:length(line)]
  if (!is_valid_lines_to_board(n, mat)) return(list(NA))
  mat <- ifelse(mat == "*", 0, 1)
  mat <- matrix(mat, n, byrow = T)
  return(board(mat))
}

#' is_valid_lines_to_board, helper function for read_boards
#'
#' @param n row and col
#' @param mat matrix
#'
#' @return True if valid
#' @export
#'
#' @examples
is_valid_lines_to_board <- function(n, mat){
  if(is.na(n)) return(FALSE)
  if(!is.numeric(n)) return(FALSE)
  if(!n %% 1 == 0) return(FALSE)
  if(!n > 0) return(FALSE)
  if(!all(mat %in% c("*", "."))) return(FALSE)
  if(!n*n == length(mat)) return(FALSE)
  TRUE
}
