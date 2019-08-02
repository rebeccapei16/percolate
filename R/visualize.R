#' plot.board
#'
#' @param board board object
#' @param grid True, dashed lines indicate the grid
#'
#' @return a plot of the board
#' @export
#'
#' @examples
plot.board <- function(board, grid = TRUE){
  if (!is_valid(board)) {
    stop("Must be a valid board")
  }
  n <- attr(board, "n")
  df_board <- tidyr::gather(data.frame(row = 1:n, board),
                            key = "column", value = "value", -row)
  df_board$column <- as.numeric(substr(df_board$column, 2, nchar(df_board$column)))
  graph <- ggplot(df_board, aes(x = row, y =  column, fill = factor(value))) +
    geom_tile(color = "black", size = ifelse(grid, 0.5, 0), linetype="dashed") +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          aspect.ratio = 1) +
    scale_fill_manual(values = c("0" = "black",
                                 "1" = "white",
                                 "2" = "lightblue3")) +
    labs(title = paste("Board size: ", n)) +
    coord_flip(xlim = NULL, ylim = NULL, expand = TRUE,
               clip = "off")
  return(graph)
}
