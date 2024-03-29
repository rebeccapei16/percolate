% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{generate_board_mat}
\alias{generate_board_mat}
\title{generate_board_mat}
\usage{
generate_board_mat(n = 5, p = 0.25)
}
\arguments{
\item{n}{a positive integer denote the size of the board}

\item{p}{a number between 0 and 1 that denotes the fraction of the `n^2` squares are blocked}
}
\value{
a matrix
}
\description{
generate_board_mat
}
\examples{
generate_board_mat(n = 10, p = 0.5)
}
