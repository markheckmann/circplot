% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/plot.R
\name{circ_rect}
\alias{circ_rect}
\title{Draw rectangle}
\usage{
circ_rect(xleft, ybottom, xright, ytop, increasing = TRUE, ...)
}
\arguments{
\item{xleft}{a vector (or scalar) of left x positions.}

\item{ybottom}{a vector (or scalar) of bottom y positions.}

\item{xright}{a vector (or scalar) of right x positions.}

\item{ytop}{a vector (or scalar) of top y positions.}

\item{increasing}{Draw from low to high x values.}

\item{...}{Passed on to \code{\link{rect}}.}
}
\description{
Draw rectangle
}
\examples{

circ_plot()
circ_par_init()
ring_set(1)
circ_rect(0,0,.2,1, col="#FF000030")
ring_set(2)
circ_rect(1,0,.2,1, col="#FF000030")

# rectangles are drawn using increasing values by default.
ring_set(3)
circ_rect(1,0,.2,1, increasing=FALSE, col="#FF000030")
}

