% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_streamorder-level.R
\name{add_streamorder}
\alias{add_streamorder}
\alias{add_streamorder.data.frame}
\title{add Streamorder}
\usage{
add_streamorder(x, status = TRUE)

\method{add_streamorder}{data.frame}(x, status = TRUE)
}
\arguments{
\item{x}{data.frame network compatible with \link{hydroloom_names}.}

\item{status}{boolean if status updates should be printed.}
}
\value{
data.frame containing added \code{stream_order} and \code{stream_calc} attribute.
}
\description{
Adds a strahler stream order.
Algorithm: If more than one upstream flowpath has an order equal to the
maximum upstream order then the downstream flowpath is assigned the maximum
upstream order plus one. Otherwise it is assigned the maximum upstream order.

To match the nhdplus algorithm, non-dendritic network connectivity and a
\code{divergence} attribute must be included. All secondary paths will have the
\code{stream_order} of upstream primary paths and a \code{stream_calc} value of 0.
Secondary paths have no affect on the order of downstream paths.
}
\examples{
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

x <- add_toids(x, return_dendritic = FALSE)

x <- add_streamorder(x)

plot(sf::st_geometry(x), lwd = x$stream_order, col = "blue")
plot(sf::st_geometry(x), lwd = x$stream_calc, col = "blue")

}
