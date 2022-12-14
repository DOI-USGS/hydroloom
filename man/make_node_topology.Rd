% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_node_topology.R
\name{make_node_topology}
\alias{make_node_topology}
\alias{make_node_topology.data.frame}
\alias{make_node_topology.hy}
\title{make node topology from edge topology}
\usage{
make_node_topology(x, add_div = NULL, add = TRUE)

\method{make_node_topology}{data.frame}(x, add_div = NULL, add = TRUE)

\method{make_node_topology}{hy}(x, add_div = NULL, add = TRUE)
}
\arguments{
\item{x}{data.frame network compatible with \link{hydroloom_names}.}

\item{add_div}{data.frame containing id and toid diverted paths to add.
Should have id and toid fields.}

\item{add}{logical if TRUE, node topology will be added to x in return.}
}
\value{
data.frame containing id, fromnode, and tonode attributes or all
attributes provided with id, fromnode and tonode in the first three columns.
}
\description{
creates a node topology table from an edge topology
}
\examples{
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

y <- dplyr::select(add_toids(x), -ToNode, -FromNode)

y <- make_node_topology(y)

# just the divergences which have unique fromids in x but don't in new hope.
div <- add_toids(dplyr::select(x, COMID, FromNode, ToNode),
                 return_dendritic = FALSE)
div <- div[div$toid \%in\%
           x$COMID[x$Divergence == 2],]

y <- dplyr::select(add_toids(x), -ToNode, -FromNode)

y <- make_node_topology(y, add_div = div)

}
