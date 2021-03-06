% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/breed.R
\name{breed}
\alias{breed}
\title{Breeding to create a new combination of predictors to use in
  the regression, via genetic crossover and mutation by default.}
\usage{
breed(parents, C, n = 1, op = NULL, cluster = NA...)
}
\arguments{
\item{parents}{A tuple (2-list) of ordered numeric vectors,
representing the parents which will breed. Each parent is
represented by the indices of the genes which are active
in its chromosome, e.g. c(1, 4, 7) corresponds to using the
first, fourth, and seventh predictors in the regression.}

\item{C}{The length of chromosomes, i.e. the maximum number of
possible predictors.}

\item{n}{Number of crossover points, up to a maximum of C - 1.}

\item{op}{An optional, user-specified genetic operator function
to carry out the breeding.}

\item{...}{Additional parameters to pass to the user-specific op
function.}
}
\value{
A tuple (2-list) of ordered numeric vector representing
  the two offspring produced by the breeding as indices of the
  genes (predictors) which are active in the chromosome (regression).
}
\description{
Breeding to create a new combination of predictors to use in
  the regression, via genetic crossover and mutation by default.
}
\examples{
C <- 5 ## 5 genes / max number of predictors
## parent generation of size 3
parent_gen <- list(list(c(1, 3), c(4)),
                   list(c(2, 3), c(1,4)),
                   list(c(3), c(1, 3, 4)))
## list of numeric vectors representing the next generation
next_gen <- unlist(lapply(parent_gen, breed, C), FALSE, FALSE)
}
