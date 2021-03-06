% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generation_gap.R
\name{generation_gap}
\alias{generation_gap}
\title{Replace a proportion G of the old generation with their offspring.}
\usage{
generation_gap(old_gen, children, G = 1)
}
\arguments{
\item{old_gen}{Output of ranked_models() of the previous generation.
A data frame.}

\item{children}{Output of ranked_models() of the children of the
previous generation. A data frame.}

\item{G}{Proportion of old_gen to replace with children.}
}
\value{
The next generation, in descending order of fitness
  (most fit first). A data frame.
}
\description{
Replace a proportion G of the old generation with their offspring.
}
