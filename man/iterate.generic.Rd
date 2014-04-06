\name{iterate.generic}
\alias{iterate.generic}
\title{Dummy for iterating a function that takes its previous result as a parameter}
\usage{
  iterate.generic(f, alpha_0, iterations, ...)
}
\arguments{
  \item{f}{function to iterate. It must be a function with
  at least a numeric input parameter and one numeric result
  (it uses the result of the last iteration as input for
  the next one)}

  \item{alpha_0}{Initial value}

  \item{iterations}{Number of iterations}

  \item{...}{Additional parameters for the function}
}
\description{
  No description.
}
\details{
  No details.
}

