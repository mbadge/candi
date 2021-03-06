% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{str_case_snake}
\alias{str_case_snake}
\title{Convert string to snake_case.}
\usage{
str_case_snake(x_chr)
}
\arguments{
\item{x_chr}{character vector}
}
\value{
x_snake character vector with same length as x_chr, but snake format
}
\description{
Transform a character vector into a snake case representation.
First, separate camelCase and alphanumeric boundaries into spaces
Second, split pieces by symbols or spaces
Third, convert all alpha to lower with \code{\link[stringr]{str_to_lower}}
Finally, collapse with "_"
}
\seealso{
Other str_case: \code{\link{str_case_title}}
}
