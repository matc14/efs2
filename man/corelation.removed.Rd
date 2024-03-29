% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corelation.R
\name{corelation.removed}
\alias{corelation.removed}
\title{Function to remove highly correlated variables}
\usage{
corelation.removed(data, list.selected.var, list.index.cross, level.cor)
}
\arguments{
\item{data}{input data where columns are variables and rows are observations (all numeric)}

\item{list.selected.var}{A \code{\link{list}} with selected variables in cross-validation}

\item{list.index.cross}{A \code{\link{list}} with indexes obtained in cross-validation}

\item{level.cor}{cutoff level of correlated variables. If equal to 1 is not performed}
}
\value{
A \code{\link{list}} with selected features for each iteration of cross-validation
}
\description{
Function to remove highly correlated variables
}
\details{
Removes highly correlated variables after selection performed in cross-validation
}
\examples{
\dontrun{

class <- data$class
data$class <- NULL

list.index.cross <- cross.val(x = data,
                             y = decisions,
                             method = 'kfoldcv',
                             params.cv = list(niter = 10, k = 3))

list.selected.var <- feature.selection(x = data,
                                       y = class,
                                       method = 'fs.utest',
                                       list.index.cross = indexes,
                                       params = list(adjust = 'fdr'))
                             
list.selected.var.uncor <- corelation.removed(data,
                                              list.selected.var,
                                              list.index.cross,
                                              0.75)

}

}
