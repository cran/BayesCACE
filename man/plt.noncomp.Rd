% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plt.noncomp.R
\name{plt.noncomp}
\alias{plt.noncomp}
\title{Plotting noncompliance rates for a given dataset}
\usage{
plt.noncomp(data, overall = TRUE, ...)
}
\arguments{
\item{data}{a dataset with structure like the example \code{epidural_c} or \code{epidural_ic}}

\item{overall}{a logical value indicating whether a summary estimate of the compliance rates 
per randomization group is provided. The default is \code{TRUE}. This overall rate is estimated 
using a logit generalized linear mixed model.}

\item{...}{optional parameters passed into the \code{forestplot} function from the
\code{forestplot} library}
}
\value{
A forest plot of noncompliance rates in an \code{R} plot window
}
\description{
Provides a forest plot of noncompliance rates in an \code{R} plot window.
}
\details{
This function provides a visual overview (forest plot) of study-specific 
noncompliance rates in both randomization arms.

 Only studies with full compliance information are included in this plot 
because noncompliance rates cannot be calculated without compliance data. In the generated 
plot, the red dot with its horizontal line shows the study-specific noncompliance rate 
with its 95\% exact confidence interval for the patients randomized to the treatment arm, 
and the blue square with its horizontal line represents that rate and interval for those 
in the control arm. The confidence intervals are calculated by the Clopper--Pearson exact 
method, which is based on the cumulative distribution function of the binomial distribution.
}
\examples{
data("epidural_c", package = "BayesCACE")
plt.noncomp(data=epidural_c, overall = TRUE)
}
