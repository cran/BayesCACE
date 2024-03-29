% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cace.meta.ic.R
\name{cace.meta.ic}
\alias{cace.meta.ic}
\title{Bayesian hierarchical models for CACE meta-analysis with incomplete compliance information}
\usage{
cace.meta.ic(
  data,
  param = c("CACE", "u1out", "v1out", "s1out", "b1out", "pic", "pin", "pia"),
  random.effects = list(),
  re.values = list(),
  model.code = "",
  digits = 3,
  n.adapt = 1000,
  n.iter = 1e+05,
  n.burnin = floor(n.iter/2),
  n.chains = 3,
  n.thin = max(1, floor((n.iter - n.burnin)/1e+05)),
  conv.diag = FALSE,
  mcmc.samples = FALSE,
  study.specific = FALSE
)
}
\arguments{
\item{data}{a input dataset the same structure as the example data \code{epidural_ic}, 
containing multiple rows referring to multiple studies in a meta-analysis.}

\item{param}{the list of parameter used. 

Default to \code{c("CACE", "u1out", "v1out", "s1out", "b1out", "pic", "pin", "pia")}.}

\item{random.effects}{a list of logical values indicating whether random effects are included in the model.
The list should contain the assignment for these parameters only: \code{delta.n} (\eqn{\delta_{in}}), 
\code{delta.a} (\eqn{\delta_{ia}}), \code{delta.u} (\eqn{\delta_{iu}}), \code{delta.v} (\eqn{\delta_{iv}}), 
\code{delta.s} (\eqn{\delta_{is}}), \code{delta.b} (\eqn{\delta_{ib}}), \code{cor}. The list should be in the
form of \code{list(delta.a = FALSE, cor = FALSE, ...)}. By default, this
is an empty list, and all parameters are default to \code{TRUE}. Parameters that are not listed in the list
are assumed to be \code{TRUE}. Note that \eqn{\rho} (\code{cor}) can only be included when both \eqn{\delta_{in}} 
(\code{delta.n}) and \eqn{\delta_{ia}} (\code{delta.a}) are set to \code{TRUE}. Otherwise, a warning 
occurs and the model continues running by forcing \code{delta.n = TRUE} and \code{delta.a = TRUE}.}

\item{re.values}{a list of parameter values for the random effects. It should contain the assignment for these
parameters only: \code{alpha.n.m} and \code{alpha.n.s}, which refer to the mean and standard deviation used
in the normal distribution estimation of \code{alpha.n}, as well as \code{alpha.a.m}, \code{alpha.a.s}, 
\code{alpha.s.m}, \code{alpha.s.s}, \code{alpha.b.m}, \code{alpha.b.s}, \code{alpha.u.m}, \code{alpha.u.s},
\code{alpha.v.m}, \code{alpha.v.s}. It also contains the shape and rate parameters of the gamma distributions
of the standard deviation variable of \code{delta.n}, \code{delta.a}, \code{delta.u}, \code{delta.v}
\code{delta.s}, \code{delta.b}. The shape parameters are named as \code{tau.n.h} and \code{tau.a.h}, for example,
and the rate parameters are named as \code{tau.n.r} and \code{tau.a.r}. You do not need to specify the shape and
rate parameters if the corresponding random effect is set to \code{FALSE} in \code{random.effects}, since they will
not be used anyways. By default, \code{re.values} is an empty list, and all the mean are set to \code{0}, and 
\code{alpha.n.s = alpha.a.s = 0.16}, and \code{alpha.s.s = alpha.b.s = alpha.u.s = alpha.v.s = 0.25},
and the shape and rate parameters are default to \code{2}.}

\item{model.code}{a string representation of the model code; each line should be separated. Default to constructing 
model code using the \code{model.meta.ic} function with the parameters that are inputted to this function. This 
parameter is only necessary if user wishes to make functional changes to the model code, such as changing the
probability distributions of the parameters. Default to empty string.}

\item{digits}{number of digits. Default to \code{3}.}

\item{n.adapt}{adapt value. Default to \code{1000}.}

\item{n.iter}{number of iterations. Default to \code{100000}.}

\item{n.burnin}{number of burn-in iterations. Default to \code{n.iter/2}.}

\item{n.chains}{number of chains. Default to \code{3}.}

\item{n.thin}{thinning rate, must be a positive integer. 

Default to \code{max(1,floor((n.iter-n.burnin)/100000))}.}

\item{conv.diag}{whether or not to show convergence diagnostics. Default to \code{FALSE}.}

\item{mcmc.samples}{whether to include JAGS samples in the final output. Default to \code{FALSE}.}

\item{study.specific}{a logical value indicating whether to calculate the study-specific 
\eqn{\theta^{\mathrm{CACE}}_i}. If \code{TRUE}, the model will first check the logical status of arguments 
\code{delta.u} and \code{delta.v}. If both are \code{FALSE}, meaning that neither response rate \eqn{u_{i1}} 
or \eqn{v_{i1}} is modeled with a random effect, then the study-specific \eqn{\theta^{\mathrm{CACE}}_i} is 
the same across studies. The function gives a warning and continues by making \code{study.specific = FALSE}. 
Otherwise, the study-specific \eqn{\theta^{\mathrm{CACE}}_i} are estimated and saved as the parameter \code{cacei}.}
}
\value{
It returns a model object whose attribute type is \code{cace.Bayes}
}
\description{
This function also estimates \eqn{\theta^{\mathrm{CACE}}} using the Bayesian hierarchcal model 
but can accommodate studies with incomplete compliance data.  
The necessary data structure and the likelihood function are presented in Section 2.3 of the 
package manuscript, "CACE for meta-analysis with incomplete compliance information".
}
\details{
Note that when compiling the \code{JAGS} model, the warning `adaptation incomplete' may 
occasionally occur, indicating that the number of iterations for the adaptation process 
is not sufficient. The default value of \code{n.adapt} (the number of iterations for adaptation) 
is 1,000. This is an initial sampling phase during which the samplers adapt their behavior 
to maximize their efficiency (e.g., a Metropolis--Hastings random walk algorithm may change 
its step size). The `adaptation incomplete' warning indicates the MCMC algorithm may not 
achieve maximum efficiency, but it generally has little impact on the posterior estimates 
of the treatment effects. To avoid this warning, users may increase \code{n.adapt}.
}
\examples{
\donttest{
data("epidural_ic", package = "BayesCACE")
set.seed(123)
out.meta.ic <- cace.meta.ic(data = epidural_ic, conv.diag = TRUE, 
mcmc.samples = TRUE, study.specific = TRUE)
}
}
\references{
\insertRef{zhou2019bayesian}{BayesCACE}
}
\seealso{
\code{\link[BayesCACE]{cace.study}}, \code{\link[BayesCACE]{cace.meta.c}}
}
