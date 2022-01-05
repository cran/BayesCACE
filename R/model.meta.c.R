#' This function generates part of the model code for meta-analysis 
#' when the dataset has complete compliance information for all studies, 
#' as described in Section 2.2, "the Bayesian hierarchical model" of the package manuscript.
#' This function will be called internally if user uses the \code{cace.meta.c} function.
#' @title Bayesian hierarchical model code for CACE meta-analysis with complete compliance data
#' @param random.effects a list of logical values indicating whether random effects are included in the model.
#' The list should contain the assignment for these parameters only: \code{delta.n} (\eqn{\delta_{in}}), 
#' \code{delta.a} (\eqn{\delta_{ia}}), \code{delta.u} (\eqn{\delta_{iu}}), \code{delta.v} (\eqn{\delta_{iv}}), 
#' \code{delta.s} (\eqn{\delta_{is}}), \code{delta.b} (\eqn{\delta_{ib}}), \code{cor}. The list should be in the
#' form of \code{list(delta.a = FALSE, cor = FALSE, ...)}. By default, this
#' is an empty list, and all parameters are default to \code{TRUE}. Parameters that are not listed in the list
#' are assumed to be \code{TRUE}. Note that \eqn{\rho} (\code{cor}) can only be included when both \eqn{\delta_{in}} 
#' (\code{delta.n}) and \eqn{\delta_{ia}} (\code{delta.a}) are set to \code{TRUE}. Otherwise, a warning 
#' occurs and the model continues running by forcing \code{delta.n = TRUE} and \code{delta.a = TRUE}. 
#' @param re.values a list of parameter values for the random effects. It should contain the assignment for these
#' parameters only: \code{alpha.n.m} and \code{alpha.n.s}, which refer to the mean and standard deviation used
#' in the normal distribution estimation of \code{alpha.n}, as well as \code{alpha.a.m}, \code{alpha.a.s}, 
#' \code{alpha.s.m}, \code{alpha.s.s}, \code{alpha.b.m}, \code{alpha.b.s}, \code{alpha.u.m}, \code{alpha.u.s},
#' \code{alpha.v.m}, \code{alpha.v.s}. It also contains the shape and rate parameters of the gamma distributions
#' of the standard deviation variable of \code{delta.n}, \code{delta.a}, \code{delta.u}, \code{delta.v}
#' \code{delta.s}, \code{delta.b}. The shape parameters are named as \code{tau.n.h} and \code{tau.a.h}, for example,
#' and the rate parameters are named as \code{tau.n.r} and \code{tau.a.r}. You do not need to specify the shape and
#' rate parameters if the corresponding random effect is set to \code{FALSE} in \code{random.effects}, since they will
#' not be used anyways. By default, \code{re.values} is an empty list, and all the mean are set to \code{0}, and 
#' \code{alpha.n.s = alpha.a.s = 0.16}, and \code{alpha.s.s = alpha.b.s = alpha.u.s = alpha.v.s = 0.25},
#' and the shape and rate parameters are default to \code{2}.
#' @return It returns a model string
#' @export
#' @examples
#' # use default settings
#' model.string <- model.meta.c()
model.meta.c <- function(random.effects = list(), re.values = list()){

    # model string
    mod.string <-
    "model{
    for (i in 1:I) {

    prob[i, 1] <- (pi.n[i]*(1-s1[i]) + pi.c[i]*(1-v1[i]))
    prob[i, 2] <- (pi.n[i]*s1[i] + pi.c[i]*v1[i])
    prob[i, 3] <- (pi.a[i]*(1-b1[i]))
    prob[i, 4] <- (pi.a[i]*b1[i])
    prob[i, 5] <- (pi.n[i]*(1-s1[i]))
    prob[i, 6] <- (pi.n[i]*s1[i])
    prob[i, 7] <- (pi.c[i]*(1-u1[i])+pi.a[i]*(1-b1[i]))
    prob[i, 8] <- (pi.c[i]*u1[i]+pi.a[i]*b1[i])

    R[i, 1:4] ~ dmulti(prob[i, 1:4], N0[i])
    R[i, 5:8] ~ dmulti(prob[i, 5:8], N1[i])

    n[i] <- alpha.n + Ind[1]*delta.n[i]
    a[i] <- alpha.a + Ind[2]*delta.a[i]
    pi.n[i] <- exp(n[i])/(1+exp(n[i])+exp(a[i]))
    pi.a[i] <- exp(a[i])/(1+exp(n[i])+exp(a[i]))
    pi.c[i] <- 1-pi.a[i]-pi.n[i]
    probit(u1[i]) <- alpha.u + Ind[3]*delta.u[i]
    probit(v1[i]) <- alpha.v + Ind[4]*delta.v[i]
    logit(s1[i]) <- alpha.s + Ind[5]*delta.s[i]
    logit(b1[i]) <- alpha.b + Ind[6]*delta.b[i]

    cacei[i] <- u1[i]-v1[i]
    "
      
  prior.string <- prior.meta(random.effects = random.effects, re.values = re.values)
  modelstring <- paste(mod.string, prior.string, sep="\n")
  return(modelstring)
}