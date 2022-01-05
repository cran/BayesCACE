#' This function returns a partially complete prior string. Used internally - cannot
#' be directly used.
#' @title The function returns a custom string that specifies part of the model (single-study).
#' @param re.values a list of parameter values for the random effects. It should contain the assignment for these
#' parameters only: \code{n.m} and \code{n.s}, which refer to the mean and standard deviation used
#' in the normal distribution estimation of \code{n}, as well as \code{a.m}, \code{a.s}, 
#' \code{alpha.s.m}, \code{alpha.s.s}, \code{alpha.b.m}, \code{alpha.b.s}, \code{alpha.u.m}, \code{alpha.u.s},
#' \code{alpha.v.m}, \code{alpha.v.s}. By default, this is an empty list, and all the mean are set to \code{0}, and 
#' \code{alpha.n.s = alpha.a.s = 0.16}, and \code{alpha.s.s = alpha.b.s = alpha.u.s = alpha.v.s = 0.25}. 
#' @return custom model string
#' @export
#' @examples
#' model.string <- prior.study()
prior.study <- function(re.values = list()){
    n.m <- a.m <- alpha.s.m <- alpha.b.m <- alpha.u.m <- alpha.v.m <- 0
    n.s <- a.s <- 0.16
    alpha.s.s <- alpha.b.s <- alpha.u.s <- alpha.v.s <- 0.25

    if ("n.m" %in% names(re.values)) {n.m <- re.values[['n.m']]}
    if ("n.s" %in% names(re.values)) {n.s <- re.values[['n.s']]}
    if ("a.m" %in% names(re.values)) {a.m <- re.values[['a.m']]}
    if ("a.s" %in% names(re.values)) {a.s <- re.values[['a.s']]}
    if ("alpha.s.m" %in% names(re.values)) {alpha.s.m <- re.values[['alpha.s.m']]}
    if ("alpha.s.s" %in% names(re.values)) {alpha.s.s <- re.values[['alpha.s.s']]}
    if ("alpha.b.m" %in% names(re.values)) {alpha.b.m <- re.values[['alpha.b.m']]}
    if ("alpha.b.s" %in% names(re.values)) {alpha.b.s <- re.values[['alpha.b.s']]}
    if ("alpha.u.m" %in% names(re.values)) {alpha.u.m <- re.values[['alpha.u.m']]}
    if ("alpha.u.s" %in% names(re.values)) {alpha.u.s <- re.values[['alpha.u.s']]}
    if ("alpha.v.m" %in% names(re.values)) {alpha.v.m <- re.values[['alpha.v.m']]}
    if ("alpha.v.s" %in% names(re.values)) {alpha.v.s <- re.values[['alpha.v.s']]}

  prior.string <- sprintf(
    "# priors
    n ~ dnorm(%s, %s)
    a ~ dnorm(%s, %s)
    alpha.s ~ dnorm(%s, %s)
    alpha.b ~ dnorm(%s, %s)
    alpha.u ~ dnorm(%s, %s)
    alpha.v ~ dnorm(%s, %s)
    }",
    n.m, n.s, a.m, a.s, alpha.s.m, alpha.s.s, 
    alpha.b.m, alpha.b.s, alpha.u.m, alpha.u.s, alpha.v.m, alpha.v.s)

return(prior.string)
}
