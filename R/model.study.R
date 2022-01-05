#' This function generates the model code for a single study using the 
#' likelihood and model specified in Section 2.1, or a two-step 
#' approach for meta-analysis with complete compliance information as 
#' described in Section 2.2, "The two-step approach" of the package manuscript.
#' This function will be called internally if user uses the \code{cace.study} function.
#' @title Model code of CACE analysis for a single study, or a two-step approach for meta-analysis 
#' with complete complice information
#' @param re.values a list of parameter values for the random effects. It should contain the assignment for these
#' parameters only: \code{n.m} and \code{n.s}, which refer to the mean and standard deviation used
#' in the normal distribution estimation of \code{n}, as well as \code{a.m}, \code{a.s}, 
#' \code{alpha.s.m}, \code{alpha.s.s}, \code{alpha.b.m}, \code{alpha.b.s}, \code{alpha.u.m}, \code{alpha.u.s},
#' \code{alpha.v.m}, \code{alpha.v.s}. By default, this is an empty list, and all the mean are set to \code{0}, and 
#' \code{alpha.n.s = alpha.a.s = 0.16}, and \code{alpha.s.s = alpha.b.s = alpha.u.s = alpha.v.s = 0.25}. 
#' @return It returns a model string
#' @export
#' @examples
#' model.string <- model.study()
model.study <- function(re.values = list()){

    mod.string <- "model{
      prob[1] <- (pi.n*(1-s1) + pi.c*(1-v1))
      prob[2] <- (pi.n*s1 + pi.c*v1)
      prob[3] <- (pi.a*(1-b1))
      prob[4] <- (pi.a*b1)
      prob[5] <- (pi.n*(1-s1))
      prob[6] <- (pi.n*s1)
      prob[7] <- (pi.c*(1-u1)+pi.a*(1-b1))
      prob[8] <- (pi.c*u1+pi.a*b1)
      
      R[1:4] ~ dmulti(prob[1:4], N0)
      R[5:8] ~ dmulti(prob[5:8], N1)
      
      pi.n <- exp(n)/(1+exp(n)+exp(a))
      pi.a <- exp(a)/(1+exp(n)+exp(a))
      pi.c <- 1-pi.a-pi.n
      probit(u1) <- alpha.u
      probit(v1) <- alpha.v
      logit(s1) <- alpha.s
      logit(b1) <- alpha.b
      
      CACE <- u1-v1
    "
    prior.string <- prior.study(re.values = re.values)
    modelstring <- paste(mod.string, prior.string, sep="\n")
      
    return(modelstring)
}
