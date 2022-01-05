#' This function returns a partially complete prior string. Used internally - cannot
#' be directly used.
#' @title The function returns a custom string that specifies part of the model.
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
#' @return custom prior string
#' @export
#' @examples
#' model.string <- prior.meta()
prior.meta <- function(random.effects = list(), re.values = list()){

    delta.n <- delta.a <- delta.u <- delta.v <- delta.s <- delta.b <- cor <- TRUE
    if ("delta.n" %in% names(random.effects)) {delta.n <- random.effects[['delta.n']]}
    if ("delta.a" %in% names(random.effects)) {delta.a <- random.effects[['delta.a']]}
    if ("delta.u" %in% names(random.effects)) {delta.u <- random.effects[['delta.u']]}
    if ("delta.v" %in% names(random.effects)) {delta.v <- random.effects[['delta.v']]}
    if ("delta.s" %in% names(random.effects)) {delta.s <- random.effects[['delta.s']]}
    if ("delta.b" %in% names(random.effects)) {delta.b <- random.effects[['delta.b']]}
    if ("cor" %in% names(random.effects)) {delta.n <- random.effects[['cor']]}
    
    if ((!(delta.n & delta.a)) & cor){
      warning("'cor' can be assigned as TRUE only if both delta.n and delta.a are TRUE.\n
              the model is continued by forcing delta.n=TRUE and delta.a=TRUE")
      delta.n <- TRUE
      delta.a <- TRUE
    }
    
    Ind <- rep(1, 7)
    if (!delta.n) Ind[1] <- 0
    if (!delta.a) Ind[2] <- 0
    if (!delta.u) Ind[3] <- 0
    if (!delta.v) Ind[4] <- 0
    if (!delta.s) Ind[5] <- 0
    if (!delta.b) Ind[6] <- 0
    if (!cor) Ind[7] <- 0

    alpha.n.m <- alpha.a.m <- alpha.s.m <- alpha.b.m <- alpha.u.m <- alpha.v.m <- 0
    alpha.n.s <- alpha.a.s <- 0.16
    alpha.s.s <- alpha.b.s <- alpha.u.s <- alpha.v.s <- 0.25

    if ("alpha.n.m" %in% names(re.values)) {alpha.n.m <- re.values[['alpha.n.m']]}
    if ("alpha.n.s" %in% names(re.values)) {alpha.n.s <- re.values[['alpha.n.s']]}
    if ("alpha.a.m" %in% names(re.values)) {alpha.a.m <- re.values[['alpha.a.m']]}
    if ("alpha.a.s" %in% names(re.values)) {alpha.a.s <- re.values[['alpha.a.s']]}
    if ("alpha.s.m" %in% names(re.values)) {alpha.s.m <- re.values[['alpha.s.m']]}
    if ("alpha.s.s" %in% names(re.values)) {alpha.s.s <- re.values[['alpha.s.s']]}
    if ("alpha.b.m" %in% names(re.values)) {alpha.b.m <- re.values[['alpha.b.m']]}
    if ("alpha.b.s" %in% names(re.values)) {alpha.b.s <- re.values[['alpha.b.s']]}
    if ("alpha.u.m" %in% names(re.values)) {alpha.u.m <- re.values[['alpha.u.m']]}
    if ("alpha.u.s" %in% names(re.values)) {alpha.u.s <- re.values[['alpha.u.s']]}
    if ("alpha.v.m" %in% names(re.values)) {alpha.v.m <- re.values[['alpha.v.m']]}
    if ("alpha.v.s" %in% names(re.values)) {alpha.v.s <- re.values[['alpha.v.s']]}


    string3 <- sprintf(
    "pin <- exp(alpha.n)/(1+exp(alpha.n)+exp(alpha.a))
    pia <- exp(alpha.a)/(1+exp(alpha.n)+exp(alpha.a))
    pic <- 1-pia-pin

    # priors
    alpha.n ~  dnorm(%s, %s)
    alpha.a ~ dnorm(%s, %s)
    alpha.s ~  dnorm(%s, %s)
    alpha.b ~  dnorm(%s, %s)
    alpha.u ~  dnorm(%s, %s)
    alpha.v ~  dnorm(%s, %s)
    ",
    alpha.n.m, alpha.n.s, alpha.a.m, alpha.a.s, alpha.s.m, alpha.s.s, 
    alpha.b.m, alpha.b.s, alpha.u.m, alpha.u.s, alpha.v.m, alpha.v.s)

    if(Ind[7]==1){
      string2_0 <- 
    "delta.n[i] <- delta.rho[1, i]
    delta.a[i] <- delta.rho[2, i]
    delta.rho[1:2, i] ~ dmnorm(c(0, 0), Omega.rho)"
      string4_0 <- 
    "II[1,1] <- 1
    II[2,2] <- 1
    II[1,2] <- 0
    II[2,1] <- 0
    Omega.rho ~  dwish (II[,], 3)
    Sigma.rho <- inverse(Omega.rho)
    sigma.n <- Sigma.rho[1, 1]
    sigma.a <- Sigma.rho[2, 2]
    rho <- Sigma.rho[1, 2]"
    string2_1 <- string2_2 <- ""
    string4_1 <- string4_2 <- ""
    }
      
    else if (Ind[7]==0){
      string2_0 <- string4_0 <- ""
      
      if(Ind[1]==1){
        tau.n.h <- tau.n.r <- 2
        if ("tau.n.h" %in% names(re.values)) {tau.n.h <- re.values[['tau.n.h']]}
        if ("tau.n.r" %in% names(re.values)) {tau.n.r <- re.values[['tau.n.r']]}
        string2_1 <- "delta.n[i] ~ dnorm(0, tau.n)"
        string4_1 <- sprintf(
      "tau.n ~ dgamma(%s, %s)
    sigma.n <- 1/sqrt(tau.n)", tau.n.h, tau.n.r)
      }
      else if (Ind[1]==0){
        string2_1 <- ""
        string4_1 <- ""
      }
      
      if(Ind[2]==1){
        tau.a.h <- tau.a.r <- 2
        if ("tau.a.h" %in% names(re.values)) {tau.a.h <- re.values[['tau.a.h']]}
        if ("tau.a.r" %in% names(re.values)) {tau.a.r <- re.values[['tau.a.r']]}
        string2_2 <- "delta.a[i] ~ dnorm(0, tau.a)"
        string4_2 <- sprintf(
      "tau.a ~ dgamma(%s, %s)
    sigma.a <- 1/sqrt(tau.a)", tau.a.h, tau.a.r)
      }
      else if (Ind[2]==0){
        string2_2 <- ""
        string4_2 <- ""
      }
    }  
      
    if(Ind[3]==1){
      tau.u.h <- tau.u.r <- 2
      if ("tau.u.h" %in% names(re.values)) {tau.u.h <- re.values[['tau.u.h']]}
      if ("tau.u.r" %in% names(re.values)) {tau.u.r <- re.values[['tau.u.r']]}
      string2_3 <- "delta.u[i] ~ dnorm(0, tau.u)"
      string4_3 <- sprintf(
    "u1out <- phi(alpha.u/sqrt(1+sigma.u^2))
    tau.u ~ dgamma(%s, %s)
    sigma.u <- 1/sqrt(tau.u)", tau.u.h, tau.u.r)
    }
    else if (Ind[3]==0){
      string2_3 <- ""
      string4_3 <- "u1out <- phi(alpha.u)"
    }
      
    if(Ind[4]==1){
      tau.v.h <- tau.v.r <- 2
      if ("tau.v.h" %in% names(re.values)) {tau.v.h <- re.values[['tau.v.h']]}
      if ("tau.v.r" %in% names(re.values)) {tau.v.r <- re.values[['tau.v.r']]}
      string2_4 <- "delta.v[i] ~ dnorm(0, tau.v)"
      string4_4 <- sprintf(
    "v1out <- phi(alpha.v/sqrt(1+sigma.v^2))
    CACE <- u1out-v1out
    tau.v ~ dgamma(%s, %s)
    sigma.v <- 1/sqrt(tau.v)", tau.v.h, tau.v.r)
    }
    else if (Ind[4]==0){
      string2_4 <- ""
      string4_4 <- 
    "v1out <- phi(alpha.v)
    CACE <- u1out-v1out"
    }
      
    if(Ind[5]==1){
      tau.s.h <- tau.s.r <- 2
      if ("tau.s.h" %in% names(re.values)) {tau.s.h <- re.values[['tau.s.h']]}
      if ("tau.s.r" %in% names(re.values)) {tau.s.r <- re.values[['tau.s.r']]}
      string2_5 <- "delta.s[i] ~ dnorm(0, tau.s)"
      string4_5 <- sprintf(
    "s1out <- ilogit(alpha.s/sqrt(1 + (16^2*3/(15^2*pi^2))*sigma.s^2))
    tau.s ~ dgamma(%s, %s)
    sigma.s <- 1/sqrt(tau.s)", tau.s.h, tau.s.r)
    }
    else if (Ind[5]==0){
      string2_5 <- ""
      string4_5 <- "s1out <- ilogit(alpha.s)"
    }  
      
    if(Ind[6]==1){
      tau.b.h <- tau.b.r <- 2
      if ("tau.b.h" %in% names(re.values)) {tau.b.h <- re.values[['tau.b.h']]}
      if ("tau.b.r" %in% names(re.values)) {tau.b.r <- re.values[['tau.b.r']]}
      string2_6 <- "delta.b[i] ~ dnorm(0, tau.b)
    }"
      string4_6 <- sprintf(
    "b1out <- ilogit(alpha.b/sqrt(1 + (16^2*3/(15^2*pi^2))*sigma.b^2))
    tau.b ~ dgamma(%s, %s)
    sigma.b <- 1/sqrt(tau.b)
    }", tau.b.h, tau.b.r)
    }
    else if (Ind[6]==0){
      string2_6 <- "
    }"
      string4_6 <- "b1out <- ilogit(alpha.b)
    }"
    }   

    
  modelstring <- paste(string2_0, string2_1, string2_2, string2_3, string2_4, string2_5, string2_6,
                       string3, string4_0, string4_1, string4_2, string4_3, string4_4, string4_5, string4_6, sep="\n")

  return(modelstring)
}

