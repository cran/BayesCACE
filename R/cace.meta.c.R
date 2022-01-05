#' This function performs the Bayesian hierarchical model method for meta-analysis 
#' when the dataset has complete compliance information for all studies, 
#' as described in the Section 2.2, "the Bayesian hierarchical model", of the package manuscript.
#' @title Bayesian hierarchical models for CACE meta-analysis with complete compliance data
#' @param data an input dataset with the same structure as the example data \code{epidural_c}, 
#' containing multiple rows referring to multiple studies in a meta-analysis. 
#' @param param a character string vector indicating the parameters to be tracked and estimated. 
#' By default the following parameters (see \code{details}) are included: \eqn{\theta^{\mathrm{CACE}}} 
#' (\code{CACE}), \eqn{E(u_{i1})} (\code{u1out}), \eqn{E(v_{i1})} (\code{v1out}), \eqn{E(s_{i1})} (\code{s1out}), 
#' \eqn{E(b_{i1})} (\code{b1out}), \eqn{\pi_a} (\code{pia}), \eqn{\pi_n} (\code{pin}), and 
#' \eqn{\pi_c=1-\pi_a-\pi_n} (\code{pic}). 
#' Users can modify the string vector to only include parameters of interest besides \eqn{\theta^{\mathrm{CACE}}}. 
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
#' @param model.code a string representation of the model code; each line should be separated. Default to constructing 
#' model code using the \code{model.meta.c} function with the parameters that are inputted to this function. This 
#' parameter is only necessary if user wishes to make functional changes to the model code, such as changing the
#' probability distributions of the parameters. Default to empty string.
#' @param digits number of digits. Default to \code{3}.
#' @param n.adapt adapt value. Default to \code{1000}.
#' @param n.iter number of iterations. Default to \code{100000}.
#' @param n.burnin number of burn-in iterations. Default to \code{n.iter/2}. 
#' @param n.chains number of chains. Default to \code{3}.
#' @param n.thin thinning rate, must be a positive integer. 
#'
#' Default to \code{max(1,floor((n.iter-n.burnin)/100000))}.
#' @param conv.diag whether or not to show convergence diagnostics. Default to \code{FALSE}.
#' @param mcmc.samples whether to include JAGS samples in the final output. Default to \code{FALSE}.
#' @param study.specific a logical value indicating whether to calculate the study-specific 
#' \eqn{\theta^{\mathrm{CACE}}_i}. If \code{TRUE}, the model will first check the logical status of arguments 
#' \code{delta.u} and \code{delta.v}. If both are \code{FALSE}, meaning that neither response rate \eqn{u_{i1}} 
#' or \eqn{v_{i1}} is modeled with a random effect, then the study-specific \eqn{\theta^{\mathrm{CACE}}_i} is 
#' the same across studies. The function gives a warning and continues by making \code{study.specific = FALSE}. 
#' Otherwise, the study-specific \eqn{\theta^{\mathrm{CACE}}_i} are estimated and saved as the parameter \code{cacei}.
#' @return It returns a model object of class \code{cace.Bayes}
#' @importFrom stats update complete.cases
#' @import Rdpack
#' @import rjags
#' @import coda
#' @export
#' @examples
#' \donttest{
#' data("epidural_c", package = "BayesCACE")
#' set.seed(123)
#' out.meta.c <- cace.meta.c(data = epidural_c, conv.diag = TRUE, 
#' mcmc.samples = TRUE, study.specific = TRUE)
#' # By calling the object smry from the output list out.meta.c, posterior estimates 
#' # (posterior mean, standard deviation, posterior median, 95\% credible interval, and 
#' # time-series standard error) are displayed.
#' out.meta.c$smry
#' out.meta.c$DIC
#' }
#' @seealso \code{\link[BayesCACE]{cace.study}}, \code{\link[BayesCACE]{cace.meta.ic}}
#' @references 
#' \insertRef{zhou2019bayesian}{BayesCACE}
#'
#' \insertRef{lunn2012bugs}{BayesCACE}
#'
#' \insertRef{zeger1988models}{BayesCACE}
#' 

cace.meta.c <-
  function(data, 
           param = c("CACE", "u1out", "v1out", "s1out", "b1out", 
                   "pic", "pin", "pia"),
           random.effects = list(), re.values = list(), model.code = '',
           digits = 3, n.adapt = 1000, n.iter = 100000,
           n.burnin = floor(n.iter/2), n.chains = 3, n.thin = max(1,floor((n.iter-n.burnin)/100000)),
           conv.diag = FALSE, mcmc.samples = FALSE, study.specific = FALSE)    {
    ## check the input parameters
    
    if(missing(data)) stop("Need to specify data.")
    if(!missing(data) ){
      study.id <- data$study.id[complete.cases(data)]
      n000<-data$n000[complete.cases(data)]
      n001<-data$n001[complete.cases(data)]
      n010<-data$n010[complete.cases(data)]
      n011<-data$n011[complete.cases(data)]
      n100<-data$n100[complete.cases(data)]
      n101<-data$n101[complete.cases(data)]
      n110<-data$n110[complete.cases(data)]
      n111<-data$n111[complete.cases(data)]
      message("NA is not allowed in the input data set; the rows containing NA are removed.")
    }

    if(length(study.id)!=length(n000) | length(n000)!=length(n001) | length(n001)!=length(n010) | 
       length(n010)!=length(n011) | length(n011)!=length(n100) | length(n100)!=length(n101) |
       length(n101)!=length(n110) | length(n110)!=length(n111) )
      stop("study.id, n000, n001, n010, n011, n100, n101, n110, and n111 have different lengths. \n")


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
    
    if ((!(delta.u|delta.v)) & study.specific){
      warning("no random effect is assigned to the response rate u1 or v1, \n
           study-specific CACE is the same across studies. \n
           the model is continued by making 'study.specific=FALSE'. \n
           to make a CACE forestplot, please run 'cace.study' to estimate study level CACEs. \n")
      study.specific <- FALSE
    }
      
    
    Ind <- rep(1, 7)
    if (!delta.n) Ind[1] <- 0
    if (!delta.a) Ind[2] <- 0
    if (!delta.u) Ind[3] <- 0
    if (!delta.v) Ind[4] <- 0
    if (!delta.s) Ind[5] <- 0
    if (!delta.b) Ind[6] <- 0
    if (!cor) Ind[7] <- 0
    
    ## jags model
    if (nchar(model.code) == 0) {
      modelstring<-model.meta.c(random.effects = random.effects, re.values = re.values)
    }
    else {modelstring <- model.code}

    ## jags data
    Ntol <- n000+n001+n010+n011+n100+n101+n110+n111
    N0 <- n000+n001+n010+n011
    N1 <- n100+n101+n110+n111
    R <- cbind(n000,n001,n010,n011, n100,n101,n110,n111)
    I <- length(Ntol)
    pi <- pi
    data.jags <- list(N0=N0, N1=N1, R=R, I=I, Ind=Ind, pi=pi)

    
    ## jags initial value
    rng.seeds<-sample(1000000,n.chains)
    init.jags <- vector("list", n.chains)
    for(ii in 1:n.chains){
      init.jags[[ii]] <- list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = rng.seeds[ii])
    }
    
    ## parameters to be paramed in jags
    if(!is.element("CACE",param)) param<-c("CACE",param)

    fullparam <- c("CACE", "u1out", "v1out", "s1out", "b1out", 
                   "pic", "pin", "pia", "alpha.n", "alpha.a", 
                   "alpha.u", "alpha.v", "alpha.s", "alpha.b", 
                   "sigma.n", "sigma.a", "rho", "Sigma.rho",
                   "sigma.u", "sigma.v", "sigma.s", "sigma.b")
    if(!any(is.element(param, fullparam))) stop("parameters must be specified from the following:  
                  CACE, u1out, v1out, s1out, b1out, 
                   pic, pin, pia, alpha.n, alpha.a, 
                alpha.u, alpha.v, alpha.s, alpha.b, 
                sigma.n, sigma.a, rho, Sigma.rho,
                sigma.u, sigma.v, sigma.s, sigma.b")
    if(study.specific) param<-c("cacei",param)
    
    ## run jags
    message("Start running MCMC...\n")
    jags.m<-jags.model(file=textConnection(modelstring),data=data.jags,inits=init.jags,
                       n.chains=n.chains,n.adapt=n.adapt)
    update(jags.m,n.iter=n.burnin)
    jags.out<-coda.samples.dic(model=jags.m,variable.names=param,n.iter=n.iter,thin=n.thin)
    
    out<-NULL
    out$Ind <- Ind
    out$model<-"cace.meta.c"
    
    smry<-summary(jags.out$samples)
    smry<-cbind(smry$statistics[,c("Mean","SD")],smry$quantiles[,c("2.5%","50%","97.5%")], 
                smry$statistics[,c("Naive SE","Time-series SE")])
    smry<-signif(smry,digits=digits)
    out$smry <- smry

    #dic
    dev<-jags.out$dic[[1]] # mean deviance
    pen<-jags.out$dic[[2]] # pD
    pen.dev<-dev+pen # DIC
    dic.stat<-rbind(dev,pen,pen.dev)
    rownames(dic.stat)<-c("D.bar","pD","DIC")
    colnames(dic.stat)<-""
    out$DIC<-dic.stat

    for (i in 1:length(fullparam)){
      if(is.element(fullparam[i],param)) {out[[fullparam[i] ]]<-smry[c(fullparam[i]), ]}
    } 
    
    if(conv.diag){
      message("MCMC convergence diagnostic statistics are calculated and saved in conv.out\n")
      conv.out<-gelman.diag(jags.out$samples,multivariate=FALSE)
      out$conv.out<-conv.out$psrf
    }
    
    if(mcmc.samples){
      out$mcmc.samples<-jags.out$samples
    }

    class(out)<-"cace.Bayes"
    return(out) 
}


