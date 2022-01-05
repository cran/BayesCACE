#' This function also estimates \eqn{\theta^{\mathrm{CACE}}} using the Bayesian hierarchcal model 
#' but can accommodate studies with incomplete compliance data.  
#' The necessary data structure and the likelihood function are presented in Section 2.3 of the 
#' package manuscript, "CACE for meta-analysis with incomplete compliance information".
#' @title Bayesian hierarchical models for CACE meta-analysis with incomplete compliance information
#' @param data a input dataset the same structure as the example data \code{epidural_ic}, 
#' containing multiple rows referring to multiple studies in a meta-analysis. 
#' @param param the list of parameter used. 
#' 
#' Default to \code{c("CACE", "u1out", "v1out", "s1out", "b1out", "pic", "pin", "pia")}.
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
#' model code using the \code{model.meta.ic} function with the parameters that are inputted to this function. This 
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
#' @details  
#' Note that when compiling the \code{JAGS} model, the warning `adaptation incomplete' may 
#' occasionally occur, indicating that the number of iterations for the adaptation process 
#' is not sufficient. The default value of \code{n.adapt} (the number of iterations for adaptation) 
#' is 1,000. This is an initial sampling phase during which the samplers adapt their behavior 
#' to maximize their efficiency (e.g., a Metropolis--Hastings random walk algorithm may change 
#' its step size). The `adaptation incomplete' warning indicates the MCMC algorithm may not 
#' achieve maximum efficiency, but it generally has little impact on the posterior estimates 
#' of the treatment effects. To avoid this warning, users may increase \code{n.adapt}.
#' @importFrom stats update complete.cases
#' @import Rdpack
#' @import rjags
#' @import coda
#' @export 
#' @examples
#' \donttest{
#' data("epidural_ic", package = "BayesCACE")
#' set.seed(123)
#' out.meta.ic <- cace.meta.ic(data = epidural_ic, conv.diag = TRUE, 
#' mcmc.samples = TRUE, study.specific = TRUE)
#' }
#' @seealso \code{\link[BayesCACE]{cace.study}}, \code{\link[BayesCACE]{cace.meta.c}}
#' @references 
#' \insertRef{zhou2019bayesian}{BayesCACE}
#' 
cace.meta.ic <-
  function(data, param = c("CACE", "u1out", "v1out", "s1out", "b1out", 
                   "pic", "pin", "pia"),
           random.effects = list(), re.values = list(), model.code = '',
           digits = 3, n.adapt = 1000, n.iter = 100000,
           n.burnin = floor(n.iter/2), n.chains = 3, 
           n.thin = max(1,floor((n.iter-n.burnin)/100000)),
           conv.diag = FALSE, mcmc.samples = FALSE, study.specific = FALSE)    {
    ## check the input parameters
    
    if(missing(data)) stop("need to specify data")
    if(!missing(data) ){
      data$miss.r0 <- ifelse((data$n000==0 & data$n001==0 & data$n010==0 & data$n011==0), 1, 0)
      data$miss.r1 <- ifelse((data$n100==0 & data$n101==0 & data$n110==0 & data$n111==0), 1, 0)
      data$miss <- ifelse((data$miss.r0==1|data$miss.r1==1), 1, 0)
      temp <- data[order(data$miss.r0, data$miss.r1),]
      study.id <- temp$study.id[complete.cases(temp)]
      n000<-temp$n000[complete.cases(temp)]
      n001<-temp$n001[complete.cases(temp)]
      n010<-temp$n010[complete.cases(temp)]
      n011<-temp$n011[complete.cases(temp)]
      n100<-temp$n100[complete.cases(temp)]
      n101<-temp$n101[complete.cases(temp)]
      n110<-temp$n110[complete.cases(temp)]
      n111<-temp$n111[complete.cases(temp)]
      n0s0<-temp$n0s0[complete.cases(temp)]
      n0s1<-temp$n0s1[complete.cases(temp)]
      n1s0<-temp$n1s0[complete.cases(temp)]
      n1s1<-temp$n1s1[complete.cases(temp)]
      miss.r0<-temp$miss.r0[complete.cases(temp)]
      miss.r1<-temp$miss.r1[complete.cases(temp)]
      miss<-temp$miss[complete.cases(temp)]
      message("NA is not allowed in the input data set; the rows containing NA are removed.")
    }
    
    if(length(study.id)!=length(n000) | length(n000)!=length(n001) | length(n001)!=length(n010) | 
       length(n010)!=length(n011) | length(n011)!=length(n100) | length(n100)!=length(n101) |
       length(n101)!=length(n110) | length(n110)!=length(n111) | length(n111)!=length(n0s0) | 
       length(n0s0)!=length(n0s1) | length(n0s1)!=length(n1s0) | length(n1s0)!=length(n1s1) )
      stop("study.id, n000, n001, n010, n011, n100, n101, n110, n111, 
           n0s0, n0s1, n1s0, and n1s1 have different lengths. \n")
    
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
    
    if (!(delta.u|delta.v)){
      warning("no random effect is assigned to the response rate u1 or v1, \n
              study-specific CACE is the same across studies. \n
              a CACE forestplot cannot be made. \n")
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
      modelstring<-model.meta.ic(random.effects = random.effects, re.values = re.values)
    }
    else {modelstring <- model.code}

    ## jags data
    
    n1 <- sum(miss.r0==0 & miss.r1==0) #  4+4
    n2 <- sum(miss.r0==0 & miss.r1==1) #  4+2
    n3 <- sum(miss.r0==1 & miss.r1==0) #  2+4
    n4 <- sum(miss.r0==1 & miss.r1==1) #  4+4
    n <- length(study.id)
    
    N0_4 <- n000+n001+n010+n011
    N0_2 <- n0s1+n0s0
    N1_4 <- n100+n101+n110+n111
    N1_2 <- n1s1+n1s0
    
    R0_4 <- cbind(n000,n001,n010,n011)
    R0_2 <- cbind(n0s0,n0s1)
    R1_4 <- cbind(n100,n101,n110,n111)
    R1_2 <- cbind(n1s0,n1s1)
    
    R1 <- cbind(R0_4, R1_4)
    R2 <- cbind(R0_4, R1_2)
    R3 <- cbind(R0_2, R1_4)
    R4 <- cbind(R0_2, R1_2)
    pi <- pi
    
    data.jags <- list(N0_4=N0_4, N0_2=N0_2, N1_4=N1_4, N1_2=N1_2, 
                 R0_4=R0_4, R0_2=R0_2, R1_4=R1_4, R1_2=R1_2, 
                 n1=n1, n2=n2, n3=n3, n4=n4, Ind=Ind, pi=pi)
    
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
    out$model<-"cace.meta.ic"
    
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


