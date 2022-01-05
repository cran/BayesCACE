#' This function performs CACE analysis for a single study using the 
#' likelihood and model specified in Section 2.1 of the package manuscript, or a two-step 
#' approach for meta-analysis with complete compliance information as 
#' described in Section 2.2, "the two-step approach".
#' @title CACE analysis for a single study, or a two-step approach for meta-analysis 
#' with complete complice information
#' @param data a input dataset the same structure as the example data \code{epidural_c}, 
#' containing either one row of observations for a single study, or multiple rows referring 
#' to multiple studies in a meta-analysis. 
#' @param param a character string vector indicating the parameters to be tracked and estimated. 
#' By default all parameters in the model (see \code{details}) are included: \eqn{\theta^{\mathrm{CACE}}} 
#' (\code{CACE}), \eqn{u_1} (\code{u1}), \eqn{v_1} (\code{v1}), \eqn{s_1} (\code{s1}), \eqn{b_1} (\code{b1}), 
#' \eqn{\pi_a} (\code{pi.a}), \eqn{\pi_n} (\code{pi.n}), and \eqn{\pi_c=1-\pi_a-\pi_n} (\code{pi.c}). 
#' Users can modify the string vector to only include parameters of interest besides 
#' \eqn{\theta^{\mathrm{CACE}}}. 
#' @param re.values a list of parameter values for the random effects. It should contain the assignment for these
#' parameters only: \code{n.m} and \code{n.s}, which refer to the mean and standard deviation used
#' in the normal distribution estimation of \code{n}, as well as \code{a.m}, \code{a.s}, 
#' \code{alpha.s.m}, \code{alpha.s.s}, \code{alpha.b.m}, \code{alpha.b.s}, \code{alpha.u.m}, \code{alpha.u.s},
#' \code{alpha.v.m}, \code{alpha.v.s}. By default, this is an empty list, and all the mean are set to \code{0}, and 
#' \code{alpha.n.s = alpha.a.s = 0.16}, and \code{alpha.s.s = alpha.b.s = alpha.u.s = alpha.v.s = 0.25}. 
#' @param model.code a string representation of the model code; each line should be separated. Default to constructing 
#' model code using the \code{model.meta.ic} function with the parameters that are inputted to this function. This 
#' parameter is only necessary if user wishes to make functional changes to the model code, such as changing the
#' probability distributions of the parameters. Default to empty string.
#' @param digits a positive integer specifying the digits after the decimal point for 
#' the effect size estimates. The default is \code{3}.
#' @param n.adapt the number of iterations for adaptation in Markov chain Monte Carlo (MCMC) algorithm; 
#' it is used to maximize the sampling efficiency. 
#' The default is \code{1,000}. If a warning "adaptation incomplete" appears, users may increase 
#' \code{n.adapt}. This argument and the following \code{n.iter}, \code{n.burnin}, \code{n.chains},
#' \code{n.thin} are passed to the functions in R package \code{rjags}. 
#' @param n.iter the number of iterations of each MCMC chain. 
#' The default is \code{100,000}. 
#' @param n.burnin the number of iterations for burn-in period. The default is 
#' the largest integer not greater than \code{n.iter/2}.
#' @param n.chains the number of MCMC chains. The default is \code{3}. 
#' @param n.thin a positive integer indicating thinning rate for MCMC chains, which is used to 
#' avoid potential high auto-correlation and to save computer memory when \code{n.iter} is 
#' large. The default is set as \code{1} or the largest integer not greater than 
#' \code{((n.iter - n.burnin)/1e+05)}, whichever is larger. 
#' @param conv.diag a logical value indicating whether to compute the Gelman and Rubin 
#' convergence statistic (\eqn{\hat{R}}) of each parameter as a convergence diagnostic.
#' It is considered the chains are well mixed and have converged to the target distribution 
#' if \eqn{\hat{R} \le 1.1}. The default is \code{FALSE}. If \code{TRUE}, \code{n.chains} must be greater than 1, 
#' and the function saves each chain's MCMC samples for all parameters, which can be used 
#' to produce trace, posterior density, and auto-correlation plots by calling the function 
#' \code{plt.cacebayes}. 
#' @param mcmc.samples a logical value indicating whether to save MCMC posterior samples
#' in the output object. The default is \code{FALSE}. If \code{TRUE}, the output object list 
#' includes each chain's MCMC samples for all parameters. They can be used in the function 
#' \code{plt.cacebayes} to generate the trace, posterior density, and auto-correlation plots 
#' for further model diagnostics. 
#' @param two.step a logical value indicating whether to conduct a two-step meta-analysis. 
#' If \code{two.step = TRUE}, the posterior mean and standard deviation of study-specific 
#' \eqn{\theta^{\mathrm{CACE}}_i} are used to perform a standard meta-analysis, using the R package \code{metafor}. 
#' @param method the method used in meta-analysis if \code{two.step = TRUE}. The default estimation 
#' method is the REML (restricted maximum-likelihood estimator) method for the random-effects 
#' model. Users can change the argument \code{method} to obtain different meta-analysis 
#' estimators from either a random-effects model or a fixed-effect model, e.g., 
#' \code{method = 'DL'} refers to the DerSimonian--Laird estimator, 
#' \code{method = 'HE'} returns the Hedges estimator, and \code{method = 'HS'} gives the Hunter--Schmidt 
#' estimator.  More details are available from the documentation of the function \code{metafor::rma}. 
#' If the input data include only one study, the meta-analysis result is just the same as 
#' the result from the single study. 
#' @return It returns a model object of class \code{cace.Bayes}
#' @details  
#' The likelihood \deqn{\log L({\boldsymbol{\beta}}) = N_{000}\log\{\pi_{c}(1-v_1)+\pi_{n}(1-s_1)\}+N_{001}
#' \log(\pi_{c}v_1+\pi_{n}s_1)+N_{010}\log\{{\pi}_{a}(1-b_1)\}}
#' \deqn{+ N_{011}\log\{\pi_{a}b_1\}+ N_{100}
#' \log\{\pi_{n}(1-s_1)\}+N_{101}\log({\pi}_{n}s_1) + N_{110}\log\{(\pi_{c}(1-u_1)}
#' \deqn{+ \pi_{a}(1-b_1)\}+{N_{111}\log(\pi_{c}u_1+\pi_{a}b_1)} + constant}. 
#' If the input \code{data} includes more than one study, the study-specific CACEs will be 
#' estimated by retrieving data row by row.
#' By default, the function \code{cace.study()} returns a list  
#' including posterior estimates (posterior mean, standard deviation, median, and a 95\% 
#' credible interval (CrI) with 2.5\% and 97.5\% quantiles as the lower and upper bounds), 
#' and the deviance information criterion (DIC) statistic for each study.
#' @importFrom stats update complete.cases
#' @import rjags
#' @import coda
#' @import metafor
#' @import Rdpack
#' @export
#' @examples
#' \donttest{
#' data("epidural_c", package = "BayesCACE")
#' set.seed(123)
#' out.study <- cace.study(data = epidural_c, conv.diag = TRUE, 
#' mcmc.samples = TRUE, two.step = TRUE) 
#' # Show the estimates of theta for each single study (posterior mean and 
#' # standard deviation, posterior median, 95% credible interval, and time-series 
#' # standard error):
#' out.study$CACE
#' # If the argument conv.diag is specified as TRUE, the output list contains 
#' # a sub-list conv.out, which outputs the Gelman and Rubin convergence statistic,
#' # labelled Point est.) calculated for each parameter from each single study, and 
#' # their upper confidence limits (labelled Upper C.I.).
#' out.study$conv.out[[1]]
#' }
#' @seealso \code{\link[BayesCACE]{cace.meta.c}}, \code{\link[BayesCACE]{cace.meta.ic}}
#' 
cace.study <-
  function(data, param = c("CACE", "u1", "v1", "s1", "b1", "pi.c", "pi.n", 
          "pi.a"), re.values = list(), model.code = '', digits = 3, n.adapt = 1000, 
           n.iter = 100000, n.burnin = floor(n.iter/2), n.chains = 3, n.thin =  
          max(1,floor((n.iter-n.burnin)/1e+05)), conv.diag = FALSE, mcmc.samples
           = FALSE, two.step = FALSE, method = "REML")    {
    ## check the input parameters
    Mean <- SD <- NULL
    
    if(missing(data)) stop("need to specify data")
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
    
    ## jags model
    if (nchar(model.code) == 0) {
      modelstring<-model.study(re.values)
    }
    else {modelstring <- model.code}
    
    ## data prep
    Ntol <- n000+n001+n010+n011+n100+n101+n110+n111
    N0 <- n000+n001+n010+n011
    N1 <- n100+n101+n110+n111
    R <- cbind(n000,n001,n010,n011, n100,n101,n110,n111)
    I <- length(Ntol)
    
    ## parameters to be paramed in jags
    if(!is.element("CACE",param)) param<-c("CACE",param)
    fullparam <- c("CACE", "u1", "v1", "s1", "b1", "pi.c", "pi.n", "pi.a")
    if(!any(is.element(param, fullparam))) stop("parameters must be specified from the following:  
                                                CACE, u1, v1, s1, b1, 
                                                pi.c, pi.n, pi.a \n")
    out<-NULL
    out$model<-"cace.single"    
    
  for (i in 1:I) {
    data.jags <- list(N0=N0[i], N1=N1[i], R=R[i,])
    
    ## jags initial value
    rng.seeds<-sample(1000000,n.chains)
    init.jags <- vector("list", n.chains)
    for(ii in 1:n.chains){
      init.jags[[ii]] <- list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = rng.seeds[ii])
    }
    
    ## run jags
    jags.m<-jags.model(file=textConnection(modelstring),data=data.jags,inits=init.jags,
                       n.chains=n.chains,n.adapt=n.adapt)
    update(jags.m,n.iter=n.burnin)
    jags.out<-coda.samples.dic(model=jags.m,variable.names=param,n.iter=n.iter,thin=n.thin)
    
    smry<-summary(jags.out$samples)
    smry<-cbind(smry$statistics[,c("Mean","SD")],smry$quantiles[,c("2.5%","50%","97.5%")], 
                smry$statistics[,c("Naive SE","Time-series SE")])
    smry<-signif(smry,digits=digits)
    out$smry[[i]] <- smry
    
    for (j in 1:length(fullparam)){
      if(is.element(fullparam[j],param)) {
        out[[fullparam[j] ]]<-rbind(out[[fullparam[j] ]], smry[c(fullparam[j]), ])
        }
    }
    
    #dic
    dev<-jags.out$dic[[1]] # mean deviance
    pen<-jags.out$dic[[2]] # pD
    pen.dev<-dev+pen # DIC
    dic.stat<-rbind(dev,pen,pen.dev)
    rownames(dic.stat)<-c("D.bar","pD","DIC")
    colnames(dic.stat)<-""
    out$DIC[[i]]<-dic.stat
    
    
    if(conv.diag){
      message("MCMC convergence diagnostic statistics are calculated and saved in conv.out\n")
      conv.out<-gelman.diag(jags.out$samples,multivariate=FALSE)
      out$conv.out[[i]]<-conv.out$psrf
    }
    
    if(mcmc.samples){
      out$mcmc.samples[[i]]<-jags.out$samples
    }
  }
    
  if (two.step) {
    out$meta <- rma(yi=Mean, sei=SD, data=out$CACE, method = method)
  }
  class(out)<-"cace.Bayes"
  return(out) 
}


