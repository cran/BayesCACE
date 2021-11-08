#' This function performs the Bayesian hierarchical model method for meta-analysis 
#' when the dataset has complete compliance information for all studies, 
#' as described in the paper Section 2.2.2, the Bayesian hierarchical model.
#' @title Bayesian hierarchical models for CACE meta-analysis with complete compliance data
#' @param data an input dataset with the same structure as the example data `epidural_c`, 
#' containing multiple rows referring to multiple studies in a meta-analysis. 
#' @param param a character string vector indicating the parameters to be tracked and estimated. 
#' By default the following parameters (see \code{details}) are included: \eqn{\theta^{CACE}} 
#' (\code{CACE}), \eqn{E(u_{i1})} (\code{u1out}), \eqn{E(v_{i1})} (\code{v1out}), \eqn{E(s_{i1})} (\code{s1out}), 
#' \eqn{E(b_{i1})} (\code{b1out}), \eqn{\pi_a} (\code{pia}), \eqn{\pi_n} (\code{pin}), and 
#' \eqn{\pi_c=1-\pi_a-\pi_n} (\code{pic}). 
#' Users can modify the string vector to only include parameters of interest besides \eqn{\theta^{CACE}}. 
#' @param prior.type the default priors are used by the default assignment `prior.type="default"`.
#' Like the function \code{\link{cace.study}}, weakly informative priors \eqn{\alpha_n, \alpha_a \sim 
#' N(0, 2.5^2)} and \eqn{\alpha_s, \alpha_b, \alpha_u, \alpha_v \sim N(0, 2^2)} are assigned to the 
#' means of these transformed parameters:
#' \eqn{\pi_{in}=\frac{\exp(n_i)}{1+\exp(n_i)+\exp(a_i)}}, \eqn{\pi_{ia}=\frac{\exp(a_i)}{1+\exp(n_i)+\exp(a_i)}}, 
#' where \eqn{n_i=\alpha_n+\delta_{in}}, \eqn{a_i=\alpha_a+\delta_{ia}}, \eqn{logit(s_{i1})=\alpha_s + \delta_{is}},
#' \eqn{logit(b_{i1})=\alpha_b + \delta_{ib}}, \eqn{probit(u_{i1})=\alpha_u + \delta_{iu}}, 
#' and \eqn{probit(v_{i1})=\alpha_v + \delta_{iv}}. 
#' Alternatively, this function allows users to specify their own prior distributions by saving a separate 
#' `R` file \code{prior.meta.R} under the same directory with the model file, and assigning the argument 
#' `prior.type = "custom"`.
#' Users can modify the above customized file \code{prior.meta.R} to assign their preferred prior 
#' distributions. Note that same as the function \code{\link{cace.study}}, the function cannot
#' combine the default priors with partial user-defined prior distributions. Thus users need to 
#' be careful when choosing the customized priors: the pre-defined `R` file \code{prior.meta.R} must 
#' include distributions for all hyper-parameters. 
#' @param delta.n logical values indicating whether the 
#' corresponding random effect is included in the model. The default model sets all of these arguments 
#' to `TRUE`. Note that \eqn{\rho} (\code{cor}) can only be included when both \eqn{\delta_{in}} 
#' (\code{delta.n}) and \eqn{\delta_{ia}} (\code{delta.a}) are set to `TRUE`. Otherwise, a warning 
#' occurs and the model continues running by forcing `delta.n = TRUE` and `delta.a = TRUE`. 
#' @param delta.a see description of delta.n for more information
#' @param delta.u see description of delta.n for more information
#' @param delta.v see description of delta.n for more information
#' @param delta.s see description of delta.n for more information
#' @param delta.b see description of delta.n for more information
#' @param cor see description of delta.n for more information
#' @param digits number of digits. Default to 3.
#' @param n.adapt adapt value. Default to 1000.
#' @param n.iter number of iterations. Default to 100000.
#' @param n.burnin number of burn-in iterations. Default to n.iter/2. 
#' @param n.chains number of chains. Default to 3.
#' @param n.thin Default to max(1,floor((n.iter-n.burnin)/100000)).
#' @param conv.diag Default to False.
#' @param mcmc.samples Default to False.
#' @param study.specific a logical value indicating whether to calculate the study-specific 
#' \eqn{\theta^{CACE}_i}. If `TRUE`, the model will first check the logical status of arguments 
#' \code{delta.u} and \code{delta.v}. If both are `FALSE`, meaning that neither response rate \eqn{u_{i1}} 
#' or \eqn{v_{i1}} is modeled with a random effect, then the study-specific \eqn{\theta^{CACE}_i} is 
#' the same across studies. The function gives a warning and continues by making `study.specific = FALSE`. 
#' Otherwise, the study-specific \eqn{\theta^{CACE}_i} are estimated and saved as the parameter \code{cacei}.
#' @return It returns a model object of class "cace.Bayes"
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
#' @references {
#' \insertRef{zhou2019bayesian}{BayesCACE}
#' \insertRef{zhou2020software}{BayesCACE}
#' \insertRef{lunn2012bugs}{BayesCACE}
#' \insertRef{zeger1988models}{BayesCACE}
#' } 

cace.meta.c <-
  function(data, 
           param = c("CACE", "u1out", "v1out", "s1out", "b1out", 
                   "pic", "pin", "pia"),
           prior.type = "default", 
           delta.n = TRUE, delta.a = TRUE, delta.u = TRUE, delta.v = TRUE, 
           delta.s = TRUE, delta.b = TRUE, cor = TRUE, 
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
    
    if ((!(delta.n & delta.a)) & cor){
      warning("'cor' can be assigned as TRUE only if both delta.n and delta.a are TRUE.\n
              the model is continued by forcing delta.n=TRUE and delta.a=TRUE")
      delta.n=TRUE
      delta.a=TRUE
    }
    
    if ((!(delta.u|delta.v)) & study.specific){
      warning("no random effect is assigned to the response rate u1 or v1, \n
           study-specific CACE is the same across studies. \n
           the model is continued by making 'study.specific=FALSE'. \n
           to make a CACE forestplot, please run 'cace.study' to estimate study level CACEs. \n")
      study.specific=FALSE
    }
      
    
    Ind <- rep(1, 7)
    if (!delta.n) Ind[1]=0
    if (!delta.a) Ind[2]=0
    if (!delta.u) Ind[3]=0
    if (!delta.v) Ind[4]=0
    if (!delta.s) Ind[5]=0
    if (!delta.b) Ind[6]=0
    if (!cor) Ind[7]=0
    
    ## jags model
    modelstring<-model.meta.c(prior.type, Ind)

    ## jags data
    if(prior.type == "default"){
      Ntol <- n000+n001+n010+n011+n100+n101+n110+n111
      N0 <- n000+n001+n010+n011
      N1 <- n100+n101+n110+n111
      R <- cbind(n000,n001,n010,n011, n100,n101,n110,n111)
      I <- length(Ntol)
      pi <- pi
      data.jags <- list(N0=N0, N1=N1, R=R, I=I, Ind=Ind, pi=pi)
    }

    
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


