#' This function creates a traceplot for a \code{cace.Bayes} object.
#' @title this plot function creates a traceplot
#' @param obj a \code{cace.Bayes} object, returned by \code{cace.meta.c}, \code{cace.meta.ic},
#' or \code{cace.study}
#' @param param list of parameters to plot
#' @param trialnumber indicator for which trial number of the mcmc samples
#' to use. The default is \code{1}
#' @param ... optional parameters to pass into the \code{plot} function
#' @return It returns a traceplot in an \code{R} plot window.
#' @importFrom grDevices rainbow 
#' @importFrom graphics plot par
#' @export
#' @examples
#' \donttest{
#' out.meta.c <- cace.meta.c(data = epidural_c, conv.diag = TRUE, 
#' mcmc.samples = TRUE, study.specific = TRUE)
#' plt.trace(obj=out.meta.c)
#' }

plt.trace <- 
  function(obj, param = c("CACE"), trialnumber = 1, ...) {
  if(missing(obj)) stop("need to specify obj, the object generated from one of the following functions:\n
         cace.study, cace.meta.c, cace.meta.ic.\n")
  if (!inherits(obj, "cace.Bayes"))
    stop("Use only with 'cace.Bayes' objects, the output generated from one of the following functions:\n
         cace.study, cace.meta.c, cace.meta.ic.\n")
  if(is.null(obj$mcmc.samples))
    stop("'obj$mcmc.samples' is missing. please set mcmc.samples=TRUE when running the function
         cace.study, cace.meta.c or cace.meta.ic.")
  if(!class(obj$mcmc.samples) %in% c("mcmc.list","list") )
    stop("'obj$mcmc.samples' must be a mcmc.list or list generated from one of the following functions:\n
         cace.study, cace.meta.c, cace.meta.ic.")
    if(class(obj$mcmc.samples) =="mcmc.list") {x <- obj$mcmc.samples}
    if(class(obj$mcmc.samples) =="list") {
      if(!trialnumber %in% c(1:length(obj$mcmc.samples)))
        stop("'trialnumber' should be the trial number to be plotted \n")
      else {x <- obj$mcmc.samples[[trialnumber]]}
    }
  
  param <- param
  
  n.chains <- length(x)
  cols<-rainbow(n.chains,s=0.6,v=0.6)
  
  # make sure parameter is reset upon function exit 
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mfrow=c(n.chains,1))
  for(j in 1:n.chains){
    temp<-as.vector(x[[j]][,param])
    traceplot <- plot(temp,type="l",col="red",ylab=param,xlab="Iterations",
        main = paste("Trace Plot of", param,", Chain",j), 
        lwd=1,cex.axis=1.2,cex.lab=1.4, ...)
  }
  par(mfrow=c(1,1))
}

#' This function creates a density plot for a \code{cace.Bayes} object.
#' @title this plot function creates a density plot
#' @param obj a \code{cace.Bayes} object, returned by \code{cace.meta.c}, \code{cace.meta.ic},
#' or \code{cace.study}
#' @param param list of parameters to plot
#' @param trialnumber indicator for which trial number of the mcmc samples
#' to use. The default is \code{1}
#' @param ... optional parameters to pass into the \code{plot} function
#' @return It returns a density plot in an \code{R} plot window.
#' @importFrom grDevices rainbow 
#' @importFrom graphics plot 
#' @importFrom stats bw.SJ density
#' @export
#' @examples
#' \donttest{
#' out.meta.c <- cace.meta.c(data = epidural_c, conv.diag = TRUE, 
#' mcmc.samples = TRUE, study.specific = TRUE)
#' plt.density(obj=out.meta.c)
#' }

plt.density <- 
  function(obj, param = c("CACE"), trialnumber = 1, ...) {
  if(missing(obj)) stop("need to specify obj, the object generated from one of the following functions:\n
         cace.study, cace.meta.c, cace.meta.ic.\n")
  if (!inherits(obj, "cace.Bayes"))
    stop("Use only with 'cace.Bayes' objects, the output generated from one of the following functions:\n
         cace.study, cace.meta.c, cace.meta.ic.\n")
  if(is.null(obj$mcmc.samples))
    stop("'obj$mcmc.samples' is missing. please set mcmc.samples=TRUE when running the function
         cace.study, cace.meta.c or cace.meta.ic.")
  if(!class(obj$mcmc.samples) %in% c("mcmc.list","list") )
    stop("'obj$mcmc.samples' must be a mcmc.list or list generated from one of the following functions:\n
         cace.study, cace.meta.c, cace.meta.ic.")
    if(class(obj$mcmc.samples) =="mcmc.list") {x <- obj$mcmc.samples}
    if(class(obj$mcmc.samples) =="list") {
      if(!trialnumber %in% c(1:length(obj$mcmc.samples)))
        stop("'trialnumber' should be the trial number to be plotted \n")
      else {x <- obj$mcmc.samples[[trialnumber]]}
    }
  
  param <- param
  
  n.chains <- length(x)
  cols<-rainbow(n.chains,s=0.6,v=0.6)
  
  if (n.chains==1) {
    nams <- dimnames(x)[[2]]
    if(is.element(param, nams)) {
      temp<-as.vector(x[,param])
      bw <- bw.SJ(temp) * 1.5
      densplot <- plot(density(temp, bw = bw), xlab = param, 
           main = paste("Density of", param), ...)
    } 
  }
  else if (n.chains >1) {
    nams <- dimnames(x[[1]])[[2]]
    if(is.element(param, nams)) {
      temp<-NULL
      for(j in 1:n.chains){
        temp<-c(temp, as.vector(x[[j]][,param]))
      }
      bw <- bw.SJ(temp) * 1.5
      densplot <- plot(density(temp, bw = bw), xlab = param, 
           main = paste("Density of", param), ...)
    } 
  }
}

#' This function creates an acf (Autocorrelation Function) plot for a \code{cace.Bayes} object.
#' @title this plot function creates an acf plot
#' @param obj a \code{cace.Bayes} object, returned by \code{cace.meta.c}, \code{cace.meta.ic},
#' or \code{cace.study}
#' @param param list of parameters to plot
#' @param trialnumber indicator for which trial number of the mcmc samples
#' to use. The default is \code{1}
#' @param ... optional parameters to pass into the \code{acf} function
#' from the \code{stats} library.
#' @return It returns an acf plot in an \code{R} plot window.
#' @importFrom grDevices rainbow 
#' @importFrom graphics plot
#' @importFrom stats acf
#' @export
#' @examples
#' \donttest{
#' out.meta.c <- cace.meta.c(data = epidural_c, conv.diag = TRUE, 
#' mcmc.samples = TRUE, study.specific = TRUE)
#' plt.acf(obj=out.meta.c)
#' }

plt.acf <- 
  function(obj, param = c("CACE"), trialnumber = 1, ...) {
  if(missing(obj)) stop("need to specify obj, the object generated from one of the following functions:\n
         cace.study, cace.meta.c, cace.meta.ic.\n")
  if (!inherits(obj, "cace.Bayes"))
    stop("Use only with 'cace.Bayes' objects, the output generated from one of the following functions:\n
         cace.study, cace.meta.c, cace.meta.ic.\n")
  if(is.null(obj$mcmc.samples))
    stop("'obj$mcmc.samples' is missing. please set mcmc.samples=TRUE when running the function
         cace.study, cace.meta.c or cace.meta.ic.")
  if(!class(obj$mcmc.samples) %in% c("mcmc.list","list") )
    stop("'obj$mcmc.samples' must be a mcmc.list or list generated from one of the following functions:\n
         cace.study, cace.meta.c, cace.meta.ic.")
    if(class(obj$mcmc.samples) =="mcmc.list") {x <- obj$mcmc.samples}
    if(class(obj$mcmc.samples) =="list") {
      if(!trialnumber %in% c(1:length(obj$mcmc.samples)))
        stop("'trialnumber' should be the trial number to be plotted \n")
      else {x <- obj$mcmc.samples[[trialnumber]]}
    }
  
  param <- param
  
  n.chains <- length(x)
  cols<-rainbow(n.chains,s=0.6,v=0.6)

  if (n.chains==1) {
    nams <- dimnames(x)[[2]]
    if(is.element(param, nams)) {
      temp<-as.vector(x[,param])
      acfplot <- acf(temp, ylab = param, main = paste("Series", param), ...)
    } 
  }
  else if (n.chains >1) {
    nams <- dimnames(x[[1]])[[2]]
    if(is.element(param, nams)) {
      temp<-NULL
      for(j in 1:n.chains){
        temp<-c(temp, as.vector(x[[j]][,param]))
      }
      acfplot <- acf(temp, ylab = param, main = paste("Series", param), ...)
    } 
  }
}