#' This is a wrapper function for \code{jags.samples} which sets a trace
#' monitor for all requested nodes, updates the model, and coerces the
#' output to a single \code{mcmc.list} object. It also converts to the output
#' to dic format. This function is based on the \code{coda.samples} function
#' from the \code{rjags} library, and modified by Prof. Matthias Mittner.
#' 
#' @title Generate posterior samples in mcmc.list format
#' @param model a jags model object
#' @param variable.names a character vector giving the names of variables
#' to be monitored
#' @param n.iter number of iterations to monitor
#' @param thin thinning interval for monitors
#' @param ... optional arguments that are passed to the \code{jags.samples} method 
#' from the \code{rjags} library, for jags model objects
#' @return It returns the output to the input model object, and in dic format. 
#' @import rjags
#' @import coda
#' @export
#' @references 
#' \insertRef{plummer2021rjags}{BayesCACE}
#' 
#' \url{https://ihrke.github.io/post/2014/10/07/dicjags/}

coda.samples.dic <- function (model, variable.names, n.iter, thin, ...)
{
  load.module('dic') # necessary for pD and deviance monitor
  
  start <- model$iter() + thin
  varnames=c(variable.names, c('deviance', 'pD'))
  out <- jags.samples(model, varnames, n.iter, thin,
                      type = "trace", ...)
  deviance <- out$deviance
  pD <- out$pD
  out$deviance <- NULL
  out$pD <- NULL
  ans <- vector("list", model$nchain())
  for (ch in 1:model$nchain()) {
    ans.ch <- vector("list", length(out))
    vnames.ch <- NULL
    for (i in seq(along = out)) {
      varname <- names(out)[[i]]
      d <- dim(out[[i]])
      if (length(d) < 3) {
        stop("Invalid dimensions for sampled output")
      }
      vardim <- d[1:(length(d) - 2)]
      nvar <- prod(vardim)
      niter <- d[length(d) - 1]
      nchain <- d[length(d)]
      values <- as.vector(out[[i]])
      var.i <- matrix(NA, nrow = niter, ncol = nvar)
      for (j in 1:nvar) {
        var.i[, j] <- values[j + (0:(niter - 1)) * nvar +
                               (ch - 1) * niter * nvar]
      }
      vnames.ch <- c(vnames.ch, coda.names(varname, vardim))
      ans.ch[[i]] <- var.i
    }
    ans.ch <- do.call("cbind", ans.ch)
    colnames(ans.ch) <- vnames.ch
    ans[[ch]] <- mcmc(ans.ch, start = start, thin = thin)
  }
  
  dic <- list(deviance = mean(as.vector(deviance)), penalty = mean(as.vector(pD)), type = 'pD')
  class(dic) <- "dic"
  return(list(samples=mcmc.list(ans), dic=dic))
}


#' This is a helper function from the \code{rjags} library in order to get the names of the individual elements
#' of a node array. See the package \code{rjags} for more details.
#' @title Get names of node array
#' @param basename the node names
#' @param dim dimension of the nodes
#' @return It returns a list of the names of individual elements
#' @import rjags 
#' @import coda 
#' @export 
#' @references 
#' \insertRef{plummer2021rjags}{BayesCACE}
coda.names <- function(basename, dim)
{
    ## Utility function used to get the names of the individual elements
    ## of a node array

    if (prod(dim) == 1)
      return(basename)

    ##Default lower and upper limits
    ndim <- length(dim)
    lower <- rep(1, ndim)
    upper <- dim

    ##If the node name is a subset, we try to parse it to get the
    ##names of its elements. For example, if basename is "A[2:3]"
    ##we want to return names "A[2]", "A[3]" not "A[2:3][1]", "A[2:3][2]".
    pn <- parse.varname(basename)
    if (!is.null(pn) && !is.null(pn$lower) && !is.null(pn$upper)) {
        if (length(pn$lower) == length(pn$upper)) {
            dim2 <- pn$upper - pn$lower + 1
            if (isTRUE(all.equal(dim[dim!=1], dim2[dim2!=1],
                                 check.attributes=FALSE))) {
                basename <- pn$name
                lower <- pn$lower
                upper <- pn$upper
                ndim <- length(dim2)
            }
        }
    }

    indices <- as.character(lower[1]:upper[1])
    if (ndim > 1) {
        for (i in 2:ndim) {
            indices <- outer(indices, lower[i]:upper[i], FUN=paste, sep=",")
        }
    }
    paste(basename,"[",as.vector(indices),"]",sep="")
}

#' This is a helper function from the \code{rjags} library in order to parse the string of form
#' "a" or "a[n,p:q,r]". See the package \code{rjags} for more details.
#' @title Parse strings of specific form
#' @param varname string name of variable
#' @return It returns a list of parsed parameters
#' @import rjags 
#' @import coda 
#' @export 
#' @references 
#' \insertRef{plummer2021rjags}{BayesCACE}
parse.varname <- function(varname) {

  ## Try to parse string of form "a" or "a[n,p:q,r]" where "a" is a
  ## variable name and n,p,q,r are integers

  v <- try(parse(text=varname, n=1), silent=TRUE)
  if (!is.expression(v) || length(v) != 1)
    return(NULL)

  v <- v[[1]]
  if (is.name(v)) {
    ##Full node array requested
    return(list(name=deparse(v)))
  }
  else if (is.call(v) && identical(deparse(v[[1]]), "[") && length(v) > 2) {
    ##Subset requested
    ndim <- length(v) - 2
    lower <- upper <- numeric(ndim)
    if (any(nchar(sapply(v, deparse)) == 0)) {
      ##We have to catch empty indices here or they will cause trouble
      ##below
      return(NULL)
    }
    for (i in 1:ndim) {
      index <- v[[i+2]]
      if (is.numeric(index)) {
        ##Single index
        lower[i] <- upper[i] <- index
      }
      else if (is.call(index) && length(index) == 3 &&
               identical(deparse(index[[1]]), ":") &&
               is.numeric(index[[2]]) && is.numeric(index[[3]]))
        {
          ##Index range
          lower[i] <- index[[2]]
          upper[i] <- index[[3]]
        }
      else return(NULL)
    }
    if (any(upper < lower))
      return (NULL)
    return(list(name = deparse(v[[2]]), lower=lower, upper=upper))
  }
  return(NULL)
}
