QUIC <- function(S, rho, path = NULL, tol = 1.0e-4, msg = 1, maxIter = 1000,
  X.init = NULL, W.init = NULL) {
### $Id: QUIC.R,v 1.4 2012-02-24 09:13:35 sustik Exp $

  if (!is.loaded("QUIC")) {

  }
  n = nrow(S)
  if (is.null(path))
    npath = 1
  else
    npath = length(path)
  if (!is.matrix(rho) && length(rho) != 1 && length(rho) != n) {
    stop("Wrong number of elements in rho")
  }

  if (is.vector(rho)){
    rho = matrix(sqrt(rho))%*%sqrt(rho)
  }
  if (length(rho) == 1){
    rho = matrix(rho, ncol = n, nrow = n)
  }

  if (is.null(path)) {
    if (is.null(X.init)) {
      X = diag(n)
      W = diag(n)
    } else {
      X = X.init
      W = W.init
    }
    opt = 0
    cputime = 0
    iter = array(0, 1)
  } else {
    if (is.null(X.init)) {
      X = array(diag(n), c(n, n, npath)) 
      W = array(diag(n), c(n, n, npath)) 
    } else {
      X = array(0, c(n, n, npath)) 
      W = array(0, c(n, n, npath)) 
      X[, , 1] = X.init
      W[, , 1] = W.init
    }
    opt = array(0, c(npath))
    cputime = array(0, c(npath))
    iter = array(0, npath)
  }
  iter[1] = maxIter
  opt = matrix(0, ncol = npath, nrow = 1)
  cputime = matrix(0, ncol = npath, nrow = 1)
  if (is.null(path))
    job = "d"
  else
    job = "p"

  tmp<-.C("QUICR", as.character(job), as.integer(n), as.double(S),
          as.double(rho), as.integer(npath), as.double(path),
          as.double(tol), as.integer(msg), iter = as.integer(iter),
          X = as.double(X), W = as.double(W), opt = as.double(opt),
          cputime = as.double(cputime))

  return (list(W = tmp$W, X = tmp$X, opt = tmp$opt, cputime = tmp$cputime,
               iter = tmp$iter, regloglik = -(n/2)*tmp$opt))
}
