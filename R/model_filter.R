#' @name Qfit_F
#' @title Qfit_F
#' @author Camille Heylen
#' @export
#' @description Returns Qfit
#' @param k k
#' @param time time
#' @param h0 h0
#' @param rb rb
#' @param tb tb
#' @param rt rt
#' @param tw tw
#' @param L L

Qfit_F <- function(k,time,h0 = 0.225,rb = 0.0975,tb = 0.015,rt = 0.135,tw = 0.0115,L = 0.235){

  Ntimes <- length(time)
  h <- dh.dt <- rep(NA,Ntimes)

  h[1] <- h0
  for (t in seq(2,Ntimes)){

    Num <- (rb^2/tb) + ((rt-rb)*h[t-1]^2/(3*L*tw)) + (rb*h[t-1]/tw)
    Den <- (rb^2) + ((rt-rb)^2*h[t-1]^2/L^2) + (2*rb*(rt-rb)*h[t-1]/L)

    dh.dt[t-1] <- -k*h[t-1]*Num/Den
    Delta_t <- time[t] - time[t-1]
    h[t] <- max(0,h[t-1] + dh.dt[t-1]*Delta_t)
  }

  Qfit <- k*pi*h*((rb^2/tb) + ((rt-rb)*h^2/(3*L*tw)) + (rb*h/tw))
  return(Qfit)
}



