#' @name Qfit_F
#' @title Qfit_FVan Halem
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
  for (t in seq(1,Ntimes-1)){

    Num <- (rb^2/tb) + ((rt-rb)*h[t]^2/(3*L*tw)) + (rb*h[t]/tw)
    Den <- (rb^2) + ((rt-rb)^2*h[t]^2/L^2) + (2*rb*(rt-rb)*h[t]/L)

    dh.dt[t] <- -k*h[t]*Num/Den
    Delta_t <- time[t+1] - time[t]
    h[t+1] <- max(0,h[t] + dh.dt[t]*Delta_t)
  }

  Qfit <- k*pi*h*((rb^2/tb) + ((rt-rb)*h^2/(3*L*tw)) + (rb*h/tw))
  return(Qfit)
}



#' @name Vfit_f
#' @title Vfit_f
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

Vfit_F <- function(k,time,h0 = 0.225,rb = 0.0975,tb = 0.015,rt = 0.135,tw = 0.0115,L = 0.235){

  delta_time <- time[2]-time[1]
  Qfit <- Qfit_F(k,time,h0 = h0,rb = rb,tb = tb,rt = rt,tw = tw,L = L)
  Vfit <- delta_time*cumsum(Qfit)

  return(Vfit)
}


