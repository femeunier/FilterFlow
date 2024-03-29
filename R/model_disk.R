#' @name RMSE_Qfit_d
#' @title RMSE_Qfit_d
#' @author Camille Heylen
#' @export
#' @description Returns RMSE
#' @param k k
#' @param tb tb
#' @param rb rb
#' @param h0 h0
#' @param data data
#'
RMSE_Qfit_d<- function(k, tb = 0.01,rb = 0.05,h0 = 0.225, data){
  time <- data %>% filter(rep == 1) %>% pull(time)

  Qfit <- Qfit_d(k = k,time = time,h0 = h0,rb = rb,tb = tb)
  Qfit_all <- rep(Qfit,length(unique(data %>% pull(rep))))
  Qexp <- data%>%pull(Qexp)
  RMSE <- sqrt(sum((Qfit_all - Qexp)^2,na.rm=TRUE)/(length(Qfit_all)-1))

  return(RMSE)
}


#' @name Qfit_d
#' @title Qfit_d
#' @author Camille Heylen
#' @export
#' @description Returns Qfit
#' @param k k
#' @param time time
#' @param h0 h0
#' @param rb rb
#' @param tb tb

Qfit_d <- function(k,time,h0 = 0.225,rb = 0.05,tb = 0.01){

  # Ntimes <- length(time)
  # h <- dh.dt <- rep(NA,Ntimes)
  #
  # h[1] <- h0
  # for (t in seq(2,Ntimes)){
  #   dh.dt[t-1] <- -k*h[t-1]/tb
  #   Delta_t <- time[t] - time[t-1]
  #   h[t] <- max(0,h[t-1] + dh.dt[t-1]*Delta_t)
  # }

  Qfit <- k*pi*(rb^2/tb)*h0*exp(-k*time/tb)
  return(Qfit)
}


#' @name Vfit_d
#' @title Vfit_d
#' @author Camille Heylen
#' @export
#' @description Returns Vfit
#' @param k k
#' @param time time
#' @param h0 h0
#' @param rb rb
#' @param tb tb

Vfit_d <- function(k,time,h0 = 0.225,rb = 0.05,tb = 0.01){

  # Ntimes <- length(time)
  # h <- dh.dt <- rep(NA,Ntimes)
  #
  # h[1] <- h0
  # for (t in seq(2,Ntimes)){
  #   dh.dt[t-1] <- -k*h[t-1]/tb
  #   Delta_t <- time[t] - time[t-1]
  #   h[t] <- max(0,h[t-1] + dh.dt[t-1]*Delta_t)
  # }
  #
  # Vfit <- pi*rb^2*(h0 - h)

  # delta_time <- time[2]-time[1]
  # Qfit <- Qfit_d(k,time,h0 = h0,rb = rb,tb = tb)
  # Vfit <- delta_time*cumsum(Qfit)

  Vfit <- pi*rb^2*h0*(1-exp(-k/tb*time))

  return(Vfit)
}

#' @name RMSE_Vfit_d
#' @title RMSE_Vfit_d
#' @author Camille Heylen
#' @export
#' @description Returns RMSE
#' @param k k
#' @param tb tb
#' @param rb rb
#' @param h0 h0
#' @param data data
#'
RMSE_Vfit_d<- function(k, tb = 0.01,rb = 0.05,h0 = 0.225, data){

  if (k < 0) k = -k

  time <- data %>% filter(rep == 1) %>% pull(time)
  Vfit <- Vfit_d(k = k,time = time,h0 = h0,rb = rb,tb = tb)
  Vfit_all <- rep(Vfit,length(unique(data %>% pull(rep))))
  Vexp <- data%>%pull(V)
  RMSE <- sqrt(sum((Vfit_all - Vexp)^2,na.rm=TRUE)/(length(Vfit_all)-1))

  if (!is.finite(RMSE)) return(RMSE = 1000)

  return(RMSE)
}

#' @name RMSE_Qfit_d_all
#' @title RMSE_Qfit_d_all
#' @author Camille Heylen
#' @export
#' @description Returns sum of RMSE
#' @param k k
#' @param tb tb
#' @param rb rb
#' @param h0 h0
#' @param data All data
#'
RMSE_Qfit_d_all<- function(k, tb = 0.01,rb = 0.05,h0 = 0.225, data){

  RMSE <- c()
  # k=c(k1,k2,k3)
  # print(k)

  if (any(k<0))return(1)

  for (iset in seq(1,3)){
    currentset = iset

    current_data <- data %>% filter(set == currentset) %>% dplyr::select(c(V,Qexp,rep,time))

    time <- current_data %>% filter(rep == 1) %>% pull(time)

    Qfit <- Qfit_d(k = k[iset],time = time,h0 = h0,rb = rb,tb = tb)
    Qfit_all <- rep(Qfit,length(unique(current_data %>% pull(rep))))
    Qexp <- current_data%>%pull(Qexp)
    RMSE <- c(RMSE,sqrt(sum((Qfit_all - Qexp)^2,na.rm=TRUE)/(length(Qfit_all)-1)))

  }


  return(sum(RMSE))
}



#' @name RMSE_Qfit_d_all
#' @title RMSE_Qfit_d_all
#' @author Camille Heylen
#' @export
#' @description Returns sum of RMSE
#' @param param k h0 rb tb
#' @param data All data
#'
RMSE_Qfit_d_allparams<- function(param, data){

  RMSE <- c()

  if (any(param<0)) return(1)

  k <- param[1:3]
  h0 <- param[4]
  rb <- param[5]
  tb <- param[6]

  if (param[4]<0.215 | param[4]>0.235) return(1)
  if (param[5]<0.049 | param[5]>0.051) return(1)
  if (param[6]<0.009 | param[6]>0.011) return(1)

  for (iset in seq(1,3)){
    currentset = iset

    current_data <- data %>% filter(set == currentset) %>% dplyr::select(c(V,Qexp,rep,time))

    time <- current_data %>% filter(rep == 1) %>% pull(time)

    Qfit <- Qfit_d(k = k[iset],time = time,h0 = h0,rb = rb,tb = tb)
    Qfit_all <- rep(Qfit,length(unique(current_data %>% pull(rep))))
    Qexp <- current_data%>%pull(Qexp)
    RMSE <- c(RMSE,sqrt(sum((Qfit_all - Qexp)^2,na.rm=TRUE)/(length(Qfit_all)-1)))

  }

  return(sum(RMSE))
}


#' @name RMSE_Vfit_d_allparams
#' @title RMSE_Vfit_d_allparams
#' @author Camille Heylen
#' @export
#' @description Returns sum of RMSE
#' @param param k h0 rb tb
#' @param data All data
#'
RMSE_Vfit_d_allparams <- function(param, data){

  RMSE <- c()



  k <- param[1:3]
  h0 <- param[4]
  rb <- param[5]
  tb <- param[6]

  for (iset in seq(1,3)){
    currentset = iset

    current_data <- data %>% filter(set == currentset) %>% dplyr::select(c(V,Qexp,rep,time))

    time <- current_data %>% filter(rep == 1) %>% pull(time)

    Vfit <- Vfit_d(k = k[iset],time = time,h0 = h0,rb = rb,tb = tb)
    Vfit_all <- rep(Vfit,length(unique(current_data %>% pull(rep))))
    Vexp <- current_data%>%pull(V)
    RMSE <- c(RMSE,sqrt(sum((Vfit_all - Vexp)^2,na.rm=TRUE)/(length(Vfit_all)-1)))

  }

  return(sum(RMSE))
}

