#' @name filter_negative
#' @title filter_negative
#' @author Camille Heylen
#' @export
#' @description Returns vector
#' @param vec_cumul vec_cumul

filter_negative <- function(vec_cumul){

  Nmes <- length(vec_cumul)

  for (i in seq(2,Nmes)){
    if (is.na(vec_cumul[i])){
      break
    }
    if (vec_cumul[i] < vec_cumul[i-1]){
      delta <- -(vec_cumul[i] - vec_cumul[i-1])
      vec_cumul[i:Nmes] <- vec_cumul[i:Nmes] + delta
    }
  }

  return(vec_cumul)
}


#' @name filter_positive
#' @title filter_positive
#' @author Camille Heylen
#' @export
#' @description Returns vector
#' @param vec_cumul vec_cumul

filter_positive <- function(vec_cumul){

  Nmes <- length(vec_cumul)

  for (i in seq(2,Nmes-1)){
    if (is.na(vec_cumul[i+1])){
      break
    }

    diff_next <- vec_cumul[i+1] - vec_cumul[i]

    diff_prev <- 0
    iccurrent <- i
    while (diff_prev <=0){
      diff_prev <- vec_cumul[iccurrent] - vec_cumul[iccurrent-1]
      iccurrent <- iccurrent-1
    }

    if (diff_next > 5*diff_prev){
      delta <- -(vec_cumul[i+1] - vec_cumul[i])
      vec_cumul[(i+1):Nmes] <- vec_cumul[(i+1):Nmes] + delta
    }
  }

  return(vec_cumul)
}



#' @name dV.dt
#' @title dV.dt
#' @author Camille Heylen
#' @export
#' @description Returns vector
#' @param vec_cumul vec_cumul

dV.dt <- function(t,V){

  Nmes <- length(V)

  dV.dt <- rep(0,Nmes)
  dV.dt[1] <- (V[2]-V[1])/(t[2]-t[1])
  for (i in seq(2,Nmes-1)){
    if (is.na(V[i+1])){
      break
    }
    dV.dt[i] <- ((V[i] + (V[i+1] - V[i])/2) - (V[i] + (V[i-1] - V[i])/2))/((t[i+1]-t[i])/2 + (t[i]-t[i-1])/2)
  }

  dV.dt[Nmes] <- (V[Nmes]-V[Nmes-1])/(t[Nmes]-t[Nmes-1])
  return(dV.dt)
}

