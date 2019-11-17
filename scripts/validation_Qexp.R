#############################################################################
# Validation with Qexp filters

rm(list = ls())

library(dplyr)
library(FilterFlow)
library(ggplot2)

load(file = "./data/data_filter.Rdata")
load(file = "./data/kopt_Q.Rdata")
# load(file = "./data/kopt_V.Rdata")

rb = 0.0975
rt =  0.15
tb = 0.015
tw = 0.0115
L = 0.235
h0 = 0.225

fit.quality <- data.frame()

for (iset in seq(1,3)){
  currentset <- iset

  kcurrent <- k_opt[currentset]
  current_data <- Qexp_filter %>% filter(set == currentset) %>% dplyr::select(c(V,Qexp,rep,time))

  time <-  current_data %>% filter(rep == 1) %>%pull(time)

  Qfit <- Qfit_F(kcurrent,time,h0 = h0,rb = rb,tb = tb,rt = rt,tw = tw,L = L)
  Nrep <- length(unique(current_data%>%pull(rep)))
  Qfit_all <- rep(Qfit,Nrep)

  fit.quality <- rbind(fit.quality,
                       data.frame(obs = current_data %>% pull(Qexp),
                                  sim = Qfit_all,
                                  set = currentset,
                                  time = rep(time,Nrep),
                                  rep = current_data %>% pull(rep)))

}

ggplot() +
  geom_point(data = fit.quality, aes(x = time,y = obs,col = as.factor(set)),size = 0.5) +
  geom_line(data = fit.quality %>% filter(rep == 1),
            aes(x = time, y = sim, col = as.factor(set)),size = 1,linetype=2) +
  theme_bw()

ggplot() +
  geom_point(data = fit.quality, aes(x = obs,y = sim,col = as.factor(set)),size = 0.5) +
  geom_abline(intercept = 0, slope = 1, col = 'black',linetype = 2, size = 1) +
  scale_y_continuous(limits = c(0,max(c(fit.quality$obs,fit.quality$sim),na.rm=TRUE))) +
  scale_x_continuous(limits = c(0,max(c(fit.quality$obs,fit.quality$sim),na.rm=TRUE))) +
  theme_bw()

obs <- fit.quality$obs
sim <- fit.quality$sim

RMSE <- fit.quality %>% group_by(set) %>% summarise(rmse = sqrt(sum((obs-sim)^2,na.rm=TRUE)/length(obs)),
                                                    rmse_rel = sqrt(sum((obs-sim)^2,na.rm=TRUE)/length(obs))/mean(obs,na.rm=TRUE),
                                                    rmse_rel_pc = sqrt(sum((obs-sim)^2,na.rm=TRUE)/length(obs))/mean(obs,na.rm=TRUE)*100)

##############################################################################################
# Validation with Vexp filter

rm(list = ls())

library(dplyr)
library(FilterFlow)
library(ggplot2)

load(file = "./data/data_filter.Rdata")
# load(file = "./data/kopt_Q.Rdata")
load(file = "./data/kopt_V.Rdata")

rb = 0.0975
rt =  0.135
tb = 0.015
tw = 0.0115
L = 0.235
h0 = 0.225

fit.quality <- data.frame()

for (iset in seq(1,3)){
  currentset <- iset

  kcurrent <- k_opt[currentset]
  current_data <- Qexp_filter %>% filter(set == currentset) %>% dplyr::select(c(V,Qexp,rep,time))

  time <-  current_data %>% filter(rep == 1) %>%pull(time)

  Vfit <- Vfit_F(kcurrent,time,h0 = h0,rb = rb,tb = tb,rt = rt,tw = tw,L = L)
  Nrep <- length(unique(current_data%>%pull(rep)))
  Vfit_all <- rep(Vfit,Nrep)

  fit.quality <- rbind(fit.quality,
                       data.frame(obs = current_data %>% pull(V),
                                  sim = Vfit_all,
                                  set = currentset,
                                  time = rep(time,Nrep),
                                  rep = current_data %>% pull(rep)))

}

ggplot() +
  geom_point(data = fit.quality, aes(x = time,y = obs,col = as.factor(set)),size = 0.5) +
  geom_line(data = fit.quality %>% filter(rep == 1),
            aes(x = time, y = sim, col = as.factor(set)),size = 1,linetype=2) +
  theme_bw()

ggplot() +
  geom_point(data = fit.quality, aes(x = obs,y = sim,col = as.factor(set)),size = 0.5) +
  geom_abline(intercept = 0, slope = 1, col = 'black',linetype = 2, size = 1) +
  scale_y_continuous(limits = c(0,max(c(fit.quality$obs,fit.quality$sim),na.rm=TRUE))) +
  scale_x_continuous(limits = c(0,max(c(fit.quality$obs,fit.quality$sim),na.rm=TRUE))) +
  theme_bw()

obs <- fit.quality$obs
sim <- fit.quality$sim

RMSE <- fit.quality %>% group_by(set) %>% summarise(rmse = sqrt(sum((obs-sim)^2,na.rm=TRUE)/length(obs)),
                                                    rmse_rel = sqrt(sum((obs-sim)^2,na.rm=TRUE)/length(obs))/mean(obs,na.rm=TRUE),
                                                    rmse_rel_pc = 100*sqrt(sum((obs-sim)^2,na.rm=TRUE)/length(obs))/mean(obs,na.rm=TRUE))
