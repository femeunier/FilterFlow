# Optimize with Qexp

rm(list = ls())

library(dplyr)
library(FilterFlow)
library(ggplot2)
library(numDeriv)
library(stats4)

load(file = "./data/data_disk.Rdata")

k_opt <- RMSE <- c()
opt_df_all <- data.frame()

h0 = 0.225
rb = 0.05
tb = 0.01
alpha = 0.01

k_CI <- c()
for (iset in seq(1,3)){
  currentset = iset

  current_data <- Qexp_disk %>% filter(set == currentset) %>% dplyr::select(c(V,Qexp,rep,time))

  f <- function(k){
    RMSE_Qfit_d(k, data = current_data,rb = rb, tb = tb, h0 = h0)
  }

  model_opt <- optim(par = 1e-5,
                     fn = f,
                     lower = 0,
                     upper = 10,
                     method = c("Brent"))

  NLM <- nlsLM(data = current_data,
               Qexp ~ k*pi*rb^2/tb*h0*exp(-k*time/tb),
               start=list(k = 2e-7), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                               printEval = TRUE, warnOnly = TRUE))

  k_CI_temp <- confint(NLM,level = 1-alpha)
  k_CI <- rbind(k_CI,k_CI_temp)

  k_opt <- c(k_opt,coef(NLM))
  RMSE <- c(RMSE, model_opt$value)

  opt_df <- data.frame(time = current_data %>% filter(rep == 1) %>%pull(time),
                       Qfit = Qfit_d(k = k_opt[iset],
                                     time = current_data %>% filter(rep == 1) %>%pull(time),
                                     rb = rb, tb = tb, h0 = h0),
                       Qfit_alphamin = Qfit_d(k = k_CI_temp[1],
                                     time = current_data %>% filter(rep == 1) %>%pull(time),
                                     rb = rb, tb = tb, h0 = h0),
                       Qfit_alphamax = Qfit_d(k = k_CI_temp[2],
                                     time = current_data %>% filter(rep == 1) %>%pull(time),
                                     rb = rb, tb = tb, h0 = h0),
                       set = currentset)

  opt_df_all <- rbind(opt_df_all,
                      opt_df)
}

RMSE_rel <- RMSE/ Qexp_disk%>%group_by(set) %>% summarise(Qm = mean(Qexp,na.rm = TRUE)) %>% pull(Qm)

ggplot() +
  geom_point(data = Qexp_disk, aes(x = time,
                                   y = Qexp,
                                   col = as.factor(set)),size = 0.5) +
  geom_ribbon(data = opt_df_all,
              aes(x = time,
                  ymin = Qfit_alphamin,
                  ymax = Qfit_alphamax,col = as.factor(set),fill = as.factor(set)),
              alpha = 0.5,linetype = 0) +
  geom_line(data = opt_df_all,aes(x = time,
                                  y = Qfit,
                                  col = as.factor(set)),size = 2,linetype=1) +
  theme_bw()

save(k_opt,file = "./data/kopt_Q.Rdata")

#################################################################################################
# Opt. with Q All k together
#
# rm(list = ls())
#
# library(dplyr)
# library(FilterFlow)
# library(ggplot2)
# library(stats4)
#
# load(file = "./data/data_disk.Rdata")
#
# opt_df_all <- data.frame()
#
# h0 = 0.225
# rb = 0.05
# tb = 0.01
#
#
# f <- function(k){
#   RMSE_Qfit_d_all(k, tb = tb,rb = rb,h0 = h0, data = Qexp_disk)
# }
#
# model_opt <- optim(par = c(1e-8,1e-8,1e-8),
#                    fn = f,
#                    method =  "Nelder-Mead")
#
# RMSE <- model_opt$value
# k_opt <- model_opt$par
#
# for (iset in seq(1,3)){
#   currentset = iset
#
#   current_data <- Qexp_disk %>% filter(set == currentset) %>% dplyr::select(c(V,Qexp,rep,time))
#
#   opt_df <- data.frame(time = current_data %>% filter(rep == 1) %>%pull(time),
#                        Qfit = Qfit_d(k = k_opt[iset],
#                                      time = current_data %>% filter(rep == 1) %>%pull(time),
#                                      rb = rb, tb = tb, h0 = h0),
#                        set = currentset)
#
#   opt_df_all <- rbind(opt_df_all,
#                       opt_df)
# }
#
# RMSE_rel <- RMSE/mean(Qexp_disk %>% pull(Qexp),na.rm = TRUE)
#
# ggplot() +
#   geom_point(data = Qexp_disk, aes(x = time,
#                                    y = Qexp,
#                                    col = as.factor(set)),size = 0.5) +
#   geom_line(data = opt_df_all,aes(x = time,
#                                   y = Qfit,
#                                   col = as.factor(set)),size = 2) +
#   theme_bw()
#
# save(k_opt,file = "./data/kopt_Q.Rdata")


#######################################################################################################
# Opt. with Q All k together + rb
#
# rm(list = ls())
#
# library(dplyr)
# library(FilterFlow)
# library(ggplot2)
# library(stats4)
#
# load(file = "./data/data_disk.Rdata")
#
# opt_df_all <- data.frame()
#
# h0_start = 0.225
# rb_start = 0.05
# tb_start = 0.01
#
#
# f <- function(param){
#   RMSE_Qfit_d_allparams(param, data = Qexp_disk)
# }
#
# model_opt <- optim(par = c(1e-8,1e-8,1e-8,h0_start,rb_start,tb_start),
#                    fn = f,
#                    method =  "Nelder-Mead")
#
# RMSE <- model_opt$value
# k_opt <- model_opt$par[1:3]
#
# h0 <- model_opt$par[4]
# rb <- model_opt$par[5]
# tb <- model_opt$par[6]
#
# for (iset in seq(1,3)){
#   currentset = iset
#
#   current_data <- Qexp_disk %>% filter(set == currentset) %>% dplyr::select(c(V,Qexp,rep,time))
#
#
#
#
#   opt_df <- data.frame(time = current_data %>% filter(rep == 1) %>%pull(time),
#                        Qfit = Qfit_d(k = k_opt[iset],
#                                      time = current_data %>% filter(rep == 1) %>%pull(time),
#                                      rb = rb, tb = tb, h0 = h0),
#                        set = currentset)
#
#   opt_df_all <- rbind(opt_df_all,
#                       opt_df)
# }
#
#
# ggplot() +
#   geom_point(data = Qexp_disk, aes(x = time,
#                                    y = Qexp,
#                                    col = as.factor(set)),size = 0.5) +
#   geom_line(data = opt_df_all,aes(x = time,
#                                   y = Qfit,
#                                   col = as.factor(set)),size = 2) +
#   theme_bw()
#
# save(k_opt,file = "./data/kopt_Q.Rdata")
#

###########################################################################################################
# Optimize with V

rm(list = ls())

library(dplyr)
library(FilterFlow)
library(ggplot2)
library(minpack.lm)

load(file = "./data/data_disk.Rdata")

k_opt <- k_opt2 <- RMSE <- RMSE_Q <- c()
opt_df_all <- data.frame()

h0 = 0.225
rb = 0.05
tb = 0.01
alpha = 0.01

k_CI <- c()

for (iset in seq(1,3)){
  currentset = iset

  current_data <- Qexp_disk %>% filter(set == currentset) %>% dplyr::select(c(V,Qexp,rep,time))

  f <- function(k){
    RMSE_Vfit_d(k, data = current_data,rb = rb, tb = tb, h0 = h0)
  }

  model_opt <- optim(par = 2e-7,
                     fn = f,
                     lower = 1e-10,
                     upper = 1e-5,
                     method = c("Brent"),hessian = TRUE)


  NLM <- nlsLM(data = current_data,
        time ~ a*log(1-V/(pi*rb^2*h0)),
        start=list(a = -tb/2e-7), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                             printEval = TRUE, warnOnly = TRUE))

 k_CI_temp <- -tb/confint(NLM,level = 1 - alpha)

 k_CI <- rbind(k_CI,k_CI_temp)

 #
 # LM <-
 #   lm(data = current_data %>% mutate(Vprim = log(1-V/(pi*rb^2*h0))),
 #       formula = time ~ Vprim,
 #       start=list(a=-tb/2e-7), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
 #                                                                            printEval = TRUE, warnOnly = TRUE))

  # test <- nlminb(start = 1e-5, objective = f, lower = 0, upper = 1000,hessian = TRUE)
  # k_opt2 <- c(k_opt2,test$par)

  k_opt <- c(k_opt,-tb/coef(NLM))

  RMSE_Q <- c(RMSE_Q,RMSE_Qfit_d(k_opt[iset], data = current_data,rb = rb, tb = tb, h0 = h0))

  RMSE <- c(RMSE, model_opt$value)

  opt_df <- data.frame(time = current_data %>% filter(rep == 1) %>%pull(time),
                       Vfit = Vfit_d(k = k_opt[iset],
                                     time = current_data %>% filter(rep == 1) %>%pull(time),
                                     rb = rb, tb = tb, h0 = h0),
                       Vfit_alpha_min = Vfit_d(k = k_CI_temp[2],
                                     time = current_data %>% filter(rep == 1) %>%pull(time),
                                     rb = rb, tb = tb, h0 = h0),
                       Vfit_alpha_max = Vfit_d(k = k_CI_temp[1],
                                     time = current_data %>% filter(rep == 1) %>%pull(time),
                                     rb = rb, tb = tb, h0 = h0),
                       Qfit = Qfit_d(k = k_opt[iset],
                                     time = current_data %>% filter(rep == 1) %>%pull(time),
                                     rb = rb, tb = tb, h0 = h0),
                       set = currentset)

  opt_df_all <- rbind(opt_df_all,
                      opt_df)
}

RMSE_rel <- (RMSE/Qexp_disk %>% group_by(set) %>% summarise(V = mean(V,na.rm=TRUE)) %>% pull(V))
RMSE_rel_Q <- (RMSE_Q/Qexp_disk %>% group_by(set) %>% summarise(Q = mean(Qexp,na.rm=TRUE)) %>% pull(Q))

ggplot() +
  geom_point(data = Qexp_disk, aes(x = time,
                                   y = V,
                                   col = as.factor(set)),size = 0.5) +
  geom_line(data = opt_df_all,aes(x = time,
                                  y = Vfit,
                                  col = as.factor(set)),size = 1,linetype=2) +
  geom_ribbon(data = opt_df_all,
              aes(x = time,
                  ymin = Vfit_alpha_min,
                  ymax = Vfit_alpha_max,col = as.factor(set),fill = as.factor(set)),
              alpha = 0.5,linetype = 0) +
  theme_bw()

# ggplot() +
#   geom_point(data = Qexp_disk, aes(x = time,
#                                    y = Qexp,
#                                    col = as.factor(set)),size = 0.5) +
#   geom_line(data = opt_df_all,aes(x = time,
#                                   y = Qfit,
#                                   col = as.factor(set)),size = 1,linetype=2) +
#   theme_bw()


save(k_opt,file = "./data/kopt_V.Rdata")

##############################################################################################################
# Opt. with V All k together + rb
#
# rm(list = ls())
#
# library(dplyr)
# library(FilterFlow)
# library(ggplot2)
# library(stats4)
#
# load(file = "./data/data_disk.Rdata")
#
# opt_df_all <- data.frame()
#
# h0_start = 0.225
# rb_start = 0.05
# tb_start = 0.01
#
# f <- function(param){
#   RMSE_Vfit_d_allparams(param, data = Qexp_disk)
# }
#
# # model_opt <- optim(par = c(1e-8,1e-8,1e-8,h0_start,rb_start,tb_start),
# #                    fn = f,
# #                    method =  "Nelder-Mead")
#
# model_opt <-
#   nlminb(start = c(1e-8,1e-8,1e-8,h0_start,rb_start,tb_start),
#        objective = f, lower = c(0,0,0,0.215,0.035,0.009),
#        upper = c(10,10,10,0.235,0.055,0.011))
#
# RMSE <- model_opt$objective
#
# k_opt <- model_opt$par[1:3]
# h0 <- model_opt$par[4]
# rb <- model_opt$par[5]
# tb <- model_opt$par[6]
#
# for (iset in seq(1,3)){
#   currentset = iset
#
#   current_data <- Qexp_disk %>% filter(set == currentset) %>% dplyr::select(c(V,Qexp,rep,time))
#
#
#   opt_df <- data.frame(time = current_data %>% filter(rep == 1) %>%pull(time),
#                        Vfit = Vfit_d(k = k_opt[iset],
#                                      time = current_data %>% filter(rep == 1) %>%pull(time),
#                                      rb = rb, tb = tb, h0 = h0),
#                        set = currentset)
#
#   opt_df_all <- rbind(opt_df_all,
#                       opt_df)
# }
#
# RMSE_rel <- RMSE/mean(Qexp_disk$V,na.rm=TRUE)
#
# ggplot() +
#   geom_point(data = Qexp_disk, aes(x = time,
#                                    y = V,
#                                    col = as.factor(set)),size = 0.5) +
#   geom_line(data = opt_df_all,aes(x = time,
#                                   y = Vfit,
#                                   col = as.factor(set)),size = 1,linetype=2) +
#   theme_bw()
#
# save(k_opt,file = "./data/kopt_V.Rdata")
#

