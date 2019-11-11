# Optimize with Qexp

rm(list = ls())

library(dplyr)
library(FilterFlow)
library(ggplot2)

load(file = "./data/data_disk.Rdata")

k_opt <- RMSE <- c()
opt_df_all <- data.frame()

h0 = 0.225
rb = 0.05
tb = 0.01

for (iset in seq(1,3)){
  currentset = iset

  current_data <- Qexp_disk %>% filter(set == currentset) %>% dplyr::select(c(V,Qexp,rep,time))
  # Qfit_d(k = 1e-7,Qexp_disk %>% filter(set == 1,rep == 1) %>%pull(time))

  f <- function(k){
    RMSE_Qfit_d(k, data = current_data,rb = rb, tb = tb, h0 = h0)
    }

  model_opt <- optim(par = 1e-5,
                     fn = f,
                     lower = 0,
                     upper = 10,
                     method = c("Brent"))

  k_opt <- c(k_opt,model_opt$par)
  RMSE <- c(RMSE, model_opt$value)

  opt_df <- data.frame(time = current_data %>% filter(rep == 1) %>%pull(time),
                       Qfit = Qfit_d(k = k_opt[iset],
                                     time = current_data %>% filter(rep == 1) %>%pull(time),
                                     rb = rb, tb = tb, h0 = h0),
                       set = currentset)

  opt_df_all <- rbind(opt_df_all,
                      opt_df)
}

ggplot() +
  geom_point(data = Qexp_disk, aes(x = time,
                                   y = Qexp,
                                   col = as.factor(set)),size = 0.5) +
  geom_line(data = opt_df_all,aes(x = time,
                                  y = Qfit,
                                  col = as.factor(set)),size = 2) +
  theme_bw()

save(k_opt,file = "./data/kopt.Rdata")


###########################################################################################################
# Optimize with V

rm(list = ls())

library(dplyr)
library(FilterFlow)
library(ggplot2)

load(file = "./data/data_disk.Rdata")

k_opt <- RMSE <- c()
opt_df_all <- data.frame()

h0 = 0.225
rb = 0.05
tb = 0.01

for (iset in seq(1,3)){
  currentset = iset

  current_data <- Qexp_disk %>% filter(set == currentset) %>% dplyr::select(c(V,Qexp,rep,time))

  f <- function(k){
    RMSE_Vfit_d(k, data = current_data,rb = rb, tb = tb, h0 = h0)
  }

  model_opt <- optim(par = 1e-5,
                     fn = f,
                     lower = 0,
                     upper = 10,
                     method = c("Brent"))

  k_opt <- c(k_opt,model_opt$par)
  RMSE <- c(RMSE, model_opt$value)

  opt_df <- data.frame(time = current_data %>% filter(rep == 1) %>%pull(time),
                       Vfit = Vfit_d(k = k_opt[iset],
                                     time = current_data %>% filter(rep == 1) %>%pull(time),
                                     rb = rb, tb = tb, h0 = h0),
                       set = currentset)

  opt_df_all <- rbind(opt_df_all,
                      opt_df)
}

ggplot() +
  geom_point(data = Qexp_disk, aes(x = time,
                                   y = V,
                                   col = as.factor(set)),size = 0.5) +
  geom_line(data = opt_df_all,aes(x = time,
                                  y = Vfit,
                                  col = as.factor(set)),size = 2) +
  theme_bw()

save(k_opt,file = "./data/kopt.Rdata")


