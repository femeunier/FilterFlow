rm(list = ls())

library(dplyr)
library(FilterFlow)
library(ggplot2)

load(file = "./data/data_disk.Rdata")

currentset = 3
h0 = 0.225
rb = 0.05
tb = 0.01

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

k_opt <- model_opt$par

opt_df <- data.frame(time = current_data %>% filter(rep == 1) %>%pull(time),
                     Qexp = Qfit_d(k = k_opt,
                                   time = current_data %>% filter(rep == 1) %>%pull(time),
                                   rb = rb, tb = tb, h0 = h0))

ggplot() +
  geom_point(data = current_data, aes(x = time,
                                      y = Qexp,
                                      col = as.factor(rep))) +
  geom_line(data = opt_df,aes(x = time,
                              y = Qexp),col = 'red') +
  theme_bw()
