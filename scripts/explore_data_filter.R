#########################################################################################################################
# Filter
rm(list = ls())

library(FilterFlow)
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)

col2remove = 1

dir_filter <- file.path(getwd(),"Figures","Filter")
if(!dir.exists(dir_filter)) dir.create(dir_filter)

data_file <- "./data/Data_Filter_2.csv"

data.csv <- read.csv(data_file,header = TRUE)
time <- data.csv[,1]
Weight.obs <- data.csv[,2:(ncol(data.csv)-col2remove)]

for (i in seq(1,ncol(Weight.obs))){
  Weight.obs[,i] <- filter_negative(Weight.obs[,i])
  Weight.obs[,i] <- filter_positive(Weight.obs[,i])
}

data.formatted <- melt(Weight.obs) %>% rename(weight.obs = value) %>% mutate(type = "Filter",
                                                                             set = as.numeric(substring(variable,2,2)),
                                                                             rep = as.numeric(substring(variable,3,3)),
                                                                             time = rep(time,length(2:(ncol(data.csv)-col2remove)))) %>% mutate(time = time*60,
                                                                                                                                                Vobs = weight.obs/1000/1000)



alpha <- 0.05
data.formatted2 <- data.formatted %>% group_by(set,time) %>% dplyr::summarise(V.mean = mean(Vobs,na.rm = TRUE),
                                                                              V.sd = sd(Vobs,na.rm = TRUE),
                                                                              V.alphamin = quantile(Vobs, alpha/2, na.rm = TRUE),
                                                                              V.alphamax = quantile(Vobs, 1 - alpha / 2, na.rm = TRUE))

plot1 <-
  ggplot(data = data.formatted,
         aes(x = time, y = Vobs,colour = as.factor(set),group=interaction(set, rep))) +
  geom_line() +
  theme_bw()

ggsave(plot = plot1,
       dpi = 300,
       width = 15,
       height = 10,
       units = "cm",
       file = file.path(dir_filter,"Vdata_curves.png"))

plot2 <-
  ggplot(data = data.formatted2,
         aes(x = time, y = V.mean,
             ymin = V.mean - 1.96*V.sd/sqrt(3),
             ymax = V.mean + 1.96*V.sd/sqrt(3),
             colour = as.factor(set),
             fill = as.factor(set))) +
  geom_line() +
  geom_ribbon(alpha = 0.5,linetype = 0) +
  theme_bw()

ggsave(plot = plot2,
       dpi = 300,
       width = 15,
       height = 10,
       units = "cm",
       file = file.path(dir_filter,"Vdata_envelopes.png"))

Qexp_temp <- data.formatted

Qexp_filter <- data.frame()
Nset <- max(Qexp_temp$set)
for (iset in seq(1,Nset)){

  reps <- unique((Qexp_temp %>% filter(set == iset) %>% pull(rep)))
  Nrep <- length(reps)
  for (irep in seq(1,Nrep)){
    Qexp_filter_temp <- Qexp_temp %>% filter(rep == reps[irep],set == iset)
    t <- Qexp_filter_temp %>% pull(time)
    V <- Qexp_filter_temp %>% pull(Vobs)
    Qexp_filter <- rbind(Qexp_filter,
                         data.frame(set = iset,
                                    rep = irep,
                                    time = t,
                                    V = V,
                                    Qexp = dV.dt(t,V)))
  }
}


dataQexp.formatted <- Qexp_filter %>% group_by(set,time) %>% dplyr::summarise(Qexp.mean = mean(Qexp,na.rm = TRUE),
                                                                              Qexp.sd = sd(Qexp,na.rm = TRUE),
                                                                              Qexp.alphamin = quantile(Qexp, alpha/2, na.rm = TRUE),
                                                                              Qexp.alphamax = quantile(Qexp, 1 - alpha / 2, na.rm = TRUE))

plot3 <-
  ggplot(data = Qexp_filter,
         aes(x = time, y = Qexp,colour = as.factor(set),group=interaction(set, rep))) +
  geom_point() +
  scale_y_log10() +
  theme_bw()

ggsave(plot = plot3,
       dpi = 300,
       width = 15,
       height = 10,
       units = "cm",
       file = file.path(dir_filter,"Qdata_curves.png"))

plot4 <-
  ggplot(data = dataQexp.formatted,
         aes(x = time, y = Qexp.mean,
             ymin = Qexp.mean - 1.96*Qexp.sd/sqrt(3),
             ymax = Qexp.mean + 1.96*Qexp.sd/sqrt(3),
             colour = as.factor(set),
             fill = as.factor(set))) +
  geom_line() +
  geom_ribbon(alpha = 0.5,linetype = 0) +
  theme_bw()

ggsave(plot = plot2,
       dpi = 300,
       width = 15,
       height = 10,
       units = "cm",
       file = file.path(dir_filter,"Qdata_envelopes.png"))

save(Qexp_filter,file = "./data/data_filter.Rdata")
