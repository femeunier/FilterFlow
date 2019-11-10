rm(list = ls())

library(FilterFlow)
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)


data_file <- "./data/Data_Disk.csv"

data.csv <- read.csv(data_file,header = TRUE)
time <- data.csv[,1]
Weight.obs <- data.csv[,2:ncol(data.csv)]

for (i in seq(1,ncol(Weight.obs))){
  Weight.obs[,i] <- filter_negative(Weight.obs[,i])
  Weight.obs[,i] <- filter_positive(Weight.obs[,i])
}

data.formatted <- melt(Weight.obs) %>% rename(weight.obs = value) %>% mutate(type = "Disk",
                                                                             set = as.numeric(substring(variable,7,7)),
                                                                             rep = as.numeric(substring(variable,8,8)),
                                                                             time = rep(time,length(2:ncol(data.csv))))

# Remove weird curve
# data.formatted  <- data.formatted  %>% filter(!(set == 3 & rep == 2 ))


alpha <- 0.05
data.formatted2 <- data.formatted %>% group_by(set,time) %>% dplyr::summarise(weight.mean = mean(weight.obs,na.rm = TRUE),
                                         weight.sd = sd(weight.obs,na.rm = TRUE),
                                         weight.alphamin = quantile(weight.obs, alpha/2, na.rm = TRUE),
                                         weight.alphamax = quantile(weight.obs, 1 - alpha / 2, na.rm = TRUE))

plot1 <-
  ggplot(data = data.formatted,
       aes(x = time, y = weight.obs,colour = as.factor(set),group=interaction(set, rep))) +
  geom_point() +
  theme_bw()

plot1

plot2 <-
  ggplot(data = data.formatted2,
         aes(x = time, y = weight.mean,
             ymin = weight.mean - 1.96*weight.sd/sqrt(3),
             ymax = weight.mean + 1.96*weight.sd/sqrt(3),
             colour = as.factor(set),
             fill = as.factor(set))) +
  geom_line() +
  geom_ribbon(alpha = 0.5,linetype = 0) +
  theme_bw()

# t <- time*60
# V <- Weight.obs[,1]/1000/1000
# Delta_V.Delta_t <- diff(c(0,V))/diff(c(0,t))
# plot(t,Delta_V.Delta_t,col='black',lty=1,pch=1)
# lines(t,dV.dt(t,V),col='red',lty=2,type='p',pch=1)

Qexp_temp <- data.formatted %>% mutate(time = time*60,
                                       weight.obs = weight.obs/1000/1000) %>% group_by(set,rep)

Qexp_disk <- data.frame()
Nset <- max(Qexp_temp$set)
for (iset in seq(1,Nset)){

    reps <- unique((Qexp_temp %>% filter(set == iset) %>% pull(rep)))
    Nrep <- length(reps)
    for (irep in seq(1,Nrep)){
      Qexp_disk_temp <- Qexp_temp %>% filter(rep == reps[irep],set == iset)
      t <- Qexp_disk_temp %>% pull(time)
      V <- Qexp_disk_temp %>% pull(weight.obs)
      Qexp_disk <- rbind(Qexp_disk,
                         data.frame(set = iset,
                                    rep = irep,
                                    time = t,
                                    V = V,
                                    Qexp = dV.dt(t,V)))
    }
}


dataQexp.formatted <- Qexp_disk %>% group_by(set,time) %>% dplyr::summarise(Qexp.mean = mean(Qexp,na.rm = TRUE),
                                                                         Qexp.sd = sd(Qexp,na.rm = TRUE),
                                                                         Qexp.alphamin = quantile(Qexp, alpha/2, na.rm = TRUE),
                                                                         Qexp.alphamax = quantile(Qexp, 1 - alpha / 2, na.rm = TRUE))

plot3 <-
  ggplot(data = Qexp_disk,
         aes(x = time, y = Qexp,colour = as.factor(set),group=interaction(set, rep))) +
  geom_point() +
  theme_bw()

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


save(Qexp_disk,file = "./data/data_disk.Rdata")
#########################################################################################################################
