

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(e1071)
library(ggridges)
library(gridExtra)

#=============== import segment data ===============#
dir_crash <- "C:/Users/yzhang/Desktop/appliedstat/OnlineSurrogate/data/segment/crash/"
setwd(dir_crash)
f_crash <- list.files(dir_crash)
data_crash <- ldply(f_crash[1:6], read.csv, header=TRUE)
data_crash <- mutate(data_crash, crash = 1)


dir_base <- "C:/Users/yzhang/Desktop/appliedstat/OnlineSurrogate/data/segment/base/"
setwd(dir_base)
f_base <- list.files(dir_base)
data_base <- ldply(f_base[1:6], read.csv, header=TRUE)
data_base <- mutate(data_base, crash = 0)

dataset <- bind_rows(data_base, data_crash)


#=============== basic description for segments ===============#


# figure for baseline segments
temp <- data_base %>% select(file_id, X, accel_x, accel_y, accel_z)
temp <- melt(temp, id.vars = c("file_id", "X"))


temp %>%
  ggplot(aes(x=X, y=value, group=variable, color=variable)) +
  geom_line(size=0.7) +
  facet_wrap("file_id")+
  ggtitle("Acceleration for baseline segments") + theme_minimal()+
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


# figure for crash segments
temp <- data_crash %>% select(file_id, X, accel_x, accel_y, accel_z)
temp <- melt(temp, id.vars = c("file_id", "X"))

temp %>%
  ggplot(aes(x=X, y=value, group=variable, color=variable)) +
    geom_line(size=0.7) +
    facet_wrap("file_id")+
    ggtitle("Acceleration for crash segments") + theme_minimal()+
    theme(plot.title = element_text(size = 15, hjust = 0.5),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())


# figure for crash segment 63772054
temp <- data_crash %>% 
  filter(file_id == "63772054") %>%
  select(X, accel_x, accel_y, accel_z)
temp <- melt(temp, id.vars = c("X"))


temp %>%
  ggplot(aes(x=X, y=value, group=variable, color=variable)) +
  geom_line(size=1.0) +
  ggtitle("Acceleration for crash segment 63772054") + theme_minimal() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())



# figure for crash segment 37159529
temp <- data_crash %>% 
  filter(file_id == "37159529") %>%
  select(X, accel_x, accel_y, accel_z)
temp <- melt(temp, id.vars = c("X"))


temp %>%
  ggplot(aes(x=X, y=value, group=variable, color=variable)) +
  geom_line(size=1.0) +
  ggtitle("Acceleration for crash segment 37159529") + theme_minimal() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())



#=============== import all segment data ===============#
dir_crash <- "C:/Users/yzhang/Desktop/appliedstat/OnlineSurrogate/data/segment/crash/"
setwd(dir_crash)
f_crash <- list.files(dir_crash)
data_crash <- ldply(f_crash, read.csv, header=TRUE)
data_crash <- mutate(data_crash, crash = 1)


dir_base <- "C:/Users/yzhang/Desktop/appliedstat/OnlineSurrogate/data/segment/base/"
setwd(dir_base)
f_base <- list.files(dir_base)
data_base <- ldply(f_base, read.csv, header=TRUE)
data_base <- mutate(data_base, crash = 0)

dataset <- bind_rows(data_base, data_crash)



#=============== basic description for all data ===============#
temp <- dataset %>% 
  select(crash, accel_x, accel_y, accel_z)
log_d1 <- function(x){
  return(log(abs(x)+1)*sign(x))
}
log_d2 <- function(x){
  return(log(abs(x+1)+1)*sign(x))
}
temp["accel_x"] <- lapply(temp["accel_x"], log_d1)
temp["accel_y"] <- lapply(temp["accel_y"], log_d1)
temp["accel_z"] <- lapply(temp["accel_z"], log_d2)
temp <- melt(temp, id.vars = c("crash"))
temp$label <- paste(temp$variable, temp$crash)

temp <- temp %>%
  filter(value<0.3) %>% filter(value>-0.3)

temp %>%
  ggplot( aes(y=label, x=value,  fill=label)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=50) +
  theme_ridges() +
  ggtitle("Acceleration density for segments") +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 1),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())
  




#=============== Choice of surrogates ===============#

# figure for standard deviation
temp <- dataset %>% 
  select(crash, file_id, accel_x, accel_y, accel_z) %>%
  group_by(crash, file_id) %>%
  summarise(
    ac_std_x = sd(accel_x),
    ac_std_y = sd(accel_y),
    ac_std_z = sd(accel_z)
  )%>% 
  select(crash, ac_std_x, ac_std_y, ac_std_z) %>%
  melt(id.vars = c("crash"))
temp$crash <- factor(temp$crash)
p <- ggplot(temp, aes(x=variable, y=value, fill=crash)) +
  geom_violin()+
  ggtitle("Standard deviation for segments") + theme_minimal()+
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
ggsave("C:/Users/yzhang/Desktop/appliedstat/online/fig_std.pdf", p)



# figure for coefficient of variation
temp <- dataset %>% 
  select(crash, file_id, accel_x, accel_y, accel_z) %>%
  group_by(crash, file_id) %>%
  summarise(
    ac_cv_x = sd(accel_x)/(mean(accel_x)),
    ac_cv_y = sd(accel_y)/(mean(accel_y)),
    ac_cv_z = sd(accel_z)/(mean(accel_z))
  )%>% 
  select(crash, ac_cv_x, ac_cv_y, ac_cv_z) %>%
  filter(ac_cv_x>-25) %>%
  filter(ac_cv_x<25) %>%
  filter(ac_cv_y>-25) %>%
  filter(ac_cv_y<25) %>%
  melt(id.vars = c("crash"))
temp$crash <- factor(temp$crash)
p <- ggplot(temp, aes(x=variable, y=value, fill=crash)) +
  geom_boxplot()+
  ggtitle("Coefficient of variation for segments") + theme_minimal()+
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
ggsave("C:/Users/yzhang/Desktop/appliedstat/online/fig_cv.pdf", p)


# figure for skewness
temp <- dataset %>% 
  select(crash, file_id, accel_x, accel_y, accel_z) %>%
  group_by(crash, file_id) %>%
  summarise(
    ac_ske_x = skewness(accel_x, type = 1),
    ac_ske_y = skewness(accel_y, type = 1),
    ac_ske_z = skewness(accel_z, type = 1)
  )%>% 
  select(crash, ac_ske_x, ac_ske_y, ac_ske_z) %>%
  melt(id.vars = c("crash"))
temp$crash <- factor(temp$crash)
p <- ggplot(temp, aes(x=variable, y=value, fill=crash)) +
  geom_violin()+
  ggtitle("Skewness for segments") + theme_minimal() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
ggsave("C:/Users/yzhang/Desktop/appliedstat/online/fig_ske.pdf", p)


# figure for maximum
temp <- dataset %>% 
  select(crash, file_id, accel_x, accel_y, accel_z) %>%
  group_by(crash, file_id) %>%
  summarise(
    ac_max_x = max(accel_x),
    ac_max_y = max(accel_y),
    ac_max_z = max(accel_z)
  )%>% 
  select(crash, ac_max_x, ac_max_y, ac_max_z) %>%
  melt(id.vars = c("crash"))
temp$crash <- factor(temp$crash)
ggplot(temp, aes(x=variable, y=value, fill=crash)) +
  geom_violin() +
  ggtitle("Maximum for segments") + theme_minimal()+
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())



#=============== Model training ===============#


# logistic regression

# GAM

# gbdt

# dnn

#=============== Model testing ===============#





#=============== Online surrogates ===============#


online_std <- function(x, r, n){
  N <- length(x)
  u <- c()
  ss <- n
  t <- 0
  
  u_r <- max(abs(x[1:n]))
  u <- c(u, u_r)
  
  while((ss+r)<=length(x)){
    
    t <- t + 1
    
    R <- (r*(r-1)/2)*var(x[(ss+1):(ss+r)])
    C <- sum(outer(x[(ss-n+1+r):ss], x[(ss+1):(ss+r)], FUN = "-")^2)/2
    
    u_r <- u_r*(n*(n-1)/2 + (t-1)*r*(r-1)/2 + (t-1)*(n-r)*r)
    u_r <- u_r + R + C
    u_r <- u_r/(n*(n-1)/2 + t*r*(r-1)/2 + t*(n-r)*r)
    
    u <- c(u, u_r)
    ss <- ss + r
  }
  return(sqrt(u))
}

online_std <- function(x, r, n){
  N <- length(x)
  u <- c()
  ss <- n
  t <- 0
  
  u_r <- var(x[1:n])
  u <- c(u, u_r)
  
  while((ss+r)<=length(x)){
    
    t <- t + 1
    
    R <- (r*(r-1)/2)*var(x[(ss+1):(ss+r)])
    C <- sum(outer(x[(ss-n+1+r):ss], x[(ss+1):(ss+r)], FUN = "-")^2)/2
      
    u_r <- u_r*(n*(n-1)/2 + (t-1)*r*(r-1)/2 + (t-1)*(n-r)*r)
    u_r <- u_r + R + C
    u_r <- u_r/(n*(n-1)/2 + t*r*(r-1)/2 + t*(n-r)*r)
    
    u <- c(u, u_r)
    ss <- ss + r
  }
  return(sqrt(u))
}


online_cv <- function(x, r, n){
  N <- length(x)
  u1 <- c()
  u2 <- c()
  ss <- n
  t <- 0
  
  u1_r <- var(x[1:n])
  u1 <- c(u1, u1_r)
  
  u2_r <- mean(x[1:n])
  u2 <- c(u2, u2_r)
  
  
  while((ss+r)<=length(x)){
    
    t <- t + 1
    
    R1 <- (r*(r-1)/2)*var(x[(ss+1):(ss+r)])
    C1 <- sum(outer(x[(ss-n+1+r):ss], x[(ss+1):(ss+r)], FUN = "-")^2)/2
    
    u1_r <- u1_r*(n*(n-1)/2 + (t-1)*r*(r-1)/2 + (t-1)*(n-r)*r)
    u1_r <- u1_r + R1 + C1
    u1_r <- u1_r/(n*(n-1)/2 + t*r*(r-1)/2 + t*(n-r)*r)
    
    u1 <- c(u1, u1_r)
    
    
    R2 <- sum(x[(ss+1):(ss+r)])
    u2_r <- (u2_r*(n + (t-1)*r) + R2)/(n + t*r)
    
    u2 <- c(u2, u2_r)    
    
    
    ss <- ss + r
  }
  return(sqrt(u1)/u2)
}


online_skew <- function(x, r, n){
  N <- length(x)
  u1 <- c()
  u2 <- c()  
  ss <- n
  t <- 0
  

  u1_r <-  n*1.0/((n-1)*(n-2))*sum((x[1:ss] - mean(x[1:ss]))^3)
  u1 <- c(u1, u1_r)
  
  u2_r <- var(x[1:n])
  u2 <- c(u2, u2_r)
  
  while((ss+r)<=length(x)){
    
    t <- t + 1
    
    R1 <- r*r/6.0*sum((x[(ss+1):(ss+r)] - mean(x[(ss+1):(ss+r)]))^3)
    temp <- (n-r)*(n-r)/6.0*sum((x[(ss-n+r+1):ss] - mean(x[(ss-n+r+1):ss]))^3)
    C1 <- n*n/6.0*sum((x[(ss-n+r+1):(ss+r)] - mean(x[(ss-n+r+1):(ss+r)]))**3) - temp - R1   


    u1_r <- u1_r*(n*(n-1)*(n-2)/6.0 + (t-1)*r*(r-1)*(r-2)/6.0 + (t-1)*(n-r)*(n-r-1)*r/2.0 + (t-1)*(n-r)*r*(r-1)/2.0)
    u1_r <- u1_r + R1 + C1
    u1_r <- u1_r/(n*(n-1)*(n-2)/6.0 + t*r*(r-1)*(r-2)/6.0 + t*(n-r)*(n-r-1)*r/2.0 + t*(n-r)*r*(r-1)/2.0)
    
    u1 <- c(u1, u1_r)
    
    
    R2 <- (r*(r-1)/2)*var(x[(ss+1):(ss+r)])
    C2 <- sum(outer(x[(ss-n+1+r):ss], x[(ss+1):(ss+r)], FUN = "-")^2)/2
    
    u2_r <- u2_r*(n*(n-1)/2 + (t-1)*r*(r-1)/2 + (t-1)*(n-r)*r)
    u2_r <- u2_r + R2 + C2
    u2_r <- u2_r/(n*(n-1)/2 + t*r*(r-1)/2 + t*(n-r)*r)
    
    u2 <- c(u2, u2_r)
    ss <- ss + r
  }
  return(u1/u2^1.5)
}

#=============== Online surrogates for trip description===============#


dir_crash <- "C:/Users/yzhang/Desktop/appliedstat/OnlineSurrogate/data/trip/crash/"
setwd(dir_crash)
f_crash <- list.files(dir_crash)
trip_crash <- ldply(f_crash[20], read.csv, header=TRUE)


x_std <- online_std(trip_crash$accel_x, 40, 100)
x_cv <- online_cv(trip_crash$accel_x, 40, 100)
x_skew <- online_skew(trip_crash$accel_x, 40, 100)
t <- c(0:(length(x_std)-1))*40+100
temp <- data.frame(t, x_std, x_cv, x_skew)

p1 <- trip_crash %>%
  ggplot(aes(x=t, y=accel_x)) +
  geom_line(size=0.7) + 
  ggtitle("Longitudinal acceleration of crash sample") + theme_classic()+
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


p2 <- temp %>%
  ggplot(aes(x=t, y=x_std)) +
  geom_line(size=1.0) + geom_point() +
  ggtitle("standard deviation") + theme_classic() +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0, 700))

p3 <- temp %>%
  ggplot(aes(x=t, y=x_cv)) +
  geom_line(size=1.0) + geom_point()+
  ggtitle("coefficient of variation") + theme_classic()+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0, 700))

p4 <- temp %>%
  ggplot(aes(x=t, y=x_skew)) +
  geom_line(size=1.0) + geom_point() +
  ggtitle("skewness") + theme_classic() +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0, 700))

grid.arrange(p1, p2, p3, p4, ncol = 1)



y_std <- online_std(trip_crash$accel_y, 40, 100)
y_cv <- online_cv(trip_crash$accel_y, 40, 100)
y_skew <- online_skew(trip_crash$accel_y, 40, 100)
t <- c(0:(length(x_std)-1))*40+100
temp <- data.frame(t, y_std, y_cv, y_skew)

p1 <- trip_crash %>%
  ggplot(aes(x=t, y=accel_y)) +
  geom_line(size=0.7) + 
  ggtitle("Lateral acceleration of crash sample") + theme_classic()+
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


p2 <- temp %>%
  ggplot(aes(x=t, y=y_std)) +
  geom_line(size=1.0) + geom_point() +
  ggtitle("standard deviation") + theme_classic() +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0, 700))

p3 <- temp %>%
  ggplot(aes(x=t, y=y_cv)) +
  geom_line(size=1.0) + geom_point()+
  ggtitle("coefficient of variation") + theme_classic()+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0, 700))

p4 <- temp %>%
  ggplot(aes(x=t, y=y_skew)) +
  geom_line(size=1.0) + geom_point() +
  ggtitle("skewness") + theme_classic() +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0, 700))

grid.arrange(p1, p2, p3, p4, ncol = 1)

#=============== Risk prediction for trip===============#





