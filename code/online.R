

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(e1071)

f_dir <- "C:/Users/yzhang/Desktop/appliedstat/online/Demo/CrashSample/"
setwd(f_dir)
data_crash <- ldply(list.files(f_dir), read.csv, header=TRUE)
data_crash <- mutate(data_crash, crash = 1)

f_dir <- "C:/Users/yzhang/Desktop/appliedstat/online/Demo/Base/"
setwd(f_dir)
data_non <- ldply(list.files(f_dir), read.csv, header=TRUE)
data_non <- mutate(data_non, crash = 0)

dataset <- bind_rows(data_non, data_crash)


## Some Driving Behaviors


temp <- data_non %>% select(file_id, X, accel_x, accel_y, accel_z)
temp <- melt(temp, id.vars = c("file_id", "X"))


temp %>%
  ggplot(aes(x=X, y=value, group=variable, color=variable)) +
  geom_line(size=0.7) +
  facet_wrap("file_id")+
  ggtitle("Acceleration for non-crash data") + theme_minimal()+
  theme(plot.title = element_text(size = 15, hjust = 0.5))


temp <- data_crash %>% select(file_id, X, accel_x, accel_y, accel_z)
temp <- melt(temp, id.vars = c("file_id", "X"))


temp %>%
  ggplot(aes(x=X, y=value, group=variable, color=variable)) +
    geom_line(size=0.7) +
    facet_wrap("file_id")+
    ggtitle("Acceleration for crash data") + theme_minimal()+
    theme(plot.title = element_text(size = 15, hjust = 0.5))



temp <- data_crash %>% 
  filter(file_id == "14219686") %>%
  select(X, accel_x, accel_y, accel_z, speed_gps) %>%
  mutate(speed_gps = speed_gps/10)
temp <- melt(temp, id.vars = c("X"))

temp %>%
  ggplot(aes(x=X, y=value, group=variable, color=variable)) +
  geom_line(size=0.7) +
  ggtitle("Acceleration and speed for crash one") + theme_minimal()+
  theme(plot.title = element_text(size = 15, hjust = 0.5))



f_dir <- "C:/Users/yzhang/Desktop/appliedstat/online/select/crash"
setwd(f_dir)
data_crash <- ldply(list.files(f_dir), read.csv, header=TRUE)
data_crash <- mutate(data_crash, crash = 1)

f_dir <- "C:/Users/yzhang/Desktop/appliedstat/online/select/baseline/"
setwd(f_dir)
data_non <- ldply(list.files(f_dir), read.csv, header=TRUE)
data_non <- mutate(data_non, crash = 0)

dataset <- bind_rows(data_non, data_crash)



temp <- dataset %>% 
  select(crash, accel_x, accel_y, accel_z)
log_d1 <- function(x){
  return(log(abs(x)+1)*sign(x))
}
log_d2 <- function(x){
  return(log(abs(x+1)+1)*sign(x)-1)
}
temp["accel_x"] <- lapply(temp["accel_x"], log_d1)
temp["accel_y"] <- lapply(temp["accel_y"], log_d1)
temp["accel_z"] <- lapply(temp["accel_z"], log_d2)
temp <- melt(temp, id.vars = c("crash"))
temp$crash <- factor(temp$crash)
ggplot(temp, aes(x=variable, y=value, fill=crash)) +
  geom_violin()+
  ggtitle("Acceleration density in three-dimen") + theme_minimal()+
  theme(plot.title = element_text(size = 15, hjust = 0.5))



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
  ggtitle("Standard deviation in three-dimension") + theme_minimal()+
  theme(plot.title = element_text(size = 15, hjust = 0.5))
ggsave("C:/Users/yzhang/Desktop/appliedstat/online/fig_std.pdf", p)


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
  ggtitle("Coefficient of variation in three-dimension") + theme_minimal()+
  theme(plot.title = element_text(size = 15, hjust = 0.5))
ggsave("C:/Users/yzhang/Desktop/appliedstat/online/fig_cv.pdf", p)


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
  ggtitle("Skewness in three-dimension") + theme_minimal()+
  theme(plot.title = element_text(size = 15, hjust = 0.5))
ggsave("C:/Users/yzhang/Desktop/appliedstat/online/fig_ske.pdf", p)

temp <- dataset %>% 
  select(crash, file_id, accel_x, accel_y, accel_z) %>%
  group_by(crash, file_id) %>%
  summarise(
    ac_kur_x = kurtosis(accel_x, type = 1),
    ac_kur_y = kurtosis(accel_y, type = 1),
    ac_kur_z = kurtosis(accel_z, type = 1)
  )%>% 
  select(crash, ac_kur_x, ac_kur_y, ac_kur_z) %>%
  melt(id.vars = c("crash"))
temp$crash <- factor(temp$crash)
ggplot(temp, aes(x=variable, y=value, fill=crash)) +
  geom_violin()+
  ggtitle("Acceleration kurtosis in three-dimension") + theme_minimal()+
  theme(plot.title = element_text(size = 15, hjust = 0.5))



online_std <- function(x, r, n, W_r=1, W_c=1, W_u=1){
  N <- length(x)
  u <- c()
  ss <- n
  t <- 0
  
  u_r <- var(x[1:n])
  u <- c(u, u_r)
  
  while(ss<length(x)){
    
    t <- t + 1
    
    R <- (r*(r-1)/2)*var(x[(ss+1):(ss+r)])
    C <- sum(outer(x[(ss-n+1):ss], x[(ss+1):(ss+r)], FUN = "-")^2)/2
      
    u_r <- u_r*(n*(n-1)/2 + (t-1)*r*(r-1)/2 + (t-1)*n*r)
    u_r <- W_u*u_r + W_r*R + W_c*C
    u_r <- u_r/(n*(n-1)/2 + t*r*(r-1)/2 + t*n*r)
    
    u <- c(u, u_r)
    ss <- ss + r
  }
  return(sqrt(u))
}

data_non1 <- read.csv("C:/Users/yzhang/Desktop/appliedstat/online/Demo5/Base/26508566.pkl")
x1 <- data_non1[["accel_x"]]
y1 <- data_non1[["accel_y"]]
z1 <- data_non1[["accel_z"]]
data_non2 <- read.csv("C:/Users/yzhang/Desktop/appliedstat/online/Demo5/Base/26508572.pkl")
x2 <- data_non2[["accel_x"]]
y2 <- data_non2[["accel_y"]]
z2 <- data_non2[["accel_z"]]
data_cra <- read.csv("C:/Users/yzhang/Desktop/appliedstat/online/Demo5/CrashSample/5591353.pkl")
xf <- data_cra[["accel_x"]]
yf <- data_cra[["accel_y"]]
zf <- data_cra[["accel_z"]]

x = c(x1, x2, xf)
t <- online_std(x=x, r=10, n = 30)
plot(t)


y = c(y1, y2, yf)
t <- online_std(x=y, r=10, n = 30)
plot(t)

z = c(z1, z2, zf)
t <- online_std(x=z, r=10, n = 30)
plot(t)
