library(tidyverse)
conidia<-read_csv("data/conidia.csv") %>%
  drop_na(isolate) %>%
  mutate(lw_ratio=length/width) %>%
  mutate(isolate=as.factor(isolate))
culture_rate<- read_csv("data/culture_rate.csv") %>%
  mutate(rate=size/days)%>%
  mutate(isolate=as.factor(isolate))
pycnidia<- read_csv("data/pycnidia.csv")

ggplot(conidia, aes(lw_ratio, group=isolate, fill=isolate))+
  geom_density(alpha=0.5) +
  theme_minimal()+
  xlab("Conidial Length to Width Ratio")+
  ylab("Density")+
  theme(legend.position = c(0.2, 0.8),
        axis.text = element_text(size=18),
        axis.title=element_text(size=20))+
  viridis::scale_fill_viridis(begin = 0, end = 0.8, discrete = T)

ggplot(culture_rate, aes(isolate, rate, group=isolate, color=isolate))+
  geom_jitter(alpha=0.7)+
  geom_boxplot(alpha=0.3, outlier.shape = NA)+
  theme_minimal() +
  ylab("Growth Rate (cm/day)")+
  xlab("Isolate")+
  theme(legend.position = c(0.6, 0.2),
        axis.text = element_text(size=18),
        axis.title=element_text(size=20))

ggplot(pycnidia, aes(size))+
  geom_density(size=2)+
  geom_vline(xintercept = mean(pycnidia$size), 
             linetype="dotted", 
             color="red",
             size=1)+
  theme_minimal() +
  ylab("Density of # of Pycnidia")+
  xlab("Size (um)")+
  theme(
        axis.text = element_text(size=18),
        axis.title=element_text(size=20))
  