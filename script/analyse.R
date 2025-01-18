#Charger packages
library(tidyverse)
library("ratdat")

ggplot(data=complete_old, aes (x=weight, y = hindfoot_length, color=sex))+
  geom_point(alpha=0.2)
