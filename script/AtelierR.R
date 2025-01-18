library(tidyverse)
library(ratdat)

#Exploration de donnees
?complete_old
summary(complete_old)
head(complete_old)
#tibble contient des informations, plus que un data frame
#tout dans un R est un vecteur
str(complete_old)


###ggplot
library(ggplot2) #grammar of graphics

ggplot(complete_old) #graphique vide

ggplot(complete_old, mapping=aes(x=weight, y = hindfoot_length)) + 
  geom_point(alpha=0.1)

#Removing Na
complete_old<-filter(complete_old, !is.na(weight) & !is.na(hindfoot_length))

#More aesthetic

#According to sex
ggplot(complete_old, mapping=aes(x=weight, y = hindfoot_length, shape=sex)) + 
  geom_point(alpha=0.1)

#According to year
ggplot(complete_old, mapping=aes(x=weight, y = hindfoot_length, color=year)) + 
  geom_point(alpha=0.1)

#According to plot type
ggplot(complete_old, mapping=aes(x=weight, y = hindfoot_length, color=plot_type)) + 
  geom_point(alpha=0.1) +
  scale_color_viridis_d() +
  scale_x_log10()



###Box plot

ggplot(complete_old, mapping=aes (x=plot_type, y=hindfoot_length))+
  geom_boxplot(outlier.shape=NA) + #pour enlever les outliers mais quand meme presents
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  scale_x_discrete(labels=label_wrap_gen(width=10))


ggplot(complete_old, mapping=aes (x=plot_type, y=hindfoot_length))+
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  geom_boxplot(outlier.shape=NA, fill = NA) + #fill c pour les objets
  scale_x_discrete(labels=label_wrap_gen(width=10))

#Violon plot and changing themes
ggplot(complete_old, mapping=aes (x=plot_type, y=hindfoot_length))+
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  geom_violin(fill = NA) + 
  scale_x_discrete(labels=label_wrap_gen(width=10)) +
  theme_bw()

ggplot(complete_old, mapping=aes (x=plot_type, y=hindfoot_length))+
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  geom_violin(fill = NA) + 
  scale_x_discrete(labels=label_wrap_gen(width=10)) +
  theme_bw()+
  theme(legend.position="none")+
  labs(x="Plot type", y="Hindfoot length (mm)")
  

plot_final<-ggplot(complete_old, mapping=aes (x=plot_type, y=hindfoot_length))+
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  geom_boxplot(outlier.shape=NA, fill=NA) +
  facet_wrap(vars(sex), ncol=1) +
  scale_x_discrete(labels=label_wrap_gen(width=10)) +
  theme_bw()+
  theme(legend.position="none")+
  labs(x="Plot type", y="Hindfoot length (mm)")

plot_final

ggsave(filename = "Figures/plot_final.png",
       plot=plot_final,
       height=6,
       width=8)

###TIDYVERSE


surveys<-read_csv("Data/Raw/surveys_complete_77_89.csv")
view(surveys)

str(surveys)

## FONCTIONS:
#select() -> colonnes
#filter() ->  lignes
#mutate() -> creer de nouvelles colonnes
#group_by()
#summarize()




#### select

select(surveys, plot_id, species_id)
select(surveys,  c(3,4))  #mieux avec noms que colonnes
select(surveys, -plot_id)
select(surveys, where(is.numeric))
select(surveys, where(anyNA))


#### filter

filter(surveys, year==1988)
filter(surveys, species_id %in% c("RM", "DO"))
filter(surveys, year==1988 & species_id %in% c("RM", "DO"))

#Challenge: 1980-1985 et year, month, species_id, plot_id

essai1<-filter(surveys, year==1980  && year==1981 && year ==1982 && year ==1983 && year==1984 && year==1985)

select(surveys, year & month & species_id & plot_id)

#Answers

#creer un objet et ajouter des choses a lui

surveys_80_85 <-filter(surveys, year >+1980 & year<=1985)
surveys_80_85
select(surveys_80_85, year, month, species_id, plot_id)

#emboiter

select(filter(surveys, year >+1980 & year<=1985), year, month, species_id, plot_id)

#pipeline -> comme emoitement mais plus clair, premier argument avant les autres, ctrl shift m pour %>% 

surveys %>% 
  filter(year==1980:1985) %>% 
  select(year, month, species_id, plot_id)

#Challenge: 1988, record_id, month, species_id

challenge2<-surveys %>% 
  filter(year==1988) %>% 
  select(record_id, month, species_id)
challenge2



#### Mutate

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg=weight/1000, 
         weight_lbs=weight_kg*2.2) %>% 
  relocate(weight_kg, .after=record_id) %>% 
  relocate(weight_lbs, .after = weight_kg)



#pas une seule variable pour data preferablement, excel peut interpreter differement, les donnees peuvent etre regroupees apres:

surveys %>% 
  mutate(date = paste(year, month, day, sep = "-")) %>% 
  relocate(date, .after =year)

library(lubridate)
surveys %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
  relocate(date, .after =year)


surveys %>% 
  group_by(sex) %>%
  summarize(mean.weight = mean(weight, na.rm=TRUE), 
            count=n())

##Challenge: pipeline, graph, 


challengePart1<-surveys %>% 
  filter(!is.na(sex)) %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
  group_by(sex, date) %>% 
  summarize(count=n()) %>% 
    ggplot(aes(x=date, y=count, color=sex))+
  geom_line()+
  labs(x="Date", y="Observations")+
  theme_bw()+
  scale_color_discrete(name="Legende")

ggsave(filename = "Figures/challengePart1.png",
       plot=challengePart1,
       height=6,
       width=8) 
challengePart1

#GIT 
' git add, git commit, git status, git log'

  
  



