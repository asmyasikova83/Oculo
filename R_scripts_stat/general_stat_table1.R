library(reshape2)
library(data.table)
library(ggplot2)
library(ggpubr)
library(stringi)
library(dplyr)

#rm(list = ls())
path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"

#eyetracker
#"tables/resp_18042022.txt"
#"tables/autists.txt"
normal <- fread(paste0(path,"norma.txt"))
autists <- fread(paste0(path,"autists.txt"))
rt <- as.data.table(rbind(autists,normal))
#rt<- na.omit(rt)

#rename
rt$fname <- gsub('_1','',rt$fname)
rt$fname <- gsub('^p','P',rt$fname)

rt <- rt[grep('P[0-9]',rt$fname)]
rt<- rt[!(rt$RT< 300 | rt$RT> 4000),]
rt<- rt[block != 6]

#subjects
#p066<-p067, because we have no pupil data for p067
normal_rt<- filter(rt, fname %in%c('P001','P004','P019','P021','P022','P034',
                                   'P035','P032','P039','P040','P044','P047',
                                   'P048','P053','P055','P058','P059','P060',
                                   'P061','P063','P064','P065','P066'))
normal_rt$group<- "normal"


autists_rt<- filter(rt, fname %in% c('P301', 'P304','P307', 'P312','P313','P314',
                                     'P316', 'P318','P321', 'P322','P323','P324',
                                     'P325','P326','P327', 'P328','P329','P333', 
                                     'P334','P335','P338','P341', 'P342'))

autists_rt$group<- "autists"

df_rt<- rbind(autists_rt,normal_rt)

df3 <-df_rt

df3$learning_type <- 'unused'
df3[trained==F]$learning_type <- 'not_trained'
df3[trained==T]$learning_type <- 'trained'

unique(df3[learning_type =='not_trained']$fname)


df3$trial_type <- 'other'
df3[prev_risk==F & risk==F & next_risk==F]$trial_type <- 'norisk'
df3[prev_risk==F & risk==T & next_risk==F]$trial_type <- 'risk'
unique(df3$fname)


lp_hp<- filter(df3, trial_type=="norisk"|trial_type=="risk")

learn<- filter(lp_hp, learning_type == 'not_trained'|learning_type == 'trained')
unique(learn$fname)

q<- learn %>%
  group_by(fname,group,trial_type, learning_type)%>%summarise(n=n(),
                                                 mean_RT=mean(RT), sd_RT = sd(RT)) #### число трайлов обученных и нет
  #group_by(group, trial_type, learning_type)%>%summarise(n=n())
  #group_by(group, trial_type, learning_type)%>%summarise(mean_RT=mean(RT), sd_RT = sd(RT))

qq<- learn %>%
  group_by(group,trial_type, learning_type)%>%summarise(n=n(),
                                                              mean_RT=mean(RT), sd_RT = sd(RT)) #### число трайлов обученных и нет
#group_by(group, trial_type, learning_type)%>%summarise(n=n())
#group_by(group, trial_type, learning_type)%>%summarise(mean_RT=mean(RT), sd_RT = sd(RT))

qq <- as.data.table(qq)

data_stat_subj = learn %>%
  group_by(group, trial_type, learning_type, fname) %>%
  count(trial_type)

data_stat_mean = data_stat_subj %>%
  group_by(group, trial_type, learning_type) %>%
  summarise(mean = mean(n),
            sd = sd(n))