library(reshape2)
library(data.table)
library(ggplot2)
library(ggpubr)
library(lme4)
library(emmeans)
library(lmerTest)
library(plotrix)
library(stringi)
library(dplyr)
library(car)

#rm(list = ls())
path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"

#eyetracker
#"tables/resp_18042022.txt"
#"tables/autists.txt"
normal <- fread(paste0(path,"norma.txt"))
autists <- fread(paste0(path,"autists.txt"))
rt <- as.data.table(rbind(autists,normal))
rt<- na.omit(rt)

#rename
rt$fname <- gsub('_1','',rt$fname)
rt$fname <- gsub('^p','P',rt$fname)

rt <- rt[grep('P[0-9]',rt$fname)]


rt <- rt[!(rt$RT< 300 | rt$RT> 4000),]
rt<- rt[block != 6]

#percent of risk in not_trained

rt[trained == FALSE & learning == FALSE & critical == FALSE, train:='not_trained'][trained== TRUE, train:='trained']

rt$trial_type <- 'other'
rt[prev_risk==F & risk==F & next_risk==F]$trial_type <- 'norisk'
rt[prev_risk==F & risk==T & next_risk==F]$trial_type <- 'risk'
rt$risk

rt[,`:=`(proportion_risk=sum(risk)/.N),
   by=.(fname, train)]

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
       
df_rt<- filter(df_rt, trial_type=="norisk"|trial_type=="risk")
df_rt<- filter(df_rt, train=="trained"|train=="not_trained")

q<- df_rt %>%
  group_by(group, train)%>%summarise(mean_prop_risk=mean(proportion_risk),
                                                 sd_prop_risk=sd(proportion_risk))
#  group_by(fname, train, group)%>%summarise(mean_proportion_risk=mean(proportion_risk),
#                                            num_gains=count) #### число трайлов обученных и нет
#  group_by(group, train, block)%>%summarise(mean_n =mean(n()))

q <- as.data.table(unique(q))




