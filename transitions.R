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
rt <- rt[block != 6]

#percent of risk in not_trained

rt[trained == FALSE, train:='not_trained'][trained== TRUE, train:='trained']

rt[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

rt$transition_loss<- FALSE
rt$transition_loss
rt[feedback_prev=='negative' & prev_risk==F & risk==T]$transition_loss <- TRUE

rt$transition_gain<- FALSE
rt$transition_gain
rt[feedback_prev=='positive' & prev_risk==F & risk==T]$transition_gain <- TRUE


rt <- rt[train =='trained']

rt[,`:=`(proportion_transition_loss=sum(transition_loss)/.N),
   by=.(fname)]
rt[,`:=`(proportion_transition_gain=sum(transition_gain)/.N),
   by=.(fname)]

rt[,`:=`(sN=.N),
   by=.(fname)]

rt$sN
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
       

data_stat_subj = df_rt %>%
  group_by(group) %>% summarise(mean_proportion_transition_loss=mean(proportion_transition_loss),
                                mean_proportion_transition_gain=mean(proportion_transition_gain),
                                sd_proportion_transition_loss=sd(proportion_transition_loss),
                                sd_proportion_transition_gain=sd(proportion_transition_gain))

a_m <-data_stat_subj[1,2]/(data_stat_subj[1,2]+data_stat_subj[1,3])
a_m <- 100*a_m
a_m
n_m<- data_stat_subj[2,2]/(data_stat_subj[2,2]+data_stat_subj[2,3])
n_m <- 100*n_m
n_m
a_sd<-data_stat_subj[1,5]/(data_stat_subj[1,4]+data_stat_subj[1,5])
a_sd <- 100*a_sd
a_sd
n_sd<-sqrt(((data_stat_subj[2,4])^2+(data_stat_subj[2,5])^2)/2)
n_sd <- 100*n_sd
n_sd

#chi squared test for proportions
transit = as.numeric(round(n_m))
transit
proportion = as.numeric(round(a_m))
#proportion = 30%, proportion = 100%, against each other
#binom.test(transit, N, 0.3)
MAT = rbind(c(transit),c(proportion))
#trained
chisq.test(MAT, cor=F, sim=T, B = 10000)
