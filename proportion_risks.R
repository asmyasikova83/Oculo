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

#rt[trained == FALSE, train:='not_trained'][trained== TRUE, train:='trained']

rt[trained == FALSE & learning == FALSE & critical==FALSE, train:='not_trained'][trained== TRUE, train:='trained']
rt <- rt[train %in% c('not_trained','trained')]
unique(rt$train)

rt[,`:=`(proportion_risk=sum(risk)/.N),
   by=.(fname,block)]

#repetitive LP
#rt$trial_type2 <- 'other'
#rt[prev_risk==1 & risk==1 & next_risk==1, trial_type2:= 'rLP']
#single LP
#rt[prev_risk==0 & risk==1 & next_risk==0, trial_type2:= 'LP']
#rt <- rt[trial_type2 %in% c('LP','rLP')]

rt$trial_type2 <- 'other'
rt[risk==1, trial_type2:= 'risk']
rt[risk==0, trial_type2:= 'norisk']
rt <- rt[trial_type2 %in% c('risk','norisk')]

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

unique(df_rt[train == 'not_trained']$fname)
length(unique(df_rt$fname))

q<- df_rt %>%
  #group_by(group, train, norisk)%>%summarise(N = n())
   group_by(group, train, trial_type2)%>%summarise(N = n())

   #group_by(train, group)%>%summarise(mean_proportion_risk=mean(proportion_risk)) #### число трайлов обученных и нет

q <- as.data.table(unique(q))


qq<- df_rt %>%
  group_by(group,train, trial_type2)%>%summarise(mean_rt=mean(RT), sd_rt=sd(RT))

#ttest
#df_rt <- df_rt[train== 'trained']
#t.test (proportion_risk ~ group, var.equal=FALSE, data = df_rt)
#df_rt <- df_rt[group=='normal']
#t.test (proportion_risk ~ train, var.equal=TRUE, data = q)