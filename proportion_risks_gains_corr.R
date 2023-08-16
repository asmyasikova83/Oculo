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
#rt<- na.omit(rt)

#rename
rt$fname <- gsub('_1','',rt$fname)
rt$fname <- gsub('^p','P',rt$fname)

rt <- rt[grep('P[0-9]',rt$fname)]


rt <- rt[!(rt$RT< 300 | rt$RT> 4000),]

#percent of risk in not_trained

rt[trained == FALSE, train:='not_trained'][trained== TRUE, train:='trained']

rt[,`:=`(proportion_risk=sum(risk)/.N),
   by=.(fname, train)]
rt <- rt[block != 6]

rt[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']


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

q_proportion_risk<- df_rt %>%
  #group_by(group, train)%>%summarise(mean_prop_risk=mean(proportion_risk), sd_prop_risk=sd(proportion_risk))
  group_by(fname, group, train)%>%summarise(mean_proportion_risk=mean(proportion_risk)) #### число трайлов обученных и нет
#  group_by(group, train, block)%>%summarise(mean_n =mean(n()))

q_fb<- df_rt %>%
  group_by(fname,group, train, feedback_prev)%>%summarise(n=n()) #### число трайлов обученных и нет
  #group_by(group, train, feedback_prev)%>%summarise(n=n())

q_proportion_risk <- as.data.table(unique(q_proportion_risk))
q_fb <- as.data.table(unique(q_fb))

modes1 <- 'trained'
modes2 <- 'autists'
  
q_fb <- q_fb[train == as.character(modes1)][feedback_prev == 'positive'][group==as.character(modes2)]
q_proportion_risk <- q_proportion_risk[train == as.character(modes1)][group==as.character(modes2)]

cor.test(q_proportion_risk$mean_proportion_risk, q_fb$n)
title <- paste0(as.character(modes1),'_', as.character(modes2), '_% of LP choices')
title

q <- merge(q_fb, q_proportion_risk, by = 'fname')

q$mean_proportion_risk <- 100*q$mean_proportion_risk 

g <- ggscatter(q, x = "mean_proportion_risk", y = "n", 
               add = "reg.line", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "pearson",
               cor.coeff.args = list(label.x = 20,label.y = 130, label.sep = "\n"),
               xlab = title, ylab = "Number of gains")
g

p1 <- ggpar(g,
            font.ytickslab = 30,
            font.xtickslab = 27,
            font.main = 25,
            font.submain = 25,
            font.x = 27,
            font.y = 20,
            
)
p1