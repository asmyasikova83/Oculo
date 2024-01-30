
library(data.table)
library(ggplot2)

#rm(list = ls())
path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"

#eyetracker
#"tables/resp_18042022.txt"
#"tables/autists.txt"
normal <- fread(paste0(path,"norma.txt"))
autists <- fread(paste0(path,"autists.txt"))
rt <- as.data.table(rbind(autists,normal))
rt[, index := 1:.N, by=c('fname','block')]
#rt<- na.omit(rt)

#rename
rt$fname <- gsub('_1','',rt$fname)
rt$fname <- gsub('^p','P',rt$fname)

rt <- rt[grep('P[0-9]',rt$fname)]


rt <- rt[!(rt$RT< 300 | rt$RT> 4000),]
rt <- rt[block != 6]

#RT
rt[,RT_raw:=RT,by=fname]
rt[,RT_raw_sd:=sd(RT_raw),by=fname]
rt[,RT_flag:=abs(RT_raw)<3*RT_raw_sd]
flag <- 'RT_flag'
rt<- rt[rt[,get(flag)==T]]

rt[,RT_log_raw:=log10(RT)]

#percent of risk in not_trained

rt[trained == FALSE, train:='not_trained'][trained== TRUE, train:='trained']

rt[prev_risk==0 & risk==1 & next_risk==0, trial_type:= 'risk']
rt[prev_risk==0 & risk==0 & next_risk==0, trial_type:= 'norisk']

rt<-rt[train=='trained']

#proportion of risk
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

q_propor<- df_rt %>%
  group_by(fname, group)%>%summarise(mean_proportion_risk=mean(proportion_risk), sd_prop_risk=sd(proportion_risk))

q_propor <- as.data.table(q_propor)

q<- df_rt %>%
  group_by(fname, group, trial_type)%>%summarise(mean_rt=mean(RT_log_raw))

q <- as.data.table(q)

#mean raw RT fot table1
qq<- df_rt %>%
  group_by(train, group, trial_type)%>%summarise(mean_rt=mean(RT_raw),
                                                 sd_rt=sd(RT_raw))

qq <- as.data.table(qq)

q_risk <- q[trial_type == 'risk']
q_norisk <- q[trial_type == 'norisk']

q_norisk <- q_norisk[fname != 'P313']
q_propor <- q_propor[fname != 'P313']

q_propor$rt_slowing <-q_risk$mean_rt - q_norisk$mean_rt

modes1 <- 'trained'
modes2 <- 'normal'

q_propor <- q_propor[group == modes2]
cor.test(q_propor$mean_proportion_risk, q_propor$rt_slowing)
title <- paste0(as.character(modes1),'_', as.character(modes2), '_% of LP')
title

q_propor$mean_proportion_risk <- 100*q_propor$mean_proportion_risk 

g <- ggscatter(q_propor, x = "mean_proportion_risk", y = "rt_slowing", 
               add = "reg.line", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "pearson",
               xlab = title, ylab = "RT log difference")
g

p1 <- ggpar(g,
            ylim = c(-0.2,0.3),
            font.ytickslab = 30,
            font.xtickslab = 27,
            font.main = 25,
            font.submain = 25,
            font.x = 27,
            font.y = 20,
            
)
p1