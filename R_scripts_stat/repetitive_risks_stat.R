
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


df_rt[trained == FALSE, train:='not_trained'][trained== TRUE, train:='trained']

df_rt[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']
df_rt$trial_type = 'other'
df_rt[prev_risk==F & risk==T & next_risk==F]$trial_type <- 'risk'
df_rt[prev_risk==F & risk==F & next_risk==F]$trial_type <- 'norisk'
df_rt[prev_risk==T & risk==T & next_risk==T]$trial_type <- 'repet_risk'


#df_rt <- df_rt[train =='not_trained']

#subjects
#p066<-p067, because we have no pupil data for p067

df_rt <-df_rt[train == 'not_trained'][group == 'autists']
data_stat_subj = df_rt %>%
  group_by(group, train, trial_type) %>% summarise(n = n())

#normals TODO 
#repet_trained <- 100*(12/(1217+460+130+12))
#repet_trained
#norisk_trained <- 100*(1217/(1217+460+130+12))
#norisk_trained
#risk_trained <- 100*(130/(1217+460+130+12))
#risk_trained
#other_trained <- 100*(460/(1217+460+130+12))
#other_trained


#repet_notrained <- 100*(116/(339+763+116+148))
#repet_notrained
#norisk_notrained <- 100*(339/(339+763+116+148))
#norisk_notrained
#risk_notrained <- 100*(148/(339+763+116+148))
#risk_notrained
#other_notrained <- 100*(763/(339+763+116+148))
#other_notrained

#autists TODO
repet_trained <- 100*(12/(12+1359+627+202))
repet_trained
norisk_trained <- 100*(1359/(12+1359+627+202))
norisk_trained
risk_trained <- 100*(202/(12+1359+627+202))
risk_trained
other_trained <- 100*(627/(12+1359+627+202))
other_trained

repet_notrained <- 100*(83/(83+347+597+115))
repet_notrained
norisk_notrained <- 100*(347/(83+347+597+115))
norisk_notrained
risk_notrained <- 100*(115/(83+347+597+115))
risk_notrained
other_notrained <- 100*(597/(83+347+597+115))
other_notrained

transit <- as.numeric(round(norisk_trained))
proportion <- as.numeric(round(norisk_notrained))
MAT = rbind(c(transit),c(proportion))
chisq.test(MAT, cor=F, sim=T, B = 10000)