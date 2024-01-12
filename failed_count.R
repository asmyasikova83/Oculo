library(data.table)
library(ggplot2)
library(dplyr)

#rm(list = ls())
path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"

#eyetracker
#"tables/resp_18042022.txt"
#"tables/autists.txt"
normal <- fread(paste0(path,"norma.txt"))
autists <- fread(paste0(path,"autists.txt"))
rt <- as.data.table(rbind(autists,normal))

#rename
rt$fname <- gsub('_1','',rt$fname)
rt$fname <- gsub('^p','P',rt$fname)

rt <- rt[grep('P[0-9]',rt$fname)]
#rt<- rt[!(rt$RT< 300 | rt$RT> 4000),]
rt <- rt[block != 6]

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

#failed blocks trained==F & learning==F & critical==F
df3$learning_type <- 'unused'
df3[trained==F & learning==F & critical==F ]$learning_type <- 'not_trained'
#df3[trained==F]$learning_type <- 'not_trained'
df3[trained==T]$learning_type <- 'trained'

lp_hp<-df3

#learn<- filter(lp_hp, learning_type == 'not_trained'|learning_type == 'trained')

#num of blocks descriptive
learn<- filter(lp_hp, learning_type == 'not_trained'|learning_type == 'trained')
data_stat_subj = learn %>%
  group_by(group, learning_type, block, fname) %>%
  summarise(n=n())

#count num of failed blocks
data_stat_subj %>% group_by(learning_type, fname) %>% tally() %>%summarise(mean_block=mean(n),
                                                                            sd_block=sd(n))
data_stat_subj = as.data.table(data_stat_subj)


#nottrain_asd = data_stat_subj[group=='autists'][learning_type=='not_trained']
#nottrain_nt = data_stat_subj[group=='normal'][learning_type=='not_trained']

#t.test (nottrain_asd$n,nottrain_nt$n )

#Chi for failed
learn<- filter(lp_hp, learning_type == 'not_trained')

#check
unique(learn[learning_type == 'not_trained']$fname)
q_failed_blocks<- learn %>%
  group_by(fname, group,  learning_type)%>%summarise(N=n(), blocks = N/40)

q_failed_blocks <- as.data.table(q_failed_blocks)

ntblocks = sum(as.numeric(q_failed_blocks[group=='normal']$blocks))
asdblocks = sum(as.numeric(q_failed_blocks[group=='autists']$blocks))
asdN = nrow(q_failed_blocks[group=='autists'])
ntN = nrow(q_failed_blocks[group=='normal'])

MAT =rbind(c(ntblocks, asdblocks), c(ntN, asdN))

#Chi with permutations
chisq.test(MAT, cor=F, sim=T, B = 10000)
chisq.test(MAT)