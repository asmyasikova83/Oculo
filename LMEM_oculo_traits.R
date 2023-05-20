# LMM analysis separately on trained and not_trained or both.
# Choose mode to operate the subset you want  

#rm(list = ls())

accuracy_threshold <- 0.65
sequence_threshold <- 4
trained_trials_threshold <- 0
whole_block_flag <- FALSE

library(reshape2)
library(data.table)
library(ggplot2)
library(lme4)
library("ggpubr")
library(emmeans)
library(lmerTest)
library(readxl)
library(tidyr)
library(dplyr)
library(stringi)

sterr <- function(x) sd(x)/sqrt(length(x))

path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"
out_path <- paste0(path,'pupil_Z_feedback_prev/')

df1 <- fread(paste0(path,"norma.txt"))
df2 <- fread(paste0(path,"autists.txt"))
df_large <- rbind(df1,df2)


df_large <- df_large[block != 6]

df_large[,`:=`(percent_risk=sum(risk)/.N),

#standardazitng fname
df_large$fname <- gsub ("_1", "", df_large$fname)
df_large$fname <- gsub ("p0", "P0", df_large$fname)
df_large$trial_type <-gsub ("_", "", df_large$trial_type) 
# load filtered dataset

autists_info <- fread(paste0(path,'autism_all.txt'))
normals_info <- fread(paste0(path,'normals_all.txt'))
pairs_info <- fread(paste0(path,'pairs_ed.txt'))

colnames(pairs_info) <- c('fname','ASD','pair')

df_AT <- df_large[fname %in% autists_info$fname]
df_AT$group <- 'autists'
df_NT <- df_large[fname %in% normals_info$fname]
df_NT$group <- 'normals'

df_large_group <- rbind(df_AT, df_NT)

#1
#average over time interval
int_beg <- '1001'
int_end <- '2200'

ncol_min <- grep(paste0('_', int_beg, '_'), colnames(df_large_group))[1]
ncol_max <- grep(paste0('_', int_end, ''), colnames(df_large_group))[1]
col <- c(colnames(df_large_group)[ncol_min:ncol_max])
colname <- paste0('resp_',int_beg,'_',int_end)

df_large_group[,(colname):=rowMeans(df_large_group[,.SD,.SDcol=col])]
df_large_group[, index := 1:.N, by=c('fname','block')]

#2
#RT filter (for all intervals)
df_large_group <- df_large_group[RT>300 & RT<4000]
df_large_group[rew == 1, feedback_cur:='positive'][rew == 0, feedback_cur:='negative']
df_large_group[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

#Z for pupil

ncol_min <- grep('V_m999_m950',colnames(df_large_group))
ncol_max <- grep('V_2951_3000',colnames(df_large_group))
col <- c(colnames(df_large_group)[ncol_min:ncol_max])

df_filt_m <- data.table(melt(df_large_group[blink==F], id.vars = c('fname','time'), measure.vars = col))
Zmeans <- df_filt_m[, mean(value), by=fname]
setnames(Zmeans,'V1','Zmean_2')
Zsds <- df_filt_m[, sd(value), by=fname]
setnames(Zsds,'V1','Zsd_2')

df_large_group <- merge(df_large_group,Zmeans,by='fname',all.x = TRUE)
df_large_group <- merge(df_large_group,Zsds,by='fname',all.x = TRUE)

#apply Z for pupil
cols <- colnames(df_large_group)[grep('resp_|V_|BL_|inter_trial',colnames(df_large_group))]
for (j in cols) set(df_large_group, j = j, value = (df_large_group[[j]]-df_large_group$Zmean_2)/df_large_group$Zsd_2)

#df_large_group$trial_type4 <- df_large$trial_type
df_large_group[prev_risk==1 & risk==1 & next_risk==1, trial_type4:= 'repetitve_risk']
df_large_group[prev_risk==0 & risk==1 & next_risk==1, trial_type4:= 'repetitve_risk']
df_large_group[prev_risk==1 & risk==1 & next_risk==0, trial_type4:= 'repetitve_risk']
df_large_group[prev_risk==0 & risk==1 & next_risk==0, trial_type4:= 'risk']
df_large_group[prev_risk==1 & risk==0 & next_risk==1, trial_type4:= 'between_risk']
df_large_group[prev_risk==0 & risk==0 & next_risk==1, trial_type4:= 'prerisk']
df_large_group[prev_risk==1 & risk==0 & next_risk==0, trial_type4:= 'postrisk']
df_large_group[prev_risk==0 & risk==0 & next_risk==0, trial_type4:= 'norisk']


#3
df_large_group <- df_large_group[trial_type!='final' & trial_type!='criterion' & !fname %in% c('P308','P309','P311')]

df_large_group[,inter_flag:=(!is.na(BL_fix_m300)) & (!is.na(inter_trial))]
pupil_sd <- df_large_group[blink==F][,sd(resp_1001_2200)]
df_large_group[,resp_flag:=!blink & abs(resp_1001_2200)<3*pupil_sd]

#settings for train untrain

df_large_group <- df_large_group[!is.na(resp_1001_2200)]

#trained
df_large_group <- df_large_group[trained==FALSE & learning==FALSE, train:='not_trained']
df_large_group <- df_large_group[trained==TRUE, train:='trained']

#df_large_group[trained == FALSE, trained_edited:='untrained'][trained == TRUE, trained_edited:='trained']
#df_large_group[trained_custom==TRUE,train:='trained']
#df_large_group[trained_block==FALSE,train:='not_trained']


df_large_group <- df_large_group[!is.na(train)]
df_large_group$trained_edited<- as.factor(df_large_group$train)

#intervals for plotting

intervals <- data.table(int=c('resp_m399_2200', 
                              'resp_m399_0',
                              'resp_1_1000', 
                              'resp_1001_2200',
                              'RT_Z',
                              'BL_fix_m300'),
                        
                        int_name=c('Relative pupil area (-400-2200)', 
                                   'Relative pupil area (-400-0)',
                                   'Relative pupil area (0-1000)', 
                                   'Relative pupil area (1000-2200)',
                                   'RT (Z)',
                                   'Relative pupil area (fix-300-0)'),
                        
                        int_name_graph=c('Pupil size [Z]', 
                                         'Pupil size [Z]',
                                         'Pupil size [Z]', 
                                         'Pupil size [Z]',
                                         'RT [Z]',
                                         'Pupil size [Z]'),
                        
                        short_int_name=c('(-400-2200)', 
                                         '(-400-0)',
                                         '(0-1000)', 
                                         '(1000-2200)',
                                         'RT_Z',
                                         '(fix-300-0)'))

interval <-  intervals$int[4]
interval
int_name <- intervals[int==interval]$int_name_graph # title for graph

flag <- 'resp_flag'

#for pupils and RT_Z
df_large_group <- df_large_group[df_large_group[,get(flag)==T]]

######################################Traits#################################################
#from /tables
question <- read_excel(paste0(path, 'Questionnares.xls'))

#replace blank spaces in colnames
colClean1 <- function(question){ colnames(question) <- gsub(" ", "_", colnames(question)); question }
question <- colClean1(question)
colClean2 <- function(question){ colnames(question) <- gsub("...1", "fname", colnames(question)); question }
question <- colClean2(question) 
colnames(question)

subj_list_1 <- c('001', '004','019','021','022','032', '034','035','035','039','040','047', '053','055','058','059','060','061',
                 '063', '064','065', '301', '304', '307', '312', '313','314', '316', '320', '321', '322','324', '326', '327', '328', '329', '333',
                  '334', '335', '338', '341')
unique(question$fname)

question -> question[!is.na("Tolerance_for_uncertanty")]
question$"Tolerance_for_uncertanty"

question <- as.data.table(question)
question[, fname:=stri_extract_first_regex(question$fname,"[0-9]+")]
question$fname

question <- question[fname %in% subj_list_1]

####################################
df_large_group <- df_large_group[group == 'normals']

df_large_group$fname <- as.factor(gsub("P", "", df_large_group$fname))

data_pupil = df_large_group %>%
  group_by(fname) %>%
  summarize(Pupil_m999_2200 = mean(resp_1001_2200))

###########################
#1 - for normals, 2 - for autists
b <- question[DIAGN == '1']

combined <- merge(data_pupil, b, by = 'fname')

cor.test(combined$Pupil_m999_2200, combined$Performance_IQ) ### просто распечатываем корреляции

g <- ggscatter(combined, x = "Performance_IQ", y = "Pupil_m999_2200", 
               add = "reg.line", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "spearman",
               xlab = "Normals", ylab = "Pupil_minus999_2200") 
g


################################LMM####################################################

