# LMM analysis separately on trained and not_trained or both.
# Choose mode to operate the subset you want  

#rm(list = ls())

library(reshape2)
library(data.table)
library(ggplot2)
library(lme4)
library("ggpubr")
library(emmeans)
library(lmerTest)
library(dplyr)
#install.packages('viridis')
library(viridis)

sterr <- function(x) sd(x)/sqrt(length(x))
path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"
out_path <- paste0(path,'pics_articles/article_at_nt/')

#"tables/resp_18042022.txt"
#"tables/autists.txt"
df1 <- fread(paste0(path,"norma.txt"))
df2 <- fread(paste0(path,"autists.txt"))
#df_large <- rbind(df1,df2)
df_large <- as.data.table(rbind(df1,df2))
#df_large<- na.omit(df_large)

#standardazitng fname
df_large$fname <- gsub ("_1", "", df_large$fname)
df_large$fname <- gsub ("p0", "P0", df_large$fname)
df_large$trial_type <-gsub ("_", "", df_large$trial_type) 

#1
#average over time interval
int_beg <- '1001'
int_end <- '2200'

ncol_min <- grep(paste0('_', int_beg, '_'), colnames(df_large))[1]
ncol_max <- grep(paste0('_', int_end, ''), colnames(df_large))[1]
col <- c(colnames(df_large)[ncol_min:ncol_max])

colname <- paste0('resp_',int_beg,'_',int_end)

df_large[,(colname):=rowMeans(df_large[,.SD,.SDcol=col])]
df_large[, index := 1:.N, by=c('fname','block')]

#2
#RT filter (for all intervals)
df_large <- df_large[RT>300 & RT<4000]
#df_large  <- df_large[block!='6']
df_large[prev_rew == 1, feedback_prev:='prev_rew'][prev_rew == 0, feedback_prev:='prev_lose']

#Z for pupil

ncol_min <- grep('V_m999_m950',colnames(df_large))
ncol_max <- grep('V_2951_3000',colnames(df_large))
col <- c(colnames(df_large)[ncol_min:ncol_max])

df_filt_m <- data.table(melt(df_large[blink==F], id.vars = c('fname','time'), measure.vars = col))
Zmeans <- df_filt_m[, mean(value), by=fname]
setnames(Zmeans,'V1','Zmean_2')
Zsds <- df_filt_m[, sd(value), by=fname]
setnames(Zsds,'V1','Zsd_2')

df_large <- merge(df_large,Zmeans,by='fname',all.x = TRUE)
df_large <- merge(df_large,Zsds,by='fname',all.x = TRUE)

#apply Z for pupil
cols <- colnames(df_large)[grep('resp_|V_|BL_|inter_trial',colnames(df_large))]
for (j in cols) set(df_large, j = j, value = (df_large[[j]]-df_large$Zmean_2)/df_large$Zsd_2)

df_large$trial_type4 <- df_large$trial_type
df_large[prev_risk==0 & risk==1 & next_risk==0, trial_type4:= 'risk']
df_large[prev_risk==0 & risk==0 & next_risk==0, trial_type4:= 'norisk']


#3
df_large <- df_large[trial_type!='final' & trial_type!='criterion' & !fname %in% c('P308','P309','P311')]

pupil_sd <- df_large[blink==F][,sd(resp_1001_2200)]
df_large[,resp_flag:=!blink & abs(resp_1001_2200)<3*pupil_sd]


# load filtered dataset

nt <- c('P001','P004','P019', 'P021', 'P022', 'P032', 'P034', 'P035', 'P039','P040', 'P044','P047','P048', 'P053',
        'P055', 'P058', 'P059', 'P060', 'P061', 'P063', 'P064', 'P065', 'P066')
at <- c('P301', 'P304','P307', 'P312','P313','P314','P316','P318','P321','P322','P323','P324','P325','P326','P327',
        'P328','P329','P333', 'P334','P335','P338','P341', 'P342')
df_NT <- df_large[fname %in% nt]
df_NT$group <- 'normals'
df_AT <- df_large[fname %in% at]
df_AT$group <- 'autists'
df_large_group <- rbind(df_AT, df_NT)
unique(df_large_group$fname)

#settings for train untrain
df_large_group <- df_large_group[!is.na(resp_1001_2200)]

df_large_group[trained == FALSE, train:='not_trained'][trained== TRUE, train:='trained']

df_large_group <- df_large_group[!is.na(train)]
df_large_group$train<- as.factor(df_large_group$train)

flag <- 'resp_flag'
interval <- 'resp_1001_2200'

#df_large_group <- df_large_group[df_large_group[,get(flag)==T]]
length(unique(df_large_group[group == 'autists' & train == 'not_trained']$fname))
length(unique(df_large_group[group == 'normals' & train == 'not_trained']$fname))

print(unique(df_large_group[group == 'autists' & train == 'trained']$fname))

df_large_group <- df_large_group[trial_type4 %in% c('norisk','risk')]

######################################Traits#################################################
question <- read_excel(paste0(path, 'Questionnares.xls'))

#replace blank spaces in colnames
colClean1 <- function(question){ colnames(question) <- gsub(" ", "_", colnames(question)); question }
question <- colClean1(question)
colClean2 <- function(question){ colnames(question) <- gsub("...1", "fname", colnames(question)); question }
question <- colClean2(question) 
colnames(question)

question -> question[!is.na("Tolerance_for_uncertanty")]
question$"Tolerance_for_uncertanty"

question <- as.data.table(question)
question[, fname:=stri_extract_first_regex(question$fname,"[0-9]+")]

modes1 <- 'trained'
modes2 <- 'autists'

modes3 = 'norisk'

#modes3 <- ''
df_large_group <- df_large_group[train == as.character(modes1)]
df_large_group <- df_large_group[group == as.character(modes2)]
df_large_group <- df_large_group[trial_type4 == as.character(modes3)]
unique(df_large_group$trial_type4)
subj_list1 <- unique(df_large_group$fname)
subj_list1
length(unique(subj_list1))

subj_list1 <- df_large_group$fname <- gsub ("P", "", df_large_group$fname)

question <- question[fname %in% subj_list1]
question$fname
length(unique(question$fname))

####################################

df_large_group$fname <- as.factor(gsub("P", "", df_large_group$fname))

data_pupil = df_large_group %>%
  #group_by(fname, block, index) %>%
  group_by(fname) %>%
  summarize(Pupil_m999_2200 = mean(resp_1001_2200))

###########################
       
combined <- merge(data_pupil, question, by = 'fname')

length(unique(combined$fname))
unique(combined$fname)

cor.test(combined$Pupil_m999_2200, combined$"Intolerance_for_uncertanty") ### просто распечатываем корреляции

title <- paste0(as.character(modes1),'_', as.character(modes2),'_', as.character(modes3),'_Intolerance_for_uncertanty')
#title <-'Испытуемые с РАС после обучения: HP, Intolerance_of_uncertainty'
title <- 'Correlation: ASD in HP after learning and IU '

#Intolerance_for_uncertanty
#Tolerance_for_uncertanty
#FUN_SEEKING
#Behavioral inhibition
#Behavioral_activation

g <- ggscatter(combined, x = "Intolerance_for_uncertanty", y = "Pupil_m999_2200", 
               add = "reg.line", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "spearman",
               xlab = title, ylab = 'Rel.pupil size 1000-2200 ms',
               font.size = 2.5, 
               cor.coef.name = c("rho"),
               cor.coeff.args = list(label.x = 50,label.y = 0.8, size = 12, label.sep = "\n"),
               size = 4.7) 
g

p1 <- ggpar(g,
            #ylim = c(-0.35, 0.4),
            font.ytickslab = 40,
            font.xtickslab = 37,
            font.main = 25,
            font.submain = 25,
            font.x = 40,
            font.y = 40,
)

p1
################################LMM####################################################

ggsave(filename = paste0(out_path, '_Pupil_IU_corr', '_Tukey','.png'), p1, width =  15, height =9)
