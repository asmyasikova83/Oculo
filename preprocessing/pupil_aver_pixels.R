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
library(readxl)
library(stringi)

sterr <- function(x) sd(x)/sqrt(length(x))

path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"
out_path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"

#"tables/resp_18042022.txt"
#"tables/autists.txt"
df1 <- fread(paste0(path,"norma.txt"))
df2 <- fread(paste0(path,"autists.txt"))
df_large <- rbind(df1,df2)

#standardazitng fname
df_large$fname <- gsub ("_1", "", df_large$fname)
df_large$fname <- gsub ("p0", "P0", df_large$fname)
df_large$trial_type <-gsub ("_", "", df_large$trial_type)
df_large[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

#1
df_large[, index := 1:.N, by=c('fname')]
df_large <- na.omit(df_large)
#2
#RT filter (for all intervals)
df_large <- df_large[RT>300 & RT<4000]
df_large <- df_large[block != 6]

#Z for pupil

ncol_min <- grep('V_m999_m950',colnames(df_large))
ncol_max <- grep('V_2951_3000',colnames(df_large))
col <- c(colnames(df_large)[ncol_min:ncol_max])
col

df_filt_m <- data.table(melt(df_large[blink==F], id.vars = c('fname','time'), measure.vars = col))
Zmeans <- df_filt_m[, mean(value), by=fname]
setnames(Zmeans,'V1','Zmean_2')
Zsds <- df_filt_m[, sd(value), by=fname]
setnames(Zsds,'V1','Zsd_2')
#df_large[,RT_raw:=RT]

df_large <- merge(df_large,Zmeans,by='fname',all.x = TRUE)
df_large <- merge(df_large,Zsds,by='fname',all.x = TRUE)

#do not apply Z for pupil
cols <- colnames(df_large)[grep('resp_|V_|BL_|inter_trial',colnames(df_large))]
#for (j in cols) set(df_large, j = j, value = (df_large[[j]]-df_large$Zmean_2)/df_large$Zsd_2)

df_large$trial_type4 <- df_large$trial_type

df_large[prev_risk==0 & risk==1 & next_risk==0, trial_type4:= 'risk']
df_large[prev_risk==0 & risk==0 & next_risk==0, trial_type4:= 'norisk']

# load filtered dataset

nt <- c('P001','P004', 'P019', 'P021', 'P022', 'P032', 'P034', 'P035', 'P039','P040', 'P044','P047','P048', 'P053',
        'P055', 'P058', 'P059', 'P060', 'P061', 'P063', 'P064', 'P065', 'P066')
at <- c('P301', 'P304','P307', 'P312','P313','P314','P316','P318','P321','P322','P323','P324','P325','P326','P327',
        'P328','P329','P333', 'P334','P335','P338','P341', 'P342')
df_NT <- df_large[fname %in% nt]
df_NT$group <- 'normals'
df_AT <- df_large[fname %in% at]
df_AT$group <- 'autists'
df_large_group <- rbind(df_AT, df_NT)
unique(df_large_group$fname)

#trained
df_large_group <- df_large_group[trained==FALSE, train:='not_trained']
df_large_group <- df_large_group[trained==TRUE, train:='trained']

#outliers

#pupil_sd <- df_large_group [blink==F][,sd(resp_1001_2200)]
#df_large_group [,resp_flag:=!blink & abs(resp_1001_2200)<3*pupil_sd]
#flag <- 'resp_flag'
#df_large_group <- df_large_group[df_large_group[,get(flag)==T]]

df_large_group$train<- as.factor(df_large_group$train)
unique(df_large_group[train=='not_trained']$fname)
################################LMM####################################################

df_large_group <- df_large_group[trial_type4 == 'risk']
df_large_group <- as.data.table(df_large_group)
df_large_group <- df_large_group[group == 'normals']

write.csv(df_large_group,paste0(out_path, "risk_normals_pixels.csv"))
