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

interval <- 'resp_1001_2200'

ncol_min <- grep(paste0('_', int_beg, '_'), colnames(df_large))[1]
ncol_max <- grep(paste0('_', int_end, ''), colnames(df_large))[1]
col <- c(colnames(df_large)[ncol_min:ncol_max])

colname <- paste0('resp_',int_beg,'_',int_end)

df_large[,(colname):=rowMeans(df_large[,.SD,.SDcol=col])]
df_large[, index := 1:.N, by=c('fname','block')]

#2
#RT and blink filter (for all intervals)
df_large <- df_large[RT>300 & RT<4000]
df_large  <- df_large[block!='6']
df_large <- df_large[blink==F]

#df_large[rew == 1, feedback_cur:='positive'][rew == 0, feedback_cur:='negative']

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

#df_large$resp_1001_2200 = log10(df_large$resp_1001_2200)

#apply Z for pupil
#cols <- colnames(df_large)[grep('resp_|V_|BL_|inter_trial',colnames(df_large))]
#for (j in cols) set(df_large, j = j, value = (df_large[[j]]-df_large$Zmean_2)/df_large$Zsd_2)

df_large$trial_type4 <- df_large$trial_type
df_large[prev_risk==0 & risk==1 & next_risk==0, trial_type4:= 'risk']
df_large[prev_risk==0 & risk==0 & next_risk==0, trial_type4:= 'norisk']


#3
df_large <- df_large[trial_type!='final' & trial_type!='criterion' & !fname %in% c('P308','P309','P311')]

pupil_sd <- df_large[blink==F][,sd(resp_1001_2200)]
#df_large[,resp_flag:=!blink & abs(resp_1001_2200)<3*pupil_sd]

df_large[,resp_flag:= abs(resp_1001_2200)<3*pupil_sd]
#df_large[,resp_flag:= T]
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
#we only remove blinks in this analysis
#df_large_group <- df_large_group[df_large_group[,get(flag)==T]]
length(unique(df_large_group[group == 'autists' & train == 'not_trained']$fname))
length(unique(df_large_group[group == 'normals' & train == 'not_trained']$fname))

print(unique(df_large_group[group == 'autists' & train == 'trained']$fname))

df_large_group <- df_large_group[trial_type4 %in% c('norisk','risk')]

#####check for normality#########################
#pupil_raw_normals <- df_large_group[group == 'normals']
#hist(pupil_raw_normals$resp_1001_2200)
#pupil_raw_autists <- df_large_group[group == 'autists']
#hist(pupil_raw_autists$resp_1001_2200)
#qqnorm(pupil_raw_normals$resp_1001_2200, main='normals')
#qqline(pupil_raw_normals$resp_1001_2200)
#perform shapiro-wilk test
#shapiro.test(pupil_raw_autists$resp_1001_2200)

##################################################
df_large_group <- df_large_group[train %in% c('not_trained')]
unique(df_large_group$train)

################################LMM##########################

m <- lmer(resp_1001_2200 ~ trial_type4*group + (1|fname) + (1|block),df_large_group)
#m <- lmer(RT_Z ~ trial_type4*feedback_prev*group*trained_edited + (1|fname)+ (1|block),df_large_group)
#m <- lmer(RT_Z ~ trial_type4*feedback_prev*trained_edited + (1|fname)+ (1|block),df_large_group)
#m <- lmer(resp_1001_2200 ~ trial_type4*group*train + (1|fname)+ (1|block),df_large_group)

print(anova(m))

s <- step(m)
m2 <- get_model(s)
print(anova(m2))

#Tuk
Tuk <- emmeans(m, pairwise ~ trial_type4 | group  , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000)

Tuk_stat<- summary(Tuk)$contrasts
Tuk_stat<- as.data.table(Tuk_stat)
output <- as.data.table(Tuk$emmeans)

Tuk_stat[p.value<0.001, stars:='***']
Tuk_stat[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk_stat[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk_stat[p.value>0.05 & p.value<0.1 , stars:='#']

Tuk1 <- Tuk_stat

Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]

n <- Tuk1[!is.na(p_significant), .N]

thr <- max(df_large_group[, mean(get(interval)) + sterr(get(interval)), by=trial_type4]$V1) #
thr <- thr-0.15
if (n>1){
  Tuk1 <- Tuk1[!is.na(p_significant), y.position := seq((thr+0.01), (thr+0.3), 0.29/(n-1))]
} else {
  Tuk1 <- Tuk1[!is.na(p_significant), y.position := thr+0.36]
}

Tuk1[p.value<0.001, stars:='***']
Tuk1[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk1[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk1[p.value>0.05 & p.value<0.1 , stars:='#']
Tuk1[,y.position:=y.position-800]
Tuk1

signif <- Tuk1
################################plot###############################################
modes1 <- 'not_trained'
modes2 = 'autists'
Tuk1 <- Tuk1[group == as.character(modes2)]
Tuk1
output <- output[group == as.character(modes2)]
dff_trained <- df_large_group[group == as.character(modes2)]

length(unique(dff_trained$fname))
unique(df_large_group$train)

title <- paste0(as.character(modes1),' ',as.character(modes2))
#title <- 'ASD: after learning'
#originak means
#p <-ggline(dff_trained, 'trial_type4', interval,
#           add = c("mean_se"),
#           order = c("norisk","risk"),
#           ylab = 'Pupil [Z] 1001-2200 ms', xlab = "Choice",
#           size = 1.5, 
#           point.size = 1.8,
#           font.label = list(size = 16, color = "black"))+
#  scale_x_discrete(name=title, labels = c("HP", 'LP'))#+
#  geom_hline(yintercept=-0.0, linetype='dashed', col = 'black', size = 1)
#p <- p + stat_pvalue_manual(Tuk1, label = 'stars', size = 12,tip.length = 0.01,bracket.size = 1.2)

#consider emmeans in this analysis
p <-ggline(output, 'trial_type4', 'emmean',
           #add = c("mean_se"),
           order = c("norisk","risk"),
           ylab = 'Pup.raw 1001-2200 ms', xlab = "Choice",
           size = 1.5, 
           point.size = 1.8,
           font.label = list(size = 16, color = "black"))+
  scale_x_discrete(name=title, labels = c("HP", 'LP'))+
  geom_hline(yintercept=-0.0, linetype='dashed', col = 'black', size = 1)
  geom_errorbar(aes(x=trial_type4,ymin=lower.CL,ymax=upper.CL, width=0.35))
p <- p + stat_pvalue_manual(Tuk1, label = 'stars', size = 12,tip.length = 0.001,bracket.size = 1.2)

pic<- ggpar(p,
                    #ylim = c(-0.4, 0.2),
                    ylim = c(4900, 5000),
                    font.ytickslab = 30,
                    font.xtickslab = 27,
                    font.main = 25,
                    font.submain = 25,
                    font.x = 27,
                    font.y = 27)
pic

