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
int_end <- '1500'

ncol_min <- grep(paste0('_', int_beg, '_'), colnames(df_large))[1]
ncol_max <- grep(paste0('_', int_end, ''), colnames(df_large))[1]
col <- c(colnames(df_large)[ncol_min:ncol_max])

colname <- paste0('resp_',int_beg,'_',int_end)

df_large[,(colname):=rowMeans(df_large[,.SD,.SDcol=col])]
df_large[, index := 1:.N, by=c('fname','block')]

#2
#RT filter (for all intervals)
df_large <- df_large[RT>300 & RT<4000]
df_large  <- df_large[block!='6']

df_large$block <- as.factor(df_large$block)

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

pupil_sd <- df_large[blink==F][,sd(resp_1001_1500)]
df_large[,resp_flag:=!blink & abs(resp_1001_1500)<3*pupil_sd]


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
df_large_group <- df_large_group[!is.na(resp_1001_1500)]

df_large_group[trained == FALSE, train:='not_trained'][trained== TRUE, train:='trained']

df_large_group <- df_large_group[!is.na(train)]
df_large_group$train<- as.factor(df_large_group$train)

flag <- 'resp_flag'
interval <- 'resp_1001_1500'
df_large_group <- df_large_group[df_large_group[,get(flag)==T]]
length(unique(df_large_group[group == 'autists' & train == 'not_trained']$fname))
length(unique(df_large_group[group == 'normals' & train == 'not_trained']$fname))

print(unique(df_large_group[group == 'autists' & train == 'trained']$fname))

df_large_group <- df_large_group[trial_type4 %in% c('norisk','risk')]

################################LMM##########################

m <- lmer(resp_1001_1500 ~  group*block + (1|fname),df_large_group)
#m <- lmer(RT_Z ~ trial_type4*feedback_prev*group*trained_edited + (1|fname)+ (1|block),df_large_group)
#m <- lmer(RT_Z ~ trial_type4*feedback_prev*trained_edited + (1|fname)+ (1|block),df_large_group)
#m <- lmer(resp_1001_2200 ~ trial_type4*group*train + (1|fname)+ (1|block),df_large_group)

print(anova(m))

s <- step(m)
m2 <- get_model(s)
print(anova(m2))

#for singkLP repetLP
#Tuk <- emmeans(m, pairwise ~ trial_type4 | train | group   , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000)
# for trained not_trained
Tuk <- emmeans(m2, pairwise ~   group | block   , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000)

#Tuk <- emmeans(m2, pairwise ~ train | group | trial_type4   , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000)
#Tuk <- emmeans(m2, pairwise ~ trained_edited | trial_type4  , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000)

Tuk_stat<- summary(Tuk)$contrasts
Tuk_stat<- as.data.table(Tuk_stat)
output2 <- as.data.table(Tuk$emmeans)

Tuk_stat[p.value<0.001, stars:='***']
Tuk_stat[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk_stat[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk_stat[p.value>0.05 & p.value<0.1 , stars:='#']

Tuk1 <- Tuk_stat

Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]

n <- Tuk1[!is.na(p_significant), .N]

thr <- max(df_large_group[, mean(get(interval)) + sterr(get(interval)), by=block]$V1) #
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
Tuk1[,y.position:=y.position+0.1]
Tuk1

signif <- Tuk1


sequence <-data.table(block=c("1","2","3","4","5"),number=c(1,2,3,4,5))

y_values_rew <- df_large_group[group == 'autists',
                               mean(get('resp_1001_1500')),by='block']

setnames(y_values_rew,'V1','y_values_rew')

y_values_lose <- df_large_group[group == 'normals',
                                mean(get('resp_1001_1500')),by='block']

setnames(y_values_lose,'V1','y_values_lose')

y_values <- merge(y_values_lose,y_values_rew,by='block')
y_values <- merge(y_values,sequence,by='block')
y_values[,y_max:=max(y_values_lose,y_values_rew),by=block]
y_values <- merge(y_values,signif,by='block')

p <-ggline(df_large_group, x = 'block', y = 'resp_1001_1500',
                     color='group',
                     add =  'mean_se',
                     #palette = c("#FF33CC", "#FF9900"),
                     palette = c("magenta","orange"),
                     #palette = c("darkgreen","blue"),
                     position=position_dodge(0.15),
                     order=c('1','2','3','4','5'),
                     ylab = 'Pupil [Z] 1001-1500 ms', xlab = 'Block',
                     size = 2.7,
                     font.label = list(size = 26, color = "black")
                     
    )+

  #scale_x_discrete(name='Block', labels = c("1", '2',"3", '4','5'))+

  geom_hline(yintercept=-0.0, linetype='dashed', col = 'black', size = 1) +
  geom_signif(y_position=c(y_values$y_max+0.27),
              xmin=c(y_values$number-0.075), xmax=c(y_values$number+0.075),
              annotation=c(Tuk_stat$stars), 
              tip_length=0.001,textsize = 10,vjust = 0.4)
  

p1 <- ggpar(p,
            ylim = c(0.2, 1.0),
            font.ytickslab = 30,
            font.xtickslab = 27,
            font.main = 25,
            font.submain = 25,
            font.x = 27,
            font.y = 20,
)

p1
ggsave(filename = paste0(out_path, 'NT','_Pupil_before_after', '_Tukey','.png'), p1, width =  5, height = 5)
