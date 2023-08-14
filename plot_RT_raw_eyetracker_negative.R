# LMM analysis RT

#rm(list = ls())
library(reshape2)
library(data.table)
library(ggplot2)
library("ggpubr")
library(readxl)
library(tidyr)
library(dplyr)
library(lme4)
library("ggpubr")
library(emmeans)
library(lmerTest)
library(readxl)
#install.packages('stringr')
library(stringi)
library(readr)

path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"
out_path <- paste0(path,'pupil_Z_feedback_prev/')

"tables/resp_18042022.txt"
#"tables/autists.txt"
df1 <- fread(paste0(path,"norma.txt"))
df2 <- fread(paste0(path,"autists.txt"))
df_large <- rbind(df1,df2)

df_large <- df_large[block != 6]

#standardazitng fname
df_large$fname <- gsub ("_1", "", df_large$fname)
df_large$fname <- gsub ("p0", "P0", df_large$fname)
df_large$trial_type <-gsub ("_", "", df_large$trial_type) 
# load filtered dataset

sterr <- function(x) sd(x)/sqrt(length(x))

path  <- 'C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/'
out_path <- 'C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_RT_raw/' #path to save pictures and tables


#p066<-p067, because we have no pupil data for p067
nt <- c('P001','P004','P019','P021','P022','P034',
        'P035','P032','P039','P040','P044','P047',
        'P048','P053','P055','P058','P059','P060',
        'P061','P063','P064','P065','P066')
at <- c('P301', 'P304','P307', 'P312','P313','P314',
        'P316', 'P318','P321', 'P322','P323','P324',
        'P325','P326','P327', 'P328','P329','P333', 
        'P334','P335','P338','P341', 'P342')

df_NT <- df_large[fname %in% nt]
df_NT$group <- 'normals'
df_AT <- df_large[fname %in% at]
df_AT$group <- 'autists'
df_large_group <- rbind(df_AT, df_NT)
df_large_group[, index := 1:.N, by=c('fname','block')]
#2
#RT filter (for all intervals)
df_large_group <- as.data.table(df_large_group)
df_large_group <- df_large_group[RT>300 & RT<4000]

df_large_group[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

#Z for RT
#df_large_group[,RT_Zmean:=mean(RT),by=fname]
#df_large_group[,RT_Zsd:=sd(RT),by=fname]
#df_large_group[,RT_Z:=(RT-RT_Zmean)/RT_Zsd]
#df_large_group <- df_large_group[!is.na(RT_Z)]

#RT raw
df_large_group[,RT_raw:=RT,by=fname]
df_large_group[,RT_raw_mean:=mean(RT),by=fname]
df_large_group[,RT_raw_sd:=sd(RT),by=fname]


#3
#outliers
flag <- 'RT_flag'
#df_large_group[!blink & abs(RT_Z)<3*RT_Zsd]
#df_large_group[, RT_flag:=abs(RT_Z)<3*RT_Zsd]
df_large_group[, RT_flag:=abs(RT_raw)<3*RT_raw_sd]
#df_large_group[, RT_flag:=!blink]

df_large_group <- df_large_group[df_large_group[,get(flag)==T]]
df_large_group[,RT_log_raw:=log10(RT_raw)]
df_large_group <- df_large_group[!is.na(RT_log_raw)]

#settings for train untrain
#trained
df_large_group <- df_large_group[trained==FALSE, train:='not_trained']
df_large_group <- df_large_group[trained==TRUE, train:='trained']

df_large_group <- df_large_group[!is.na(train)]

#intervals for plotting

#interval <-  'RT_Z'
#int_name <- 'RT_Z_from_eyetracker' # title for graph
interval <-  'RT_log_raw'
int_name <- 'RT_log_raw_from_eyetracker' # title for graph

#df_large_group[prev_risk==0 & risk==1 & next_risk==0, trial_type:= 'risk']

df_large_neg <- df_large_group[feedback_prev == 'negative']
df_large_neg[prev_risk==0 & risk==1 & next_risk==0, trial_type:= 'risk']
df_large_neg <- df_large_neg[trial_type == 'risk']

df_large_group[prev_risk==0 & risk==0 & next_risk==0, trial_type:= 'norisk']

df_large_group_pos <- df_large_group[trial_type == 'norisk']

df_large_group_test <- rbind(df_large_neg, df_large_group_pos)

######################################demographics#################################################

modes1 = 'trained vs untrained'
  
if (modes1 != 'trained vs untrained'){
  #df_large_group <- df_large_group[train == modes[t1]]
  #unique(df_large_group$fname)
  m <- lmer(get(interval) ~ trial_type*feedback_prev*group + (1|fname) + (1|block), data = df_large_group_test)
  #a <- aov(get(interval) ~ trial_type4*feedback_prev*group, data = df_large_group)
}
if (modes1 == 'trained vs untrained'){
  interval
  m <- lmer(get(interval) ~ trial_type*group*train + (1|fname) + (1|block), data = df_large_group_test)
  #a <- aov(get(interval) ~ trial_type4*feedback_prev*group*train, data = df_large_group)
}
#

#balance the num of trained and not_trained choices
#(0+train|fname)

#summary(a)
anova(m)
s <- step(m)
m2 <- get_model(s)
anova(m2)

# save anova and ranova
#an <- NULL
#an <- anova(m2)
#an <- data.table(an,keep.rownames = TRUE)
# an[, eta2:=F_to_eta2(`F value`, NumDF, DenDF)$Eta2_partial]
#an[`Pr(>F)`<0.001, stars:='***']
#an[`Pr(>F)`<0.01 & `Pr(>F)`>0.001 , stars:='**']
#an[`Pr(>F)`<0.05 & `Pr(>F)`>0.01 , stars:='*']
#an[`Pr(>F)`>0.05 & `Pr(>F)`<0.1 , stars:='#']

##### Tukey: trial_type #####

# prepare table with stats  

thr <- max(df_large_group[, mean(get(interval)), by=trial_type]$V1) #
#thr_min <- min(df_large_group[, mean(get(interval)) - sterr(get(interval)), by=trial_type4]$V1) 


#if ('trial_type4:group:train' %in% an$rn){
if (TRUE){
  Tuk <- NULL
  if (modes1== 'trained vs untrained'){
    Tuk <- data.table(summary(emmeans(m2, pairwise ~ trial_type|train|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    #Tuk <- data.table(summary(emmeans(m2, pairwise ~ train|trial_type|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
   }
  if (modes1 != 'trained vs untrained'){
    Tuk <- data.table(summary(emmeans(m2, pairwise ~ trial_type4|train|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    #Tuk <- data.table(summary(emmeans(a, pairwise ~ trial_type4|train|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
  }
}

#if ('trial_type4:feedback_prev:train' %in% an$rn){
if (FALSE){
  #by feedback
  Tuk <- NULL
  if (modes1 == 'trained vs untrained'){
    Tuk <- data.table(summary(emmeans(m, pairwise ~ feedback_prev|trial_type|train|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    #Tuk <- data.table(summary(emmeans(m2, pairwise ~ train|trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
  }
  if (modes1 != 'trained vs untrained'){
    #Tuk <- data.table(summary(emmeans(m2, pairwise ~ feedback_prev|train|trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    #Tuk <- data.table(summary(emmeans(m2, pairwise ~ feedback_prev|trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    #Tuk <- data.table(summary(emmeans(m, pairwise ~ feedback_prev|trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    Tuk <- data.table(summary(emmeans(m, pairwise ~ train|trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    
    }
}

Tuk

Tuk <- Tuk[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk <- Tuk[p.value<0.1, p_significant:=format(p.value, digits = 3)]
  
n <- Tuk[!is.na(p_significant), .N]
thr <- max(df_large_group[, mean(get(interval)), by=trial_type]$V1) #

if (n>1){
  Tuk <- Tuk[!is.na(p_significant), y.position := seq((thr+0.01), (thr+0.3), 0.29/(n-1))]
} else {
  Tuk <- Tuk[!is.na(p_significant), y.position := thr+0.16]
}
  
Tuk[p.value<0.001, stars:='***']
Tuk[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk[p.value<0.05 & p.value>0.01 , stars:='*']
#Tuk[p.value>0.05 & p.value<0.1 , stars:='#']
Tuk[,y.position:=y.position-0.2]
Tuk

choice_type = TRUE
choice_type_group = FALSE
feedback_prev = FALSE
feedback_prev_combined = FALSE


# plot Tukey and save table

Tuk
                          
if (choice_type == TRUE){
    modes2 = 'autists'
    modes3 = 'trained'
    dff_trained <- df_large_group_test[train == as.character(modes3)][group == as.character(modes2)]
    Tuk1 <- Tuk[train == as.character(modes3)][group == as.character(modes2)]
    Tuk1
    title <- paste0(as.character(modes3),' ',as.character(modes2))
    interval
    #plot
    p <-ggline(dff_trained, 'trial_type', interval,
               add = c("mean_se"),
               order = c("norisk", "risk"),
               ylab = int_name, xlab = "Choice type",
               size = 1.5, 
               point.size = 1.8,
               font.label = list(size = 16, color = "black"))+
      scale_x_discrete(name=title, labels = c("HP", 'LP'))
    p <- p + stat_pvalue_manual(Tuk1, label = 'stars', size = 12,tip.length = 0.001,bracket.size = 1.2)
    p
   
}

if (feedback_prev == TRUE){
    #by feedback in choice type by train
    modes1='trained'
    modes2='normals'
    modes1
    modes2
    dff_trained <- df_large_group[train == as.character(modes1)][group == as.character(modes2)]

    #Tuk1 <- Tuk[train == as.character(modes[t1])][group == as.character(modes[t2])]
    #title <- paste0(as.character(modes[t1]), as.character(modes[t2]),'by feedback_prev')
    
    #if in trained/untrained separately
    Tuk1 <- Tuk[train == as.character(modes1)][group == as.character(modes2)]
    Tuk1

    #plot
    #signif <- Tuk
    
    #plot settings
    sequence <-data.table(trial_type=c("norisk","risk"),number=c(1,2)) 
    
    y_values_rew <- dff_trained[feedback_prev == 'positive',
                                mean(get(interval)),by='trial_type']
    setnames(y_values_rew,'V1','y_values_rew')
    
    y_values_lose <- dff_trained[feedback_prev  == 'negative',
                                 mean(get(interval)),by='trial_type']
    setnames(y_values_lose,'V1','y_values_lose')
    
    y_values <- merge(y_values_lose,y_values_rew,by='trial_type')
    y_values <- merge(y_values,sequence,by='trial_type')
    y_values[,y_max:=max(y_values_lose,y_values_rew),by=trial_type]
    y_values <- merge(y_values,signif,by='trial_type')
    
    y_values <- y_values[train == as.character(modes1)][group == as.character(modes2)]
    #y_values <- y_values[group == as.character(modes[t2])]
    y_values
    title <- paste0(as.character(modes1), '_', as.character(modes2),'_fb_prev')
    p <-ggline(dff_trained, 'trial_type', interval,
               color = 'feedback_prev',
               add = c("mean_se"),
               #palette = c('green','red'),
               order = c("norisk","risk"),
               ylab = int_name, xlab = "Choice type",
               size = 1.5, 
               point.size = 1.8,
               font.label = list(size = 16, color = "black"))+
      scale_x_discrete(name=title, labels = c("HP", 'LP'))+
      geom_signif(y_position=c(y_values$y_max+0.1),
                 xmin=c(y_values$number-0.075), xmax=c(y_values$number+0.075),
                  annotation=c(Tuk1$stars), 
                 tip_length=0.001,textsize = 7,vjust = 0.4)
    p
}

p1 <- ggpar(p,
      #ylim = c(-0.4,0.6),
      ylim = c(2.9,3.2),
      font.ytickslab = 30,
      font.xtickslab = 27,
      font.main = 25,
      font.submain = 25,
      font.x = 27,
      font.y = 20,
      
)
p1
out_path
ggsave(filename = paste0(out_path, modes2,'_',modes3,'_', interval,'_Tukey','.png'), width =  6, height = 5)