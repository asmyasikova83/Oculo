# LMM analysis separately on trained and not_trained or both.
# Choose mode to operate the subset you want  

#rm(list = ls())

accuracy_threshold <- 0.65
sequence_threshold <- 4
trained_trials_threshold <- 0

library(reshape2)
library(data.table)
library(ggplot2)
library(lme4)
library("ggpubr")
library(emmeans)
library(lmerTest)
library(gridExtra)

sterr <- function(x) sd(x)/sqrt(length(x))

path  <- 'C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/'
out_path <- 'C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_RTT_raw/' #path to save pictures and tables

#loading files and subjects 

autists_info <- fread(paste0(path,'autism_repet.txt'))
normals_info <- fread(paste0(path,'normals_repet.txt'))


df1 <- fread(paste0(path,"norma.txt"))
df2 <- fread(paste0(path,"autists.txt"))

df_large <- rbind(df1,df2)
#df_large <- fread("C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/DFF_FOR_BEHAVIOR.csv")


#standardazitng fname
df_large$fname <- gsub ("_1", "", df_large$fname)
df_large$fname <- gsub ("p0", "P0", df_large$fname)
df_large$trial_type <-gsub ("_", "", df_large$trial_type) 

autists_info <- fread(paste0(path,'autism.txt'))
normals_info <- fread(paste0(path,'normals.txt'))

#df_AT <- df_large[fname %in% autists_info$fname]
#df_AT$group <- 'autists'
#df_NT <- df_large[fname %in% normals_info$fname]
#df_NT$group <- 'normals'
#df_large_group <- rbind(df_AT, df_NT)

#larger sample
nt <- unique(df_large$fname[grepl('P0', df_large$fname)])
df_NT <- df_large[fname %in% nt]
df_NT$group <- 'normals'
at <- unique(df_large$fname[grepl('P3', df_large$fname)])
df_AT <- df_large[fname %in% at]
df_AT$group <- 'autists'
df_large_group <- rbind(df_AT, df_NT)

#preprocessing
df_large_group[, index := 1:.N, by=c('fname','block')]

#2
#RT filter (for all intervals)

df_large_group <- df_large_group[blink == FALSE]
df_large_group <- df_large_group[RT>300 & RT<4000]
df_large_group <- df_large_group[trial_type!='final' & trial_type!='criterion' & !fname %in% c('P308','P309','P311')]

#rename
df_large_group[rew == 1, feedback_cur:='cur_rew'][rew == 0, feedback_cur:='cur_lose']
df_large_group[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

#repetitive risks
df_large_group[prev_risk==TRUE & risk==TRUE & next_risk==TRUE, trial_type4:= 'repetitive_risk']
#df_large_group[prev_risk==TRUE & risk==FALSE & next_risk==TRUE, trial_type4:= 'between_risk']
df_large_group[prev_risk==FALSE & risk==FALSE & next_risk==FALSE, trial_type4:= 'norisk']

#3
#outliers

df_large_group[,RT_raw:=RT]
df_large_group[,RT_raw_sd:=sd(RT_raw),by=fname]
df_large_group[,RT_flag:=!blink & abs(RT_raw)<3*RT_raw_sd]

#block 6 has a specific probability
df_large_group <- df_large_group[block!='6']

#Vladimir's conditions
#df_large_group <- df_large_group[critical==FALSE & learning==FALSE]
#df_large_group[trained_custom==TRUE,train:='trained']
#df_large_group[trained_block==FALSE,train:='not_trained']

#Kseniya's conditions
df_large_group <- df_large_group[trained==FALSE & learning==FALSE, train:='not_trained']
df_large_group <- df_large_group[trained==TRUE, train:='trained']

df_large_group <- df_large_group[!is.na(train)]
df_large_group$train <- as.factor(df_large_group$train)

df_large_group <- df_large_group[trial_type4 %in% c('norisk','repetitive_risk')]

length(unique(df_large_group[group == 'autists'][train == 'not_trained']$fname))
unique(df_large_group[group == 'normals'][train == 'not_trained']$fname)

# create datatable with variables to analyze in data (int), 
# their labels for graps (int_name), short names for saving(short_int_name)

intervals <- data.table(int=c('resp_m399_2200', 
                              'resp_m399_0',
                              'resp_1_1000', 
                              'resp_1001_2200',
                              'RT_raw',
                              'BL_fix_m300'),
                        
                        int_name=c('Relative pupil area (-400-2200)', 
                                   'Relative pupil area (-400-0)',
                                   'Relative pupil area (0-1000)', 
                                   'Relative pupil area (1000-2200)',
                                   'RT_raw',
                                   'Relative pupil area (fix-300-0)'),
                        
                        int_name_graph=c('Pupil size [Z]', 
                                         'Pupil size [Z]',
                                         'Pupil size [Z]', 
                                         'Pupil size [Z]',
                                         'RT_raw',
                                         'Pupil size [Z]'),
                        
                        short_int_name=c('(-400-2200)', 
                                         '(-400-0)',
                                         '(0-1000)', 
                                         '(1000-2200)',
                                         'RT_raw',
                                         '(fix-300-0)'))

#### Analyze not_trained and trained separately ####
modes <- c('trained','not_trained','trained vs untrained', 'normals', 'autists', 'both groups')
t1 <- 2
t2 <- 6

modes[t1]
modes[t2]

df_large_group[,RT_flag:=T]
flag <- 'RT_flag'
df_large_group <- df_large_group[df_large_group[,get(flag)==T]]

interval <-  intervals$int[5]
interval
int_name <- intervals[int==interval]$int_name_graph # title for graph

df_large_group <- df_large_group[train  == as.character(modes[t1])]
unique(df_large_group$train)

if (modes[t1] != 'trained vs untrained'){
  df_large_group <- df_large_group[train == modes[t1]]
  m <- lmer(get(interval) ~ trial_type4*feedback_prev*group + (1|fname) + (1|block), data = df_large_group)
  #m <- lmer(get(interval) ~ trial_type4*feedback_prev + (1|fname) + (1|block), data = df_large_group)
}
if (modes[t1] == 'trained vs untrained'){
  unique(df_large_group$train)
  unique(df_large_group$group)
  m <- lmer(get(interval) ~ trial_type4*feedback_prev*group*train + (1|fname) + (1|block), data = df_large_group)
}

anova(m)
s <- step(m)
m2 <- get_model(s)
anova(m2)

choice_type = FALSE
choice_type_combined = FALSE
choice_type_group = FALSE
feedback_prev = FALSE
feedback_prev_combined = TRUE

if (choice_type == TRUE){
  Tuk <- NULL
  if (modes[t1] == 'trained vs untrained'){
    Tuk <- data.table(summary(emmeans(m2, pairwise ~ trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    #Tuk <- data.table(summary(emmeans(a, pairwise ~ trial_type4|train|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
  }
  if (modes[t1] != 'trained vs untrained'){
    Tuk <- data.table(summary(emmeans(m, pairwise ~ trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    #Tuk <- data.table(summary(emmeans(a, pairwise ~ trial_type4|train|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
  }
}
if (feedback_prev_combined == TRUE){
  #modes[t1] = 'trained'
  modes[t2] = 'autists'
  df_large_group <- df_large_group[group == as.character(modes[t2])]
  Tuk1 <- data.table(summary(emmeans(m, pairwise ~ feedback_prev|trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
  Tuk1 <- Tuk1[group == as.character(modes[t2])]
  dff_trained <- df_large_group
  unique(dff_trained$train)
  unique(dff_trained$group)
  unique(dff_trained$feedback_cur)
  title <- paste0(as.character(modes[t1]), as.character(modes[t2]))
}

# plot and save table

if (choice_type == TRUE){
  Tuk
  
  Tuk <- Tuk[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
  Tuk <- Tuk[p.value<0.1, p_significant:=format(p.value, digits = 3)]
  
  n <- Tuk[!is.na(p_significant), .N]
  thr <- max(df_large_group[, mean(get(interval)) + sterr(get(interval)), by=trial_type4]$V1) #
  
  if (n>1){
    Tuk <- Tuk[!is.na(p_significant), y.position := seq((thr+0.01), (thr+0.3), 0.29/(n-1))]
  } else {
    Tuk <- Tuk[!is.na(p_significant), y.position := thr+0.16]
  }
  
  Tuk[p.value<0.001, stars:='***']
  Tuk[p.value<0.01 & p.value>0.001 , stars:='**']
  Tuk[p.value<0.05 & p.value>0.01 , stars:='*']
  #Tuk[p.value>0.05 & p.value<0.1 , stars:='#']
  Tuk[,y.position:=y.position+0.16]
  Tuk
  
  modes[t2] = 'normals'
  dff_trained <- df_large_group[group == as.character(modes[t2])]
  Tuk1 <- Tuk[group == as.character(modes[t2])]
  Tuk1
  title <- paste0(as.character(modes[t1]),' ',as.character(modes[t2]))
  
  #plot
  p <-ggline(dff_trained, 'trial_type4', interval,
             add = c("mean_se"),
             order = c("norisk", "repetitive_risk"),
             ylab = int_name, xlab = "Choice type",
             size = 1.5, 
             point.size = 1.8,
             font.label = list(size = 16, color = "black"))+
    scale_x_discrete(name=title, labels = c("HP", 'repet_LP'))
  p <- p + stat_pvalue_manual(Tuk1, label = 'stars', size = 12,tip.length = 0.001,bracket.size = 1.2)
  p
}

if (feedback_prev_combined == TRUE){
  Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
  Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]
  
  n <- Tuk1[!is.na(p_significant), .N]
  
  thr <- max(df_large_group[, mean(get(interval)) + sterr(get(interval)), by=trial_type4]$V1) #
  thr <- thr-0.15
  if (n>1){
    Tuk1 <- Tuk1[!is.na(p_significant), y.position := seq((thr+0.01), (thr+0.3), 0.29/(n-1))]
  } else {
    Tuk1 <- Tuk1[!is.na(p_significant), y.position := thr+600]
  }
  
  Tuk1[p.value<0.001, stars:='***']
  Tuk1[p.value<0.01 & p.value>0.001 , stars:='**']
  Tuk1[p.value<0.05 & p.value>0.01 , stars:='*']
  Tuk1[p.value>0.05 & p.value<0.1 , stars:='#']
  Tuk1[,y.position:=y.position+0.4]
  Tuk1
  
  signif <- Tuk1
  signif
  
  #TODO for norm fb vs autists fb
  #by feedback in choice type train combined
  sequence <-data.table(trial_type4=c("norisk", "repetitive_risk"),number=c(1,2)) 
  y_values_rew <- df_large_group[feedback_prev == 'positive',
                                 #y_values_rew <- df_large_group[train == 'trained',
                                 mean(get(interval)),by='trial_type4']
  setnames(y_values_rew,'V1','y_values_rew')
  y_values_rew
  y_values_lose <- df_large_group[feedback_prev == 'negative',
                                  #y_values_rew <- df_large_group[train == 'not_trained',
                                  mean(get(interval)),by='trial_type4']
  setnames(y_values_lose,'V1','y_values_lose')
  y_values_lose
  y_values <- merge(y_values_lose,y_values_rew,by='trial_type4')
  y_values <- merge(y_values,sequence,by='trial_type4')
  y_values[,y_max:=max(y_values_lose,y_values_rew),by=trial_type4]
  y_values <- merge(y_values,signif,by='trial_type4')
  y_values
  
  Tuk1
  
  #plot
  p <-ggline(dff_trained, 'trial_type4', interval,
             color = 'feedback_prev',
             add = c("mean_se"),
             #palette = c('grey','black'),
             order = c("norisk", "repetitive_risk"),
             ylab = int_name, xlab = "Choice type",
             size = 1.5, 
             point.size = 1.8,
             font.label = list(size = 16, color = "black"))+
    scale_x_discrete(name=title, labels = c("HP", 'repetitive_LP'))+
    geom_hline(yintercept=-0.0, linetype='dashed', col = 'black', size = 1) +
    geom_signif(y_position=c(y_values$y_max+300),
                xmin=c(y_values$number), xmax=c(y_values$number),
                annotation=c(Tuk1$stars), 
                tip_length=0.001,textsize = 7,vjust = 0.4)
  fb_prev = '_fb_prev'
  p
}
p1 <- ggpar(p,
      ylim = c(1000,2400),
      font.ytickslab = 30,
      font.xtickslab = 27,
      font.main = 25,
      font.submain = 25,
      font.x = 27,
      font.y = 20,
      
)
p1
out_path
ggsave(filename = paste0(out_path, modes[t1],'_',modes[t2],'_', intervals[int==interval]$short_int_name,fb_prev,'LMM_stat.png'), width =  6, height = 5)