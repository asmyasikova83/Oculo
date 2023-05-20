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
#library(gridExtra)
#install.packages("matrixStats")
#library(matrixStats)
library(dplyr)

sterr <- function(x) sd(x)/sqrt(length(x))

path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"
out_path <- paste0(path,'pupil_Z_feedback_cur/inside_fb/')

#"tables/resp_18042022.txt"
#"tables/autists.txt"
df1 <- fread(paste0(path,"norma.txt"))
df2 <- fread(paste0(path,"autists.txt"))
df_large <- rbind(df1,df2)

df_large[,`:=`(percent_after=1-((sum(risk)-cumsum(risk))/(.N-1):0),
               norisk_seq=(1:.N)-cummax(risk*(1:.N))),
         by=.(fname,block)]
df_large[is.na(percent_after),percent_after:=0] #fixing "percent after" to avoid NAs
df_large[,`:=`(trained_custom=(cumsum(norisk_seq>=sequence_threshold & percent_after>=accuracy_threshold))>0),by=.(fname,block)]
#df_large[,`:=`(trained_custom=(sum(!risk)/.N)>=accuracy_threshold),by=.(fname,block)]
df_large[trained_custom-shift(trained_custom)==1,trained_custom:=0] #fixing new trained column
df_large[,`:=`(N_trained_trials=sum(trained_custom)),by=.(fname,block)]
df_large[N_trained_trials<trained_trials_threshold,trained_custom:=FALSE]

df_large[,`:=`(trained_block=(max(trained_custom)>0)),by=.(fname,block)]
df_large[,trained_block:=as.logical(trained_block)]

df_large[trained_block==T & trained_custom==F,
         `:=`(critical_custom=((.N:1)<=sequence_threshold)),by=.(fname,block)]


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

length(unique(df_large_group$fname))


length(unique(df_large_group[group == 'autists']$fname))
length(unique(df_large_group[group == 'normals']$fname))
unique(df_large_group[group == 'normals']$fname)
unique(df_large_group[group == 'autists']$fname)

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

#for RT
df_large_group <- df_large_group[block != 6]
length(unique(df_large_group[group == 'autists']$fname))
length(unique(df_large_group[group == 'normals']$fname))

################################LMM####################################################

df_large_group <- df_large_group[trial_type4 %in% c('norisk','prerisk','risk','postrisk')]
#df_large_group <- df_large_group[trial_type4 %in% c('norisk','risk')]

modes <- c('trained','not_trained','trained vs untrained', 'normals', 'autists', 'both groups')
t1 <- 3
t2 <-6

modes[t1]
modes[t2]

#df_large_group <- df_large_group[group  == as.character(modes[t2])]
unique(df_large_group$trained_edited)
unique(df_large_group$group)
unique(df_large_group$feedback_cur)

m <- lmer(resp_1001_2200 ~ trial_type4*feedback_cur*group*train + (1|fname)+ (1|block),df_large_group)
#m <- lmer(resp_1001_2200 ~ trial_type4*feedback_prev*trained_edited + (1|fname)+ (1|block),df_large_group)
#m <- lmer(RT_Z ~ trial_type4*feedback_prev*trained_edited + (1|fname)+ (1|block),df_large_group)
#m <- lmer(resp_1001_2200 ~ trial_type4*feedback_prev*trained_edited + (1|fname)+ (1|block),df_large_group)
unique(df_large_group$group)
print(anova(m))
s <- step(m)
m2 <- get_model(s)
print(anova(m2))

#Tuk <- data.table(summary(emmeans(m2, pairwise ~ trained_edited | group | trial_type4, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)

#Tuk <- data.table(summary(emmeans(m, pairwise ~ trained_edited | feedback_prev| trial_type4, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)


choice_type = TRUE
choice_type_group = FALSE
choice_type_combined = FALSE
feedback_prev = FALSE
feedback_prev_combined = FALSE


if (choice_type == TRUE){
    Tuk <-  data.table(summary(emmeans(m2, pairwise ~ trial_type4 | group | train , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    Tuk
    modes[t1] = 'trained'
    modes[t2] = 'normals'
    Tuk1 <- Tuk[train == as.character(modes[t1])][group == as.character(modes[t2])]
    Tuk1
    dff_trained <- df_large_group[train == as.character(modes[t1])][group == as.character(modes[t2])]
    length(unique(dff_trained$fname))
    title <- paste0(as.character(modes[t1]),' ',as.character(modes[t2]),' ','choice type')
  }
if (choice_type_combined == TRUE){
    #trained vs untrained (Vladimir's)
    Tuk <- data.table(summary(emmeans(m2, pairwise ~ train | group | trial_type4  , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)

    modes[t2] = 'autists'
    Tuk1 <- Tuk[group == as.character(modes[t2])]
    Tuk1
    dff_trained <- df_large_group[group == as.character(modes[t2])]
    title <- paste0(as.character('Combined learning'),as.character(modes[t2]),' ','choice type_combined_fb_cur')
}
if (feedback_prev == TRUE){
  modes[t1] = 'trained'
  modes[t2] = 'autists'
  df_large_group <- df_large_group[train == as.character(modes[t1])][group == as.character(modes[t2])]
  m <- lmer(resp_1001_2200 ~ trial_type4*feedback_cur + (1|fname)+ (1|block),df_large_group)
  print(anova(m))
  s<-step(m)
  m2 <- get_model(s)
  print(anova(m2))
  Tuk <- data.table(summary(emmeans(m2, pairwise ~ trial_type4|feedback_cur, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
  Tuk
  #fb <- 'positive'
  fb <- 'negative'
  Tuk1 <- Tuk[feedback_cur == fb]
  Tuk1
  dff_trained <- df_large_group[feedback_cur == as.character(fb)]
  unique(dff_trained$group)
  unique(dff_trained$feedback_cur)
  title <- paste0(as.character(modes[t1]),as.character(modes[t2]),'',fb)
}

if (feedback_prev_combined == TRUE){
  modes[t1] = 'trained'
  modes[t2] = 'autists'
  #df_large_group <- df_large_group[group == as.character(modes[t2])]
  df_large_group <- df_large_group[train == as.character(modes[t1])][group == as.character(modes[t2])]

  m <- lmer(resp_1001_2200 ~ trial_type4*feedback_cur + (1|fname)+ (1|block),df_large_group)
  print(anova(m))
  s<-step(m)
  m2 <- get_model(s)
  print(anova(m2))
  Tuk1 <- data.table(summary(emmeans(m, pairwise ~ feedback_cur|trial_type4, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
  dff_trained <- df_large_group
  unique(dff_trained$train)
  unique(dff_trained$group)
  unique(dff_trained$feedback_cur)
  title <- paste0('Curfb',as.character(modes[t1]), as.character(modes[t2]))
}


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
Tuk1[,y.position:=y.position+0.1]
Tuk1

signif <- Tuk1
signif

# plot and save table

if (choice_type == TRUE){
  #by choice type by train by group
  #plot
  p <-ggline(dff_trained, 'trial_type4', interval,
             add = c("mean_se"),
             order = c("norisk","prerisk","risk", "postrisk"),
             #order = c("norisk","risk"),
             ylab = int_name, xlab = "Choice type",
             size = 1.5, 
             point.size = 1.8,
             font.label = list(size = 16, color = "black"))+
    scale_x_discrete(name=title, labels = c("HP", "pre-LP",'LP','post-LP'))+
    #scale_x_discrete(name=title, labels = c("HP", 'LP'))+
    geom_hline(yintercept=-0.0, linetype='dashed', col = 'black', size = 1)
    #scale_x_discrete(name=title, labels = c('LP','post-LP'))
  p <- p + stat_pvalue_manual(Tuk1, label = 'stars', size = 12,tip.length = 0.01,bracket.size = 1.2)
  p
  fb_prev = 'fb_cur_ch_type'
}

if (choice_type_combined == TRUE){
  #combined learning in a group
  
  #plot settings
  sequence <-data.table(trial_type4=c("norisk","prerisk","risk", "postrisk"),number=c(1,2,3,4)) 
  
  y_values_rew <- dff_trained[train == 'trained',
                              mean(get(interval)),by='trial_type4']
  setnames(y_values_rew,'V1','y_values_rew')
  
  y_values_lose <- dff_trained[train  == 'not_trained',
                               mean(get(interval)),by='trial_type4']
  setnames(y_values_lose,'V1','y_values_lose')
  
  y_values <- merge(y_values_lose,y_values_rew,by='trial_type4')
  y_values <- merge(y_values,sequence,by='trial_type4')
  y_values[,y_max:=max(y_values_lose,y_values_rew),by=trial_type4]
  y_values <- merge(y_values,signif,by='trial_type4')
  
  y_values <- y_values[group == as.character(modes[t2])]
  
  #plot
  unique(dff_trained$train)
  p <-ggline(dff_trained, 'trial_type4', interval,
             color = 'train',
             add = c("mean_se"),
             #palette = c('magenta','orange'),
             palette = c('green','red'),
             order = c("norisk","prerisk","risk", "postrisk"),
             ylab = int_name, xlab = "Choice type",
             size = 1.5, 
             point.size = 1.8,
             font.label = list(size = 16, color = "black"))+
    scale_x_discrete(name=title, labels = c("HP", "pre-LP",'LP','post-LP'))+
    geom_hline(yintercept=-0.0, linetype='dashed', col = 'black', size = 1) +
    geom_signif(y_position=c(y_values$y_max),
                xmin=c(y_values$number-0.075), xmax=c(y_values$number+0.075),
                annotation=c(Tuk1$stars), 
                tip_length=0.001,textsize = 7,vjust = 0.4)
  p
  fb_prev = ''
}

if (choice_type_group == TRUE){
  if (modes[t1] != 'trained vs untrained'){
    #by choice type by train both groups
    dff_trained <- df_large_group[train == as.character(modes[t1])]
    #plot
    p <-ggline(dff_trained, 'trial_type4', interval,
               color = 'group',
               add = c("mean_se"),
               palette = c('magenta','orange'),
               #palette = c('green','red'),
               order = c("norisk","prerisk","risk", "postrisk"),
               ylab = int_name, xlab = "Choice type",
               size = 1.5, 
               point.size = 1.8,
               font.label = list(size = 16, color = "black"))+
      scale_x_discrete(name=title, labels = c("HP", "pre-LP",'LP','post-LP'))+
      geom_hline(yintercept=-0.0, linetype='dashed', col = 'black', size = 1)
      p
    fb_prev = 'choice type'
  }
}

if (feedback_prev == TRUE){
  #by feedback in choice type by train
  #plot settings
  Tuk1
  p <-ggline(dff_trained, 'trial_type4', interval,
             #color = 'feedback_prev',
             color = c("red"),
             #color = c("blue"),
             #color = c("lightblue"),
             #color = c("salmon"),
             add = c("mean_se"),
             title = paste('Cur feedback_', fb),
             #palette = c('green','red'),
             order = c("norisk","prerisk","risk", "postrisk"),
             ylab = int_name, xlab = "(Kseniya) Choice type",
             size = 1.5, 
             point.size = 1.8,
             font.label = list(size = 16, color = "black"))+
    scale_x_discrete(name=title, labels = c("HP", "pre-LP", "LP", "post-LP"))+
    geom_hline(yintercept=-0.0, linetype='dashed', col = 'black', size = 1)
  p <- p + stat_pvalue_manual(Tuk1, label = 'stars', size = 12,tip.length = 0.01,bracket.size = 1.2)
  p
    
    #geom_signif(y_position=c(y_values$y_max+0.1),
    #            xmin=c(y_values$number), xmax=c(y_values$number),
    #            annotation=c(Tuk1$stars), 
    #            tip_length=0.001,textsize = 7,vjust = 0.4)
  p
  #fb_prev = '_feedback_prev'
  fb_prev = fb
}

if (feedback_prev_combined == TRUE){
  #TODO for norm fb vs autists fb
    #by feedback in choice type train combined
    sequence <-data.table(trial_type4=c("norisk","prerisk","risk", "postrisk"),number=c(1,2,3,4)) 
    #sequence <-data.table(trial_type4=c("norisk","risk"),number=c(1,2)) 
    y_values_rew <- df_large_group[feedback_cur == 'positive',
    #y_values_rew <- df_large_group[train == 'trained',
                                mean(resp_1001_2200),by='trial_type4']
    setnames(y_values_rew,'V1','y_values_rew')
    y_values_rew
    y_values_lose <- df_large_group[feedback_cur == 'negative',
    #y_values_rew <- df_large_group[train == 'not_trained',
                                 mean(resp_1001_2200),by='trial_type4']
    setnames(y_values_lose,'V1','y_values_lose')
    y_values_lose
    y_values <- merge(y_values_lose,y_values_rew,by='trial_type4')
    y_values <- merge(y_values,sequence,by='trial_type4')
    y_values[,y_max:=max(y_values_lose,y_values_rew),by=trial_type4]
    y_values <- merge(y_values,signif,by='trial_type4')
    y_values
    #y_values <- y_values[train == as.character(modes[t1])][group == as.character(modes[t2])]
    #y_values <- y_values[group == as.character(modes[t2])]
    
    Tuk1
    #Tuk1 <- Tuk1[,y.position:=y.position+0.26]
    #plot
    unique(dff_trained$group)
    unique(dff_trained$train)
    p <-ggline(dff_trained, 'trial_type4', interval,
               color = 'feedback_cur',
               #color = 'trained_edited',
               add = c("mean_se"),
               #palette = c('grey','black'),
               order = c("norisk","prerisk","risk", "postrisk"),
               #order = c("norisk","risk"),
               ylab = int_name, xlab = "Choice type",
               size = 1.5, 
               point.size = 1.8,
               font.label = list(size = 16, color = "black"))+
      scale_x_discrete(name=title, labels = c("HP", "pre-LP",'LP','post-LP'))+
      #scale_x_discrete(name=title, labels = c("HP", 'LP'))+
      geom_hline(yintercept=-0.0, linetype='dashed', col = 'black', size = 1) +
      geom_signif(y_position=c(y_values$y_max+0.13),
                  xmin=c(y_values$number), xmax=c(y_values$number),
                  annotation=c(Tuk1$stars), 
                  tip_length=0.001,textsize = 7,vjust = 0.4)
    #fb_prev = paste0('_fb_',as.character(fb),'_inside_train')
    fb_prev = '_fb_cur'
}

p1 <- ggpar(p,
            ylim = c(-0.6, 0.6),
            font.ytickslab = 30,
            font.xtickslab = 27,
            font.main = 25,
            font.submain = 25,
            font.x = 27,
            font.y = 20,
            
)
p1
out_path
ggsave(filename = paste0(out_path, modes[t1],'_',modes[t2],'_', intervals[int==interval]$short_int_name,fb_prev,'Pupil.png'), width =  6, height = 5)