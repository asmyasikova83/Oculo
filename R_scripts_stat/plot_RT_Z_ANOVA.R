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
library("car")
#install.packages("effectsize")
library(effectsize)
#effectsize https://cran.r-project.org/web/packages/effectsize/vignettes/effectsize.html

sterr <- function(x) sd(x)/sqrt(length(x))

path  <- 'C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/'
out_path <- 'C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_RT_ANOVA/' #path to save pictures and tables

#### CHOOSE MODE ####

# load filtered dataset

#loading files and subjects 

df1 <- fread(paste0(path,"norma.txt"))
df2 <- fread(paste0(path,"autists.txt"))

df_large <- rbind(df1,df2)


#standardazitng fname
df_large$fname <- gsub ("_1", "", df_large$fname)
df_large$fname <- gsub ("p0", "P0", df_large$fname)
df_large$trial_type <-gsub ("_", "", df_large$trial_type) 

autists_info <- fread(paste0(path,'autism_all.txt'))
normals_info <- fread(paste0(path,'normals_all.txt'))

df_AT <- df_large[fname %in% autists_info$fname]
df_AT$group <- 'autists'
df_NT <- df_large[fname %in% normals_info$fname]
df_NT$group <- 'normals'

df_large_group <- rbind(df_AT, df_NT)

#preprocessing
df_large_group[,`:=`(percent_after=1-((sum(risk)-cumsum(risk))/(.N-1):0),
                     norisk_seq=(1:.N)-cummax(risk*(1:.N))),
               by=.(fname,block)]
df_large_group[is.na(percent_after),percent_after:=0] #fixing "percent after" to avoid NAs
df_large_group[,`:=`(trained_custom=(cumsum(norisk_seq>=sequence_threshold & percent_after>=accuracy_threshold))>0),by=.(fname,block)]
df_large_group[trained_custom-shift(trained_custom)==1,trained_custom:=0] #fixing new trained column
df_large_group[,`:=`(N_trained_trials=sum(trained_custom)),by=.(fname,block)]
df_large_group[N_trained_trials<trained_trials_threshold,trained_custom:=FALSE]

df_large_group[,`:=`(trained_block=(max(trained_custom)>0)),by=.(fname,block)]
df_large_group[,trained_block:=as.logical(trained_block)]

df_large_group[trained_block==T & trained_custom==F,
               `:=`(critical_custom=((.N:1)<=sequence_threshold)),by=.(fname,block)]

df_large_group[, index := 1:.N, by=c('fname','block')]

#2
#RT filter (for all intervals)

df_large_group <- df_large_group[blink == FALSE]
df_large_group <- df_large_group[RT>300 & RT<4000]
df_large_group[rew == 1, feedback_cur:='cur_rew'][rew == 0, feedback_cur:='cur_lose']
df_large_group[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

#Z for RT
df_large_group[,RT_Zmean:=mean(RT),by=fname]
df_large_group[,RT_Zsd:=sd(RT),by=fname]
df_large_group[,RT_Z:=(RT-RT_Zmean)/RT_Zsd]
df_large_group <- df_large_group[!is.na(RT_Z)]

df_large_group[,RT_raw:=RT]

df_large_group$trial_type4 <- df_large_group$trial_type
df_large_group[prev_risk==1 & risk==1 & next_risk==1, trial_type4:= 'repetitve_risk']
df_large_group[prev_risk==0 & risk==1 & next_risk==1, trial_type4:= 'repetitve_risk']
df_large_group[prev_risk==1 & risk==1 & next_risk==0, trial_type4:= 'repetitve_risk']
df_large_group[prev_risk==0 & risk==1 & next_risk==0, trial_type4:= 'risk']
df_large_group[prev_risk==1 & risk==0 & next_risk==1, trial_type4:= 'between_risk']
df_large_group[prev_risk==0 & risk==0 & next_risk==1, trial_type4:= 'prerisk']
df_large_group[prev_risk==1 & risk==0 & next_risk==0, trial_type4:= 'postrisk']
df_large_group[prev_risk==0 & risk==0 & next_risk==0, trial_type4:= 'norisk']


#3
#outliers
df_large_group <- df_large_group[trial_type!='final' & trial_type!='criterion' & !fname %in% c('P308','P309','P311')]

df_large_group[,RT_flag:=!blink & abs(RT_Z)<3*RT_Zsd]
df_large_group <- df_large_group[block!='6']

#trained
df_large_group <- df_large_group[trained==FALSE & learning==FALSE, train:='not_trained']
df_large_group <- df_large_group[trained==TRUE, train:='trained']

df_large_group <- df_large_group[!is.na(train)]
df_large_group$train <- as.factor(df_large_group$train)

#df_large_group <- df_large_group[trial_type4 %in% c('prerisk','risk','norisk','postrisk')]
df_large_group <- df_large_group[trial_type4 %in% c('risk','postrisk')]

#length(unique(df_large_group[group=='normals']$fname))

# create datatable with variables to analyze in data (int), 
# their labels for graps (int_name), short names for saving(short_int_name)

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

#### Analyze not_trained and trained separately ####
modes <- c('trained','not_trained','trained vs untrained', 'normals', 'autists', 'both groups')
t1 <- 3
t2 <- 6

modes[t1]
modes[t2]


interval <-  intervals$int[5]
interval
int_name <- intervals[int==interval]$int_name_graph # title for graph

df_large_group[,RT_flag:=T]
flag <- 'RT_flag'
df_large_group <- df_large_group[df_large_group[,get(flag)==T]]
unique(df_large_group$trial_type4)
length(unique(df_large_group$fname))

#normals_not_trained <- df_large_group[group == 'normals'][train == 'not_trained']
#hist(normals_not_trained$RT_Z)
#qqPlot(normals_not_trained$RT_Z)
#shapiro.test(as.numeric(normals_not_trained$RT_Z))
#normals_trained <- df_large_group[group == 'normals'][train == 'trained']
#hist(normals_trained$RT_Z)
#qqPlot(normals_trained$RT_Z)
#shapiro.test(as.numeric(normals_trained$RT_Z))
#autists_not_trained <- df_large_group[group == 'autists'][train == 'not_trained']
#hist(autists_not_trained$RT_Z)
#qqPlot(autists_not_trained$RT_Z)
#shapiro.test(as.numeric(autists_not_trained$RT_Z))
#autists_trained <- df_large_group[group == 'autists'][train == 'trained']
#hist(autists_trained$RT_Z)
#shapiro.test(as.numeric(autists_trained$RT_Z))
#qqPlot(autists_trained$RT_Z)

if (modes[t1] != 'trained vs untrained'){
  df_large_group <- df_large_group[train == modes[t1]]
  unique(df_large_group$fname)
  #m <- lmer(get(interval) ~ trial_type4*feedback_prev*group + (1|fname) + (1|block), data = df_large_group)
  a <- aov(get(interval) ~ trial_type4*feedback_prev*group, data = df_large_group)
}
if (modes[t1] == 'trained vs untrained'){
  #m <- lmer(get(interval) ~ trial_type4*feedback_prev*group*train + (1|fname) + (1|block), data = df_large_group)
  a <- aov(get(interval) ~ trial_type4*feedback_prev*group*train, data = df_large_group)
}
#

summary(a)
#anova(m)
#s <- step(m)
#m2 <- get_model(s)
#anova(m2)

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

thr <- max(df_large_group[, mean(get(interval)) + sterr(get(interval)), by=trial_type4]$V1) #
thr_min <- min(df_large_group[, mean(get(interval)) - sterr(get(interval)), by=trial_type4]$V1) 


#if ('trial_type4:group:train' %in% an$rn){
if (TRUE){
  Tuk <- NULL
  if (modes[t1] == 'trained vs untrained'){
    #Tuk <- data.table(summary(emmeans(m2, pairwise ~ trial_type4|train|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    Tuk <- data.table(summary(emmeans(a, pairwise ~ trial_type4|train|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
   }
  if (modes[t1] != 'trained vs untrained'){
    #Tuk <- data.table(summary(emmeans(m2, pairwise ~ trial_type4|train|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    Tuk <- data.table(summary(emmeans(a, pairwise ~ trial_type4|train|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
  }
}

#if ('trial_type4:feedback_prev:train' %in% an$rn){
if (FALSE){
  #by feedback
  Tuk <- NULL
  if (modes[t1] == 'trained vs untrained'){
    Tuk <- data.table(summary(emmeans(m2, pairwise ~ feedback_prev|trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    #Tuk <- data.table(summary(emmeans(m2, pairwise ~ train|trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
  }
  if (modes[t1] != 'trained vs untrained'){
    #Tuk <- data.table(summary(emmeans(m2, pairwise ~ feedback_prev|train|trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    #Tuk <- data.table(summary(emmeans(m2, pairwise ~ feedback_prev|trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    #Tuk <- data.table(summary(emmeans(m, pairwise ~ feedback_prev|trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    Tuk <- data.table(summary(emmeans(m2, pairwise ~ train|trial_type4|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
    
    }
}

Tuk

Tuk <- Tuk[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk <- Tuk[p.value<0.1, p_significant:=format(p.value, digits = 3)]
  
n <- Tuk[!is.na(p_significant), .N]
thr <- max(df_large_group[, mean(get(interval)) + sterr(get(interval)), by=trial_type4]$V1) #

if (n>1){
  Tuk <- Tuk[!is.na(p_significant), y.position := seq((thr+0.01), (thr+0.3), 0.29/(n-1))]
} else {
  Tuk <- Tuk[!is.na(p_significant), y.position := thr+0.36]
}
  
Tuk[p.value<0.001, stars:='***']
Tuk[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk[p.value<0.05 & p.value>0.01 , stars:='*']
#Tuk[p.value>0.05 & p.value<0.1 , stars:='#']
Tuk[,y.position:=y.position+0.16]
Tuk

choice_type = TRUE
choice_type_group = FALSE
feedback_prev = FALSE
feedback_prev_combined = FALSE


# plot Tukey and save table

                          
if (choice_type == TRUE){
   if (modes[t1] == 'trained vs untrained'){
    #by choice type by train by group
    modes[t1] = 'not_trained'
    modes[t2] = 'autists'
   }
    dff_trained <- df_large_group[train == as.character(modes[t1])][group == as.character(modes[t2])]
    Tuk1 <- Tuk[train == as.character(modes[t1])][group == as.character(modes[t2])]
    Tuk1
    #title <- 'Испытуемые с РАС'
    #title <- paste0(as.character(modes[t1]),' ',as.character(modes[t2]),' ','choice type')
    title <- paste0(as.character(modes[t1]),' ',as.character(modes[t2]),' ','TukANOVA')
    #plot
    p <-ggline(dff_trained, 'trial_type4', interval,
               add = c("mean_se"),
               #order = c("norisk","prerisk","risk", "postrisk"),
               order = c("risk", "postrisk"),
               ylab = int_name, xlab = "Choice type",
               #ylab = int_name, xlab = "Тип выбора",
               size = 1.5, 
               point.size = 1.8,
               font.label = list(size = 16, color = "black"))+
      #scale_x_discrete(name=title, labels = c("HP", "pre-LP",'LP','post-LP'))
      scale_x_discrete(name=title, labels = c("LP", 'post-LP'))
    p <- p + stat_pvalue_manual(Tuk1, label = 'stars', size = 12,tip.length = 0.001,bracket.size = 1.2)
   
}
p
if (choice_type_group == TRUE){
  if (modes[t1] == 'trained vs untrained'){
    #by choice type by train both groups
    modes[t2] = 'autists'
    dff_trained <- df_large_group[group == as.character(modes[t2])]
    
    Tuk1 <- Tuk[group == as.character(modes[t2])]
    title <- paste0(as.character(modes[t1]),as.character(modes[t2]),'choice type')
    #plot
    
    #signif <- Tuk
    
    #plot settings
    sequence <-data.table(trial_type4=c("norisk","prerisk","risk", "postrisk"),number=c(1,2,3,4)) 
    
    y_values_rew <- dff_trained[train == 'trained',
                                mean(get('RT_Z')),by='trial_type4']
    setnames(y_values_rew,'V1','y_values_rew')
    
    y_values_lose <- dff_trained[train  == 'not_trained',
                                 mean(get('RT_Z')),by='trial_type4']
    setnames(y_values_lose,'V1','y_values_lose')
    
    y_values <- merge(y_values_lose,y_values_rew,by='trial_type4')
    y_values <- merge(y_values,sequence,by='trial_type4')
    y_values[,y_max:=max(y_values_lose,y_values_rew),by=trial_type4]
    y_values <- merge(y_values,signif,by='trial_type4')
    
    y_values <- y_values[group == as.character(modes[t2])]
    
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
      geom_signif(y_position=c(y_values$y_max +0.35),
                  xmin=c(y_values$number-0.075), xmax=c(y_values$number+0.075),
                  annotation=c(Tuk1$stars), 
                  tip_length=0.001,textsize = 7,vjust = 0.4)
    p
  }
}

if (feedback_prev == TRUE){
  modes[t1]
  if (modes[t1] != 'trained vs untrained'){
    #by feedback in choice type by train
    modes[t1]
    modes[t2]
    
    dff_trained <- df_large_group[train == as.character(modes[t1])][group == as.character(modes[t2])]

    #Tuk1 <- Tuk[train == as.character(modes[t1])][group == as.character(modes[t2])]
    #title <- paste0(as.character(modes[t1]), as.character(modes[t2]),'by feedback_prev')
    
    #if in trained/untrained separately
    Tuk1 <- Tuk[group == as.character(modes[t2])]
    Tuk1
    title <- paste0('In',as.character(modes[t1]),'separ', as.character(modes[t2]),'by feedback_prev')
    #plot
    #signif <- Tuk
    
    #plot settings
    sequence <-data.table(trial_type4=c("norisk","prerisk","risk", "postrisk"),number=c(1,2,3,4)) 
    
    y_values_rew <- dff_trained[feedback_prev == 'positive',
                                mean(get('RT_Z')),by='trial_type4']
    setnames(y_values_rew,'V1','y_values_rew')
    
    y_values_lose <- dff_trained[feedback_prev  == 'negative',
                                 mean(get('RT_Z')),by='trial_type4']
    setnames(y_values_lose,'V1','y_values_lose')
    
    y_values <- merge(y_values_lose,y_values_rew,by='trial_type4')
    y_values <- merge(y_values,sequence,by='trial_type4')
    y_values[,y_max:=max(y_values_lose,y_values_rew),by=trial_type4]
    y_values <- merge(y_values,signif,by='trial_type4')
    
    #y_values <- y_values[train == as.character(modes[t1])][group == as.character(modes[t2])]
    y_values <- y_values[group == as.character(modes[t2])]
    y_values
    
    p <-ggline(dff_trained, 'trial_type4', interval,
               color = 'feedback_prev',
               add = c("mean_se"),
               #palette = c('green','red'),
               order = c("norisk","prerisk","risk", "postrisk"),
               ylab = int_name, xlab = "Choice type",
               size = 1.5, 
               point.size = 1.8,
               font.label = list(size = 16, color = "black"))+
      scale_x_discrete(name=title, labels = c("HP", "pre-LP",'LP','post-LP'))+
      geom_signif(y_position=c(y_values$y_max +0.35),
                  xmin=c(y_values$number-0.075), xmax=c(y_values$number+0.075),
                  annotation=c(Tuk1$stars), 
                  tip_length=0.001,textsize = 7,vjust = 0.4)
    p
  }
}
if (feedback_prev_combined == TRUE){
  if (modes[t1] == 'trained vs untrained'){
    #by feedback in choice type train combined
    modes[t1]
    modes[t2]
    dff_trained <- df_large_group[group == as.character(modes[t2])][]
    Tuk1 <- Tuk[group == as.character(modes[t2])]
    title <- paste0(as.character( 'Combined learning'), as.character(modes[t2]),'by feedback_prev')
    #plot
    signif <- Tuk
    
    #plot settings
    sequence <-data.table(trial_type4=c("norisk","prerisk","risk", "postrisk"),number=c(1,2,3,4)) 
    
    y_values_rew <- dff_trained[feedback_prev == 'positive',
                                mean(get('RT_Z')),by='trial_type4']
    setnames(y_values_rew,'V1','y_values_rew')
    
    y_values_lose <- dff_trained[feedback_prev  == 'negative',
                                 mean(get('RT_Z')),by='trial_type4']
    setnames(y_values_lose,'V1','y_values_lose')
    
    y_values <- merge(y_values_lose,y_values_rew,by='trial_type4')
    y_values <- merge(y_values,sequence,by='trial_type4')
    y_values[,y_max:=max(y_values_lose,y_values_rew),by=trial_type4]
    y_values <- merge(y_values,signif,by='trial_type4')
    
    y_values <- y_values[group == as.character(modes[t2])]
    
    y_values
    
    p <-ggline(dff_trained, 'trial_type4', interval,
               color = 'feedback_prev',
               add = c("mean_se"),
               #palette = c('green','red'),
               order = c("norisk","prerisk","risk", "postrisk"),
               ylab = int_name, xlab = "Choice type",
               size = 1.5, 
               point.size = 1.8,
               font.label = list(size = 16, color = "black"))+
      scale_x_discrete(name=title, labels = c("HP", "pre-LP",'LP','post-LP'))+
      geom_signif(y_position=c(y_values$y_max +0.35),
                  xmin=c(y_values$number-0.075), xmax=c(y_values$number+0.075),
                  annotation=c(Tuk1$stars), 
                  tip_length=0.001,textsize = 7,vjust = 0.4)
  }
}
p1 <- ggpar(p,
      ylim = c(-0.4,0.9),
      font.ytickslab = 30,
      font.xtickslab = 27,
      font.main = 25,
      font.submain = 25,
      font.x = 27,
      font.y = 20,
      
)
p1
out_path
ggsave(filename = paste0(out_path, modes[t1],'_',modes[t2],'_', intervals[int==interval]$short_int_name,'_Tukey_ANOVA','.png'), width =  6, height = 5)