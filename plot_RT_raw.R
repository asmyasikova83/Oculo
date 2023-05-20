# LMM analysis separately on trained and not_trained or both.
# Choose mode to operate the subset you want  

#rm(list = ls())

accuracy_threshold <- 0.65
sequence_threshold <- 4
trained_trials_threshold <- 0

library(data.table)
library(ggplot2)

path  <- 'C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/'
out_path <- 'C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_RT_raw/' #path to save pictures and tables

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

autists_info <- fread(paste0(path,'autism.txt'))
normals_info <- fread(paste0(path,'normals.txt'))

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
df_large_group <- df_large_group[trial_type!='final' & trial_type!='criterion' & !fname %in% c('P308','P309','P311')]

#rename

df_large_group[rew == 1, feedback_cur:='cur_rew'][rew == 0, feedback_cur:='cur_lose']
df_large_group[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

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

df_large_group[,RT_raw:=RT]
df_large_group[,RT_raw_sd:=sd(RT_raw),by=fname]
df_large_group[,RT_flag:=!blink & abs(RT_raw)<3*RT_raw_sd]

#return
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

df_large_group <- df_large_group[trial_type4 %in% c('prerisk','risk','norisk','postrisk')]

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
t1 <- 3
t2 <- 4

modes[t1]
modes[t2]

df_large_group[,RT_flag:=T]
flag <- 'RT_flag'
df_large_group <- df_large_group[df_large_group[,get(flag)==T]]

interval <-  intervals$int[5]
interval
int_name <- intervals[int==interval]$int_name_graph # title for graph

# save anova and ranova

choice_type = FALSE
choice_type_combined = FALSE
choice_type_group = FALSE
feedback_prev = FALSE
feedback_prev_combined = TRUE

# plot and save table
                          
if (choice_type == TRUE){
   if (modes[t1] != 'trained vs untrained'){
    #by choice type by train by group
    dff_trained <- df_large_group[train == as.character(modes[t1])][group == as.character(modes[t2])]
    title <- paste0(as.character(modes[t1]),' ',as.character(modes[t2]),' ','choice type')
    #plot
    p <-ggline(dff_trained, 'trial_type4', interval,
               add = c("mean_se"),
               order = c("norisk","prerisk","risk", "postrisk"),
               ylab = int_name, xlab = "Choice type",
               size = 1.5, 
               point.size = 1.8,
               font.label = list(size = 16, color = "black"))+
      scale_x_discrete(name=title, labels = c("HP", "pre-LP",'LP','post-LP'))
    p
    fb_prev = ''
   }
}

if (choice_type_combined == TRUE){
  if (modes[t1] == 'trained vs untrained'){
    #by choice type by train both groups
    dff_trained <- df_large_group[group == as.character(modes[t2])]
    title <- paste0(as.character(modes[t1]),as.character(modes[t2]),'choice type')
    #plot
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
      scale_x_discrete(name=title, labels = c("HP", "pre-LP",'LP','post-LP'))
    p
    fb_prev = ''
  }
}

if (choice_type_group == TRUE){
  if (modes[t1] != 'trained vs untrained'){
    #by choice type by train both groups
    dff_trained <- df_large_group[train == as.character(modes[t1])]
    title <- paste0(as.character(modes[t1]),as.character(modes[t2]),'choice type')
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
      scale_x_discrete(name=title, labels = c("HP", "pre-LP",'LP','post-LP'))
    p
    fb_prev = ''
  }
}

if (feedback_prev == TRUE){
  modes[t1]
  if (modes[t1] != 'trained vs untrained'){
    #by feedback in choice type by train
    dff_trained <- df_large_group[train == as.character(modes[t1])][group == as.character(modes[t2])]
    title <- paste0(as.character(modes[t1]), as.character(modes[t2]),'by feedback_prev')
    p <-ggline(dff_trained, 'trial_type4', interval,
               color = 'feedback_prev',
               add = c("mean_se"),
               #palette = c('green','red'),
               order = c("norisk","prerisk","risk", "postrisk"),
               ylab = int_name, xlab = "Choice type",
               size = 1.5, 
               point.size = 1.8,
               font.label = list(size = 16, color = "black"))+
      scale_x_discrete(name=title, labels = c("HP", "pre-LP",'LP','post-LP'))
    p
    fb_prev = '_feedback_prev'
  }
}
if (feedback_prev_combined == TRUE){
  if (modes[t1] == 'trained vs untrained'){
    #by feedback in choice type train combined
    modes[t1]
    modes[t2]
    dff_trained <- df_large_group[group == as.character(modes[t2])]
    unique(dff_trained$train)
    unique(dff_trained$group)
    title <- paste0(as.character( 'Combined learning'), as.character(modes[t2]),'by feedback_prev')
    #plot
    
    p <-ggline(dff_trained, 'trial_type4', interval,
               color = 'feedback_prev',
               add = c("mean_se"),
               #palette = c('green','red'),
               order = c("norisk","prerisk","risk", "postrisk"),
               ylab = int_name, xlab = "Choice type",
               size = 1.5, 
               point.size = 1.8,
               font.label = list(size = 16, color = "black"))+
      scale_x_discrete(name=title, labels = c("HP", "pre-LP",'LP','post-LP'))
    fb_prev = '_feedback_prev_combined_train'
  }
}
ggpar(p,
      ylim = c(1000,2100),
      font.ytickslab = 30,
      font.xtickslab = 27,
      font.main = 25,
      font.submain = 25,
      font.x = 27,
      font.y = 20,
      
)
out_path
ggsave(filename = paste0(out_path, modes[t1],'_',modes[t2],'_', intervals[int==interval]$short_int_name,fb_prev,'.png'), width =  6, height = 5)