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
library(gridExtra)

sterr <- function(x) sd(x)/sqrt(length(x))

#loading files and subjects 
#rm(list = ls())
path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/" 
  
df1 <- fread(paste0(path,"norma.txt"))
df2 <- fread(paste0(path,"autists.txt"))

df_large <- rbind(df1,df2)

#standardazitng fname
df_large$fname <- gsub ("_1", "", df_large$fname)
df_large$fname <- gsub ("p0", "P0", df_large$fname)
df_large$trial_type <-gsub ("_", "", df_large$trial_type) 

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

#preprocessing
df_large_group[, index := 1:.N, by=c('fname','block')]

#2
#RT filter (for all intervals)

#df_large_group <- df_large_group[blink == FALSE]
df_large_group <- df_large_group[RT>300 & RT<4000]
#block 6 has a specific probability
df_large_group <- df_large_group[block!='6']

#rename
df_large_group[rew == 1, feedback_cur:='cur_rew'][rew == 0, feedback_cur:='cur_lose']
df_large_group[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

df_large_group[prev_risk==0 & risk==1 & next_risk==0, trial_type:= 'risk']
df_large_group[prev_risk==0 & risk==0 & next_risk==0, trial_type:= 'norisk']

#Z for RT
#df_large_group[,RT_Zmean:=mean(RT),by=fname]
#df_large_group[,RT_Zsd:=sd(RT),by=fname]
#df_large_group[,RT_Z:=(RT-RT_Zmean)/RT_Zsd]
#df_large_group <- df_large_group[!is.na(RT_Z)]
#df_large_group[, RT_flag:=abs(RT_Z)<3*RT_Zsd]
#3
#outliers
df_large_group[,RT_raw:=RT,by=fname]
df_large_group[,RT_raw_sd:=sd(RT_raw),by=fname]
df_large_group[,RT_flag:=abs(RT_raw)<3*RT_raw_sd]
flag <- 'RT_flag'
df_large_group <- df_large_group[df_large_group[,get(flag)==T]]

df_large_group[,RT_log_raw:=log10(RT)]

#Kseniya's conditions
df_large_group <- df_large_group[trained==FALSE, train:='not_trained']
df_large_group <- df_large_group[trained==TRUE, train:='trained']

df_large_group <- df_large_group[!is.na(train)]
df_large_group$train <- as.factor(df_large_group$train)

df_large_group <- df_large_group[trial_type %in% c('norisk','risk')]

length(unique(df_large_group[group == 'autists'][train == 'not_trained']$fname))
unique(df_large_group[group == 'normals'][train == 'not_trained']$fname)

# create datatable with variables to analyze in data

interval <- 'RT_log_raw'

##################################################

modes2 <- 'norisk'
modes3 <- 'normals'

df_large_group <- df_large_group[group == modes3][trial_type == modes2]


q<- df_large_group %>%
  group_by(fname,group, train)%>%summarise(mean_RT=mean(RT_log_raw))

#you can also analyze RT Z
#q<- df_large_group %>%
#  group_by(fname)%>%summarise(mean_RT_log_raw=mean(RT_log_raw), 
#                                     mean_RT_Z=mean(RT_Z))

q <- as.data.table(unique(q))
q_trained <- q[train == 'trained']
q_not_trained <- q[train == 'not_trained']
#q_trained <- q_trained[!(fname == 'P314' | fname == 'P326'| fname == 'P328')]
q_trained 
q_trained$diff <- abs(q_trained$mean_RT - q_not_trained$mean_RT)


######################################Traits#################################################
question <- read_excel(paste0(path, 'Questionnares.xls'))

#replace blank spaces in colnames
colClean1 <- function(question){ colnames(question) <- gsub(" ", "_", colnames(question)); question }
question <- colClean1(question)
colClean2 <- function(question){ colnames(question) <- gsub("...1", "fname", colnames(question)); question }
question <- colClean2(question) 
colnames(question)


question <- as.data.table(question)
question[, fname:=stri_extract_first_regex(question$fname,"[0-9]+")]

subj_both <- unique(df_large_group$fname)
subj_both <- as.factor(gsub("P", "", subj_both))
question <- question[fname %in% subj_both]

q_trained$fname <- gsub ("P", "", q_trained$fname)
#q$fname <- gsub ("P", "", q$fname)
###############################################################################################
combined <- merge(q_trained, question, by = 'fname')
#combined <- merge(q, question, by = 'fname')
cor.test(combined$diff, combined$"Intolerance_for_uncertanty")

title <- paste0('HP:trained_minus_not_trained', '_', as.character(modes3), '_IU')

g <- ggscatter(combined, x = "Intolerance_for_uncertanty", y = "diff",
               color = "black", shape = 20, size = 5, 
               add = "reg.line", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "pearson",
               cor.coeff.args = list(label.x = 50,label.y = 0.17, label.sep = "\n"),
               xlab = title, ylab = "RT log: trained_vs_not_trained")
g

p1 <- ggpar(g,
            ylim = c(-0.15, 0.25),
            font.ytickslab = 30,
            font.xtickslab = 27,
            font.main = 20,
            font.submain = 25,
            font.x = 27,
            font.y = 20,
            
)
p1

pic_name <- paste0('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_articles/article_at_nt/RT_HP_trained_vs_not_trained', '_', modes3,'.png' )
ggsave(pic_name,p1,width =  9, height = 5)
