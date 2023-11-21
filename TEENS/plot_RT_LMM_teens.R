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
library(stringi)
library(readr)

path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/TEENS/"
out_path <- paste0(path,'pics/')

#eyetracker
rt_teens <- read.csv(paste0(path,"resp_merged_teens_big.csv"))
rt_teens <- as.data.table(rt_teens)

#rt <- rt[grep('P[0-9]',rt$fname)]
#rt_teens<- filter(rt_teens, fname %in% c("P701", "P702", "P705", "P706", "P707", "P708", "P709", "P710", "P711", "P712",
#                                   "P713", "P714","P715", "P716",  "P718",  "P721", "P722",  "P724",  "P725", "P723",
#                                   "P726", "P727", "P728", "P729", "P730"))
length(unique(rt_teens$fname))
rt_teens$group <- 'teens'
rt_adults <- fread(paste0(path,"norma.txt"))

rt_adults <- as.data.table(rt_adults[,!c(21:25)])
adults <- c("P001_1", "P002_1", "P003_1", "P004_1", "P005_1",
            "P006_1", "P007_1", "P008_1", "P009_1", "P065_1",
            "P011_1", "P012_1", "P043_1", "P044_1", "P045_1",
            "P046_1", "P047_1", "P048_1", "P049_1", "P050_1" )
rt_adults<- rt_adults[fname %in% adults]
rt_adults <- rt_adults[block!=6]
rt_adults$group <- 'adults'
length(unique(rt_adults$fname))

rt <- rbind(rt_teens, rt_adults)
rt[, index := 1:.N, by=c('fname','block')]

#fb prev
rt[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

#2
#RT filter (for all intervals)
df_large_group <- as.data.table(rt)
df_large_group <- df_large_group[RT>300 & RT<4000]

df_large_group[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

#3
#outliers
#df_large_group[,RT_raw:=RT,by=fname]
#df_large_group[,RT_raw_sd:=sd(RT_raw),by=fname]
#df_large_group[,RT_flag:=abs(RT_raw)<3*RT_raw_sd]
#flag <- 'RT_flag'
#df_large_group <- df_large_group[df_large_group[,get(flag)==T]]
df_large_group[,RT_log_raw:=log10(RT)]
df_large_group <- df_large_group[!is.na(RT_log_raw)]

#settings for train untrain
#trained

#interval <-  'RT_raw'
#int_name <- 'RT raw, ms'

interval <-  'RT_log_raw'
int_name <- 'log10(RT)' # title for graph

#repetitive risk
df_large_group[prev_risk==1 & risk==1, trial_type2:= 'rLP']
df_large_group[prev_risk==0 & risk==0, trial_type2:= 'rHP']

unique(df_large_group$group)
length(unique(df_large_group$fname))

df_large_group <- df_large_group[trial_type2 %in% c('rLP', 'rHP')]
unique(df_large_group$trial_type2)
######################################demographics#################################################

interval
m <- lmer(get(interval) ~ trial_type2*group + (1|fname) + (1|block), data = df_large_group)
#m <- lmer(get(interval) ~ trial_type*train*feedback_prev*group + (1|fname) + (1|block), data = df_large_group)
#a <- aov(get(interval) ~ trial_type4*feedback_prev*group*train, data = df_large_group)

#balance the num of trained and not_trained choices
#(0+train|fname)

#summary(a)
anova(m)
s <- step(m)
m2 <- get_model(s)
anova(m2)

##### Tukey: trial_type #####

# prepare table with stats  

thr <- max(df_large_group[, mean(get(interval)), by=trial_type2]$V1) #
#thr_min <- min(df_large_group[, mean(get(interval)) - sterr(get(interval)), by=trial_type4]$V1) 

#choice type group
Tuk <- data.table(summary(emmeans(m2, pairwise ~ group|trial_type2, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
#choice type 
#Tuk <- data.table(summary(emmeans(m, pairwise ~ trial_type2|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)

Tuk

Tuk <- Tuk[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk <- Tuk[p.value<0.1, p_significant:=format(p.value, digits = 3)]
  
n <- Tuk[!is.na(p_significant), .N]
thr <- max(df_large_group[, mean(get(interval)), by=trial_type2]$V1) #

#check the means
#df_large_group <- df_large_group[group == 'adults']
#thr <- df_large_group[, mean(get(interval)), by=trial_type2]$V1
#thr
#df_large_group <- df_large_group[group == 'teens']
#thr <- df_large_group[, mean(get(interval)), by=trial_type2]$V1
#thr


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

choice_type = FALSE
choice_type_group = TRUE



# plot Tukey and save table

Tuk
                          
if (choice_type == TRUE){
    modes2 = 'teens'
    dff_trained <- df_large_group[group == as.character(modes2)]
    Tuk1 <- Tuk[group == as.character(modes2)]
    Tuk1

    title <- modes2
    interval
    unique(dff_trained$trial_type)
    #plots
    p <-ggline(dff_trained, 'trial_type2', interval,
               add = c("mean_se"),
               order = c("rHP", "rLP"),
               ylab = int_name, xlab = "Choice type",
               size = 1.5, 
               point.size = 1.8,
               font.label = list(size = 16, color = "black"))+
      scale_x_discrete(name=title, labels = c("rHP", 'rLP'))
    p <- p + stat_pvalue_manual(Tuk1, label = 'stars', size = 12,tip.length = 0.001,bracket.size = 1.2)
    p
    f <- paste0('_RT_log_rHP_rLP_', modes2)
   
}

if (choice_type_group == TRUE){
    dff_trained <- df_large_group
    Tuk1 <- Tuk
    #plot
    
    #signif <- Tuk
    
    #plot settings
    sequence <-data.table(trial_type2=c("rHP","rLP"),number=c(1,2)) 
    
    y_values_rew <- dff_trained[group == 'adults',
                                mean(get(interval)),by='trial_type2']
    setnames(y_values_rew,'V1','y_values_rew')
    
    y_values_lose <- dff_trained[group == 'teens',
                                 mean(get(interval)),by='trial_type2']
    setnames(y_values_lose,'V1','y_values_lose')
    
    y_values <- merge(y_values_lose,y_values_rew,by='trial_type2')
    y_values <- merge(y_values,sequence,by='trial_type2')
    y_values[,y_max:=max(y_values_lose,y_values_rew),by=trial_type2]
    y_values <- merge(y_values,signif,by='trial_type2')
    
    y_values <- y_values[group == as.character(modes[t2])]
    title <- 'Teens and adults'
    p <-ggline(dff_trained, 'trial_type2', interval,
               color = 'group',
               add = c("mean_se"),
               #palette = c('magenta','orange'),
               palette = c('green','red'),
               order = c("rHP","rLP"),
               ylab = int_name, xlab = title,
               size = 1.5, 
               point.size = 1.8,
               font.label = list(size = 26, color = "black"),
               font.legend = c(20, "plain", "black"))+
      #scale_x_discrete(labels = c("No learning", 'After learning'))+
      scale_x_discrete(name=title, labels = c("rHP", 'rLP'))+
      scale_color_discrete(name = "Group", labels = c("adults", "teens"))+
      geom_signif(y_position=c(y_values$y_max+0.02),
                  xmin=c(y_values$number-0.075), xmax=c(y_values$number+0.075),
                  annotation=c(Tuk1$stars), 
                  tip_length=0.001,textsize = 7,vjust = 0.4)
    p
    f <- '_RT_log_rHP_rLP_group'
  }



p1 <- ggpar(p,
      ylim = c(3.03,3.2),
      #ylim = c(1050,1320),
      font.ytickslab = 30,
      font.xtickslab = 27,
      font.main = 25,
      font.submain = 25,
      font.x = 27,
      font.y = 20,
      
)
p1
out_path


#ggsave(filename = paste0(out_path, modes2,'_',modes3,'_', interval,'_Tukey','.png'), p1, width =  6, height = 6)
ggsave(filename = paste0(out_path, f, '_Tukey','.png'), p1, width =  7, height = 6)
#ggsave(filename = paste0(out_path, '_RT_both_groups', '_Tukey','.png'), p1, width =  5, height = 4)
