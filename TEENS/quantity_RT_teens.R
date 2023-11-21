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
rt_teens$group <- 'teens'
length(unique(rt_teens$fname))

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

rt$quanity <- 'other'
rt[prev_rew == 1 & risk == 1, quantity := 'risk_prev_wins'][prev_rew == 0 & risk == 1, quantity := 'risk_prev_losses']
rt[prev_rew == 1 & risk == 0, quantity := 'norisk_prev_wins'][prev_rew == 0 & risk == 0, quantity := 'norisk_prev_losses']

unique(rt$quantity)

rt[,RT_log_raw:=log10(RT)]
rt <- rt[!is.na(RT_log_raw)]
################################lmem#########################################
m <- lmer(get('RT_log_raw') ~ quantity*group + (1|fname) + (1|block), data = rt)
anova(m)
s <- step(m)
m2 <- get_model(s)
anova(m2)

interval <- 'RT_log_raw'
thr <- max(rt[, mean(get(interval)), by=quantity]$V1) #
#thr_min <- min(df_large_group[, mean(get(interval)) - sterr(get(interval)), by=trial_type4]$V1) 

#choice type group
Tuk <- data.table(summary(emmeans(m2, pairwise ~ group|quantity, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)

Tuk

Tuk <- Tuk[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk <- Tuk[p.value<0.1, p_significant:=format(p.value, digits = 3)]

n <- Tuk[!is.na(p_significant), .N]
thr <- max(rt[, mean(get(interval)), by=quantity]$V1) #


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
int_name <- 'log10(RT)'
title <- 'Teens vs Adults in choice types'
p <- ggline(rt, 'quantity', interval,
       color = 'group',
       add = c("mean_se"),
       #palette = c('magenta','orange'),
       #palette = c('green','red'),
       order = c("norisk_prev_losses","norisk_prev_wins","risk_prev_losses","risk_prev_wins"),
       ylab = int_name, xlab = title,
       size = 1.5, 
       point.size = 1.8,
       font.label = list(size = 26, color = "black"),
       font.legend = c(20, "plain", "black"))+
  #scale_x_discrete(labels = c("No learning", 'After learning'))+
  scale_x_discrete(name=title, labels = c("HP_prev_losses","HP_prev_wins","LP_prev_losses","LP_prev_wins"))+
  scale_color_discrete(name = "Group", labels = c("adults", "teens"))
p

################################stat#########################################
q<- rt %>%
  group_by(group, quantity)%>%summarise(N = n(),
                                        mean_RT = mean(RT_log_raw))
q
