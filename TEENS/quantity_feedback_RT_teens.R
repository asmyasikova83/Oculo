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

#rt filter
rt <- rt[RT>300 & RT<4000]

#fb prev
rt[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

#rt$quanity <- 'other'
#rt[prev_rew == 1 & risk == 1, quantity := 'risk_prev_wins'][prev_rew == 0 & risk == 1, quantity := 'risk_prev_losses']
#rt[prev_rew == 1 & risk == 0, quantity := 'norisk_prev_wins'][prev_rew == 0 & risk == 0, quantity := 'norisk_prev_losses']

#rt[prev_rew == 1 & prev_risk == 0 & risk == 1 & next_risk == 0, quantity := 'seq_risk_prev_wins'][prev_rew == 0 & prev_risk == 0 & risk == 1 & next_risk == 0, quantity := 'seq_risk_prev_losses']
#rt[prev_rew == 1 & prev_risk == 0 & risk == 0 & next_risk == 0, quantity := 'seq_norisk_prev_wins'][prev_rew == 0 & prev_risk == 0 & risk == 0 & next_risk == 0, quantity := 'seq_norisk_prev_losses']
#rt <- rt[quantity != 'other']

rt$choice_type < 'other'
#rt[prev_risk == 0 & risk == 1 & next_risk == 0, choice_type := 'risk']
#rt[prev_risk == 0 & risk == 0 & next_risk == 0, choice_type := 'norisk']
rt[risk == 1, choice_type := 'risk']
rt[risk == 0, choice_type := 'norisk']
rt <- rt[choice_type %in% c('norisk', 'risk')]


unique(rt$choice_type)

rt[,RT_log_raw:=log10(RT)]
rt <- rt[!is.na(RT_log_raw)]
################################lmem#########################################
#m <- lmer(get('RT_log_raw') ~ quantity*group + (1|fname) + (1|block), data = rt)
m <- lmer(get('RT_log_raw') ~ choice_type*feedback_prev*group + (1|fname) + (1|block), data = rt)

anova(m)
s <- step(m)
m2 <- get_model(s)
anova(m2)

interval <- 'RT_log_raw'

thr <- max(rt[, mean(get(interval)), by=choice_type]$V1) #
#thr <- max(rt[, mean(get(interval)), by=quantity]$V1) #
#thr_min <- min(df_large_group[, mean(get(interval)) - sterr(get(interval)), by=trial_type4]$V1) 

#choice type group
#Tuk <- data.table(summary(emmeans(m, pairwise ~ quantity|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
#Tuk <- data.table(summary(emmeans(m, pairwise ~ feedback_prev|choice_type|group, adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000))$contrasts)
Tuk <- emmeans(m, pairwise ~ feedback_prev|choice_type|group  , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000,level = 0.99)

Tuk1<- summary(Tuk)$contrasts
Tuk1<- as.data.table(Tuk1)
output2 <- as.data.table(Tuk$emmeans)

Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]
Tuk1 
n <- Tuk1[!is.na(p_significant), .N]

thr <- max(rt[, mean(get(interval)), by=choice_type]$V1) #
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
Tuk1[,y.position:=y.position-0.1]
Tuk1



emm_options(pbkrtest.limit = 4363)

signif <- Tuk1
signif
#preparing for plots

sequence <-data.table(choice_type=c("norisk","risk"),number=c(1,2))

y_values_rew <- rt[feedback_prev == 'positive',
                               mean(get(interval)),by='choice_type']

setnames(y_values_rew,'V1','y_values_rew')

y_values_lose <- rt[feedback_prev == 'negative',
                                mean(get(interval)),by='choice_type']

setnames(y_values_lose,'V1','y_values_lose')

y_values <- merge(y_values_lose,y_values_rew,by='choice_type')
y_values <- merge(y_values,sequence,by='choice_type')
y_values[,y_max:=max(y_values_lose,y_values_rew),by=choice_type]
y_values <- merge(y_values,signif,by='choice_type')


#####################normals not trained###########################
output_p1 <-output2[group == 'adults']
y_values_p1 <- y_values[group == 'adults']
Tuk1_p1 <- Tuk1[group == 'adults']
signif_p1 <- signif[group == 'adults']
output_p1

pic_1 <- ggplot(output_p1, aes(x=choice_type, y=emmean, ymin=emmean-SE, ymax = emmean+SE, fill = feedback_prev))+
  geom_col(position = "dodge",colour =  c('black')) +
  geom_hline(yintercept= 0.0, linetype='dashed', col = 'black', size = 1.5)+
  theme_classic((size = 20))+ scale_x_discrete(labels=c("singlHP", "singlLP"))+
  geom_point(position=position_dodge(0.09)) + geom_errorbar(width = 0.2,  position=position_dodge(0.9), size=0.6)+
  labs(x = 'Choice type', y = "RT log emmeans",col="black")+
  ggtitle("adults") +
  scale_fill_discrete(name="Previous\nFeedback",
                      labels=c("Loss", "Gain"))
pic_1
pic1_signif <- pic_1+  geom_signif(y_position=c(y_values_p1$y_max+0.22),
                                   xmin=c(y_values_p1$number-0.4), xmax=c(y_values_p1$number+0.4),
                                   annotation=c(Tuk1_p1$stars), 
                                   tip_length=0.1,textsize = 10,vjust = 0.4)

pic1_signif<- ggpar(pic1_signif,
                    ylim = c(3.0, 3.5),
                    font.ytickslab = 30,
                    font.xtickslab = 27,
                    font.main = 25,
                    font.submain = 25,
                    font.x = 27,
                    font.y = 27)

pic1_signif
################################stat#########################################
q<- rt %>%
  group_by(group, choice_type)%>%summarise(N = n(),
                                        mean_RT = mean(RT_log_raw))
q
