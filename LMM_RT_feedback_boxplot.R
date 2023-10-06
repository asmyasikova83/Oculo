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

path  <- 'C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/'

#loading files and subjects 

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

# create datatable with variables to analyze in data (int), 
# their labels for graps (int_name), short names for saving(short_int_name)

interval <- 'RT_log_raw'

m <- lmer(get(interval) ~ trial_type*feedback_prev*group*train + (1|fname) + (1|block), data = df_large_group)

anova(m)
s <- step(m)
m2 <- get_model(s)
anova(m2)

################### Compare feedbacks prev ######################


Tuk <- emmeans(m, pairwise ~ feedback_prev|train|trial_type|group  , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000,level = 0.99)

Tuk1<- summary(Tuk)$contrasts
Tuk1<- as.data.table(Tuk1)
output2 <- as.data.table(Tuk$emmeans)

Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]
Tuk1 
n <- Tuk1[!is.na(p_significant), .N]

thr <- max(df_large_group[, mean(get(interval)) + sterr(get(interval)), by=trial_type]$V1) #
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

sequence <-data.table(trial_type=c("norisk","risk"),number=c(1,2))

y_values_rew <- df_large_group[feedback_prev == 'positive',
                               mean(get(interval)),by='trial_type']

setnames(y_values_rew,'V1','y_values_rew')

y_values_lose <- df_large_group[feedback_prev == 'negative',
                                mean(get(interval)),by='trial_type']

setnames(y_values_lose,'V1','y_values_lose')

y_values <- merge(y_values_lose,y_values_rew,by='trial_type')
y_values <- merge(y_values,sequence,by='trial_type')
y_values[,y_max:=max(y_values_lose,y_values_rew),by=trial_type]
y_values <- merge(y_values,signif,by='trial_type')


#####################normals not trained###########################
output_p1 <-output2[group == 'normals'][train == 'not_trained']
y_values_p1 <- y_values[group == 'normals'][train == 'not_trained']
Tuk1_p1 <- Tuk1[group == 'normals'][train == 'not_trained']
signif_p1 <- signif[group == 'normals'][train == 'not_trained']
output_p1
          
pic_1 <- ggplot(output_p1, aes(x=trial_type, y=emmean, ymin=emmean-SE, ymax = emmean+SE, fill = feedback_prev))+
  geom_col(position = "dodge",colour =  c('black')) +
  geom_hline(yintercept= 0.0, linetype='dashed', col = 'black', size = 1.5)+
  theme_classic((size = 20))+ scale_x_discrete(labels=c("HP", "LP"))+
  geom_point(position=position_dodge(0.09)) + geom_errorbar(width = 0.2,  position=position_dodge(0.9), size=0.6)+
  labs(x = 'Choice type', y = "RT log emmeans",col="black")+
  ggtitle("not_trained NT participants") +
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
#####################normals trained###########################
output_p2 <-output2[group == 'normals'][train == 'trained']
y_values_p2 <- y_values[group == 'normals'][train == 'trained']
Tuk1_p2 <- Tuk1[group == 'normals'][train == 'trained']
signif_p2 <- signif[group == 'normals'][train == 'trained']


pic_2 <- ggplot(output_p2, aes(x=trial_type, y=emmean, ymin=emmean-SE, ymax = emmean+SE, fill = feedback_prev))+
  geom_col(position = "dodge",colour =  c('black')) +
  geom_hline(yintercept= 0.0, linetype='dashed', col = 'black', size = 1.5)+
  theme_classic((size = 20))+ scale_x_discrete(labels=c("HP", "LP"))+
  geom_point(position=position_dodge(0.09)) + geom_errorbar(width = 0.2,  position=position_dodge(0.9), size=0.6)+
  labs(x = 'Choice type', y = "RT log emmeans",col="black")+
  ggtitle("trained NT participants") +
  scale_fill_discrete(name="Previous\nFeedback",
                      labels=c("Loss", "Gain"))

pic2_signif <- pic_2+  geom_signif(y_position=c(y_values_p2$y_max+0.22),
                                   xmin=c(y_values_p2$number-0.4), xmax=c(y_values_p2$number+0.4),
                                   annotation=c(Tuk1_p2$stars), 
                                   tip_length=0.1,textsize = 10,vjust = 0.4)

pic2_signif<- ggpar(pic2_signif,
                    ylim = c(3.0, 3.5),
                    font.ytickslab = 30,
                    font.xtickslab = 27,
                    font.main = 25,
                    font.submain = 25,
                    font.x = 27,
                    font.y = 27)

pic2_signif
#####################autists not trained###########################
output_p4 <-output2[group == 'autists'][train == 'not_trained']
y_values_p4 <- y_values[group == 'autists'][train == 'not_trained']
Tuk1_p4 <- Tuk1[group == 'autists'][train == 'not_trained']
signif_p4 <- signif[group == 'autists'][train == 'not_trained']


pic_4 <- ggplot(output_p4, aes(x=trial_type, y=emmean, ymin=emmean-SE, ymax = emmean+SE, fill = feedback_prev))+
  geom_col(position = "dodge",colour =  c('black')) +
  geom_hline(yintercept= 0.0, linetype='dashed', col = 'black', size = 1.5)+
  theme_classic((size = 20))+ scale_x_discrete(labels=c("HP", "LP"))+
  geom_point(position=position_dodge(0.09)) + geom_errorbar(width = 0.2,  position=position_dodge(0.9), size=0.6)+
  labs(x = 'Choice type', y = "RT log emmeans",col="black")+
  ggtitle("not trained ASD participants") +
  scale_fill_discrete(name="Previous\nFeedback",
                      labels=c("Loss", "Gain"))

pic4_signif <- pic_4 + geom_signif(y_position=c(y_values_p4$y_max+0.22),
                                   xmin=c(y_values_p4$number-0.4), xmax=c(y_values_p4$number+0.4),
                                   annotation=c(Tuk1_p4$stars), 
                                   tip_length=0.1,textsize = 10,vjust = 0.4)

pic4_signif<- ggpar(pic4_signif,
                    ylim = c(3.0, 3.5),
                    font.ytickslab = 30,
                    font.xtickslab = 27,
                    font.main = 25,
                    font.submain = 25,
                    font.x = 27,
                    font.y = 27)

pic4_signif
#####################autists trained###########################
output_p3 <-output2[group == 'autists'][train == 'trained']
y_values_p3 <- y_values[group == 'autists'][train == 'trained']
Tuk1_p3 <- Tuk1[group == 'autists'][train == 'trained']
signif_p3 <- signif[group == 'autists'][train == 'trained']


pic_3 <- ggplot(output_p3, aes(x=trial_type, y=emmean, ymin=emmean-SE, ymax = emmean+SE, fill = feedback_prev))+
  geom_col(position = "dodge",colour =  c('black')) +
  geom_hline(yintercept= 0.0, linetype='dashed', col = 'black', size = 1.5)+
  theme_classic((size = 20))+ scale_x_discrete(labels=c("HP", "LP"))+
  geom_point(position=position_dodge(0.09)) + geom_errorbar(width = 0.2,  position=position_dodge(0.9), size=0.6)+
  labs(x = 'Choice type', y = "RT log emmeans",col="black")+
  ggtitle("trained ASD participants") +
  scale_fill_discrete(name="Previous\nFeedback",
                      labels=c("Loss", "Gain"))

pic3_signif <- pic_3 + geom_signif(y_position=c(y_values_p3$y_max+0.22),
                                   xmin=c(y_values_p3$number-0.4), xmax=c(y_values_p3$number+0.4),
                                   annotation=c(Tuk1_p3$stars), 
                                   tip_length=0.1,textsize = 10,vjust = 0.4)

pic3_signif<- ggpar(pic3_signif,
                    ylim = c(3.0, 3.5),
                    font.ytickslab = 30,
                    font.xtickslab = 27,
                    font.main = 25,
                    font.submain = 25,
                    font.x = 27,
                    font.y = 27)

pic3_signif



pn_nt<- plot_grid(pic1_signif,pic2_signif+ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                      axis.ticks.y = ggplot2::element_blank(),
                                      axis.title.y = ggplot2::element_blank()), nrow = 1, align = "hv")

pn_nt

pn_at<- plot_grid(pic4_signif,pic3_signif+ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                                                              axis.ticks.y = ggplot2::element_blank(),
                                                                              axis.title.y = ggplot2::element_blank()), nrow = 1, align = "hv")

pn_at


ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_articles/article_at_nt/RT_feedback_prev_AT.png',pn_at,width =  12, height = 5)

ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_articles/article_at_nt/RT_feedback_prev_NT.png',pn_nt,width =  12, height = 5)

plotl <- function(...) {
  x <- seq(0, 30, 0.01)
  plot(besselJ(x, 0), col = 2, type = "l",
       lwd = 2, ylab = "Jn(x)", xlab = "", ...)
  lines(besselJ(x, 2), col = 3, type = "l", lwd = 2, lty = 2)
}

plotl()
legend("topright",
       legend = c(3, 4, 5),
       fill = 2:4,       # Color of the squares
       border = "black")

legend("topright",title="Current feedback", legend = c("Loss", "Gain"),
       fill = c("coral2","darkturquoise"))