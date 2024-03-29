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
library(dplyr)

sterr <- function(x) sd(x)/sqrt(length(x))
path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"
out_path <- paste0(path,'pics_articles/article_at_nt/')

#"tables/resp_18042022.txt"
#"tables/autists.txt"
df1 <- fread(paste0(path,"norma.txt"))
df2 <- fread(paste0(path,"autists.txt"))
#df_large <- rbind(df1,df2)
df_large <- as.data.table(rbind(df1,df2))
#df_large<- na.omit(df_large)

#standardazitng fname
df_large$fname <- gsub ("_1", "", df_large$fname)
df_large$fname <- gsub ("p0", "P0", df_large$fname)
df_large$trial_type <-gsub ("_", "", df_large$trial_type) 

#1
#average over time interval
int_beg <- '1001'
int_end <- '2200'

ncol_min <- grep(paste0('_', int_beg, '_'), colnames(df_large))[1]
ncol_max <- grep(paste0('_', int_end, ''), colnames(df_large))[1]
col <- c(colnames(df_large)[ncol_min:ncol_max])

colname <- paste0('resp_',int_beg,'_',int_end)

df_large[,(colname):=rowMeans(df_large[,.SD,.SDcol=col])]
df_large[, index := 1:.N, by=c('fname','block')]

#2
#RT filter (for all intervals)
df_large <- df_large[RT>300 & RT<4000]
df_large  <- df_large[block!='6']
df_large[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']


#q_trials <- df_large %>%
#  group_by(fname)%>%summarise(N=n()) #### num of risks

#Z for pupil

ncol_min <- grep('V_m999_m950',colnames(df_large))
ncol_max <- grep('V_2951_3000',colnames(df_large))
col <- c(colnames(df_large)[ncol_min:ncol_max])

df_filt_m <- data.table(melt(df_large[blink==F], id.vars = c('fname','time'), measure.vars = col))
Zmeans <- df_filt_m[, mean(value), by=fname]
setnames(Zmeans,'V1','Zmean_2')
Zsds <- df_filt_m[, sd(value), by=fname]
setnames(Zsds,'V1','Zsd_2')

df_large <- merge(df_large,Zmeans,by='fname',all.x = TRUE)
df_large <- merge(df_large,Zsds,by='fname',all.x = TRUE)

#apply Z for pupil
cols <- colnames(df_large)[grep('resp_|V_|BL_|inter_trial',colnames(df_large))]
for (j in cols) set(df_large, j = j, value = (df_large[[j]]-df_large$Zmean_2)/df_large$Zsd_2)

df_large$trial_type4 <- df_large$trial_type
df_large[prev_risk==0 & risk==1 & next_risk==0, trial_type4:= 'risk']
df_large[prev_risk==0 & risk==0 & next_risk==0, trial_type4:= 'norisk']


#3
df_large <- df_large[trial_type!='final' & trial_type!='criterion' & !fname %in% c('P308','P309','P311')]

pupil_sd <- df_large[blink==F][,sd(resp_1001_2200)]
df_large[,resp_flag:=!blink & abs(resp_1001_2200)<3*pupil_sd]


# load filtered dataset

nt <- c('P001','P004','P019', 'P021', 'P022', 'P032', 'P034', 'P035', 'P039','P040', 'P044','P047','P048', 'P053',
        'P055', 'P058', 'P059', 'P060', 'P061', 'P063', 'P064', 'P065', 'P066')
at <- c('P301', 'P304','P307', 'P312','P313','P314','P316','P318','P321','P322','P323','P324','P325','P326','P327',
        'P328','P329','P333', 'P334','P335','P338','P341', 'P342')
df_NT <- df_large[fname %in% nt]
df_NT$group <- 'normals'
df_AT <- df_large[fname %in% at]
df_AT$group <- 'autists'
df_large_group <- rbind(df_AT, df_NT)
unique(df_large_group$fname)

#settings for train untrain
df_large_group <- df_large_group[!is.na(resp_1001_2200)]

df_large_group[trained == FALSE, train:='not_trained'][trained== TRUE, train:='trained']

df_large_group <- df_large_group[!is.na(train)]
df_large_group$train<- as.factor(df_large_group$train)

flag <- 'resp_flag'
interval <- 'resp_1001_2200'
df_large_group <- df_large_group[df_large_group[,get(flag)==T]]
length(unique(df_large_group[group == 'autists' & train == 'not_trained']$fname))
length(unique(df_large_group[group == 'normals' & train == 'not_trained']$fname))

print(unique(df_large_group[group == 'autists' & train == 'trained']$fname))

df_large_group <- df_large_group[trial_type4 %in% c('norisk','risk')]

################################LMM##########################

m <- lmer(resp_1001_2200 ~ trial_type4*group*feedback_prev*train + (1|fname) + (1|block),df_large_group)
#m <- lmer(RT_Z ~ trial_type4*feedback_prev*group*trained_edited + (1|fname)+ (1|block),df_large_group)
#m <- lmer(RT_Z ~ trial_type4*feedback_prev*trained_edited + (1|fname)+ (1|block),df_large_group)
#m <- lmer(resp_1001_2200 ~ trial_type4*group*train + (1|fname)+ (1|block),df_large_group)

print(anova(m))

s <- step(m)
m2 <- get_model(s)
print(anova(m2))
################### Compare feedbacks prev #########################


Tuk <- emmeans(m, pairwise ~ feedback_prev|train|trial_type4|group  , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000,level = 0.99)

Tuk1<- summary(Tuk)$contrasts
Tuk1<- as.data.table(Tuk1)
output2 <- as.data.table(Tuk$emmeans)

Tuk1 <- Tuk1[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1 <- Tuk1[p.value<0.1, p_significant:=format(p.value, digits = 3)]
Tuk1 
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
Tuk1[,y.position:=y.position-0.1]
Tuk1


##############################positive_vs_positive####################
Tuk_pos <- emmeans(m, pairwise ~ train|trial_type4|feedback_prev|group  , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000,level = 0.99)

Tuk1_pos<- summary(Tuk_pos)$contrasts
Tuk1_pos<- as.data.table(Tuk1_pos)
Tuk1_pos
#####################################################################
output2_pos <- as.data.table(Tuk_pos$emmeans)

Tuk1_pos <- Tuk1_pos[, group1:=gsub(' -.*', '', contrast)][, group2:=gsub('.*- ', '', contrast)]
Tuk1_pos <- Tuk1_pos[p.value<0.1, p_significant:=format(p.value, digits = 3)]
Tuk1_pos <- Tuk1_pos[feedback_prev == 'positive']
Tuk1_pos
Tuk1_pos[p.value<0.001, stars:='***']
Tuk1_pos[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk1_pos[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk1_pos[p.value>0.05 & p.value<0.1 , stars:='#']
#Tuk1_pos[,y.position:=y.position-0.1]
Tuk1_pos
################################plot##################################
emm_options(pbkrtest.limit = 4363)

signif <- Tuk1
signif
#preparing for plots

sequence <-data.table(trial_type4=c("norisk","risk"),number=c(1,2))

y_values_rew <- df_large_group[feedback_prev == 'positive',
                               mean(get(interval)),by='trial_type4']

setnames(y_values_rew,'V1','y_values_rew')

y_values_lose <- df_large_group[feedback_prev == 'negative',
                                mean(get(interval)),by='trial_type4']

setnames(y_values_lose,'V1','y_values_lose')

y_values <- merge(y_values_lose,y_values_rew,by='trial_type4')
y_values <- merge(y_values,sequence,by='trial_type4')
y_values[,y_max:=max(y_values_lose,y_values_rew),by=trial_type4]
y_values <- merge(y_values,signif,by='trial_type4')


#####################normals not trained###########################
output_p1 <-output2[group == 'normals'][train == 'not_trained']
y_values_p1 <- y_values[group == 'normals'][train == 'not_trained']
Tuk1_p1 <- Tuk1[group == 'normals'][train == 'not_trained']
signif_p1 <- signif[group == 'normals'][train == 'not_trained']
output_p1

pic_1 <- ggplot(output_p1, aes(x=trial_type4, y=emmean, ymin=emmean-SE, ymax = emmean+SE, fill = feedback_prev))+
  #geom_col(position = "dodge",colour =  c('black')) +
  geom_col_pattern(aes(x=trial_type4, fill=feedback_prev),colour='black',
                   position = "dodge", pattern='stripe') +
  geom_hline(yintercept= 0.0, linetype='dashed', col = 'black', size = 1.5)+
  theme_classic((size = 20))+ scale_x_discrete(labels=c("HP", "LP"))+
  geom_point(position=position_dodge(0.09)) + geom_errorbar(width = 0.2,  position=position_dodge(0.9), size=0.6)+
  labs(x = 'Choice type', y = "Pupil [Z]",col="black")+
  ggtitle("NT: no learning") +
  scale_fill_discrete(name="Previous\nFeedback",
                      labels=c("Loss", "Gain"))

pic_1 
pic1_signif <- pic_1+  geom_signif(y_position=c(y_values_p1$y_max+0.27),
                                   xmin=c(y_values_p1$number-0.4), xmax=c(y_values_p1$number+0.4),
                                   annotation=c(Tuk1_p1$stars), 
                                   tip_length=0.1,textsize = 10,vjust = 0.4)

pic1_signif<- ggpar(pic1_signif,
                    ylim = c(-0.6, 0.6),
                    font.ytickslab = 30,
                    font.xtickslab = 27,
                    font.main = 25,
                    font.submain = 25,
                    font.x = 27,
                    font.y = 27)

pic1_signif
ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_articles/article_at_nt/dec_prefin/pic1_nt_signif.png',pic1_signif,width =  6, height = 5)


#####################normals trained###########################
output_p2 <-output2[group == 'normals'][train == 'trained']
y_values_p2 <- y_values[group == 'normals'][train == 'trained']
Tuk1_p2 <- Tuk1[group == 'normals'][train == 'trained']
signif_p2 <- signif[group == 'normals'][train == 'trained']


pic_2 <- ggplot(output_p2, aes(x=trial_type4, y=emmean, ymin=emmean-SE, ymax = emmean+SE, fill = feedback_prev))+
  #geom_col(position = "dodge",colour =  c('black')) +
  geom_col_pattern(aes(x=trial_type4, fill=feedback_prev),
                   position = "dodge", pattern='weave') +
  geom_hline(yintercept= 0.0, linetype='dashed', col = 'black', size = 1.5)+
  theme_classic((size = 20))+ scale_x_discrete(labels=c("HP", "LP"))+
  geom_point(position=position_dodge(0.09)) + geom_errorbar(width = 0.2,  position=position_dodge(0.9), size=0.6)+
  labs(x = 'Choice type', y = "Pupil [Z]",col="black")+
  ggtitle("NT: after learning") +
  scale_fill_discrete(name="Previous\nFeedback",
                      labels=c("Loss", "Gain"))

pic2_signif <- pic_2 + geom_signif(y_position=c(y_values_p2$y_max+0.27),
                                   xmin=c(y_values_p2$number-0.4), xmax=c(y_values_p2$number+0.4),
                                   annotation=c(Tuk1_p2$stars), 
                                   tip_length=0.1,textsize = 10,vjust = 0.4)

pic2_signif<- ggpar(pic2_signif,
                    ylim = c(-0.6, 0.6),
                    font.ytickslab = 30,
                    font.xtickslab = 27,
                    font.main = 25,
                    font.submain = 25,
                    font.x = 27,
                    font.y = 27)

pic2_signif

ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_articles/article_at_nt/dec_prefin/pic2_nt_signif.png',pic2_signif,width =  6, height = 5)

##############################################################
Tuk_pos <- emmeans(m, pairwise ~ train|feedback_prev| trial_type4| group  , adjust = 'tukey',lmer.df = "satterthwaite",lmerTest.limit=8000,level = 0.99)

Tuk1_pos<- summary(Tuk_pos)$contrasts
Tuk1_pos<- as.data.table(Tuk1_pos)
Tuk1_pos[p.value<0.001, stars:='***']
Tuk1_pos[p.value<0.01 & p.value>0.001 , stars:='**']
Tuk1_pos[p.value<0.05 & p.value>0.01 , stars:='*']
Tuk1_pos[p.value>0.05 & p.value<0.1 , stars:='#']
Tuk1_pos
#############################################################
#####################autists not trained###########################
output_p4 <-output2[group == 'autists'][train == 'not_trained']
y_values_p4 <- y_values[group == 'autists'][train == 'not_trained']
Tuk1_p4 <- Tuk1[group == 'autists'][train == 'not_trained']
signif_p4 <- signif[group == 'autists'][train == 'not_trained']


pic_4 <- ggplot(output_p4, aes(x=trial_type4, y=emmean, ymin=emmean-SE, ymax = emmean+SE, fill = feedback_prev))+
  #geom_col(position = "dodge",colour =  c('black')) +
  geom_col_pattern(aes(x=trial_type4, fill=feedback_prev),colour='black',
                   position = "dodge", pattern='stripe') +
  geom_hline(yintercept= 0.0, linetype='dashed', col = 'black', size = 1.5)+
  theme_classic((size = 20))+ scale_x_discrete(labels=c("HP", "LP"))+
  geom_point(position=position_dodge(0.09)) + geom_errorbar(width = 0.2,  position=position_dodge(0.9), size=0.6)+
  labs(x = 'Choice type',  y = "Pupil [Z]",col="black")+
  ggtitle("ASD: no learning") +
  scale_fill_discrete(name="Previous\nFeedback",
                      labels=c("Loss", "Gain"))

pic4_signif <- pic_4 + geom_signif(y_position=c(y_values_p4$y_max+0.27),
                                   xmin=c(y_values_p4$number-0.4), xmax=c(y_values_p4$number+0.4),
                                   annotation=c(Tuk1_p4$stars), 
                                   tip_length=0.1,textsize = 10,vjust = 0.4)

pic4_signif<- ggpar(pic4_signif,
                    ylim = c(-0.6, 0.6),
                    font.ytickslab = 30,
                    font.xtickslab = 27,
                    font.main = 25,
                    font.submain = 25,
                    font.x = 27,
                    font.y = 27)

pic4_signif
ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_articles/article_at_nt/dec_prefin/pic4_at_signif.png',pic4_signif,width =  6, height = 5)

#####################autists trained###########################
output_p3 <-output2[group == 'autists'][train == 'trained']
y_values_p3 <- y_values[group == 'autists'][train == 'trained']
Tuk1_p3 <- Tuk1[group == 'autists'][train == 'trained']
signif_p3 <- signif[group == 'autists'][train == 'trained']


pic_3 <- ggplot(output_p3, aes(x=trial_type4, y=emmean, ymin=emmean-SE, ymax = emmean+SE, fill = feedback_prev))+
  #geom_col(position = "dodge",colour =  c('black')) +
  geom_col_pattern(aes(x=trial_type4, fill=feedback_prev),
                   position = "dodge", pattern='weave') +
  geom_hline(yintercept= 0.0, linetype='dashed', col = 'black', size = 1.5)+
  theme_classic((size = 20))+ scale_x_discrete(labels=c("HP", "LP"))+
  geom_point(position=position_dodge(0.09)) + geom_errorbar(width = 0.2,  position=position_dodge(0.9), size=0.6)+
  labs(x = 'Choice type',  y = "Pupil [Z]",col="black")+
  ggtitle("ASD: after learning") +
  scale_fill_discrete(name="Previous\nFeedback",
                      labels=c("Loss", "Gain"))

pic3_signif <- pic_3 + geom_signif(y_position=c(y_values_p3$y_max+0.27),
                                   xmin=c(y_values_p3$number-0.4), xmax=c(y_values_p3$number+0.4),
                                   annotation=c(Tuk1_p3$stars), 
                                   tip_length=0.1,textsize = 10,vjust = 0.4)

pic3_signif<- ggpar(pic3_signif,
                    ylim = c(-0.6, 0.6),
                    font.ytickslab = 30,
                    font.xtickslab = 27,
                    font.main = 25,
                    font.submain = 25,
                    font.x = 27,
                    font.y = 27)

pic3_signif
ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_articles/article_at_nt/dec_prefin/pic3_at_signif.png',pic3_signif,width =  6, height = 5)


Tuk1_pos_nt <- Tuk1_pos[group == 'normals']
Tuk1_pos_nt <- Tuk1_pos_nt[!is.na(Tuk1_pos_nt$stars)]
Tuk1_pos_nt$stars
Tuk1_pos_nt

pn_nt<- plot_grid(pic1_signif,pic2_signif+ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                                         axis.ticks.y = ggplot2::element_blank(),
                                                         axis.title.y = ggplot2::element_blank()), nrow = 1, align = "hv")

Tuk1_pos_nt$stars


pn_at<- plot_grid(pic4_signif,pic3_signif+ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                                         axis.ticks.y = ggplot2::element_blank(),
                                                         axis.title.y = ggplot2::element_blank()), nrow = 1, align = "hv")

pn_at


ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_articles/article_at_nt/dec_prefin/Pupil_feedback_prev_AT.png',pn_at,width =  12, height = 5)

ggsave('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/pics_articles/article_at_nt/dec_prefin/Pupil_feedback_prev_NT.png',pn_nt,width =  12, height = 5)

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