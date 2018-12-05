rm(list=ls())
rm(list=setdiff(ls(), c("ogtt", "data", "body", "glucose", "hba1c", "infect")))
setwd("C:/Users/Abrar/OneDrive - University of North Carolina at Chapel Hill/Shaikh Lab/NHANES/")
library(RNHANES)
library(gplots)
library("dplyr")
library("ggpubr")
library(tidyverse)

#load in file for Dietary Interview - Total Nutrient Intakes, First Day
nhanes_load_data("DR1TOT_H", "2013-2014")

#diet data
data <- nhanes_load_data("DR1TOT_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)

#diet data
infect <- nhanes_load_data("HSQ_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
infect_sub <- infect[,c(1,52)] # subset by ID and OGTT

#body measures and BMI data
body <- nhanes_load_data("BMX_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
body_sub <- body[,c(1,58)] # subset by ID and BMI

#merging
merge <- merge(data, infect_sub, by="SEQN")
merge <- merge(merge, body_sub, by="SEQN")
#merge_sub <- merge[,c(1,42,142,219)]
#merge_sub_rmNA <- na.omit(merge_sub)

#subset by age
#merge$RIDAGEYR <- as.numeric(as.character(merge$RIDAGEYR)) #converting age to numeric vector
#newdata <- merge[merge$RIDAGEYR >= 18,] #ages older than 18

merge_sub <- merge[,c(1,42,5,6,9,144,219,220)] #ID, population weights, gender, age, race, DHA intake, infection info, BMI
merge_sub_rmNA <- na.omit(merge_sub)
hist(merge_sub_rmNA$DR1TP226)
plot(merge_sub_rmNA$DR1TP226, merge_sub_rmNA$LBXGLT)

#Quartiles
quantile(merge_sub_rmNA$DR1TP226)
#q1 <- subset(merge_sub_rmNA, DR1TP226 <= quantile(DR1TP226, 0.25))
#q2 <- subset(merge_sub_rmNA, DR1TP226 <= quantile(DR1TP226, 0.5) & DR1TP226 > quantile(DR1TP226, 0.25))
#q3 <- subset(merge_sub_rmNA, DR1TP226 <= quantile(DR1TP226, 0.75) & DR1TP226 > quantile(DR1TP226, 0.5))

#Labeling data by quartiles
merge_sub_rmNA$Quartiles <- ifelse(merge_sub_rmNA$DR1TP226 <= quantile(merge_sub_rmNA$DR1TP226, 0.25),1,
                                   ifelse(merge_sub_rmNA$DR1TP226 > quantile(merge_sub_rmNA$DR1TP226, 0.25) & merge_sub_rmNA$DR1TP226 <= quantile(merge_sub_rmNA$DR1TP226, 0.5),2,
                                          ifelse(merge_sub_rmNA$DR1TP226 > quantile(merge_sub_rmNA$DR1TP226, 0.5) & merge_sub_rmNA$DR1TP226 <= quantile(merge_sub_rmNA$DR1TP226, 0.75),3,4
                                          )))

#Counting n individuals who were or were not infected in each quartile
with(merge_sub_rmNA, table(HSQ520, Quartiles))

#####EXTRA CODE#####
# i1 <- as.numeric(with(merge_sub_rmNA, table(HSQ520, Quartiles))[3])
# i2 <- as.numeric(with(merge_sub_rmNA, table(HSQ520, Quartiles))[6])
# i3 <- as.numeric(with(merge_sub_rmNA, table(HSQ520, Quartiles))[9])
# i4 <- as.numeric(with(merge_sub_rmNA, table(HSQ520, Quartiles))[12])
# 
# #add frequencies of Yes responses to infection
# merge_sub_rmNA$Frequency <- ifelse(merge_sub_rmNA$Quartiles == "1",i1,
#                                    ifelse(merge_sub_rmNA$Quartiles == "2",i2,
#                                           ifelse(merge_sub_rmNA$Quartiles == "3",i3,i4
#                                           )))
###############

df <- data.frame(with(merge_sub_rmNA, table(HSQ520, Quartiles)))
df <- df[c(3,6,9,12),]

kruskal.test(Freq ~ Quartiles,  data = df)
compare_means(Freq ~ Quartiles,  data = df)

#Counting n individuals in each quartile
n1 <- sum(merge_sub_rmNA$Quartiles == "1")
n2 <- sum(merge_sub_rmNA$Quartiles == "2")
n3 <- sum(merge_sub_rmNA$Quartiles == "3")
n4 <- sum(merge_sub_rmNA$Quartiles == "4")

#plotting all ages and genders
#my_comparisons <- list(c("1", "2"), c("2", "3"), c("1", "3"), c("1", "4"))

ggbarplot(df, x = "Quartiles", y = "Freq", xlab = "Quartiles of DHA Intake", ylab = "Frequency of Infection \n (Flu, pneumonia, ear infection)", label = TRUE, width = 0.5, fill = "Quartiles", color = "Quartiles", palette = c("lightskyblue", "lightblue2", "lightblue3", "lightblue4"), title = "Infection by DHA All Ages")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.011\nn = ', n2), paste('3\n0.056\nn = ', n3), paste('4\n4.08\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 105, label.x = 1)      # Add global p-value
#stat_compare_means(comparisons = my_comparisons, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(125, 127, 129, 131)) #p.format to place actual p-values (uses wilcox.test)

tal <- data.frame(tally(group_by(merge_sub_rmNA, HSQ520, Quartiles, RIAGENDR)))
tal <- tal[c(16:23),]

#wilcox.test(n ~ Quartiles, data = tal, paired = TRUE)
#comparing for each gender seperately
compare_means(n ~ Quartiles,  data = tal, group.by = "RIAGENDR")

#plotting by gender
ggbarplot(tal, x = "Quartiles", y = "n", xlab = "Quartiles of DHA Intake", ylab = "Frequency of Infection \n (Flu, pneumonia, ear infection)", label = TRUE, width = 0.5, fill = "RIAGENDR", color = "RIAGENDR", position = position_dodge(0.9), legend.title = "Gender", palette = "Paired", title = "Infection by DHA All Ages")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.011\nn = ', n2), paste('3\n0.056\nn = ', n3), paste('4\n4.08\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 60, label.x = 1)
#  stat_compare_means(comparisons = my_comparisons, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(125, 127, 129, 131)) #p.format to place actual p-values (uses wilcox.test)

#Stratifying by BMI
merge_sub_rmNA_age18 <- merge_sub_rmNA[merge_sub_rmNA$RIDAGEYR >= 18,]

merge_sub_rmNA_less30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI < 30,] #subsetting BMIs less than 30
merge_sub_rmNA_great30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI >= 30,] #subsetting BMIs greater than 30

with(merge_sub_rmNA_less30, table(HSQ520, Quartiles))
df_l <- data.frame(with(merge_sub_rmNA_less30, table(HSQ520, Quartiles)))
df_l <- df_l[c(3,6,9,12),]

with(merge_sub_rmNA_great30, table(HSQ520, Quartiles))
df_g <- data.frame(with(merge_sub_rmNA_great30, table(HSQ520, Quartiles)))
df_g <- df_g[c(3,6,9,12),]

kruskal.test(Freq ~ Quartiles,  data = df_l)
compare_means(Freq ~ Quartiles,  data = df_l)

kruskal.test(Freq ~ Quartiles,  data = df_g)
compare_means(Freq ~ Quartiles,  data = df_g)

##### PLOTS FOR BMI LESS THAN 30 #####

#my_comp1 <- list(c("2", "3"), c("3","4"), c("1","3"))
ggbarplot(df_l, x = "Quartiles", y = "Freq", xlab = "Quartiles of DHA Intake", ylab = "Frequency of Infection \n (Flu, pneumonia, ear infection)", label = TRUE, width = 0.5, fill = "Quartiles", color = "Quartiles", palette = c("lightskyblue", "lightblue2", "lightblue3", "lightblue4"), title = "Infection by DHA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.011\nn = ', n2), paste('3\n0.056\nn = ', n3), paste('4\n4.08\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 70, label.x = 1)      # Add global p-value
#  stat_compare_means(comparisons = my_comp1, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(119, 121, 125)) #p.format to place actual p-values (uses wilcox.test)

tal_l <- data.frame(tally(group_by(merge_sub_rmNA_less30, HSQ520, Quartiles, RIAGENDR)))
tal_l <- tal_l[c(13:20),]

#plotting by gender (uses Wilcoxon pairwise test like above)
ggbarplot(tal_l, x = "Quartiles", y = "n", xlab = "Quartiles of DHA Intake", ylab = "Frequency of Infection \n (Flu, pneumonia, ear infection)", label = TRUE, width = 0.5, fill = "RIAGENDR", color = "RIAGENDR", position = position_dodge(0.9), legend.title = "Gender", palette = "Paired", title = "Infection by DHA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.011\nn = ', n2), paste('3\n0.056\nn = ', n3), paste('4\n4.08\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 34, label.x = 1)

#comparing for each gender seperately
compare_means(n ~ Quartiles,  data = tal_l, group.by = "RIAGENDR")

##### PLOTS FOR BMI GREATER THAN 30 #####

#my_comp2 <- list(c("2", "3"), c("1", "3"), c("1", "4"))
ggbarplot(df_g, x = "Quartiles", y = "Freq", xlab = "Quartiles of DHA Intake", ylab = "Frequency of Infection \n (Flu, pneumonia, ear infection)", label = TRUE, width = 0.5, fill = "Quartiles", color = "Quartiles", palette = c("lightskyblue", "lightblue2", "lightblue3", "lightblue4"), title = "Infection by DHA All Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.011\nn = ', n2), paste('3\n0.056\nn = ', n3), paste('4\n4.08\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 30, label.x = 1)      # Add global p-value
#  stat_compare_means(comparisons = my_comp2, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(135, 144, 147)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
tal_g <- data.frame(tally(group_by(merge_sub_rmNA_great30, HSQ520, Quartiles, RIAGENDR)))
tal_g <- tal_g[c(12:19),]

ggbarplot(tal_g, x = "Quartiles", y = "n", xlab = "Quartiles of DHA Intake", ylab = "Frequency of Infection \n (Flu, pneumonia, ear infection)", label = TRUE, width = 0.5, fill = "RIAGENDR", color = "RIAGENDR", position = position_dodge(0.9), legend.title = "Gender", palette = "Paired", title = "Infection by DHA All Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.011\nn = ', n2), paste('3\n0.056\nn = ', n3), paste('4\n4.08\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 20, label.x = 1)

#comparing for each gender seperately
compare_means(n ~ Quartiles,  data = tal_g, group.by = "RIAGENDR")
