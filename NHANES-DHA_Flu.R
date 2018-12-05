rm(list=ls())
rm(list=setdiff(ls(), c("data2", "data", "infect", "infect2", "body", "body2")))
setwd("C:/Users/Abrar/OneDrive - University of North Carolina at Chapel Hill/Shaikh Lab/NHANES/")
library(RNHANES)
library(gplots)
library("dplyr")
library("ggpubr")
library(tidyverse)
library(plyr)

#load in file for Dietary Interview - Total Nutrient Intakes, First Day
nhanes_load_data("DR1TOT_H", "2013-2014")
nhanes_load_data("DR1TOT_G", "2011-2012")

#diet data
data <- nhanes_load_data("DR1TOT_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
data2 <- nhanes_load_data("DR1TOT_G", "2011-2012", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)

data_combo <- rbind.fill(data,data2)

#diet data
infect <- nhanes_load_data("CSQ_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE)
infect_sub <- infect[,c(1,75)] # subset by ID and flu

infect2 <- nhanes_load_data("CSQ_G", "2011-2012", cache = "./nhanes_data", demographics = TRUE)
infect_sub2 <- infect[,c(1,75)] # subset by ID and flu

infect_sub_combo <- rbind.fill(infect_sub, infect_sub2)

#body measures and BMI data
body <- nhanes_load_data("BMX_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
body_sub <- body[,c(1,58)] # subset by ID and BMI

body2 <- nhanes_load_data("BMX_G", "2011-2012", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
body_sub2 <- body[,c(1,58)] # subset by ID and BMI

body_sub_combo <- rbind.fill(body_sub, body_sub2)

#merging
merge <- merge(data_combo, infect_sub_combo, by="SEQN")
merge <- merge(merge, body_sub_combo, by="SEQN")
#merge_sub <- merge[,c(1,42,142,219)]
#merge_sub_rmNA <- na.omit(merge_sub)

#subset by age
#merge$RIDAGEYR <- as.numeric(as.character(merge$RIDAGEYR)) #converting age to numeric vector
#newdata <- merge[merge$RIDAGEYR >= 18,] #ages older than 18

merge_sub <- merge[,c(1,42,5,6,9,144,220,221)] #ID, population weights, gender, age, race, DHA intake, infection info, BMI
merge_sub_rmNA <- na.omit(merge_sub)
hist(merge_sub_rmNA$DR1TP226)
plot(merge_sub_rmNA$DR1TP226, merge_sub_rmNA$LBXGLT)

#Quartiles
quantile(merge_sub_rmNA$DR1TP226)

#Half
quantile(merge_sub_rmNA$DR1TP226, prob = c(0.5,1))

merge_sub_rmNA <- merge_sub_rmNA[merge_sub_rmNA$CSQ200 == 1,] #only YES responses
#Quartiles
quantile(merge_sub_rmNA$DR1TP226)

#Labeling data by high and low (50% & 100%)
merge_sub_rmNA$Quartiles <- ifelse(merge_sub_rmNA$DR1TP226 <= quantile(merge_sub_rmNA$DR1TP226, 0.5),1,2)

#Quartiles
merge_sub_rmNA$Quartiles <- ifelse(merge_sub_rmNA$DR1TP226 <= quantile(merge_sub_rmNA$DR1TP226, 0.25),1,
                                   ifelse(merge_sub_rmNA$DR1TP226 > quantile(merge_sub_rmNA$DR1TP226, 0.25) & merge_sub_rmNA$DR1TP226 <= quantile(merge_sub_rmNA$DR1TP226, 0.5),2,
                                          ifelse(merge_sub_rmNA$DR1TP226 > quantile(merge_sub_rmNA$DR1TP226, 0.5) & merge_sub_rmNA$DR1TP226 <= quantile(merge_sub_rmNA$DR1TP226, 0.75),3,4
                                          )))

#Counting n individuals who were or were not infected in each quartile
with(merge_sub_rmNA, table(CSQ200, Quartiles))

df <- data.frame(with(merge_sub_rmNA, table(CSQ200, Quartiles)))

kruskal.test(Freq ~ Quartiles,  data = df)
compare_means(Freq ~ Quartiles,  data = df)

#Counting n individuals in each quartile
n1 <- sum(merge_sub_rmNA$Quartiles == "1")
n2 <- sum(merge_sub_rmNA$Quartiles == "2")
n3 <- sum(merge_sub_rmNA$Quartiles == "3")
n4 <- sum(merge_sub_rmNA$Quartiles == "4")

#plotting all ages and genders
#my_comparisons <- list(c("1", "2"), c("2", "3"), c("1", "3"), c("1", "4"))

ggbarplot(df, x = "Quartiles", y = "Freq", xlab = "Quartiles of DHA Intake", ylab = "Frequency of Infection \n (Flu/cold)", label = TRUE, width = 0.5, fill = "Quartiles", color = "Quartiles", palette = c("lightskyblue", "lightblue2", "lightblue3", "lightblue4"), title = "Infection by DHA All Ages")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.002'), paste('2\n0.009'), paste('3\n0.048'), paste('4\n1.62')))
#  stat_compare_means(method = "kruskal.test", label.y = 330, label.x = 1)      # Add global p-value
#stat_compare_means(comparisons = my_comparisons, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(125, 127, 129, 131)) #p.format to place actual p-values (uses wilcox.test)

tal <- data.frame(tally(group_by(merge_sub_rmNA, CSQ200, Quartiles, RIAGENDR)))

#wilcox.test(n ~ Quartiles, data = tal, paired = TRUE)
#comparing for each gender seperately
compare_means(n ~ Quartiles,  data = tal, group.by = "RIAGENDR")

females <- c(216,96,136,132)
males <- c(68,72,84,88)
wilcox.test(females,males, paired = FALSE)
pairwise.wilcox.test(females,males) 

#plotting by gender
ggbarplot(tal, x = "Quartiles", y = "n", xlab = "Quartiles of DHA Intake", ylab = "Frequency of Infection \n (Flu/cold)", label = TRUE, width = 0.5, fill = "RIAGENDR", color = "RIAGENDR", position = position_dodge(0.9), legend.title = "Gender", palette = "Paired", title = "Infection by DHA All Ages")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.002'), paste('2\n0.009'), paste('3\n0.048'), paste('4\n1.62')))
#  stat_compare_means(method = "kruskal.test", label.y = 250, label.x = 1)
#  stat_compare_means(comparisons = my_comparisons, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(125, 127, 129, 131)) #p.format to place actual p-values (uses wilcox.test)

#Stratifying by BMI
merge_sub_rmNA_age18 <- merge_sub_rmNA[merge_sub_rmNA$RIDAGEYR >= 18,]

merge_sub_rmNA_less30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI < 30,] #subsetting BMIs less than 30
merge_sub_rmNA_great30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI >= 30,] #subsetting BMIs greater than 30

with(merge_sub_rmNA_less30, table(CSQ200, Quartiles))
df_l <- data.frame(with(merge_sub_rmNA_less30, table(CSQ200, Quartiles)))

with(merge_sub_rmNA_great30, table(CSQ200, Quartiles))
df_g <- data.frame(with(merge_sub_rmNA_great30, table(CSQ200, Quartiles)))

kruskal.test(Freq ~ Quartiles,  data = df_l)
compare_means(Freq ~ Quartiles,  data = df_l)

kruskal.test(Freq ~ Quartiles,  data = df_g)
compare_means(Freq ~ Quartiles,  data = df_g)

##### PLOTS FOR BMI LESS THAN 30 #####

#my_comp1 <- list(c("2", "3"), c("3","4"), c("1","3"))
ggbarplot(df_l, x = "Quartiles", y = "Freq", xlab = "Quartiles of DHA Intake", ylab = "Frequency of Infection \n (Flu/cold)", label = TRUE, width = 0.5, fill = "Quartiles", color = "Quartiles", palette = c("lightskyblue", "lightblue2", "lightblue3", "lightblue4"), title = "Infection by DHA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.002'), paste('2\n0.009'), paste('3\n0.048'), paste('4\n1.62')))
#  stat_compare_means(method = "kruskal.test", label.y = 60, label.x = 1)      # Add global p-value
#  stat_compare_means(comparisons = my_comp1, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(119, 121, 125)) #p.format to place actual p-values (uses wilcox.test)

tal_l <- data.frame(tally(group_by(merge_sub_rmNA_less30, CSQ200, Quartiles, RIAGENDR)))

#plotting by gender (uses Wilcoxon pairwise test like above)
ggbarplot(tal_l, x = "Quartiles", y = "n", xlab = "Quartiles of DHA Intake", ylab = "Frequency of Infection \n (Flu/cold)", label = TRUE, width = 0.5, fill = "RIAGENDR", color = "RIAGENDR", position = position_dodge(0.9), legend.title = "Gender", palette = "Paired", title = "Infection by DHA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.002'), paste('2\n0.009'), paste('3\n0.048'), paste('4\n1.62')))
#  stat_compare_means(method = "kruskal.test", label.y = 34, label.x = 1)

#comparing for each gender seperately
compare_means(n ~ Quartiles,  data = tal_l, group.by = "RIAGENDR")

##### PLOTS FOR BMI GREATER THAN 30 #####

#my_comp2 <- list(c("2", "3"), c("1", "3"), c("1", "4"))
ggbarplot(df_g, x = "Quartiles", y = "Freq", xlab = "Quartiles of DHA Intake", ylab = "Frequency of Infection \n (Flu/cold)", label = TRUE, width = 0.5, fill = "Quartiles", color = "Quartiles", palette = c("lightskyblue", "lightblue2", "lightblue3", "lightblue4"), title = "Infection by DHA All Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.002'), paste('2\n0.009'), paste('3\n0.048'), paste('4\n1.62')))
#  stat_compare_means(method = "kruskal.test", label.y = 30, label.x = 1)      # Add global p-value
#  stat_compare_means(comparisons = my_comp2, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(135, 144, 147)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
tal_g <- data.frame(tally(group_by(merge_sub_rmNA_great30, CSQ200, Quartiles, RIAGENDR)))

ggbarplot(tal_g, x = "Quartiles", y = "n", xlab = "Quartiles of DHA Intake", ylab = "Frequency of Infection \n (Flu/cold)", label = TRUE, width = 0.5, fill = "RIAGENDR", color = "RIAGENDR", position = position_dodge(0.9), legend.title = "Gender", palette = "Paired", title = "Infection by DHA All Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.002'), paste('2\n0.009'), paste('3\n0.048'), paste('4\n1.62')))
#  stat_compare_means(method = "kruskal.test", label.y = 37, label.x = 1)

#comparing for each gender seperately
compare_means(n ~ Quartiles,  data = tal_g, group.by = "RIAGENDR")
