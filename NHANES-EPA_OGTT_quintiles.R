rm(list=ls())
rm(list=setdiff(ls(), c("ogtt", "data", "body", "glucose", "hba1c")))
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

#oral glucose tolerance test data
ogtt <- nhanes_load_data("OGTT_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
ogtt_sub <- ogtt[,c(1,50)] # subset by ID and OGTT

#body measures and BMI data
body <- nhanes_load_data("BMX_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
body_sub <- body[,c(1,58)] # subset by ID and BMI

#merging
merge <- merge(data, ogtt_sub, by="SEQN")
merge <- merge(merge, body_sub, by="SEQN")
#merge_sub <- merge[,c(1,42,142,219)]
#merge_sub_rmNA <- na.omit(merge_sub)

#subset by age
#merge$RIDAGEYR <- as.numeric(as.character(merge$RIDAGEYR)) #converting age to numeric vector
#newdata <- merge[merge$RIDAGEYR >= 18,] #ages older than 18

merge_sub <- merge[,c(1,42,5,6,9,142,219,220)] #ID, population weights, gender, age, race, EPA intake, OGTTs, BMI
merge_sub_rmNA <- na.omit(merge_sub)
hist(merge_sub_rmNA$DR1TP205)
plot(merge_sub_rmNA$DR1TP205, merge_sub_rmNA$LBXGLT)

#Quintiles
quantile(merge_sub_rmNA$DR1TP205, prob = c(0.20, 0.40, 0.60, 0.80, 1))

#Labeling data by quintiles
merge_sub_rmNA$Quintiles <- ifelse(merge_sub_rmNA$DR1TP205 <= quantile(merge_sub_rmNA$DR1TP205, 0.20),1,
                                   ifelse(merge_sub_rmNA$DR1TP205 > quantile(merge_sub_rmNA$DR1TP205, 0.2) & merge_sub_rmNA$DR1TP205 <= quantile(merge_sub_rmNA$DR1TP205, 0.4),2,
                                          ifelse(merge_sub_rmNA$DR1TP205 > quantile(merge_sub_rmNA$DR1TP205, 0.4) & merge_sub_rmNA$DR1TP205 <= quantile(merge_sub_rmNA$DR1TP205, 0.6),3,
                                          ifelse(merge_sub_rmNA$DR1TP205 > quantile(merge_sub_rmNA$DR1TP205, 0.6) & merge_sub_rmNA$DR1TP205 <= quantile(merge_sub_rmNA$DR1TP205, 0.8),3,5
                                          ))))

kruskal.test(LBXGLT ~ Quintiles,  data = merge_sub_rmNA)
compare_means(LBXGLT ~ Quintiles,  data = merge_sub_rmNA)

#Counting n individuals in each quartile
n1 <- sum(merge_sub_rmNA$Quintiles == "1")
n2 <- sum(merge_sub_rmNA$Quintiles == "2")
n3 <- sum(merge_sub_rmNA$Quintiles == "3")
n4 <- sum(merge_sub_rmNA$Quintiles == "4")
n5 <- sum(merge_sub_rmNA$Quintiles == "5")

#plotting all ages and genders

#YOU HAVE TO CHANGE THE SUBSET OF COMPARISONS BASED ON WHICH COMPARISONS WERE SIGNIFICANT IN THE COMPARE_MEANS FUNCTION ABOVE, SO THAT IT ONLY PLOTS THE SIGNIFICANT P-VALUES IN THE WILCOXON TEST
#my_comparisons <- list( c("1", "2"), c("1", "3"), c("1", "4"), c("2", "3"), c("2", "4"), c("3", "4") )
my_comparisons <- list(c("1", "2"), c("1", "3"), c("1", "5"))

ggerrorplot(merge_sub_rmNA, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA All Ages")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(method = "kruskal.test", label.y = 133, label.x = 1)+      # Add global p-value
  stat_compare_means(comparisons = my_comparisons, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(125, 127, 129, 131)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender
ggerrorplot(merge_sub_rmNA, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by EPA All Ages")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(118, 113, 122, 123))

ggerrorplot(merge_sub_rmNA, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by EPA All Ages")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(129, 119, 115, 122)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGLT ~ Quintiles,  data = merge_sub_rmNA, group.by = "RIAGENDR")

# Line facetted by gender
#p <- ggline(merge_sub_rmNA, x = "Quintiles", y = "LBXGLT",
#               #color = "Quintiles", palette = "npg",
#               add = "mean_se",
#               facet.by = "RIAGENDR", short.panel.labs = FALSE, title = "OGTT by EPA All Ages", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", panel.labs = list(RIAGENDR = c("Female", "Male")))
#p + stat_compare_means(method = "kruskal.test", label.y = 129, label.x = 1)

#To label one facet/panel with text
#ann_text <- data.frame(Quintiles = 4,LBXGLT = 129,lab = "Text", RIAGENDR = factor("Male",levels = c("Female", "Male")))
#p + stat_compare_means(method = "kruskal.test", label.y = 129, label.x = 1)+ geom_text(data = ann_text,label = "p-val")

#Just plot male by its self
merge_sub_rmNA_M <- merge_sub_rmNA[merge_sub_rmNA$RIAGENDR == "Male",] #subsetting males

my_comparisons2 <- list(c("2", "3"), c("1", "3"), c("1", "4"))
p1 <- ggerrorplot(merge_sub_rmNA_M, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA All Ages - Males")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(method = "kruskal.test", label.y = 136, label.x = 1)+      # Add global p-value
  stat_compare_means(comparisons = my_comparisons2, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(123, 129, 132)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_F <- merge_sub_rmNA[merge_sub_rmNA$RIAGENDR == "Female",] #subsetting mfeales

my_comparisons3 <- list(c("1", "3"))
p2 <- ggerrorplot(merge_sub_rmNA_F, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA All Ages - Females")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(method = "kruskal.test", label.y = 136, label.x = 1)+      # Add global p-value
  stat_compare_means(comparisons = my_comparisons3, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(125)) #p.format to place actual p-values (uses wilcox.test)
#use ..p.adj.. for adjusted p-values
ggarrange(p1,p2, ncol = 2, nrow = 1, common.legend = TRUE)

#Stratifying by BMI
merge_sub_rmNA_age18 <- merge_sub_rmNA[merge_sub_rmNA$RIDAGEYR >= 18,]

merge_sub_rmNA_less30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI < 30,] #subsetting BMIs less than 30
merge_sub_rmNA_great30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI >= 30,] #subsetting BMIs greater than 30

kruskal.test(LBXGLT ~ Quintiles,  data = merge_sub_rmNA_less30)
compare_means(LBXGLT ~ Quintiles,  data = merge_sub_rmNA_less30)

kruskal.test(LBXGLT ~ Quintiles,  data = merge_sub_rmNA_great30)
compare_means(LBXGLT ~ Quintiles,  data = merge_sub_rmNA_great30)

##### PLOTS FOR BMI LESS THAN 30 #####

my_comp1 <- list(c("2", "3"), c("3","4"), c("1","3"))
ggerrorplot(merge_sub_rmNA_less30, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", title = "OGTT by EPA Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(method = "kruskal.test", label.y = 129, label.x = 1)+
  stat_compare_means(comparisons = my_comp1, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(119, 121, 125)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
ggerrorplot(merge_sub_rmNA_less30, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by EPA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(117, 117, 123, 125))

ggerrorplot(merge_sub_rmNA_less30, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by EPA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(128, 119, 114, 119)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGLT ~ Quintiles,  data = merge_sub_rmNA_less30, group.by = "RIAGENDR")

#Just plot male by its self
merge_sub_rmNA_less30_M <- merge_sub_rmNA_less30[merge_sub_rmNA_less30$RIAGENDR == "Male",] #subsetting males

my_comp3 <- list(c("1", "3"))
p3 <- ggerrorplot(merge_sub_rmNA_less30_M, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA Ages > 18 BMI < 30", subtitle = "Males")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(method = "kruskal.test", label.y = 131, label.x = 1)+
  stat_compare_means(comparisons = my_comp3, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(127)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_less30_F <- merge_sub_rmNA_less30[merge_sub_rmNA_less30$RIAGENDR == "Female",] #subsetting mfeales

my_comp4 <- list(c("1", "3"))
p4 <- ggerrorplot(merge_sub_rmNA_less30_F, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA Ages > 18 BMI < 30", subtitle = "Females")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(method = "kruskal.test", label.y = 131, label.x = 1)+
  stat_compare_means(comparisons = my_comp4, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(123)) #p.format to place actual p-values (uses wilcox.test)

ggarrange(p3,p4, ncol = 2, nrow = 1, common.legend = TRUE)

##### PLOTS FOR BMI GREATER THAN 30 #####
kruskal.test(LBXGLT ~ Quintiles,  data = merge_sub_rmNA_great30)
compare_means(LBXGLT ~ Quintiles,  data = merge_sub_rmNA_great30)

my_comp2 <- list(c("2", "3"), c("1", "3"), c("1", "4"))

ggerrorplot(merge_sub_rmNA_great30, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", title = "OGTT by EPA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(method = "kruskal.test", label.y = 150, label.x = 1)+
  stat_compare_means(comparisons = my_comp2, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(135, 144, 147)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
ggerrorplot(merge_sub_rmNA_great30, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by EPA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(118, 113, 122, 123))

ggerrorplot(merge_sub_rmNA_great30, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by EPA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(159, 135, 131, 141)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGLT ~ Quintiles,  data = merge_sub_rmNA_great30, group.by = "RIAGENDR")

#Just plot male by its self
merge_sub_rmNA_great30_M <- merge_sub_rmNA_great30[merge_sub_rmNA_great30$RIAGENDR == "Male",] #subsetting males

my_comp5 <- list(c("2","3"), c("2", "4"), c("1","3"), c("1", "4"))
p5 <- ggerrorplot(merge_sub_rmNA_great30_M, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA Ages > 18 BMI 30+", subtitle = "Males")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(method = "kruskal.test", label.y = 180, label.x = 1)+
  stat_compare_means(comparisons = my_comp5, tip.length = c(0.003), label = "p.signif", hide.ns = TRUE, label.y = c(140,157,166,173)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_great30_F <- merge_sub_rmNA_great30[merge_sub_rmNA_great30$RIAGENDR == "Female",] #subsetting mfeales

#my_comp6 <- list(c("2", "3"))
p6 <- ggerrorplot(merge_sub_rmNA_great30_F, x = "Quintiles", y = "LBXGLT", xlab = "Quintiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA Ages > 18 BMI 30+", subtitle = "Females")+
  scale_x_discrete(name = '\nQuintiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4', '5'), 
                   labels = c(paste('1\n0.002\nn =', n1), paste('2\n0.005\nn = ', n2), paste('3\n0.01\nn = ', n3), paste('4\n0.02\nn = ', n4), paste('5\n2.69\nn = ', n5)))+
  stat_compare_means(method = "kruskal.test", label.y = 180, label.x = 1)
#stat_compare_means(comparisons = my_comp6, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(121)) #p.format to place actual p-values (uses wilcox.test)

ggarrange(p5,p6, ncol = 2, nrow = 1, common.legend = TRUE)