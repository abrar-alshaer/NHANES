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

#Quartiles
quantile(merge_sub_rmNA$DR1TP205)
#q1 <- subset(merge_sub_rmNA, DR1TP205 <= quantile(DR1TP205, 0.25))
#q2 <- subset(merge_sub_rmNA, DR1TP205 <= quantile(DR1TP205, 0.5) & DR1TP205 > quantile(DR1TP205, 0.25))
#q3 <- subset(merge_sub_rmNA, DR1TP205 <= quantile(DR1TP205, 0.75) & DR1TP205 > quantile(DR1TP205, 0.5))

#Labeling data by quartiles
merge_sub_rmNA$Quartiles <- ifelse(merge_sub_rmNA$DR1TP205 <= quantile(merge_sub_rmNA$DR1TP205, 0.25),1,
                                   ifelse(merge_sub_rmNA$DR1TP205 > quantile(merge_sub_rmNA$DR1TP205, 0.25) & merge_sub_rmNA$DR1TP205 <= quantile(merge_sub_rmNA$DR1TP205, 0.5),2,
                                          ifelse(merge_sub_rmNA$DR1TP205 > quantile(merge_sub_rmNA$DR1TP205, 0.5) & merge_sub_rmNA$DR1TP205 <= quantile(merge_sub_rmNA$DR1TP205, 0.75),3,4
                                          )))

kruskal.test(LBXGLT ~ Quartiles,  data = merge_sub_rmNA)
compare_means(LBXGLT ~ Quartiles,  data = merge_sub_rmNA)

#Counting n individuals in each quartile
n1 <- sum(merge_sub_rmNA$Quartiles == "1")
n2 <- sum(merge_sub_rmNA$Quartiles == "2")
n3 <- sum(merge_sub_rmNA$Quartiles == "3")
n4 <- sum(merge_sub_rmNA$Quartiles == "4")

#plotting all ages and genders

#YOU HAVE TO CHANGE THE SUBSET OF COMPARISONS BASED ON WHICH COMPARISONS WERE SIGNIFICANT IN THE COMPARE_MEANS FUNCTION ABOVE, SO THAT IT ONLY PLOTS THE SIGNIFICANT P-VALUES IN THE WILCOXON TEST
#my_comparisons <- list( c("1", "2"), c("1", "3"), c("1", "4"), c("2", "3"), c("2", "4"), c("3", "4") )
my_comparisons <- list(c("1", "2"), c("2", "3"), c("1", "3"), c("1", "4"))

ggerrorplot(merge_sub_rmNA, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA All Ages")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 133, label.x = 1)+      # Add global p-value
  stat_compare_means(comparisons = my_comparisons, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(125, 127, 129, 131)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender
ggerrorplot(merge_sub_rmNA, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by EPA All Ages")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(118, 113, 122, 123))

ggerrorplot(merge_sub_rmNA, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by EPA All Ages")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(129, 119, 115, 122)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGLT ~ Quartiles,  data = merge_sub_rmNA, group.by = "RIAGENDR")

# Line facetted by gender
#p <- ggline(merge_sub_rmNA, x = "Quartiles", y = "LBXGLT",
#               #color = "Quartiles", palette = "npg",
#               add = "mean_se",
#               facet.by = "RIAGENDR", short.panel.labs = FALSE, title = "OGTT by EPA All Ages", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", panel.labs = list(RIAGENDR = c("Female", "Male")))
#p + stat_compare_means(method = "kruskal.test", label.y = 129, label.x = 1)

#To label one facet/panel with text
#ann_text <- data.frame(Quartiles = 4,LBXGLT = 129,lab = "Text", RIAGENDR = factor("Male",levels = c("Female", "Male")))
#p + stat_compare_means(method = "kruskal.test", label.y = 129, label.x = 1)+ geom_text(data = ann_text,label = "p-val")

#Just plot male by its self
merge_sub_rmNA_M <- merge_sub_rmNA[merge_sub_rmNA$RIAGENDR == "Male",] #subsetting males

my_comparisons2 <- list(c("2", "3"), c("1", "3"), c("1", "4"))
p1 <- ggerrorplot(merge_sub_rmNA_M, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA All Ages - Males")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 136, label.x = 1)+      # Add global p-value
  stat_compare_means(comparisons = my_comparisons2, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(123, 129, 132)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_F <- merge_sub_rmNA[merge_sub_rmNA$RIAGENDR == "Female",] #subsetting mfeales

my_comparisons3 <- list(c("1", "3"))
p2 <- ggerrorplot(merge_sub_rmNA_F, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA All Ages - Females")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 136, label.x = 1)+      # Add global p-value
  stat_compare_means(comparisons = my_comparisons3, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(125)) #p.format to place actual p-values (uses wilcox.test)
  #use ..p.adj.. for adjusted p-values
ggarrange(p1,p2, ncol = 2, nrow = 1, common.legend = TRUE)

#Stratifying by BMI
merge_sub_rmNA_age18 <- merge_sub_rmNA[merge_sub_rmNA$RIDAGEYR >= 18,]

merge_sub_rmNA_less30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI < 30,] #subsetting BMIs less than 30
merge_sub_rmNA_great30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI >= 30,] #subsetting BMIs greater than 30

kruskal.test(LBXGLT ~ Quartiles,  data = merge_sub_rmNA_less30)
compare_means(LBXGLT ~ Quartiles,  data = merge_sub_rmNA_less30)

kruskal.test(LBXGLT ~ Quartiles,  data = merge_sub_rmNA_great30)
compare_means(LBXGLT ~ Quartiles,  data = merge_sub_rmNA_great30)

##### PLOTS FOR BMI LESS THAN 30 #####

my_comp1 <- list(c("2", "3"), c("3","4"), c("1","3"))
ggerrorplot(merge_sub_rmNA_less30, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", title = "OGTT by EPA Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 129, label.x = 1)+
  stat_compare_means(comparisons = my_comp1, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(119, 121, 125)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
ggerrorplot(merge_sub_rmNA_less30, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by EPA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(117, 117, 123, 125))

ggerrorplot(merge_sub_rmNA_less30, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by EPA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(128, 119, 114, 119)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGLT ~ Quartiles,  data = merge_sub_rmNA_less30, group.by = "RIAGENDR")

#Just plot male by its self
merge_sub_rmNA_less30_M <- merge_sub_rmNA_less30[merge_sub_rmNA_less30$RIAGENDR == "Male",] #subsetting males

my_comp3 <- list(c("1", "3"))
p3 <- ggerrorplot(merge_sub_rmNA_less30_M, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA Ages > 18 BMI < 30", subtitle = "Males")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 131, label.x = 1)+
  stat_compare_means(comparisons = my_comp3, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(127)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_less30_F <- merge_sub_rmNA_less30[merge_sub_rmNA_less30$RIAGENDR == "Female",] #subsetting mfeales

my_comp4 <- list(c("1", "3"))
p4 <- ggerrorplot(merge_sub_rmNA_less30_F, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA Ages > 18 BMI < 30", subtitle = "Females")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 131, label.x = 1)+
  stat_compare_means(comparisons = my_comp4, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(123)) #p.format to place actual p-values (uses wilcox.test)

ggarrange(p3,p4, ncol = 2, nrow = 1, common.legend = TRUE)

##### PLOTS FOR BMI GREATER THAN 30 #####
kruskal.test(LBXGLT ~ Quartiles,  data = merge_sub_rmNA_great30)
compare_means(LBXGLT ~ Quartiles,  data = merge_sub_rmNA_great30)

my_comp2 <- list(c("2", "3"), c("1", "3"), c("1", "4"))

ggerrorplot(merge_sub_rmNA_great30, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", title = "OGTT by EPA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 150, label.x = 1)+
  stat_compare_means(comparisons = my_comp2, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(135, 144, 147)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
ggerrorplot(merge_sub_rmNA_great30, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by EPA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(118, 113, 122, 123))

ggerrorplot(merge_sub_rmNA_great30, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by EPA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(159, 135, 131, 141)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGLT ~ Quartiles,  data = merge_sub_rmNA_great30, group.by = "RIAGENDR")

#Just plot male by its self
merge_sub_rmNA_great30_M <- merge_sub_rmNA_great30[merge_sub_rmNA_great30$RIAGENDR == "Male",] #subsetting males

my_comp5 <- list(c("2","3"), c("2", "4"), c("1","3"), c("1", "4"))
p5 <- ggerrorplot(merge_sub_rmNA_great30_M, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA Ages > 18 BMI 30+", subtitle = "Males")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 180, label.x = 1)+
  stat_compare_means(comparisons = my_comp5, tip.length = c(0.003), label = "p.signif", hide.ns = TRUE, label.y = c(140,157,166,173)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_great30_F <- merge_sub_rmNA_great30[merge_sub_rmNA_great30$RIAGENDR == "Female",] #subsetting mfeales

#my_comp6 <- list(c("2", "3"))
p6 <- ggerrorplot(merge_sub_rmNA_great30_F, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by EPA Ages > 18 BMI 30+", subtitle = "Females")+
  scale_x_discrete(name = '\nQuartiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.008\nn = ', n2), paste('3\n0.016\nn = ', n3), paste('4\n2.69\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 180, label.x = 1)
#stat_compare_means(comparisons = my_comp6, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(121)) #p.format to place actual p-values (uses wilcox.test)

ggarrange(p5,p6, ncol = 2, nrow = 1, common.legend = TRUE)

########Removing Zeros#########
#removing zeros
merge_sub_rmNA_z <- merge_sub_rmNA[merge_sub_rmNA$DR1TP205 > 0,]
#without zeros
merge_sub_rmNA_z$Quartiles <- ifelse(merge_sub_rmNA_z$DR1TP205 <= quantile(merge_sub_rmNA_z$DR1TP205, 0.25),1,
                                     ifelse(merge_sub_rmNA_z$DR1TP205 > quantile(merge_sub_rmNA_z$DR1TP205, 0.25) & merge_sub_rmNA_z$DR1TP205 <= quantile(merge_sub_rmNA_z$DR1TP205, 0.5),2,
                                            ifelse(merge_sub_rmNA_z$DR1TP205 > quantile(merge_sub_rmNA_z$DR1TP205, 0.5) & merge_sub_rmNA_z$DR1TP205 <= quantile(merge_sub_rmNA_z$DR1TP205, 0.75),3,4
                                            )))

#rename column
#merge_sub_rmNA <- rename(merge_sub_rmNA, Quartiles = Quantiles)

#Graphing
# Plot the mean of OGTT by EPA quartiles
plotmeans(LBXGLT ~ Quartiles, data = merge_sub_rmNA, frame = TRUE, main = "OGTT by EPA - All Ages")

# Plot the mean of OGTT by EPA quartiles WITHOUT zeros
plotmeans(LBXGLT ~ Quartiles, data = merge_sub_rmNA_z, frame = TRUE, main = "OGTT by EPA - Ages > 18", xlab = "EPA Quartiles", ylab = "OGTT", p=0.95)

#big dot plot
stripchart(LBXGLT ~ Quartiles, data = merge_sub_rmNA_z,
           frame = FALSE, vertical = TRUE,
           method = "jitter", pch = c(21, 18, 16),
           col = c("#999999", "#E69F00", "#55B4E9", "red"),
           main = "OGTT by EPA - Ages > 18", xlab = "EPA Quartiles", ylab = "OGTT")

#calculating quantiles with RNHANES
#nhanes_quantile(merge_sub_rmNA_z, "DR1TP205", "Quartiles", "WTINT2YR", c(0.5, 0.95, 0.99))

#convert Quartiles to characters
merge_sub_rmNA$Quartiles <- as.character(as.numeric(merge_sub_rmNA$Quartiles))

############Extra Code################
#Sorting dataframe
merge_sub_rmNA <- merge_sub_rmNA[order(merge_sub_rmNA$Quartiles),] 

#stratifying by age groups
merge_sub_rmNA_z$AgeGroup <- ifelse(merge_sub_rmNA_z$RIDAGEYR <= quantile(merge_sub_rmNA_z$RIDAGEYR, 0.25),1,
                                    ifelse(merge_sub_rmNA_z$RIDAGEYR > quantile(merge_sub_rmNA_z$RIDAGEYR, 0.25) & merge_sub_rmNA_z$RIDAGEYR <= quantile(merge_sub_rmNA_z$RIDAGEYR, 0.5),2,
                                           ifelse(merge_sub_rmNA_z$RIDAGEYR > quantile(merge_sub_rmNA_z$RIDAGEYR, 0.5) & merge_sub_rmNA_z$RIDAGEYR <= quantile(merge_sub_rmNA_z$RIDAGEYR, 0.75),3,4
                                           )))
merge_sub_rmNA_z$AgeGroup <- as.character(as.numeric(merge_sub_rmNA_z$AgeGroup)) #convert age groups to characters

ggerrorplot(merge_sub_rmNA, x = "Quartiles", y = "LBXGLT", 
            desc_stat = "mean_se", error.plot = "errorbar", add = "mean",
            color = "Quartiles", palette = "jco",
            position = position_dodge(0.3)     # Adjust the space between bars
)

ggline(merge_sub_rmNA, x = "Quartiles", y = "LBXGLT", 
       add = c("mean_se"),
       color = "Quartiles", palette = "jco")

########Printing to PDFs#############
#OUTPUT PLOTS TO FILE - for OGTT by EPA Stratifying by Ages
pdf("EPA_OGTT_Ages_stats.pdf")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR >= 12 & merge_sub_rmNA$RIDAGEYR <= 22),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.25),1,
                         ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.25) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.5),2,
                                ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.5) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.75),3,4
                                )))
p1 <- ggline(ageQ, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean_se"), title = "OGTT by EPA Ages Q1 (12-22)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(LBXGLT ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p1 <- p1 + annotation_custom(grob)

kruskal.test(LBXGLT ~ Quartiles,  data = ageQ)
compare_means(LBXGLT ~ Quartiles,  data = ageQ)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 22 & merge_sub_rmNA$RIDAGEYR <= 39),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.25),1,
                         ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.25) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.5),2,
                                ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.5) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.75),3,4
                                )))
p2<- ggline(ageQ, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean_se"), title = "OGTT by EPA Ages Q2 (23-39")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(LBXGLT ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p2 <- p2 + annotation_custom(grob)

kruskal.test(LBXGLT ~ Quartiles,  data = ageQ)
compare_means(LBXGLT ~ Quartiles,  data = ageQ)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 39 & merge_sub_rmNA$RIDAGEYR <= 55),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.25),1,
                         ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.25) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.5),2,
                                ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.5) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.75),3,4
                                )))
p3 <- ggline(ageQ, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean_se"), title = "OGTT by EPA Ages Q3 (40-55)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(LBXGLT ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p3 <- p3 + annotation_custom(grob)

kruskal.test(LBXGLT ~ Quartiles,  data = ageQ)
compare_means(LBXGLT ~ Quartiles,  data = ageQ)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 55 & merge_sub_rmNA$RIDAGEYR <= 79),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.25),1,
                         ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.25) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.5),2,
                                ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.5) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.75),3,4
                                )))
p4 <- ggline(ageQ, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean_se"), title = "OGTT by EPA Ages Q4 (56-79)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(LBXGLT ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p4 <- p4 + annotation_custom(grob)

kruskal.test(LBXGLT ~ Quartiles,  data = ageQ)
compare_means(LBXGLT ~ Quartiles,  data = ageQ)

ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2, common.legend = TRUE)

############################################### INCLUDED GENDER BELOW

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR >= 12 & merge_sub_rmNA$RIDAGEYR <= 22),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.25),1,
                         ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.25) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.5),2,
                                ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.5) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.75),3,4
                                )))
p1 <- ggline(ageQ, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "OGTT by EPA Ages Q1 (12-22)")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 22 & merge_sub_rmNA$RIDAGEYR <= 39),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.25),1,
                         ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.25) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.5),2,
                                ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.5) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.75),3,4
                                )))
p2<- ggline(ageQ, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "OGTT by EPA Ages Q2 (23-39)")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 39 & merge_sub_rmNA$RIDAGEYR <= 55),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.25),1,
                         ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.25) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.5),2,
                                ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.5) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.75),3,4
                                )))
p3 <- ggline(ageQ, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "OGTT by EPA Ages Q3 (40-55)")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 55 & merge_sub_rmNA$RIDAGEYR <= 79),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.25),1,
                         ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.25) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.5),2,
                                ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.5) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.75),3,4
                                )))
p4 <- ggline(ageQ, x = "Quartiles", y = "LBXGLT", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "OGTT by EPA Ages Q4 (56-79)")

ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2, common.legend = TRUE)

############################################### INCLUDED RACE BELOW

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR >= 12 & merge_sub_rmNA$RIDAGEYR <= 22),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.25),1,
                         ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.25) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.5),2,
                                ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.5) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.75),3,4
                                )))
#Plotting by race
p5 <- ggerrorplot(ageQ, x = "Quartiles", y = "LBXGLT", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "OGTT by EPA", subtitle = "Ages Q1 (12-22)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 22 & merge_sub_rmNA$RIDAGEYR <= 39),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.25),1,
                         ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.25) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.5),2,
                                ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.5) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.75),3,4
                                )))
#Plotting by race
p6 <- ggerrorplot(ageQ, x = "Quartiles", y = "LBXGLT", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "OGTT by EPA", subtitle = "Ages Q2 (23-39)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 39 & merge_sub_rmNA$RIDAGEYR <= 55),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.25),1,
                         ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.25) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.5),2,
                                ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.5) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.75),3,4
                                )))
#Plotting by race
p7 <- ggerrorplot(ageQ, x = "Quartiles", y = "LBXGLT", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "OGTT by EPA", subtitle = "Ages Q3 (40-55)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 55 & merge_sub_rmNA$RIDAGEYR <= 79),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.25),1,
                         ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.25) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.5),2,
                                ifelse(ageQ$DR1TP205 > quantile(ageQ$DR1TP205, 0.5) & ageQ$DR1TP205 <= quantile(ageQ$DR1TP205, 0.75),3,4
                                )))
#Plotting by race
p8 <- ggerrorplot(ageQ, x = "Quartiles", y = "LBXGLT", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of EPA Intake", ylab = "Two Hour Glucose (mg/dl)",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "OGTT by EPA", subtitle = "Ages Q4 (56-79)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ggarrange(p5,p6,p7,p8, ncol = 2, nrow = 2, common.legend = TRUE)

dev.off() # flush that pdf! Closes the PDF so you can open it
