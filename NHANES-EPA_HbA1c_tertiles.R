rm(list=ls())
rm(list=setdiff(ls(), c("data", "body", "glucose", "hba1c")))
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
hba1c <- nhanes_load_data("GHB_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
hba1c_sub <- hba1c[,c(1,49)] # subset by ID and HbA1c

#body measures and BMI data
body <- nhanes_load_data("BMX_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
body_sub <- body[,c(1,58)] # subset by ID and BMI

#merging
merge <- merge(data, hba1c_sub, by="SEQN")
merge <- merge(merge, body_sub, by="SEQN")
#merge_sub <- merge[,c(1,42,142,219)]
#merge_sub_rmNA <- na.omit(merge_sub)

#subset by age
#merge$RIDAGEYR <- as.numeric(as.character(merge$RIDAGEYR)) #converting age to numeric vector
#newdata <- merge[merge$RIDAGEYR >= 18,] #ages older than 18

merge_sub <- merge[,c(1,42,5,6,9,142,219,220)] #ID, population weights, gender, age, race, EPA intake, Glycohemoglobins, BMI
merge_sub_rmNA <- na.omit(merge_sub)
hist(merge_sub_rmNA$DR1TP205)
plot(merge_sub_rmNA$DR1TP205, merge_sub_rmNA$LBXGH)

#Tertiles
quantile(merge_sub_rmNA$DR1TP205, prob = c(0.33, 0.67, 1))

#Labeling data by tertiles
merge_sub_rmNA$Tertiles <- ifelse(merge_sub_rmNA$DR1TP205 <= quantile(merge_sub_rmNA$DR1TP205, 0.33),1,
                                  ifelse(merge_sub_rmNA$DR1TP205 > quantile(merge_sub_rmNA$DR1TP205, 0.33) & merge_sub_rmNA$DR1TP205 <= quantile(merge_sub_rmNA$DR1TP205, 0.67),2,3
                                  ))

kruskal.test(LBXGH ~ Tertiles,  data = merge_sub_rmNA)
compare_means(LBXGH ~ Tertiles,  data = merge_sub_rmNA)

n1 <- sum(merge_sub_rmNA$Tertiles == "1")
n2 <- sum(merge_sub_rmNA$Tertiles == "2")
n3 <- sum(merge_sub_rmNA$Tertiles == "3")

#plotting all ages and genders

#YOU HAVE TO CHANGE THE SUBSET OF COMPARISONS BASED ON WHICH COMPARISONS WERE SIGNIFICANT IN THE COMPARE_MEANS FUNCTION ABOVE, SO THAT IT ONLY PLOTS THE SIGNIFICANT P-VALUES IN THE WILCOXON TEST
#my_comparisons <- list( c("1", "2"), c("1", "3"), c("1", "4"), c("2", "3"), c("2", "4"), c("3", "4") )
my_comparisons <- list(c("1", "3"))

ggerrorplot(merge_sub_rmNA, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Glycohemoglobin by EPA All Ages")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 5.7, label.x = 1)
#  stat_compare_means(comparisons = my_comparisons, tip.length = c(0.0005), label = "p.format", hide.ns = TRUE, label.y = c(5.71)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender
ggerrorplot(merge_sub_rmNA, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "Glycohemoglobin by EPA All Ages")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(5.73, 5.712, 5.62, 5.70))

ggerrorplot(merge_sub_rmNA, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "Glycohemoglobin by EPA All Ages")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(5.73, 5.712, 5.70)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGH ~ Tertiles,  data = merge_sub_rmNA, group.by = "RIAGENDR")

# Line facetted by gender
#p <- ggerrorplot(merge_sub_rmNA, x = "Tertiles", y = "LBXGH",
#               #color = "Tertiles", palette = "npg",
#               add = "mean_se",
#               facet.by = "RIAGENDR", short.panel.labs = FALSE, title = "Glycohemoglobin by EPA All Ages", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", panel.labs = list(RIAGENDR = c("Female", "Male")))
#p + stat_compare_means(method = "kruskal.test", label.y = 129, label.x = 1)

#To label one facet/panel with text
#ann_text <- data.frame(Tertiles = 4,LBXGH = 129,lab = "Text", RIAGENDR = factor("Male",levels = c("Female", "Male")))
#p + stat_compare_means(method = "kruskal.test", label.y = 129, label.x = 1)+ geom_text(data = ann_text,label = "p-val")

#Just plot male by its self
merge_sub_rmNA_M <- merge_sub_rmNA[merge_sub_rmNA$RIAGENDR == "Male",] #subsetting males

#my_comparisons2 <- list(c("2", "3"), c("1", "3"), c("1", "4"))
p1 <- ggerrorplot(merge_sub_rmNA_M, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Glycohemoglobin by EPA All Ages - Males")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 5.73, label.x = 1)      # Add global p-value
#  stat_compare_means(comparisons = my_comparisons2, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(123, 129, 132)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_F <- merge_sub_rmNA[merge_sub_rmNA$RIAGENDR == "Female",] #subsetting mfeales

#my_comparisons3 <- list(c("1", "3"))
p2 <- ggerrorplot(merge_sub_rmNA_F, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Glycohemoglobin by EPA All Ages - Females")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 5.73, label.x = 1)      # Add global p-value
#  stat_compare_means(comparisons = my_comparisons3, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(123)) #p.format to place actual p-values (uses wilcox.test)
#use ..p.adj.. for adjusted p-values
ggarrange(p1,p2, ncol = 2, nrow = 1, common.legend = TRUE)

#Stratifying by BMI
merge_sub_rmNA_age18 <- merge_sub_rmNA[merge_sub_rmNA$RIDAGEYR >= 18,]

merge_sub_rmNA_less30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI < 30,] #subsetting BMIs less than 30
merge_sub_rmNA_great30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI >= 30,] #subsetting BMIs greater than 30

kruskal.test(LBXGH ~ Tertiles,  data = merge_sub_rmNA_less30)
compare_means(LBXGH ~ Tertiles,  data = merge_sub_rmNA_less30)

kruskal.test(LBXGH ~ Tertiles,  data = merge_sub_rmNA_great30)
compare_means(LBXGH ~ Tertiles,  data = merge_sub_rmNA_great30)

##### PLOTS FOR BMI LESS THAN 30 #####

my_comp1 <- list(c("1", "2"))
ggerrorplot(merge_sub_rmNA_less30, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", title = "Glycohemoglobin by EPA Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 5.7, label.x = 1)+
  stat_compare_means(comparisons = my_comp1, tip.length = c(0.0005), label = "p.format", hide.ns = TRUE, label.y = c(5.65)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
ggerrorplot(merge_sub_rmNA_less30, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "Glycohemoglobin by EPA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(5.68,5.69,5.52,5.66))

ggerrorplot(merge_sub_rmNA_less30, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "Glycohemoglobin by EPA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(5.7,5.68,5.61)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGH ~ Tertiles,  data = merge_sub_rmNA_less30, group.by = "RIAGENDR")

#Just plot male by its self
merge_sub_rmNA_less30_M <- merge_sub_rmNA_less30[merge_sub_rmNA_less30$RIAGENDR == "Male",] #subsetting males

#my_comp3 <- list(c("1", "3"))
p3 <- ggerrorplot(merge_sub_rmNA_less30_M, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Glycohemoglobin by EPA Ages > 18 BMI < 30", subtitle = "Males")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 5.7, label.x = 1)
#  stat_compare_means(comparisons = my_comp3, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(5.68)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_less30_F <- merge_sub_rmNA_less30[merge_sub_rmNA_less30$RIAGENDR == "Female",] #subsetting mfeales

my_comp4 <- list(c("1","2"))
p4 <- ggerrorplot(merge_sub_rmNA_less30_F, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Glycohemoglobin by EPA Ages > 18 BMI < 30", subtitle = "Females")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 5.7, label.x = 1)+
  stat_compare_means(comparisons = my_comp4, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(5.65)) #p.format to place actual p-values (uses wilcox.test)

ggarrange(p3,p4, ncol = 2, nrow = 1, common.legend = TRUE)

##### PLOTS FOR BMI GREATER THAN 30 #####
kruskal.test(LBXGH ~ Tertiles,  data = merge_sub_rmNA_great30)
compare_means(LBXGH ~ Tertiles,  data = merge_sub_rmNA_great30)

#my_comp2 <- list(c("1", "3"), c("1","4"))

ggerrorplot(merge_sub_rmNA_great30, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", title = "Glycohemoglobin by EPA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 6.12, label.x = 1)
#  stat_compare_means(comparisons = my_comp2, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(144, 147)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
ggerrorplot(merge_sub_rmNA_great30, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "Glycohemoglobin by EPA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(6.12,6.11,5.96,6.12))

ggerrorplot(merge_sub_rmNA_great30, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "Glycohemoglobin by EPA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(6.28,6.12,6.05,6.14)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGH ~ Tertiles,  data = merge_sub_rmNA_great30, group.by = "RIAGENDR")

#Just plot male by its self
merge_sub_rmNA_great30_M <- merge_sub_rmNA_great30[merge_sub_rmNA_great30$RIAGENDR == "Male",] #subsetting males

my_comp5 <- list(c("1", "3"))
p5 <- ggerrorplot(merge_sub_rmNA_great30_M, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Glycohemoglobin by EPA Ages > 18 BMI 30+", subtitle = "Males")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 6.3, label.x = 1)+
  stat_compare_means(comparisons = my_comp5, tip.length = c(0.0007), label = "p.format", hide.ns = TRUE, label.y = c(6.23)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_great30_F <- merge_sub_rmNA_great30[merge_sub_rmNA_great30$RIAGENDR == "Female",] #subsetting mfeales

#my_comp6 <- list(c("2", "3"))
p6 <- ggerrorplot(merge_sub_rmNA_great30_F, x = "Tertiles", y = "LBXGH", xlab = "Tertiles of EPA Intake", ylab = "Glycohemoglobin (%)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Glycohemoglobin by EPA Ages > 18 BMI 30+", subtitle = "Females")+
  scale_x_discrete(name = '\nTertiles of EPA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.004\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n2.69\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 6.3, label.x = 1)
#stat_compare_means(comparisons = my_comp6, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(121)) #p.format to place actual p-values (uses wilcox.test)

ggarrange(p5,p6, ncol = 2, nrow = 1, common.legend = TRUE)
