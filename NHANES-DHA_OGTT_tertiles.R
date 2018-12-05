rm(list=ls())
rm(list=setdiff(ls(), c("ogtt", "data", "body", "glucose", "hba1c")))
setwd("C:/Users/Abrar/OneDrive - University of North Carolina at Chapel Hill/Shaikh Lab/NHANES/")
library(RNHANES)
library(gplots)
library("dplyr")
library(grid)
library("ggpubr")

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

merge_sub <- merge[,c(1,42,5,6,9,144,219,220)] #ID, population weights, gender, age, race, DHA intake, OGTTs, BMIs
merge_sub_rmNA <- na.omit(merge_sub)
hist(merge_sub_rmNA$DR1TP226)
plot(merge_sub_rmNA$DR1TP226, merge_sub_rmNA$LBXGLT)

#Tertiles
quantile(merge_sub_rmNA$DR1TP226, prob = c(0.33, 0.67, 1))

#Labeling data by tertiles
merge_sub_rmNA$Tertiles <- ifelse(merge_sub_rmNA$DR1TP226 <= quantile(merge_sub_rmNA$DR1TP226, 0.33),1,
                                  ifelse(merge_sub_rmNA$DR1TP226 > quantile(merge_sub_rmNA$DR1TP226, 0.33) & merge_sub_rmNA$DR1TP226 <= quantile(merge_sub_rmNA$DR1TP226, 0.67),2,3
                                  ))

kruskal.test(LBXGLT ~ Tertiles,  data = merge_sub_rmNA)
compare_means(LBXGLT ~ Tertiles,  data = merge_sub_rmNA)

#Counting n individuals in each tertile
n1 <- sum(merge_sub_rmNA$Tertiles == "1")
n2 <- sum(merge_sub_rmNA$Tertiles == "2")
n3 <- sum(merge_sub_rmNA$Tertiles == "3")

#plotting all ages and genders

#YOU HAVE TO CHANGE THE SUBSET OF COMPARISONS BASED ON WHICH COMPARISONS WERE SIGNIFICANT IN THE COMPARE_MEANS FUNCTION ABOVE, SO THAT IT ONLY PLOTS THE SIGNIFICANT P-VALUES IN THE WILCOXON TEST
#my_comparisons <- list( c("1", "2"), c("1", "3"), c("2", "3") )
my_comparisons <- list(c("2", "4"))

ggerrorplot(merge_sub_rmNA, x = "Tertiles", y = "LBXGLT", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by DHA All Ages")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 123, label.x = 1)
#  stat_compare_means(comparisons = my_comparisons, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(123)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
ggerrorplot(merge_sub_rmNA, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by DHA All Ages")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(118, 113, 122, 123))

ggerrorplot(merge_sub_rmNA, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by DHA All Ages")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(117, 119, 125)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGLT ~ Tertiles,  data = merge_sub_rmNA, group.by = "RIAGENDR")

# Line plot facetted by gender
# p <- ggline(merge_sub_rmNA, x = "Tertiles", y = "LBXGLT",
#             #color = "Tertiles", palette = "npg",
#             add = "mean_se",
#             facet.by = "RIAGENDR", short.panel.labs = FALSE, title = "OGTT by DHA All Ages", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", panel.labs = list(RIAGENDR = c("Female", "Male")))
# p + stat_compare_means(method = "kruskal.test", label.y = 129, label.x = 1)

#Just plot male by its self
merge_sub_rmNA_M <- merge_sub_rmNA[merge_sub_rmNA$RIAGENDR == "Male",] #subsetting males

my_comparisons2 <- list(c("2", "4"))
p1 <- ggerrorplot(merge_sub_rmNA_M, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by DHA All Ages - Males")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 122, label.x = 1)
#  stat_compare_means(comparisons = my_comparisons2, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(122)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_F <- merge_sub_rmNA[merge_sub_rmNA$RIAGENDR == "Female",] #subsetting mfeales

#my_comparisons3 <- list(c("2", "3"))
p2 <- ggerrorplot(merge_sub_rmNA_F, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by DHA All Ages - Females")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 122, label.x = 1)
#stat_compare_means(comparisons = my_comparisons3, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(121)) #p.format to place actual p-values (uses wilcox.test)

ggarrange(p1,p2, ncol = 2, nrow = 1, common.legend = TRUE)

#Stratifying by BMI
merge_sub_rmNA_age18 <- merge_sub_rmNA[merge_sub_rmNA$RIDAGEYR >= 18,]

merge_sub_rmNA_less30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI < 30,] #subsetting BMIs less than 30
merge_sub_rmNA_great30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI >= 30,] #subsetting BMIs greater than 30

kruskal.test(LBXGLT ~ Tertiles,  data = merge_sub_rmNA_less30)
compare_means(LBXGLT ~ Tertiles,  data = merge_sub_rmNA_less30)

kruskal.test(LBXGLT ~ Tertiles,  data = merge_sub_rmNA_great30)
compare_means(LBXGLT ~ Tertiles,  data = merge_sub_rmNA_great30)

##### PLOTS FOR BMI LESS THAN 30 #####
my_comp1 <- list(c("2", "3"))

ggerrorplot(merge_sub_rmNA_less30, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", title = "OGTT by DHA Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 123, label.x = 1)+      # Add global p-value
  stat_compare_means(comparisons = my_comp1, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(121)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
ggerrorplot(merge_sub_rmNA_less30, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by DHA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(117, 117, 123))

ggerrorplot(merge_sub_rmNA_less30, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by DHA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(117, 117, 124)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGLT ~ Tertiles,  data = merge_sub_rmNA_less30, group.by = "RIAGENDR")

#Just plot male by its self
merge_sub_rmNA_less30_M <- merge_sub_rmNA_less30[merge_sub_rmNA_less30$RIAGENDR == "Male",] #subsetting males

#my_comp3 <- list(c("2", "4"))
p3 <- ggerrorplot(merge_sub_rmNA_less30_M, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by DHA Ages > 18 BMI < 30", subtitle = "Males")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 123, label.x = 1)      # Add global p-value
#stat_compare_means(comparisons = my_comp3, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(122)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_less30_F <- merge_sub_rmNA_less30[merge_sub_rmNA_less30$RIAGENDR == "Female",] #subsetting mfeales

#my_comp4 <- list(c("2", "3"))
p4 <- ggerrorplot(merge_sub_rmNA_less30_F, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by DHA Ages > 18 BMI < 30", subtitle = "Females")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 123, label.x = 1)
#stat_compare_means(comparisons = my_comp4, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(121)) #p.format to place actual p-values (uses wilcox.test)

ggarrange(p3,p4, ncol = 2, nrow = 1, common.legend = TRUE)

##### PLOTS FOR BMI GREATER THAN 30 #####
kruskal.test(LBXGLT ~ Tertiles,  data = merge_sub_rmNA_great30)
compare_means(LBXGLT ~ Tertiles,  data = merge_sub_rmNA_great30)

#my_comp2 <- list(c("1", "2"))

ggerrorplot(merge_sub_rmNA_great30, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", title = "OGTT by DHA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 135, label.x = 1)
#  stat_compare_means(comparisons = my_comp2, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(141)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
ggerrorplot(merge_sub_rmNA_great30, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by DHA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(118, 113, 122, 123))

ggerrorplot(merge_sub_rmNA_great30, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "OGTT by DHA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(140, 143, 141)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGLT ~ Tertiles,  data = merge_sub_rmNA_great30, group.by = "RIAGENDR")

#Just plot male by its self
merge_sub_rmNA_great30_M <- merge_sub_rmNA_great30[merge_sub_rmNA_great30$RIAGENDR == "Male",] #subsetting males

#my_comp5 <- list(c("1","2"))
p5 <- ggerrorplot(merge_sub_rmNA_great30_M, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by DHA Ages > 18 BMI 30+", subtitle = "Males")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 145, label.x = 1)
#  stat_compare_means(comparisons = my_comp5, tip.length = c(0.003), label = "p.format", hide.ns = TRUE, label.y = c(152)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_great30_F <- merge_sub_rmNA_great30[merge_sub_rmNA_great30$RIAGENDR == "Female",] #subsetting mfeales

#my_comp6 <- list(c("2", "3"))
p6 <- ggerrorplot(merge_sub_rmNA_great30_F, x = "Tertiles", y = "LBXGLT", xlab = "Tertiles of DHA Intake", ylab = "Two Hour Glucose (mg/dl)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "OGTT by DHA Ages > 18 BMI 30+", subtitle = "Females")+
  scale_x_discrete(name = '\nTertiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3'), 
                   labels = c(paste('1\n0.005\nn =', n1), paste('2\n0.034\nn = ', n2), paste('3\n4.08\nn = ', n3)))+
  stat_compare_means(method = "kruskal.test", label.y = 145, label.x = 1) #use ..p.adj.. for adjusted p-values
#stat_compare_means(comparisons = my_comp6, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(121)) #p.format to place actual p-values (uses wilcox.test)

ggarrange(p5,p6, ncol = 2, nrow = 1, common.legend = TRUE)
