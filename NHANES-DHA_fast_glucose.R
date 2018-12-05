rm(list=ls())
rm(list=setdiff(ls(), c("ogtt", "glucose", "data", "body", "hba1c")))
setwd("C:/Users/Abrar/OneDrive - University of North Carolina at Chapel Hill/Shaikh Lab/NHANES/")
library(RNHANES)
library(gplots)
library("dplyr")
library("ggpubr")

#load in file for Dietary Interview - Total Nutrient Intakes, First Day
nhanes_load_data("DR1TOT_H", "2013-2014")

#diet data
data <- nhanes_load_data("DR1TOT_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)

#fasting glucose data
glucose <- nhanes_load_data("GLU_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
glu_sub <- glucose[,c(1,50)] # subset by ID and fasting glucose

#body measures and BMI data
body <- nhanes_load_data("BMX_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
body_sub <- body[,c(1,58)] # subset by ID and BMI

#merging
merge <- merge(data, glu_sub, by="SEQN")
merge <- merge(merge, body_sub, by="SEQN")
#merge_sub <- merge[,c(1,42,142,219)]
#merge_sub_rmNA <- na.omit(merge_sub)

#subset by age
#merge$RIDAGEYR <- as.numeric(as.character(merge$RIDAGEYR)) #converting age to numeric vector
#newdata <- merge[merge$RIDAGEYR >= 18,] #ages older than 18

merge_sub <- merge[,c(1,42,5,6,9,144,219,220)] #ID, population weights, gender, age, race, DHA intake, fasting glucose
merge_sub_rmNA <- na.omit(merge_sub)
hist(merge_sub_rmNA$DR1TP226)
plot(merge_sub_rmNA$DR1TP226, merge_sub_rmNA$LBXGLU)

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

kruskal.test(LBXGLU ~ Quartiles,  data = merge_sub_rmNA)
compare_means(LBXGLU ~ Quartiles,  data = merge_sub_rmNA)

n1 <- sum(merge_sub_rmNA$Quartiles == "1")
n2 <- sum(merge_sub_rmNA$Quartiles == "2")
n3 <- sum(merge_sub_rmNA$Quartiles == "3")
n4 <- sum(merge_sub_rmNA$Quartiles == "4")

#plotting all ages and genders

#YOU HAVE TO CHANGE THE SUBSET OF COMPARISONS BASED ON WHICH COMPARISONS WERE SIGNIFICANT IN THE COMPARE_MEANS FUNCTION ABOVE, SO THAT IT ONLY PLOTS THE SIGNIFICANT P-VALUES IN THE WILCOXON TEST
#my_comparisons <- list( c("1", "2"), c("1", "3"), c("1", "4"), c("2", "3"), c("2", "4"), c("3", "4") )
my_comparisons <- list(c("2", "3"), c("1", "3"), c("2", "4"), c("1","4"))

ggerrorplot(merge_sub_rmNA, x = "Quartiles", y = "LBXGLU", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Fasting Glucose by DHA All Ages")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 112, label.x = 1)+      # Add global p-value
  stat_compare_means(comparisons = my_comparisons, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(108.5,107,110,111)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
ggerrorplot(merge_sub_rmNA, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "Fasting Glucose by DHA All Ages")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(107,106,112,113))

ggerrorplot(merge_sub_rmNA, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "Fasting Glucose by DHA All Ages")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(107,106,112,113)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGLU ~ Quartiles,  data = merge_sub_rmNA, group.by = "RIAGENDR")

# Line plot facetted by gender
# p <- ggerrorplot(merge_sub_rmNA, x = "Quartiles", y = "LBXGLU",
#             #color = "Quartiles", palette = "npg",
#             add = "mean_se",
#             facet.by = "RIAGENDR", short.panel.labs = FALSE, title = "Fasting Glucose by DHA All Ages", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", panel.labs = list(RIAGENDR = c("Female", "Male")))
# p + stat_compare_means(method = "kruskal.test", label.y = 129, label.x = 1)

#Just plot male by its self
merge_sub_rmNA_M <- merge_sub_rmNA[merge_sub_rmNA$RIAGENDR == "Male",] #subsetting males

my_comparisons2 <- list(c("2", "3"), c("1", "3"), c("2", "4"), c("1", "4"))
p1 <- ggerrorplot(merge_sub_rmNA_M, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Fasting Glucose by DHA All Ages - Males")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 119, label.x = 1)+      # Add global p-value
  stat_compare_means(comparisons = my_comparisons2, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(112,113.5,115.3,117)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_F <- merge_sub_rmNA[merge_sub_rmNA$RIAGENDR == "Female",] #subsetting mfeales

#my_comparisons3 <- list(c("2", "3"))
p2 <- ggerrorplot(merge_sub_rmNA_F, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Fasting Glucose by DHA All Ages - Females")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 119, label.x = 1)
#stat_compare_means(comparisons = my_comparisons3, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(121)) #p.format to place actual p-values (uses wilcox.test)

ggarrange(p1,p2, ncol = 2, nrow = 1, common.legend = TRUE)

#Stratifying by BMI
merge_sub_rmNA_age18 <- merge_sub_rmNA[merge_sub_rmNA$RIDAGEYR >= 18,]

merge_sub_rmNA_less30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI < 30,] #subsetting BMIs less than 30
merge_sub_rmNA_great30 <- merge_sub_rmNA_age18[merge_sub_rmNA_age18$BMXBMI >= 30,] #subsetting BMIs greater than 30

kruskal.test(LBXGLU ~ Quartiles,  data = merge_sub_rmNA_less30)
compare_means(LBXGLU ~ Quartiles,  data = merge_sub_rmNA_less30)

kruskal.test(LBXGLU ~ Quartiles,  data = merge_sub_rmNA_great30)
compare_means(LBXGLU ~ Quartiles,  data = merge_sub_rmNA_great30)

##### PLOTS FOR BMI LESS THAN 30 #####
#my_comp1 <- list(c("2", "4"))

ggerrorplot(merge_sub_rmNA_less30, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", title = "Fasting Glucose by DHA Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 105, label.x = 1)      # Add global p-value
#stat_compare_means(comparisons = my_comp1, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(123)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
ggerrorplot(merge_sub_rmNA_less30, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "Fasting Glucose by DHA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(106, 106, 110, 113))

ggerrorplot(merge_sub_rmNA_less30, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "Fasting Glucose by DHA All Ages > 18 BMI < 30")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(106.5, 106.5, 110, 113)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGLU ~ Quartiles,  data = merge_sub_rmNA_less30, group.by = "RIAGENDR")

#Just plot male by its self
merge_sub_rmNA_less30_M <- merge_sub_rmNA_less30[merge_sub_rmNA_less30$RIAGENDR == "Male",] #subsetting males

#my_comp3 <- list(c("2", "3"), c("1","3"))
p3 <- ggerrorplot(merge_sub_rmNA_less30_M, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Fasting Glucose by DHA Ages > 18 BMI < 30", subtitle = "Males")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 111, label.x = 1)
#  stat_compare_means(comparisons = my_comp3, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(111,113)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_less30_F <- merge_sub_rmNA_less30[merge_sub_rmNA_less30$RIAGENDR == "Female",] #subsetting mfeales

#my_comp4 <- list(c("2", "3"))
p4 <- ggerrorplot(merge_sub_rmNA_less30_F, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Fasting Glucose by DHA Ages > 18 BMI < 30", subtitle = "Females")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 111, label.x = 1)
#stat_compare_means(comparisons = my_comp4, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(121)) #p.format to place actual p-values (uses wilcox.test)

ggarrange(p3,p4, ncol = 2, nrow = 1, common.legend = TRUE)

##### PLOTS FOR BMI GREATER THAN 30 #####
kruskal.test(LBXGLU ~ Quartiles,  data = merge_sub_rmNA_great30)
compare_means(LBXGLU ~ Quartiles,  data = merge_sub_rmNA_great30)

my_comp2 <- list(c("2", "4"))

ggerrorplot(merge_sub_rmNA_great30, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", title = "Fasting Glucose by DHA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 122, label.x = 1)+
  stat_compare_means(comparisons = my_comp2, tip.length = c(0.001), label = "p.format", hide.ns = TRUE, label.y = c(120)) #p.format to place actual p-values (uses wilcox.test)

#plotting by gender (uses Wilcoxon pairwise test like above)
ggerrorplot(merge_sub_rmNA_great30, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "Fasting Glucose by DHA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.signif", hide.ns = TRUE, label.y = c(118, 113, 129, 122))

ggerrorplot(merge_sub_rmNA_great30, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", color = "RIAGENDR", palette = "npg", legend.title = "Gender", title = "Fasting Glucose by DHA Ages > 18 BMI 30+")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(aes(group = RIAGENDR), label = "p.format", hide.ns = TRUE, label.y = c(123, 122, 129, 122)) #color palette jco

#comparing for each gender seperately
compare_means(LBXGLU ~ Quartiles,  data = merge_sub_rmNA_great30, group.by = "RIAGENDR")

#Just plot male by its self
merge_sub_rmNA_great30_M <- merge_sub_rmNA_great30[merge_sub_rmNA_great30$RIAGENDR == "Male",] #subsetting males

#my_comp5 <- list(c("1","2"), c("1", "4"))
p5 <- ggerrorplot(merge_sub_rmNA_great30_M, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Fasting Glucose by DHA Ages > 18 BMI 30+", subtitle = "Males")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 130, label.x = 1)
#  stat_compare_means(comparisons = my_comp5, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(152,155)) #p.format to place actual p-values (uses wilcox.test)

#Just plot female by its self
merge_sub_rmNA_great30_F <- merge_sub_rmNA_great30[merge_sub_rmNA_great30$RIAGENDR == "Female",] #subsetting mfeales

#my_comp6 <- list(c("2", "3"))
p6 <- ggerrorplot(merge_sub_rmNA_great30_F, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean", "mean_se"), error.plot = "errorbar", palette = "jco", title = "Fasting Glucose by DHA Ages > 18 BMI 30+", subtitle = "Females")+
  scale_x_discrete(name = '\nQuartiles of DHA Intake (grams)', 
                   breaks = c('1', '2', '3', '4'), 
                   labels = c(paste('1\n0.003\nn =', n1), paste('2\n0.012\nn = ', n2), paste('3\n0.057\nn = ', n3), paste('4\n4.34\nn = ', n4)))+
  stat_compare_means(method = "kruskal.test", label.y = 130, label.x = 1) #use ..p.adj.. for adjusted p-values
#stat_compare_means(comparisons = my_comp6, tip.length = c(0.001), label = "p.signif", hide.ns = TRUE, label.y = c(121)) #p.format to place actual p-values (uses wilcox.test)

ggarrange(p5,p6, ncol = 2, nrow = 1, common.legend = TRUE)

######Extra Code#####
#rename column
#merge_sub_rmNA <- rename(merge_sub_rmNA, Quartiles = Quantiles)
#Graphing
# Plot the mean of Fasting Glucose by DHA quartiles
plotmeans(LBXGLU ~ Quartiles, data = merge_sub_rmNA, frame = TRUE, main = "Fasting Glucose by DHA - All Ages")

# Plot the mean of Fasting Glucose by DHA quartiles WITHOUT zeros
plotmeans(LBXGLU ~ Quartiles, data = merge_sub_rmNA_z, frame = TRUE, main = "Fasting Glucose by DHA - Ages > 18", xlab = "DHA Quartiles", ylab = "Fasting Glucose", p=0.95)

#big dot plot
stripchart(LBXGLU ~ Quartiles, data = merge_sub_rmNA_z,
           frame = FALSE, vertical = TRUE,
           method = "jitter", pch = c(21, 18, 16),
           col = c("#999999", "#E69F00", "#57B4E9", "red"),
           main = "Fasting Glucose by DHA - Ages > 18", xlab = "DHA Quartiles", ylab = "Fasting Glucose")

#calculating quantiles with RNHANES
#nhanes_quantile(merge_sub_rmNA_z, "DR1TP226", "Quartiles", "WTINT2YR", c(0.5, 0.95, 0.99))

#convert Quartiles to characters
merge_sub_rmNA$Quartiles <- as.character(as.numeric(merge_sub_rmNA$Quartiles))

#####Removing Zeros######
#removing zeros
merge_sub_rmNA_z <- merge_sub_rmNA[merge_sub_rmNA$DR1TP226 > 0,]

#without zeros
merge_sub_rmNA_z$Quartiles <- ifelse(merge_sub_rmNA_z$DR1TP226 <= quantile(merge_sub_rmNA_z$DR1TP226, 0.25),1,
                                     ifelse(merge_sub_rmNA_z$DR1TP226 > quantile(merge_sub_rmNA_z$DR1TP226, 0.25) & merge_sub_rmNA_z$DR1TP226 <= quantile(merge_sub_rmNA_z$DR1TP226, 0.5),2,
                                            ifelse(merge_sub_rmNA_z$DR1TP226 > quantile(merge_sub_rmNA_z$DR1TP226, 0.5) & merge_sub_rmNA_z$DR1TP226 <= quantile(merge_sub_rmNA_z$DR1TP226, 0.75),3,4
                                            )))

#Sorting dataframe
merge_sub_rmNA <- merge_sub_rmNA[order(merge_sub_rmNA$Quartiles),] 

#stratifying by age groups
merge_sub_rmNA_z$AgeGroup <- ifelse(merge_sub_rmNA_z$RIDAGEYR <= quantile(merge_sub_rmNA_z$RIDAGEYR, 0.25),1,
                                    ifelse(merge_sub_rmNA_z$RIDAGEYR > quantile(merge_sub_rmNA_z$RIDAGEYR, 0.25) & merge_sub_rmNA_z$RIDAGEYR <= quantile(merge_sub_rmNA_z$RIDAGEYR, 0.5),2,
                                           ifelse(merge_sub_rmNA_z$RIDAGEYR > quantile(merge_sub_rmNA_z$RIDAGEYR, 0.5) & merge_sub_rmNA_z$RIDAGEYR <= quantile(merge_sub_rmNA_z$RIDAGEYR, 0.75),3,4
                                           )))
merge_sub_rmNA_z$AgeGroup <- as.character(as.numeric(merge_sub_rmNA_z$AgeGroup)) #convert age groups to characters

ggerrorplot(merge_sub_rmNA, x = "Quartiles", y = "LBXGLU", 
            desc_stat = "mean_se", error.plot = "errorbar", add = "mean",
            color = "AgeGroup", palette = "jco",
            position = position_dodge(0.3)     # Adjust the space between bars
)

ggline(merge_sub_rmNA, x = "Quartiles", y = "LBXGLU", 
       add = c("mean_se"),
       color = "Quartiles", palette = "jco")

####Making PDFs####
#OUTPUT PLOTS TO FILE - for Fasting Glucose by DHA Stratifying by Ages
pdf("DHA_Fasting_Glucose_stats.pdf")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR >= 12 & merge_sub_rmNA$RIDAGEYR <= 23),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#convert Quartiles to factors (categorical variables)
#ageQ$Quartiles <- as.factor(ageQ$Quartiles)

p1 <- ggline(ageQ, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean_se"), title = "Fasting Glucose by DHA", subtitle = "Ages Q1 (12-23)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(LBXGLU ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p1 <- p1 + annotation_custom(grob)

kruskal.test(LBXGLU ~ Quartiles,  data = ageQ)
compare_means(LBXGLU ~ Quartiles,  data = ageQ)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 23 & merge_sub_rmNA$RIDAGEYR <= 41),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p2<- ggline(ageQ, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean_se"), title = "Fasting Glucose by DHA", subtitle = "Ages Q2 (24-41)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(LBXGLU ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p2 <- p2 + annotation_custom(grob)

kruskal.test(LBXGLU ~ Quartiles,  data = ageQ)
compare_means(LBXGLU ~ Quartiles,  data = ageQ)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 41 & merge_sub_rmNA$RIDAGEYR <= 57),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p3 <- ggline(ageQ, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean_se"), title = "Fasting Glucose by DHA", subtitle = "Ages Q3 (42-57)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(LBXGLU ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p3 <- p3 + annotation_custom(grob)

kruskal.test(LBXGLU ~ Quartiles,  data = ageQ)
compare_means(LBXGLU ~ Quartiles,  data = ageQ)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 57 & merge_sub_rmNA$RIDAGEYR <= 79),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p4 <- ggline(ageQ, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean_se"), title = "Fasting Glucose by DHA", subtitle = "Ages Q4 (58-79)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(LBXGLU ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p4 <- p4 + annotation_custom(grob)

kruskal.test(LBXGLU ~ Quartiles,  data = ageQ)
compare_means(LBXGLU ~ Quartiles,  data = ageQ)

ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2, common.legend = TRUE)

############################################### INCLUDED GENDER BELOW

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR >= 12 & merge_sub_rmNA$RIDAGEYR <= 23),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p1 <- ggline(ageQ, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "Fasting Glucose by DHA", subtitle = "Ages Q1 (12-23)")

#ggline(ageQ, x = "Quartiles", y = "LBXGLU", add = "mean_se",
#       color = "RIAGENDR", palette = "jco")+
#  stat_compare_means(aes(group = RIAGENDR), label = "p.signif")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 23 & merge_sub_rmNA$RIDAGEYR <= 41),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p2<- ggline(ageQ, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "Fasting Glucose by DHA", subtitle = "Ages Q2 (24-41)")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 41 & merge_sub_rmNA$RIDAGEYR <= 57),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p3 <- ggline(ageQ, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "Fasting Glucose by DHA", subtitle = "Ages Q3 (42-57)")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 57 & merge_sub_rmNA$RIDAGEYR <= 79),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p4 <- ggline(ageQ, x = "Quartiles", y = "LBXGLU", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "Fasting Glucose by DHA", subtitle = "Ages Q4 (58-79)")

ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2, common.legend = TRUE)

############################################### INCLUDED RACE BELOW

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR >= 12 & merge_sub_rmNA$RIDAGEYR <= 23),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#Plotting by race
p5 <- ggerrorplot(ageQ, x = "Quartiles", y = "LBXGLU", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "Fasting Glucose by DHA", subtitle = "Ages Q1 (12-23)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 23 & merge_sub_rmNA$RIDAGEYR <= 41),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#Plotting by race
p6 <- ggerrorplot(ageQ, x = "Quartiles", y = "LBXGLU", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "Fasting Glucose by DHA", subtitle = "Ages Q2 (24-41)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 41 & merge_sub_rmNA$RIDAGEYR <= 57),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#Plotting by race
p7 <- ggerrorplot(ageQ, x = "Quartiles", y = "LBXGLU", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "Fasting Glucose by DHA", subtitle = "Ages Q3 (42-57)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 57 & merge_sub_rmNA$RIDAGEYR <= 79),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#Plotting by race
p8 <- ggerrorplot(ageQ, x = "Quartiles", y = "LBXGLU", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of DHA Intake", ylab = "Fasting Glucose (mg/dL)",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "Fasting Glucose by DHA", subtitle = "Ages Q4 (58-79)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ggarrange(p5,p6,p7,p8, ncol = 2, nrow = 2, common.legend = TRUE)

dev.off() # flush that pdf! Closes the PDF so you can open it
