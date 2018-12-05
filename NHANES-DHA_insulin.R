rm(list=ls())
setwd("C:/Users/Abrar/OneDrive - University of North Carolina at Chapel Hill/Shaikh Lab/NHANES/")
library(RNHANES)
library(gplots)
library("dplyr")
library("ggpubr")

#load in file for Dietary Interview - Total Nutrient Intakes, First Day
nhanes_load_data("DR1TOT_H", "2013-2014")

#diet data
data <- nhanes_load_data("DR1TOT_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)

#insulin data
insulin <- nhanes_load_data("INS_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
ins_sub <- insulin[,c(1,50)] # subset by ID and Insulin (uU/mL)

#merging
merge <- merge(data, ins_sub, by="SEQN")
#merge_sub <- merge[,c(1,42,142,219)]
#merge_sub_rmNA <- na.omit(merge_sub)

#subset by age
merge$RIDAGEYR <- as.numeric(as.character(merge$RIDAGEYR)) #converting age to numeric vector
#newdata <- merge[merge$RIDAGEYR >= 18,] #ages older than 18

merge_sub <- merge[,c(1,42,5,6,9,144,219)] #ID, population weights, gender, age, race, DHA intake, insulin
merge_sub_rmNA <- na.omit(merge_sub)
hist(merge_sub_rmNA$DR1TP226)
plot(merge_sub_rmNA$DR1TP226, merge_sub_rmNA$LBXIN)

#removing zeros
merge_sub_rmNA_z <- merge_sub_rmNA[merge_sub_rmNA$DR1TP226 > 0,]

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
#without zeros
merge_sub_rmNA_z$Quartiles <- ifelse(merge_sub_rmNA_z$DR1TP226 <= quantile(merge_sub_rmNA_z$DR1TP226, 0.25),1,
                                     ifelse(merge_sub_rmNA_z$DR1TP226 > quantile(merge_sub_rmNA_z$DR1TP226, 0.25) & merge_sub_rmNA_z$DR1TP226 <= quantile(merge_sub_rmNA_z$DR1TP226, 0.5),2,
                                            ifelse(merge_sub_rmNA_z$DR1TP226 > quantile(merge_sub_rmNA_z$DR1TP226, 0.5) & merge_sub_rmNA_z$DR1TP226 <= quantile(merge_sub_rmNA_z$DR1TP226, 0.75),3,4
                                            )))

#rename column
#merge_sub_rmNA <- rename(merge_sub_rmNA, Quartiles = Quantiles)

#Graphing
# Plot the mean of Insulin (uU/mL) by DHA quartiles
plotmeans(LBXIN ~ Quartiles, data = merge_sub_rmNA, frame = TRUE, main = "Insulin (uU/mL) by DHA - Ages > 18")

# Plot the mean of Insulin (uU/mL) by DHA quartiles WITHOUT zeros
plotmeans(LBXIN ~ Quartiles, data = merge_sub_rmNA_z, frame = TRUE, main = "Insulin (uU/mL) by DHA - Ages > 18", xlab = "DHA Quartiles", ylab = "Insulin (uU/mL)", p=0.95)

#big dot plot
stripchart(LBXIN ~ Quartiles, data = merge_sub_rmNA_z,
           frame = FALSE, vertical = TRUE,
           method = "jitter", pch = c(21, 18, 16),
           col = c("#999999", "#E69F00", "#56B4E9", "red"),
           main = "Insulin (uU/mL) by DHA - Ages > 18", xlab = "DHA Quartiles", ylab = "Insulin (uU/mL)")

#calculating quantiles with RNHANES
#nhanes_quantile(merge_sub_rmNA_z, "DR1TP226", "Quartiles", "WTINT2YR", c(0.5, 0.95, 0.99))

#convert Quartiles to characters
merge_sub_rmNA$Quartiles <- as.character(as.numeric(merge_sub_rmNA$Quartiles))

###################
#Sorting dataframe
merge_sub_rmNA <- merge_sub_rmNA[order(merge_sub_rmNA$Quartiles),] 

#stratifying by age groups
merge_sub_rmNA_z$AgeGroup <- ifelse(merge_sub_rmNA_z$RIDAGEYR <= quantile(merge_sub_rmNA_z$RIDAGEYR, 0.25),1,
                                    ifelse(merge_sub_rmNA_z$RIDAGEYR > quantile(merge_sub_rmNA_z$RIDAGEYR, 0.25) & merge_sub_rmNA_z$RIDAGEYR <= quantile(merge_sub_rmNA_z$RIDAGEYR, 0.5),2,
                                           ifelse(merge_sub_rmNA_z$RIDAGEYR > quantile(merge_sub_rmNA_z$RIDAGEYR, 0.5) & merge_sub_rmNA_z$RIDAGEYR <= quantile(merge_sub_rmNA_z$RIDAGEYR, 0.75),3,4
                                           )))
merge_sub_rmNA_z$AgeGroup <- as.character(as.numeric(merge_sub_rmNA_z$AgeGroup)) #convert age groups to characters

ggerrorplot(merge_sub_rmNA, x = "Quartiles", y = "LBXIN", 
            desc_stat = "mean_se", error.plot = "errorbar", add = "mean",
            color = "AgeGroup", palette = "jco",
            position = position_dodge(0.3)     # Adjust the space between bars
)

ggline(merge_sub_rmNA, x = "Quartiles", y = "LBXIN", 
       add = c("mean_se"),
       color = "AgeGroup", palette = "jco")

#OUTPUT PLOTS TO FILE - for Insulin (uU/mL) by DHA Stratifying by Ages
pdf("DHA_Insulin_Ages_Mean_stats.pdf")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR >= 12 & merge_sub_rmNA$RIDAGEYR <= 23),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p1 <- ggline(ageQ, x = "Quartiles", y = "LBXIN", xlab = "Quartiles of DHA Intake", ylab = "Insulin (uU/mL)", add = c("mean_se"), title = "Insulin by DHA Ages Q1 (12-23)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(LBXIN ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p1 <- p1 + annotation_custom(grob)

kruskal.test(LBXIN ~ Quartiles,  data = ageQ)
compare_means(LBXIN ~ Quartiles,  data = ageQ)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 24 & merge_sub_rmNA$RIDAGEYR <= 40),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p2<- ggline(ageQ, x = "Quartiles", y = "LBXIN", xlab = "Quartiles of DHA Intake", ylab = "Insulin (uU/mL)", add = c("mean_se"), title = "Insulin by DHA Ages Q2 (24-40)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(LBXIN ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p2 <- p2 + annotation_custom(grob)

kruskal.test(LBXIN ~ Quartiles,  data = ageQ)
compare_means(LBXIN ~ Quartiles,  data = ageQ)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 40 & merge_sub_rmNA$RIDAGEYR <= 56),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p3 <- ggline(ageQ, x = "Quartiles", y = "LBXIN", xlab = "Quartiles of DHA Intake", ylab = "Insulin (uU/mL)", add = c("mean_se"), title = "Insulin by DHA Ages Q3 (41-56)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(LBXIN ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p3 <- p3 + annotation_custom(grob)

kruskal.test(LBXIN ~ Quartiles,  data = ageQ)
compare_means(LBXIN ~ Quartiles,  data = ageQ)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 56 & merge_sub_rmNA$RIDAGEYR <= 79),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p4 <- ggline(ageQ, x = "Quartiles", y = "LBXIN", xlab = "Quartiles of DHA Intake", ylab = "Insulin (uU/mL)", add = c("mean_se"), title = "Insulin by DHA Ages Q4 (57-79)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(LBXIN ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p4 <- p4 + annotation_custom(grob)

kruskal.test(LBXIN ~ Quartiles,  data = ageQ)
compare_means(LBXIN ~ Quartiles,  data = ageQ)

ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2, common.legend = TRUE)

############################################### INCLUDED GENDER BELOW

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR >= 12 & merge_sub_rmNA$RIDAGEYR <= 23),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p1 <- ggline(ageQ, x = "Quartiles", y = "LBXIN", xlab = "Quartiles of DHA Intake", ylab = "Insulin (uU/mL)", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "Insulin by DHA Ages Q1 (12-23)")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 24 & merge_sub_rmNA$RIDAGEYR <= 40),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p2<- ggline(ageQ, x = "Quartiles", y = "LBXIN", xlab = "Quartiles of DHA Intake", ylab = "Insulin (uU/mL)", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "Insulin by DHA Ages Q2 (24-40)")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 40 & merge_sub_rmNA$RIDAGEYR <= 56),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p3 <- ggline(ageQ, x = "Quartiles", y = "LBXIN", xlab = "Quartiles of DHA Intake", ylab = "Insulin (uU/mL)", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "Insulin by DHA Ages Q3 (41-56)")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 56 & merge_sub_rmNA$RIDAGEYR <= 79),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p4 <- ggline(ageQ, x = "Quartiles", y = "LBXIN", xlab = "Quartiles of DHA Intake", ylab = "Insulin (uU/mL)", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "Insulin by DHA Ages Q4 (57-79)")

ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2, common.legend = TRUE)

############################################### INCLUDED RACE BELOW

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR >= 12 & merge_sub_rmNA$RIDAGEYR <= 23),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#Plotting by race
p5 <- ggerrorplot(ageQ, x = "Quartiles", y = "LBXIN", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of DHA Intake", ylab = "Insulin (uU/mL)",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "Insulin by DHA", subtitle = "Ages Q1 (12-23)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 24 & merge_sub_rmNA$RIDAGEYR <= 40),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#Plotting by race
p6 <- ggerrorplot(ageQ, x = "Quartiles", y = "LBXIN", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of DHA Intake", ylab = "Insulin (uU/mL)",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "Insulin by DHA", subtitle = "Ages Q2 (25-40)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 40 & merge_sub_rmNA$RIDAGEYR <= 56),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#Plotting by race
p7 <- ggerrorplot(ageQ, x = "Quartiles", y = "LBXIN", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of DHA Intake", ylab = "Insulin (uU/mL)",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "Insulin by DHA", subtitle = "Ages Q3 (41-56)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 56 & merge_sub_rmNA$RIDAGEYR <= 79),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#Plotting by race
p8 <- ggerrorplot(ageQ, x = "Quartiles", y = "LBXIN", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of DHA Intake", ylab = "Insulin (uU/mL)",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "Insulin by DHA", subtitle = "Ages Q4 (57-79)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ggarrange(p5,p6,p7,p8, ncol = 2, nrow = 2, common.legend = TRUE)

dev.off() # flush that pdf! Closes the PDF so you can open it
