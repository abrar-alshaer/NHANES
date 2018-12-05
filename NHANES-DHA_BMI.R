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

#body measures and BMI data
body <- nhanes_load_data("BMX_H", "2013-2014", cache = "./nhanes_data", demographics = TRUE, recode = TRUE)
body_sub <- body[,c(1,58)] # subset by ID and BMI

#merging
merge <- merge(data, body_sub, by="SEQN")
#merge_sub <- merge[,c(1,42,142,219)]
#merge_sub_rmNA <- na.omit(merge_sub)

#subset by age
merge$RIDAGEYR <- as.numeric(as.character(merge$RIDAGEYR))
#newdata <- merge[merge$RIDAGEYR >= 18,] #ages older than 18

merge_sub <- merge[,c(1,42,5,6,9,144,219)] #ID, population weights, gender, age, race, DHA intake, BMIs
merge_sub_rmNA <- na.omit(merge_sub)
hist(merge_sub_rmNA$DR1TP226)
plot(merge_sub_rmNA$DR1TP226, merge_sub_rmNA$BMXBMI)

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
# Plot the mean of BMI by DHA quartiles
plotmeans(BMXBMI ~ Quartiles, data = merge_sub_rmNA, frame = TRUE, main = "BMI by DHA - Ages > 18")

# Plot the mean of BMI by DHA quartiles WITHOUT zeros
plotmeans(BMXBMI ~ Quartiles, data = merge_sub_rmNA_z, frame = TRUE, main = "BMI by DHA - Ages > 18", xlab = "DHA Quartiles", ylab = "BMI", p=0.95)

#big dot plot
stripchart(BMXBMI ~ Quartiles, data = merge_sub_rmNA_z,
           frame = FALSE, vertical = TRUE,
           method = "jitter", pch = c(21, 18, 16),
           col = c("#999999", "#E69F00", "#56B4E9", "red"),
           main = "BMI by DHA - Ages > 18", xlab = "DHA Quartiles", ylab = "BMI")

#calculating quantiles with RNHANES
#nhanes_quantile(merge_sub_rmNA_z, "DR1TP226", "Quartiles", "WTINT2YR", c(0.5, 0.95, 0.99))

#convert Quartiles to characters
merge_sub_rmNA_z$Quartiles <- as.character(as.numeric(merge_sub_rmNA_z$Quartiles))

###################
#Sorting dataframe
merge_sub_rmNA <- merge_sub_rmNA[order(merge_sub_rmNA$Quartiles),] 

#stratifying by age groups
merge_sub_rmNA_z$AgeGroup <- ifelse(merge_sub_rmNA_z$RIDAGEYR <= quantile(merge_sub_rmNA_z$RIDAGEYR, 0.25),1,
                                    ifelse(merge_sub_rmNA_z$RIDAGEYR > quantile(merge_sub_rmNA_z$RIDAGEYR, 0.25) & merge_sub_rmNA_z$RIDAGEYR <= quantile(merge_sub_rmNA_z$RIDAGEYR, 0.5),2,
                                           ifelse(merge_sub_rmNA_z$RIDAGEYR > quantile(merge_sub_rmNA_z$RIDAGEYR, 0.5) & merge_sub_rmNA_z$RIDAGEYR <= quantile(merge_sub_rmNA_z$RIDAGEYR, 0.75),3,4
                                           )))
merge_sub_rmNA_z$AgeGroup <- as.character(as.numeric(merge_sub_rmNA_z$AgeGroup)) #convert age groups to characters

ggerrorplot(merge_sub_rmNA, x = "Quartiles", y = "BMXBMI", 
            desc_stat = "mean_se", error.plot = "errorbar", add = "mean",
            color = "AgeGroup", palette = "jco",
            position = position_dodge(0.3)     # Adjust the space between bars
)

ggline(merge_sub_rmNA, x = "Quartiles", y = "BMXBMI", 
       add = c("mean_se"),
       color = "AgeGroup", palette = "jco")

#OUTPUT PLOTS TO FILE - for BMI by DHA Stratifying by Ages
pdf("DHA_BMI_Ages_Mean_stats.pdf")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR >= 2 & merge_sub_rmNA$RIDAGEYR <= 13),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p1 <- ggline(ageQ, x = "Quartiles", y = "BMXBMI", xlab = "Quartiles of DHA Intake", ylab = "BMI", add = c("mean_se"), title = "BMI by DHA Ages Q1 (2-13)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(BMXBMI ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p1 <- p1 + annotation_custom(grob)

kruskal.test(BMXBMI ~ Quartiles,  data = ageQ)
compare_means(BMXBMI ~ Quartiles,  data = ageQ)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 13 & merge_sub_rmNA$RIDAGEYR <= 30),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p2<- ggline(ageQ, x = "Quartiles", y = "BMXBMI", xlab = "Quartiles of DHA Intake", ylab = "BMI", add = c("mean_se"), title = "BMI by DHA Ages Q2 (14-30)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(BMXBMI ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p2 <- p2 + annotation_custom(grob)

kruskal.test(BMXBMI ~ Quartiles,  data = ageQ)
compare_means(BMXBMI ~ Quartiles,  data = ageQ)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 30 & merge_sub_rmNA$RIDAGEYR <= 52),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p3 <- ggline(ageQ, x = "Quartiles", y = "BMXBMI", xlab = "Quartiles of DHA Intake", ylab = "BMI", add = c("mean_se"), title = "BMI by DHA Ages Q3 (31-52)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(BMXBMI ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p3 <- p3 + annotation_custom(grob)

kruskal.test(BMXBMI ~ Quartiles,  data = ageQ)
compare_means(BMXBMI ~ Quartiles,  data = ageQ)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 52 & merge_sub_rmNA$RIDAGEYR <= 79),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p4 <- ggline(ageQ, x = "Quartiles", y = "BMXBMI", xlab = "Quartiles of DHA Intake", ylab = "BMI", add = c("mean_se"), title = "BMI by DHA Ages Q4 (53-79)")
# Create a text
grob <- grobTree(textGrob(paste("Kruskal Wallis: ", signif(kruskal.test(BMXBMI ~ Quartiles,  data = ageQ)[[3]], digits = 3)), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
p4 <- p4 + annotation_custom(grob)

kruskal.test(BMXBMI ~ Quartiles,  data = ageQ)
compare_means(BMXBMI ~ Quartiles,  data = ageQ)

ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2, common.legend = TRUE)

############################################### INCLUDED GENDER BELOW

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR >= 2 & merge_sub_rmNA$RIDAGEYR <= 13),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p1 <- ggline(ageQ, x = "Quartiles", y = "BMXBMI", xlab = "Quartiles of DHA Intake", ylab = "BMI", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "BMI by DHA Ages Q1 (2-13)")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 13 & merge_sub_rmNA$RIDAGEYR <= 30),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p2<- ggline(ageQ, x = "Quartiles", y = "BMXBMI", xlab = "Quartiles of DHA Intake", ylab = "BMI", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "BMI by DHA Ages Q2 (14-30)")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 30 & merge_sub_rmNA$RIDAGEYR <= 52),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p3 <- ggline(ageQ, x = "Quartiles", y = "BMXBMI", xlab = "Quartiles of DHA Intake", ylab = "BMI", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "BMI by DHA Ages Q3 (31-52)")

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 52 & merge_sub_rmNA$RIDAGEYR <= 79),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
p4 <- ggline(ageQ, x = "Quartiles", y = "BMXBMI", xlab = "Quartiles of DHA Intake", ylab = "BMI", add = c("mean_se"), color = "RIAGENDR", palette = "jco", legend.title = "Gender", title = "BMI by DHA Ages Q4 (53-79)")

ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2, common.legend = TRUE)

############################################### INCLUDED RACE BELOW

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR >= 2 & merge_sub_rmNA$RIDAGEYR <= 13),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#Plotting by race
p5 <- ggerrorplot(ageQ, x = "Quartiles", y = "BMXBMI", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of DHA Intake", ylab = "BMI",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "BMI by DHA", subtitle = "Ages Q1 (2-13)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 13 & merge_sub_rmNA$RIDAGEYR <= 30),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#Plotting by race
p6 <- ggerrorplot(ageQ, x = "Quartiles", y = "BMXBMI", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of DHA Intake", ylab = "BMI",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "BMI by DHA", subtitle = "Ages Q2 (14-30)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 30 & merge_sub_rmNA$RIDAGEYR <= 52),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#Plotting by race
p7 <- ggerrorplot(ageQ, x = "Quartiles", y = "BMXBMI", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of DHA Intake", ylab = "BMI",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "BMI by DHA", subtitle = "Ages Q3 (31-52)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ageQ <- merge_sub_rmNA[which(merge_sub_rmNA$RIDAGEYR > 52 & merge_sub_rmNA$RIDAGEYR <= 79),]
ageQ$Quartiles <- ifelse(ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.25),1,
                         ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.25) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.5),2,
                                ifelse(ageQ$DR1TP226 > quantile(ageQ$DR1TP226, 0.5) & ageQ$DR1TP226 <= quantile(ageQ$DR1TP226, 0.75),3,4
                                )))
#Plotting by race
p8 <- ggerrorplot(ageQ, x = "Quartiles", y = "BMXBMI", 
                  desc_stat = "mean_se", error.plot = "errorbar", add = "mean", xlab = "Quartiles of DHA Intake", ylab = "BMI",
                  color = "RIDRETH3", palette = "jco", legend.title = "Race", title = "BMI by DHA", subtitle = "Ages Q4 (53-79)",
                  position = position_dodge(0.3)     # Adjust the space between bars
)

ggarrange(p5,p6,p7,p8, ncol = 2, nrow = 2, common.legend = TRUE)

dev.off() # flush that pdf! Closes the PDF so you can open it
