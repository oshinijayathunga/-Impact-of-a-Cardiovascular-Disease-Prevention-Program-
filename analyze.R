
dataset= read.csv("C:\\Users\\oshin\\Desktop\\health assignment\\CVD_dataset.csv")
dataset$Smoking <- factor(dataset$Smoking, labels = c("No", "Yes"))
dataset$Diabetes <- factor(dataset$Diabetes, labels = c("No", "Yes"))
dataset$Participation <- factor(dataset$Participation, labels = c("No", "Yes"))
dataset$Post_Smoking <- factor(dataset$Post_Smoking, labels = c("No", "Yes"))

summary(dataset)


#Standard deviations of numerical variables
sd(dataset$Age)
sd(dataset$Pre_SBP)
sd(dataset$Pre_BMI)
sd(dataset$Pre_Exercise)
sd(dataset$Sessions)
sd(dataset$Post_SBP)
sd(dataset$Post_BMI)
sd(dataset$Post_Exercise)
#mean of numerical variables
mean(dataset$Age)
mean(dataset$Pre_SBP)
mean(dataset$Pre_BMI)
mean(dataset$Pre_Exercise)
mean(dataset$Sessions)
mean(dataset$Post_SBP)
mean(dataset$Post_BMI)
mean(dataset$Post_Exercise)


#proportion tables and frequency tabels for catogorical variables
table(dataset$Gender)
prop.table(table(dataset$Gender))

table(dataset$District)
prop.table(table(dataset$District))

table(dataset$Education)
prop.table(table(dataset$Education))

table(dataset$Smoking)
prop.table(table(dataset$Smoking))

table(dataset$Diabetes)
prop.table(table(dataset$Diabetes))

table(dataset$Post_Smoking)
prop.table(table(dataset$Post_Smoking))

table(dataset$CVD_Event)
prop.table(table(dataset$CVD_Event))

table(dataset$Participation)
prop.table(table(dataset$Participation))

#frequency tables

library(descriptr)
ds_freq_table(dataset, Age, 3)
ds_freq_table(dataset, Pre_BMI, 3)

ds_freq_table(dataset, Post_BMI, 3)
ds_freq_table(dataset, Sessions, 4)

##discriptive data analysis
hist(dataset$Age,xlab="Age",ylab="Frequency",col="lightblue",main="Historgram of Age")
hist(dataset$Pre_SBP,xlab="SBP",ylab="Frequency",main="Historgram of SBP before the program", col ="lightblue")
hist(dataset$Pre_BMI,xlab="BMI",ylab="Frequency",main="Historgram of BMI before the program", col ="lightblue")
hist(dataset$Pre_Exercise,breaks = seq(-0.5, 4.5, 1),col = "skyblue",main = "Histogram of number of days exercised before the program",xlab = "No of days",ylab = "Frequency")

pie(table(dataset$Participation),main = "Participation distribution",col = c("lightblue","lightpink"))
pie(table(dataset$CVD_Event),main = "CVD event distribution",col = c("lightblue","lightpink"))

table_participated=table(dataset$Gender,dataset$Participation)
table_participated=prop.table(table_participated,1)
table_participated
barplot(table_participated,legend.text=row.names(table_participated),main='Compound Bar chart for participated status and gender',col=c('lightblue','blue'))

table_participated_district=table(dataset$District,dataset$Participation)
table_participated_district=prop.table(table_participated_district,1)
table_participated_district
barplot(table_participated_district,legend.text=row.names(table_participated_district),main='Compound Bar chart for participated status and district',col=c('lightblue','blue','darkblue'))

table_participated_education=table(dataset$Education,dataset$Participation)
table_participated_education
table_participated_education=prop.table(table_participated_education,1)
table_participated_education
barplot(table_participated_education,legend.text=row.names(table_participated_education),main='Compound Bar chart for participated status and education status',col=c('lightblue','blue','darkblue'))

table_participated_CVD=table(dataset$CVD_Event,dataset$Participation)
table_participated_CVD
table_participated_CVD=prop.table(table_participated_CVD,1)
table_participated_CVD
barplot(table_participated_CVD,legend.text=c("CVD-yes","CVD-no"),main='Compound Bar chart for participated status and CVD status',col=c('lightblue','blue','darkblue'))

smoke_table <- rbind(table(dataset$Smoking),table(dataset$Post_Smoking))
smoke_table
barplot(smoke_table, beside=TRUE,col=c("lightblue","blue"),main="Smoking Status Before vs After Program",ylab="Count",legend.text = c("Before","After"))


boxplot(dataset$Pre_SBP, dataset$Post_SBP,names=c("Before", "After"),col=c("lightblue","lightgreen"),main="SBP Before vs After Program",ylab="Systolic Blood Pressure (mmHg)")
boxplot(dataset$Pre_BMI, dataset$Post_BMI,names=c("Before", "After"),col=c("lightblue","lightgreen"),main="BMI Before vs After Program",ylab="BMI")
boxplot(dataset$Post_Exercise, dataset$Post_Exercise,names=c("Before", "After"),col=c("lightblue","lightgreen"),main="No of days exercise Before vs After Program",ylab="Days")

table(dataset$Gender,dataset$Participation)

#Shapiro test to normality assumption 
shapiro.test(dataset$Age)
shapiro.test(dataset$Pre_SBP)
shapiro.test(dataset$Pre_BMI)
shapiro.test(dataset$Pre_Exercise)
shapiro.test(dataset$Sessions)
shapiro.test(dataset$Post_SBP)
shapiro.test(dataset$Post_BMI)
shapiro.test(dataset$Post_Exercise)

#wilcoxons signed rank test for pre vs post SBP
wilcox.test(dataset$Pre_SBP, dataset$Post_SBP, paired=TRUE, alternative="greater",conf.level = 0.95)

#wilcoxons signed rank test for pre vs post Bmi
wilcox.test(dataset$Pre_BMI, dataset$Post_BMI, paired=TRUE, alternative = "greater",conf.int = 0.95)

#wilcoxons signed rank test for pre vs post Exercise days
wilcox.test(dataset$Post_Exercise, dataset$Pre_Exercise, paired=TRUE, alternative="greater",conf.level = 0.95)

# CVD incidence vs participation
chisq.test(table(dataset$CVD_Event, dataset$Participation),correct = TRUE)

#health improvement in different districts
SBP_change <- dataset$Pre_SBP - dataset$Post_SBP
kruskal.test(SBP_change ~ District, data=dataset)

BMI_change <- dataset$Pre_BMI - dataset$Post_BMI
kruskal.test(BMI_change ~ District, data=dataset)

#helath improvement in different age groups 
dataset$AgeGroup <- cut(dataset$Age, breaks = c(0, 39, 59, 100),labels = c("Young (<40)", "Middle (40-59)", "Older (60+)"))
table(dataset$AgeGroup)

kruskal.test(SBP_change ~ AgeGroup, data = dataset)
kruskal.test(BMI_change ~ AgeGroup, data = dataset)

#health improvement in gender
wilcox.test(SBP_change ~ dataset$Gender, data = dataset)
wilcox.test(BMI_change ~ dataset$Gender, data = dataset)

#program effect on diabetics (baseline factor)
wilcox.test(SBP_change ~ Diabetes, data=dataset)
wilcox.test(BMI_change ~ Diabetes, data=dataset)

#CDV event and diabatise
chisq.test(table(dataset$CVD_Event, dataset$Diabetes))

# Smoking vs CVD Event
chisq.test(table(dataset$CVD_Event, dataset$Smoking))

#Education level vs health change
kruskal.test(SBP_change ~ Education, data = dataset)
kruskal.test(BMI_change ~ Education, data = dataset)
