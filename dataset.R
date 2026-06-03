
library(dplyr)
library(truncnorm)
set.seed(16807)  

n <- 1500

ID <- sprintf("CVD%03d", 1:n)

Age <- round(rtruncnorm(n, a = 25, b = 80, mean = 50, sd = 15))

Gender <- sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.52, 0.48))

District <- sample(c("Colombo", "Kandy", "Galle"), n, replace = TRUE, prob = c(0.40, 0.35, 0.25))

Education <- sapply(District, function(d) {
  if (d == "Colombo") sample(c("Primary","Secondary","Higher"), 1, prob = c(0.20,0.50,0.30))
  else if (d == "Kandy") sample(c("Primary","Secondary","Higher"), 1, prob = c(0.35,0.45,0.20))
  else sample(c("Primary","Secondary","Higher"), 1, prob = c(0.45,0.40,0.15))
})

Pre_SBP <- round(rtruncnorm(n, a = 110, b = 200, mean = 140, sd = 20))

Pre_BMI <- round(rtruncnorm(n, a = 18.5, b = 40, mean = 26.5, sd = 4.5), 1)

Smoking <- ifelse(Gender == "Male", 
                  rbinom(n, 1, 0.35), 
                  rbinom(n, 1, 0.08)) 

Pre_Exercise <- sample(0:4, n, replace = TRUE)

Diabetes <- ifelse(Age > 50, rbinom(n, 1, 0.25), rbinom(n, 1, 0.12))


Participation <- mapply(function(d) {
  if (d == "Colombo") rbinom(1,1,0.70)
  else if (d == "Kandy") rbinom(1,1,0.60)
  else rbinom(1,1,0.50)
}, District)

Sessions <- ifelse(Participation==1, sample(6:12, n, replace = TRUE), 0)

BP_improvement <- ifelse(Participation==1, sample(5:20, n, replace=TRUE), sample(0:5, n, replace=TRUE))
Post_SBP <- pmax(100, Pre_SBP - BP_improvement)

BMI_improvement <- ifelse(Participation==1, runif(n,0.5,3.0), runif(n,0,1.0))
Post_BMI <- round(pmax(18.5, Pre_BMI - BMI_improvement),1)

Post_Smoking <- sapply(1:n, function(i){
  if (Smoking[i]==1 & Participation[i]==1) {
    if (rbinom(1,1,0.30)==1) return(0) else return(1)
  } else if (Smoking[i]==1 & Participation[i]==0) {
    if (rbinom(1,1,0.10)==1) return(0) else return(1)
  } else {
    return(0)
  }
})

Exercise_increase <- ifelse(Participation==1, sample(1:4, n, replace=TRUE), sample(0:1, n, replace=TRUE))
Post_Exercise <- pmin(7, Pre_Exercise + Exercise_increase)


CVD_prob <- rep(0.03, n)

CVD_prob <- CVD_prob + ifelse(Age>60, 0.04, 0)
CVD_prob <- CVD_prob + ifelse(Gender=="Male", 0.03, 0)
CVD_prob <- CVD_prob + ifelse(Post_Smoking==1, 0.05, 0)
CVD_prob <- CVD_prob + ifelse(Diabetes==1, 0.06, 0)
CVD_prob <- CVD_prob + ifelse(Post_SBP>160, 0.04, 0)
CVD_prob <- CVD_prob - ifelse(Participation==1, 0.02, 0)


CVD_prob <- pmin(pmax(CVD_prob,0),1)

CVD_Event <- ifelse(runif(n) < CVD_prob, "Yes", "No")


df <- data.frame(
  ID, Age, Gender, District, Education,
  Pre_SBP, Pre_BMI, Smoking, Pre_Exercise, Diabetes,
  Participation, Sessions,
  Post_SBP, Post_BMI, Post_Smoking, Post_Exercise,
  CVD_Event
)

head(df)

# Export to CSV 
write.csv(df, "C:/Users/oshin/Desktop/health assignment/CVD_synthetic_dataset.csv", row.names = FALSE)

