install.packages("dplyr") #Install Libraries if you do not have them
install.packages("psych")
install.packages("readxl")
install.packages("psy")
install.packages("tidyverse")

library("dplyr") #Initialize libraries in code
library("psych")
library("readxl")
library(psy)
library("tidyverse")
setwd("C:/Users/divit/OneDrive/Desktop/3-2/SOP")
###################
#CHC Client
chc_cli <- read_xlsx("CHC CLIENTS.xlsx")
chc_cli <- na.omit(chc_cli)
chc_cli

#If need to check for subset of questions
demographics <- select(chc_cli, 2,3,4,5,6,7,8,9,10,11) 
empathy <- select(chc_cli,12,13,14,15)
tangibility <- select(chc_cli, 16:32)
responsiveness <- select(chc_cli, 33:35)
assurance <- select(chc_cli, 36:43)
cost_eff <- select(chc_cli, 44:45)
timeliness <- select(chc_cli, 46:49)

alpha(empathy, check.keys = TRUE)
alpha(tangibility, check.keys=TRUE)
alpha(responsiveness, check.keys=TRUE)
alpha(assurance, check.keys=TRUE)
alpha(cost_eff, check.keys=TRUE)
alpha(timeliness, check.keys=TRUE)

#cronbach(empathy)[3]
###########
emp1 <- empathy
t1<- tangibility
r1<-responsiveness
a1<-assurance
c1<-cost_eff
time1<-timeliness
emp1<-emp1 %>% mutate(Total = select(., CE1:CE4) %>% rowSums(na.rm = TRUE))
t1<-t1 %>% mutate(Total = select(., CT1...16:CT17) %>% rowSums(na.rm = TRUE))
r1<-r1 %>% mutate(Total = select(., CR1:CR3) %>% rowSums(na.rm = TRUE))
t1

cor(emp1)
emp1




chc_cli1<-chc_cli %>% mutate(Total = select(., AGE:CT4...49) %>% rowSums(na.rm = TRUE))
apple<-cor(chc_cli1)


estimates = numeric(50)
pvalues = numeric(50)
for (i in 2:50){
  x<-chc_cli1[,i]
  colnames(x)<- c("x")
  
  test <- cor.test(chc_cli1$Total, x$x)
  estimates[i] = test$estimate
  pvalues[i] = test$p.value
}
p<-data.frame(pvalues)
p

p_crit <- 0.05
x1<-character(50)

for (i in 2:50){
  if (is.na(pvalues[i]) == FALSE){
    if (pvalues[i] > p_crit){
      x1[i] <- "Not Valid"
    }
    else{
      x1[i] <- "Valid"
    }
  }
  else{
    x1[i] <- "Undefined"
  }
}
x1
hypo_test <- data.frame(x1)
colnames(hypo_test)<- "significant?"

hypo_test1 <- head(hypo_test, -1)              # Apply head function
hypo_test1   
hypo_test1<-hypo_test1[-1,]
hypo_test1<-data.frame(hypo_test1)
colnames(hypo_test1)<- "Validity"

write.csv(hypo_test1, "chc_clients_validity.csv")
