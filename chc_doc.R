library("dplyr") #Initialize libraries in code
library("psych")
library("readxl")
setwd("C:/Users/divit/OneDrive/Desktop/3-2/SOP")
###################
#CHC Client
chc_doc <- read_xlsx("CHC DOCTORS.xlsx")
chc_doc <- na.omit(chc_doc)
chc_doc

#If need to check for subset of questions
demographics <- select(chc_doc, 2,3,4,5,6,7,8) 
info <- select(chc_doc,9:16)
empathy <- select(chc_doc, 17:20)
responsiveness <- select(chc_doc, 21:25)
phy_safe <- select(chc_doc, 26:30)
serv_edu <- select(chc_doc, 31:35)
train <- select(chc_doc, 36:37)
drugs <- select(chc_doc, 38:39)
equip <- select(chc_doc, 40:41)
inf_con <- select(chc_doc, 42:44)
support <- select(chc_doc, 45:50)
admin <- select(chc_doc, 51:57)
docu <- select(chc_doc, 58:63)
dis_man <- select(chc_doc, 64:68)

alpha(info, check.keys = TRUE)
alpha(empathy, check.keys=TRUE)
alpha(responsiveness, check.keys=TRUE)
alpha(phy_safe, check.keys=TRUE)
alpha(serv_edu, check.keys=TRUE)
alpha(train, check.keys=TRUE)
alpha(drugs, check.keys=TRUE)
alpha(equip, check.keys=TRUE)
alpha(inf_con, check.keys=TRUE)
alpha(support, check.keys=TRUE)
alpha(admin, check.keys=TRUE)
alpha(docu, check.keys=TRUE)
alpha(dis_man, check.keys=TRUE)

chc_doc1<-chc_doc %>% mutate(Total = select(., AGE:NDM5) %>% rowSums(na.rm = TRUE))
apple<-cor(chc_doc1)

estimates = numeric(70)
pvalues = numeric(70)
for (i in 2:69){
  x<-chc_doc1[,i]
  colnames(x)<- c("x")
  
  test <- cor.test(chc_doc1$Total, x$x)
  estimates[i] = test$estimate
  pvalues[i] = test$p.value
}
p<-data.frame(pvalues)
p

p_crit <- 0.05
x1<-character(50)

for (i in 2:69){
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

write.csv(hypo_test1, "chc_doctors_validity.csv")

