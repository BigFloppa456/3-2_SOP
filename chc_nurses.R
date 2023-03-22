library("dplyr") #Initialize libraries in code
library("psych")
library("readxl")
setwd("C:/Users/divit/OneDrive/Desktop/3-2/SOP")
###################
#CHC Client
chc_nur <- read_xlsx("CHC NURSES.xlsx")
chc_nur <- na.omit(chc_nur)
chc_nur

#If need to check for subset of questions
demographics <- select(chc_nur, 2,3,4,5,6,7,8) 
info <- select(chc_nur,9:16)
empathy <- select(chc_nur, 17:20)
responsiveness <- select(chc_nur, 21:25)
phy_safe <- select(chc_nur, 26:30)
serv_edu <- select(chc_nur, 31:35)
train <- select(chc_nur, 36:37)
drugs <- select(chc_nur, 38:39)
equip <- select(chc_nur, 40:41)
inf_con <- select(chc_nur, 42:44)
support <- select(chc_nur, 45:50)
admin <- select(chc_nur, 51:57)
nuru <- select(chc_nur, 58:63)
dis_man <- select(chc_nur, 64:68)

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
alpha(nuru, check.keys=TRUE)
alpha(dis_man, check.keys=TRUE)


chc_nur1<-chc_nur %>% mutate(Total = select(., AGE:NDM5) %>% rowSums(na.rm = TRUE))
apple<-cor(chc_nur1)

estimates = numeric(70)
pvalues = numeric(70)
for (i in 2:69){
  x<-chc_nur1[,i]
  colnames(x)<- c("x")
  
  test <- cor.test(chc_nur1$Total, x$x)
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

write.csv(hypo_test1, "chc_nurses_validity.csv")

