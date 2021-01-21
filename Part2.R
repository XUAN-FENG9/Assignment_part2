library(tidyverse)
library(tidyr)
library(dplyr)
library(readxl)
library(writexl)
library(plm)
library(lubridate)
library(psych)


main<-read_xlsx("D:/Dataset Assignment 1.2.xlsx")

#adjust the value of roa, tang, size and ret to the values at t-1 time
main<-main[duplicated(main$firmnr),]
write_xlsx(main,"D:/mainfile_part2.xlsx")

#QUestion 10: panel regressoin with time and firm effect
pmain<-pdata.frame(main)
preg1<-plm(lev~roa_lag+tang_lag+size_lag+ret_lag,data = pmain,effect = "twoways",model = "within")
summary(preg1)

# Question 11: fit
OLSreg<-lm(lev~roa_lag+tang_lag+size_lag+ret_lag,data = main)
summary(OLSreg)
main$OLSfit<-fitted(OLSreg)
fixefft<-fixef(preg1,effect = "time")
fixeffi<-fixef(preg1,effect = "individual")
fixefft<-tibble(c(2006:2015),fixefft)
names(fixefft)[1]<-"year"
fixeffi<-tibble(c(1:439),fixeffi)
names(fixeffi)[1]<-"firmnr"
main<-merge(main,fixefft,by="year")
main<-merge(main,fixeffi,by="firmnr")
main$fitted<-main$OLSfit+main$fixeffi+main$fixefft
main%>%ggplot(aes(lev,fitted))+geom_point()+xlim(0,1)+ylim(0,1)+geom_smooth(method = lm)
qqnorm(main$fitted);qqline(main$fitted)
describe(main$fitted)
#dela with outliers
low<-quantile(main$fitted,0.05)
high<-quantile(main$fitted,0.95)
main$fitted<-ifelse(main$fitted>high,high,main$fitted)
main$fitted<-ifelse(main$fitted<low,low,main$fitted)
describe(main$fitted)

#Question 12: 
main$D_Overlev<-ifelse(main$lev>main$fitted,1,0)
sum(main$D_Overlev)
length(main$D_Overlev)
describe(main$D_Overlev)

#Question 13: binary regrassion
main$fc<-as.numeric(main$fc)
Breg1<-glm(main$D_Overlev~main$D_crisis+main$D_rated+main$D_distress+main$fc,data=main,family = "binomial")
summary(Breg1)

#Questin 14£º binary regrassion of SEO
Breg2<-Breg1<-glm(main$D_SEO~main$D_Overlev+main$D_crisis+main$D_rated+main$D_distress+main$fc,data=main,family = "binomial")
summary(Breg2)

#process main file
write_xlsx(main,"D:/process_part2¡£xlsx")