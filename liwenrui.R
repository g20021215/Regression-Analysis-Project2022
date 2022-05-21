##Part a
A<-read.csv("projectdata.csv",header = TRUE)
y<-A$life.exp
x1<-A$illi
x2<-A$pop
x3<-A$inc
x4<-A$reg
x5<-A$grad
x6<-A$area
x7<-A$temp
x8<-A$emp
x9<-A$ext
x10<-A$food
s1<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
summary(s1)##对full model显著性检验
##对full model进行多重共线性检验
library(car)
vif(s1)
kappa(s1,exact=TRUE)
##逐步回归法解决多重共线性
x4[x4 == "north"]="central&north"
x4[x4 == "central"]="central&north"
s1_1<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
s2<-step(s1_1,direction = "both")
summary(s2)
#对Final model进行多重共线性检验
library(car)
kappa(s2,exact=TRUE)
vif(s2)
