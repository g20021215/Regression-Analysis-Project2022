##Predict
#A<-read.csv("predict.csv",header = TRUE)
#A<-read.csv("predict2.csv",header = TRUE)
#A<-read.csv("predict3.csv",header = TRUE)
A<-read.csv("predict4.csv",header = TRUE)
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
x4[x4 == "north"]="central&north"
x4[x4 == "central"]="central&north"
s1<-lm(y~x3+x4+x5+x6+x9+x10)
sa<-summary(s1)
mean(sa$residuals^2)##MSE
sa$r.squared##R^2
s2<-lm(y~x4+x5+x6+x9+x10)
sb<-summary(s2)
mean(sb$residuals^2)
sb$r.squared
x9_2<-x9^2
s3<-lm(y~x3+x4+x5+x6+x9_2+x10)
sc<-summary(s3)
mean(sc$residuals^2)
sc$r.squared