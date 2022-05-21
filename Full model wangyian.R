####model 1
#install.packages("corrplot")
A <- read.csv("train.csv",header = TRUE)#151 train data, 50 predict data
y <- as.vector(A$life.exp)
A$reg
table(A$reg)
# To classify the catogrical data 

c1 <- c(rep(1,sum(A$reg == "west")),rep(0,sum(A$reg == "east")),rep(0,sum(A$reg == "north")),rep(0,sum(A$reg == "south")),rep(0,sum(A$reg == "central")),
        rep(0,sum(A$reg == "west")),rep(1,sum(A$reg == "east")),rep(0,sum(A$reg == "north")),rep(0,sum(A$reg == "south")),rep(0,sum(A$reg == "central")),
        rep(0,sum(A$reg == "west")),rep(0,sum(A$reg == "east")),rep(1,sum(A$reg == "north")),rep(0,sum(A$reg == "south")),rep(0,sum(A$reg == "central")),
        rep(0,sum(A$reg == "west")),rep(0,sum(A$reg == "east")),rep(0,sum(A$reg == "north")),rep(1,sum(A$reg == "south")),rep(0,sum(A$reg == "central")))
c2 <- matrix(c1,byrow = FALSE,ncol = 4) 
# central Z1 == 1
# east    Z2 == 1
# north   Z3 == 1 
# south   Z4 == 1
# west    Zi == 0

# Get other items from A:
p1 <- as.vector(A$illi)
p2 <- as.vector(A$pop)
p3 <- as.vector(A$inc)
# <-  as.vector(A$reg)
p4 <-as.vector(A$grad)
p5 <- as.vector(A$area)
p6 <- as.vector(A$temp)
p7 <- as.vector(A$emp)
p8 <- as.vector(A$ext)
p9 <- as.vector(A$food)
c1 <- c(rep(1,25),rep(0,31),rep(0,31),rep(0,28),rep(0,36),
        rep(0,25),rep(1,31),rep(0,31),rep(0,28),rep(0,36),
        rep(0,25),rep(0,31),rep(1,31),rep(0,28),rep(0,36),
        rep(0,25),rep(0,31),rep(0,31),rep(1,28),rep(0,36))
X <- matrix(c(p1,p2,p3,p4,p5,p6,p7,p8,p9,c1),byrow = FALSE,ncol = 13)
library(corrplot)
mycor <- cor(X)
corrplot(mycor,method = "circle",type = "upper",order = "hclust")
X11()
corrplot.mixed(mycor,upper = "ellipse")


x <- matrix(c(p3,p4,p8,p9,c1),byrow = FALSE,ncol = 8)
mycor2 <- cor(x)
X11()
corrplot.mixed(mycor2,upper = "ellipse")


#############################arrise the model
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
x4[x4 == "central"]="central&north"
s1<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
summary(s1)
windows()
par(mfrow=c(2,2))
plot(s1,main = "Test For Original Model")

x4[x4 == "north"]="central&north";
x4[x4 == "central"]="central&north"
FM <- lm(y~x3+x4+x5+x6+x9+x10)
FM
summary(FM)
windows()
par(mfrow=c(2,2))
plot(FM,main  = "Tests For Final Model")
par(mfrow = c(1,1))



# Test the number of the values 
full_Model <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
PRESS <- function(model){
  sum((residuals(model)/(1-lm.influence(model)$hat))^2)
}
library(olsrr)
cp <- function(model,full_model){
  return(length(model$coefficients) - 1+(sum(residuals(model)^2) - sum(residuals(full_model)^2))*(length(length(residuals(model))-length(model$coefficients)))/(sum(residuals(full_model)^2)))
}
model1 <- lm(y~x4)
model2 <- lm(y~x4+x9)
model3 <- lm(y~x4+x9+x10)
model4 <- lm(y~x4+x9+x10+x6)
model5 <- lm(y~x4+x9+x10+x6+x5)
model6 <- lm(y~x4+x9+x10+x6+x5+x3)

v <- c(" ","x4","x4,x9","x4,x9,x10","x4,x9,x10,x6","x4,x9,x10,x6,x5","x4,x9,x10,x6,x5,x3")
adj_R_square <- c("Adjusted R^2",summary(model1)$adj.r.squared,summary(model2)$adj.r.squared,summary(model3)$adj.r.squared,summary(model4)$adj.r.squared,summary(model5)$adj.r.squared,summary(model6)$adj.r.squared)
press <- c("PRESS",PRESS(model1),PRESS(model2),PRESS(model3),PRESS(model4),PRESS(model5),PRESS(model6))
aic <- c("AIC",AIC(model1),AIC(model2),AIC(model3),AIC(model4),AIC(model5),AIC(model6))
bic <- c("BIC",BIC(model1),BIC(model2),BIC(model3),BIC(model4),BIC(model5),BIC(model6))
Cp  <- c("Cp",ols_mallows_cp(model1,full_Model),ols_mallows_cp(model2,full_Model),ols_mallows_cp(model3,full_Model),ols_mallows_cp(model4,full_Model),ols_mallows_cp(model5,full_Model),ols_mallows_cp(model6,full_Model))
data.frame(matrix(c(v,adj_R_square,press,aic,bic,Cp),ncol = 6,byrow = FALSE))

# Cancel the 59th data

A<-A[-59,]
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
FM <- lm(y~x3+x4+x5+x6^2+x9+x10)
summary(FM)
windows()
par(mfrow=c(2,2))
plot(FM,main  = "Tests For Final Model (without 59th data)")
par(mfrow = c(1,1))
X11()
boxplot(y)
which(y == max(y))



B <- read.csv("predict.csv",header = TRUE)


B

B_P <- data.frame(x3 = B$inc,x4 = B$reg,x5 = B$grad,x6 = B$area,x9 = B$ext,x10 = B$food)
a <- predict(FM,B_P)
a <- as.vector(a)# the estimated value
b <- as.vector(B$life.exp)

SSE <- mean((b-a)^2)# residual

SST <- mean((b-rep(mean(b),length(b)))^2)
SSR <- SST-SSE
SSR
Rsquare <- (SST-SSE)/SST
Rsquare


summary(lm(y~x3+x4+x5+x6+x9+x10))

plot(manpower[,-6])
