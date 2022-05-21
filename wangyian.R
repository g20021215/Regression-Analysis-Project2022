A<-read.csv("train.csv",header = TRUE)
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



####Wangyian¡®s part begins
A<-read.csv("train.csv",header = TRUE)
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
x4[x4 == "north"]="central&north";x4[x4 == "central"]="central&north"
full_Model <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
PRESS <- function(model){
  sum((residuals(model)/(1-lm.influence(model)$hat))^2)
}
library(olsrr)
modela <- lm(y~x3+x4+x5+x6+x9+x10)
modelb <- lm(y~x4+x5+x6+x9+x10)
modelc <- lm(y~x3+x4+x5+x6+x9^2+x10) 
#<- lm(y~x2+x3+x4+x5+x6+x9+x10)
v <- c(" ","Model a","Model b","Model c")
adj_R_square <- c("Adjusted R^2",summary(modela)$adj.r.squared,summary(modelb)$adj.r.squared,summary(modelc)$adj.r.squared)
press <- c("PRESS",PRESS(modela),PRESS(modelb),PRESS(modelc))
aic <- c("AIC",AIC(modela),AIC(modelb),AIC(modelc))
bic <- c("BIC",BIC(modela),BIC(modelb),BIC(modelc))
Cp  <- c("Cp",ols_mallows_cp(modela,full_Model),ols_mallows_cp(modelb,full_Model),ols_mallows_cp(modelc,full_Model))
data.frame(matrix(c(v,adj_R_square,press,aic,bic,Cp),nrow  = 4,byrow = FALSE))

x11()
par(mfrow=c(2,2))
plot(modela,main = "model a")
par(mfrow = c(1,1))



x11()
par(mfrow=c(2,2))
plot(modelb,main = "model b")
par(mfrow = c(1,1))

x11()
par(mfrow = c(2,2))
plot(modelc,main = "model c")
par(mfrow = c(1,1))

############33 model 1.y~x3+x4+x5+x6+x9^2+x10
A<-read.csv("train4.csv",header = TRUE)
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
full_Model <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
PRESS <- function(model){
  sum((residuals(model)/(1-lm.influence(model)$hat))^2)
}
library(olsrr)
modela <- lm(y~x3+x4+x5+x6+x9+x10)
modelb <- lm(y~x4+x5+x6+x9+x10)
modelc <- lm(y~x2+x3+x4+x5+x6+x9+x10)
modeld <- lm(y~x3+x4+x5+x6+x9^2+x10)
v <- c(" ","Modela","Modelb","Modelc","Modeld")
adj_R_square <- c("Adjusted R^2",summary(modela)$adj.r.squared,summary(modelb)$adj.r.squared,summary(modelc)$adj.r.squared,summary(modeld)$adj.r.squared)
press <- c("PRESS",PRESS(modela),PRESS(modelb),PRESS(modelc),PRESS(modeld))
aic <- c("AIC",AIC(modela),AIC(modelb),AIC(modelc),AIC(modeld))
bic <- c("BIC",BIC(modela),BIC(modelb),BIC(modelc),BIC(modeld))
Cp  <- c("Cp",ols_mallows_cp(modela,full_Model),ols_mallows_cp(modelb,full_Model),ols_mallows_cp(modelc,full_Model),ols_mallows_cp(modeld,full_Model))
data.frame(matrix(c(v,adj_R_square,press,aic,bic,Cp),nrow  = 5,byrow = FALSE))



x11()
par(mfrow = c(2,2))
plot(modeld,main = "model d")
par(mfrow = c(1,1))



#test
A <- read.csv("projectdata.csv",header = TRUE)
y<-A$life.exp
yc <- y[which(A$reg == "central")]
yn <- y[which(A$reg == "north")]
ys <- y[which(A$reg == "south")]
ye <- y[which(A$reg == "east")]
yw <- y[which(A$reg == "west")]
t.test(yc,yn,alt="two.sided",var.equal=FALSE)
var.test(yc,yn,alternative = "less")



#Del
A <- read.csv("projectdata.csv",header = TRUE)
y<-A$life.exp;x1<-A$illi;x2<-A$pop;x3<-A$inc;x4<-A$reg;x5<-A$grad;x6<-A$area;x7<-A$temp;x8<-A$emp;x9<-A$ext;x10<-A$food
c <- which(A$reg == "central");c2 <- x2[c];c3 <- x3[c];c4 <- x4[c];c5 <- x5[c];c6 <- x6[c];c9 <- x9[c];c10 <- x10[c]
s <- which(A$reg == "south");s2 <- x2[s];s3 <- x3[s];s4 <- x4[s];s5 <- x5[s];s6 <- x6[s];s9 <- x9[s];s10 <- x10[s]
n <- which(A$reg == "north");n2 <- x2[n];n3 <- x3[n];n4 <- x4[n];n5 <- x5[n];n6 <- x6[n];n9 <- x9[n];n10 <- x10[n]
e <- which(A$reg == "east");e2 <- x2[e];e3 <- x3[e];e4 <- x4[e];e5 <- x5[e];e6 <- x6[e];e9 <- x9[e];e10 <- x10[e]
w <- which(A$reg == "west");w2 <- x2[w];w3 <- x3[w];w4 <- x4[w];w5 <- x5[w];w6 <- x6[w];w9 <- x9[w];w10 <- x10[w]
Final_model <- as.vector(modela)
Final_model
table(A$reg)

f <- function(x3,flag,x5,x6,x9,x10){
  if (flag[1] == "central"){
    z <- c(0,0,0,0)
  }
  if (flag[1] == "south"){
    z <- c(0,0,1,0)
  }
  if (flag[1] == "north"){
    z <- c(0,1,0,0)
  }
  if (flag[1] == "east"){
    z <- c(1,0,0,0)
  }
  if (flag[1] == "west"){
    z <- c(0,0,0,1)
  }
  y <- 38.1897+1.6443*x3+2.8520*x5-1.7449*x6+4.8929*x9+4.5784*x10+t(rep(z%*%c(5.9472,0.8529,4.5104,2.7946),length(flag)))
  return(y)
}
cy <- f(x3 = c3,flag = c4,x5 = c5,x6 = c6,x9 = c9,x10 = c10)
sy <- f(x3 = s3,flag = s4,x5 = s5,x6 = s6,x9 = s9,x10 = s10)
ny <- f(x3 = n3,flag = n4,x5 = n5,x6 = n6,x9 = n9,x10 = n10)
ey <- f(x3 = e3,flag = e4,x5 = e5,x6 = e6,x9 = e9,x10 = e10)
wy <- f(x3 = w3,flag = w4,x5 = w5,x6 = w6,x9 = w9,x10 = w10)
cm <- mean(cy)
sm <- mean(sy)
nm <- mean(ny)
em <- mean(ey)
wm <- mean(wy)
lab <- c("","central","south","north","east","west")
me <- c("Mean",cm,sm,nm,em,wm)
data.frame(lab,me)

t.test(c3,n3,alt="two.sided",var.equal=FALSE)









# combine north and central and think it to be central
A <- read.csv("projectdata.csv",header = TRUE)
y<-A$life.exp;x1<-A$illi;x2<-A$pop;x3<-A$inc;x4<-A$reg;x5<-A$grad;x6<-A$area;x7<-A$temp;x8<-A$emp;x9<-A$ext;x10<-A$food
x4[x4 == "north"]="central"
fmodel <- lm(y~x3+x4+x5+x6+x9+x10)
fmodel
cn <- which(x4 == "central");c3 <- x3[cn];c4 <- x4[cn];c5 <- x5[cn];c6 <- x6[cn];c9 <- x9[cn];c10 <- x10[cn]
s <- which(x4 == "south");s3 <- x3[s];s4 <- x4[s];s5 <- x5[s];s6 <- x6[s];s9 <- x9[s];s10 <- x10[s]
e <- which(x4 == "east");e3 <- x3[e];e4 <- x4[e];e5 <- x5[e];e6 <- x6[e];e9 <- x9[e];e10 <- x10[e]
w <- which(x4 == "west");w3 <- x3[w];w4 <- x4[w];w5 <- x5[w];w6 <- x6[w];w9 <- x9[w];w10 <- x10[w]


f <- function(x3,flag,x5,x6,x9,x10){
  if (flag[1] == "central"){
    z <- c(0,0,0)
  }
  if (flag[1] == "south"){
    z <- c(0,1,0)
  }
  if (flag[1] == "north"){
    z <- c(0,0,0)
  }
  if (flag[1] == "east"){
    z <- c(1,0,0)
  }
  if (flag[1] == "west"){
    z <- c(0,0,1)
  }
  y <- 38.620 + 1.642*x3+2.838*x5-1.729*x6+4.922*x9+4.546*x10+t(rep(z%*%c(5.527,4.082,2.368),length(flag)))
  return(y)
}
cy <- f(x3 = c3,flag = c4,x5 = c5,x6 = c6,x9 = c9,x10 = c10)
sy <- f(x3 = s3,flag = s4,x5 = s5,x6 = s6,x9 = s9,x10 = s10)
ey <- f(x3 = e3,flag = e4,x5 = e5,x6 = e6,x9 = e9,x10 = e10)
wy <- f(x3 = w3,flag = w4,x5 = w5,x6 = w6,x9 = w9,x10 = w10)
cm <- mean(cy)
sm <- mean(sy)
em <- mean(ey)
wm <- mean(wy)
lab <- c("","central & north","south","east","west")
me <- c("Mean",cm,sm,em,wm)
data.frame(lab,me)
summary(fmodel)



