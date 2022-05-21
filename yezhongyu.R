train <- read.csv("train1.csv",
                  header = T)
y <- train$life.exp
x1 <- train$illi
x2 <- train$pop
x3 <- train$inc
x4 <- train$reg
x5 <- train$grad
x6 <- train$area
x7 <- train$temp
x8 <- train$emp
x9 <- train$ext
x10 <- train$food
x4[x4=="north"|x4=="central"]="central&north"
model <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
summary(model)
step(model)
model.stepped <- lm(y ~ x3 + x4 + x5 + x6 + x9 + x10)
summary(model.stepped)
model.stepping <- lm(y ~ x2 + x3 + x4 + x5 + x6 + x9 + x10)
summary(model.stepping)

train <- read.csv("train2.csv",
                  header = T)
y <- train$life.exp
x1 <- train$illi
x2 <- train$pop
x3 <- train$inc
x4 <- train$reg
x5 <- train$grad
x6 <- train$area
x7 <- train$temp
x8 <- train$emp
x9 <- train$ext
x10 <- train$food
x4[x4=="north"|x4=="central"]="central&north"
model <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
summary(model)
step(model)
model.stepped <- lm(y ~ x3 + x4 + x5 + x6 + x9 + x10)
summary(model.stepped)
model.stepping <- lm(y ~ x2 + x3 + x4 + x5 + x6 + x9 + x10)
summary(model.stepping)

train <- read.csv("train3.csv",
                  header = T)
y <- train$life.exp
x1 <- train$illi
x2 <- train$pop
x3 <- train$inc
x4 <- train$reg
x5 <- train$grad
x6 <- train$area
x7 <- train$temp
x8 <- train$emp
x9 <- train$ext
x10 <- train$food
x4[x4=="north"|x4=="central"]="central&north"
model <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
step(model)
model.stepped <- lm(y ~ x3 + x4 + x5 + x6 + x9 + x10)
summary(model.stepped)
model.stepping <- lm(y ~ x2 + x3 + x4 + x5 + x6 + x9 + x10)
summary(model.stepping)

train <- read.csv("train4.csv",
                  header = T)
y <- train$life.exp
x1 <- train$illi
x2 <- train$pop
x3 <- train$inc
x4 <- train$reg
x5 <- train$grad
x6 <- train$area
x7 <- train$temp
x8 <- train$emp
x9 <- train$ext
x10 <- train$food
x4[x4=="north"|x4=="central"]="central&north"
model <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
step(model)
model.stepped <- lm(y ~ x3 + x4 + x5 + x6 + x9 + x10)
summary(model.stepped)
model.stepping <- lm(y ~ x2 + x3 + x4 + x5 + x6 + x9 + x10)
summary(model.stepping)
