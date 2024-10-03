#Codigo para problema 2
mis_dades<-iris
mis_dades
x<-mis_dades$Petal.Length
y<-mis_dades$Sepal.Length
plot(x,y)
#media =mean()
x_bar <- mean(x)
y_bar <- mean(y)

m<- sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)
m
b<- y_bar-m*x_bar
b
m*1.5+b
x_pred <- x
x_pred
y_pred <- m*x_pred+b
y_pred
plot(x,y)
lines(x_pred,y_pred)
#r-squared
Rsq <- sum((y_pred-y_bar)^2)/sum((y-y_bar)^2)
Rsq
cor <- sqrt(Rsq)
cor

mod <- lm(y~x)
mod
summary(mod)
#otramanera de calcular cor
cor.test(x,y)
y_pred2 <- predict(mod, data.frame(x=1.5))
y_pred2
