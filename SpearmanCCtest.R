#---------------- Spearman CC test --------------

x <- seq(0,10, by = 0.01)
x
y <- sin(2000*x) + x
y
plot(y~x)

cor(x,y)
cor(x,y, method = "spearman" )

x <- seq(-10,10, by = 0.01)

y <- x*x
plot(y~x)

cor(x,y)
cor(x,y, method = "spearman" )
