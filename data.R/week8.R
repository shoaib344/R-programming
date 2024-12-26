
#WEEK 8


#MAtrix
A = matrix(c(2,3,5,7), nrow=2, ncol=2)
A
B = rbind(c(1,2),c(8,9))  # bind as rows
B
C = cbind(c(1,2),c(8,9))  # bind as columns
C

A+B
A*B
A%%B


diag(c(1,1))

t(A)

Ainv = solve(A)
Ainv

Ainv %*% A
A %*% Ainv


A
sum(A)
rowSums(A)
colSums(A)


A
mean(A)
rowMeans(A)
colMeans(A)

#Vectors
data = c(4,2,8,5,1)
data


# Visualise parallelogram law of vector addition
par(pty="s")    # square plotting region
plot(0,0,xlim=c(-5,10),ylim=c(-5,10))
grid()
arrows(0,0,a[1],a[2],col='red')
arrows(0,0,b[1],b[2],col='green')

arrows(0,0,c[1],c[2],col='blue')

arrows(a[1],a[2],a[1]+b[1],a[2]+b[2],col="magenta")
arrows(b[1],b[2],b[1]+a[1],b[2]+a[2],col="cyan")


a = rbind(c(4,2))
a
par(pty="s")    # square plotting region
plot(0,0,xlim=c(-10,10),ylim=c(-10,10))
grid()
arrows(0,0,a[1],a[2],col='red',lwd=4)

k = 0.5
b = k*a
arrows(0,0,b[1],b[2],col='green')
k = 2.0
b = k*a
arrows(0,0,b[1],b[2],col='blue')
k = -0.5
b = k*a
arrows(0,0,b[1],b[2],col='cyan')
k = -2.0
b = k*a
arrows(0,0,b[1],b[2],col='magenta')

















library(tidyverse)

#---
# Example 1.  Anscombe's quartet

a = 1   # which dataset from Anscombe (1,2,3,4)
x = anscombe[,a]
y = anscombe[,a+4]



# Linear regression by matrix operations
X = cbind(1,x)
XtX = t(X) %*% X
XtXinv = solve(XtX)
beta = XtXinv %*% t(X) %*% y
beta
y_fitted = X %*% beta
residuals = t(y - y_fitted)
residuals

# Linear regression using lm()
model = lm(y~x)
summary(model)
model$residuals
ggplot(NULL, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)




library(faraway)
G = as_tibble(gala)
G

# Linear regression by matrix operations
X = as.matrix(cbind(1,G[,c(3,4,5,6,7)]))
y = gala$Species

XtX = t(X) %*% X
XtXinv = solve(XtX)
beta = XtXinv %*% t(X) %*% y
beta
y_fitted = X %*% beta
residuals = t(y - y_fitted)
residuals

# Linear regression using lm()
model = lm(Species~Area+Elevation+Nearest+Scruz+Adjacent, data=G)
summary(model)
model$residuals






a = cbind(c(1,0))
b = cbind(c(0,1))

flag = rbind(c(0, 0, 1, 1, 0),
             c(0, 2, 2, 1, 1))

# Setup square plotting region (old-school R)
# and set the axis limits
par(pty="s")
plot(0,0,xlim=c(-2,2),ylim=c(-2,2))
grid()











