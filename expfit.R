a = exp(1)
b = 0.063
x = 1:100
y = 2.3*exp(b * x) + rnorm(100)

plot(x,y)

expfit = nls(y ~ exp(a + b*x), start=list(a=0, b=0))



abeta <- 0.05
x = 1:100
y = 2.3*exp(beta*x) + rnorm(100)


# plot data
plot(x,y)

# fit non-linear model
mod <- nls(y ~ exp(a + b * x), start = list(a=0, b=0))
