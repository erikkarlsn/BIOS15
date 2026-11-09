# part 1---- 
set.seed(1)
x = rnorm(n=100, mean=5, sd=1)
mean(x)
#hist(x, las=1, mean="")
sd(x)

# part2, bootstrapping----
out = NULL


set.seed(1)
x = rnorm(50,10,2)
se_x = sqrt(var(x)/length(x))

#bootstrapping of ... 
for (i in 1:1000){
  sample1 = sample(x, replace=TRUE)
  out[i] = mean(sample1)
}

hist(out,las=1,main="") #plot histogram of out
sd(out)
se_x
quantile(out, c(0.025, 0.975)) #check values at defined quantiles
qnorm(c(0.025,0.975))

# Exercise 1, non-parametric bootstrapping 95% conf for CV ----
set.seed(1)

# CV = std/mean
cv = function(x) {
  sd(x)/mean(x )
}

x = rnorm(1000, 10, 4)

out = NULL
for (i in 1:500){
  sample1 = sample(x, length(x), replace=TRUE)
  out[i] = cv(sample1)
}
hist(out,las=1,main="")
quantile(out, c(0.025, 0.975)) #check values at defined quantiles

# Exercise 1.2 (optional) CV vs SD(log(X)).----
set.seed(1)
out_sdlogx = NULL
out_cv = NULL

for (i in 1:100){
  x = rnorm(1000, 100, i/4)
  out_sdlogx[i] = sd(log(x))
  out_cv[i] = cv(x)
}
plot(out_cv,out_sdlogx)
#test
