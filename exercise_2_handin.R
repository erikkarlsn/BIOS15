# exercise 2. fit lin reg of real data, bird allometry -----------------------
birds = read.csv("C:/Users/erikk/OneDrive/Documents/BIOS15/bird_allometry.csv")
head(birds)

#split data into males & females
males = birds[birds$Sex=="m",]
females = birds[birds$Sex=="f",]

#fit linear models
mm = lm(log(brain_mass)~log(body_mass), data=males)
mf = lm(log(brain_mass)~log(body_mass), data=females)

#check residuals
hist(residuals(mm))
hist(residuals(mf))
summary(mm)

# save log data as variable
male_x = log(males$body_mass)
male_y = log(males$brain_mass)

female_x = log(females$body_mass)
female_y = log(females$brain_mass)

summary(mm)$r.squared  # Male R2
summary(mf)$r.squared  # Female R2


# Linear regression plotting
png("bird_allometry_v4.png",
    width = 6, height = 4.5,
    units = "in", res = 300)

plot(male_x, male_y,
     col = "steelblue",
     xlab = "Body mass (log g)",
     ylab = "Brain mass (log g)",
     ylim = c(-2, 4.5))

points(female_x, female_y,
       col = "tomato",
)

# Male regression in data range
newx_m = seq(min(male_x), max(male_x), length.out = 200)
newy_m = coef(mm)[1] + coef(mm)[2] * newx_m
lines(newx_m, newy_m, col = "black",     lwd = 3)
lines(newx_m, newy_m, col = "steelblue", lwd = 2)

# Female regression in data range
newx_f = seq(min(female_x), max(female_x), length.out = 200)
newy_f = coef(mf)[1] + coef(mf)[2] * newx_f
lines(newx_f, newy_f, col = "black",     lwd = 3)
lines(newx_f, newy_f, col = "tomato",    lwd = 2)

eq_m = sprintf(
  "Males: y = %.2f + %.2f x; R² = %.3f",
  coef(mm)[1], coef(mm)[2], summary(mm)$r.squared
)

eq_f = sprintf(
  "Females: y = %.2f + %.2f x; R² = %.3f",
  coef(mf)[1], coef(mf)[2], summary(mf)$r.squared
)

legend("topleft",
       legend = c(eq_m, eq_f),
       text.col = c("steelblue", "tomato"),
       bty = "n",
       cex = 0.75)

dev.off()

# residual histogram plotting
png("bird_allometry_residual_histograms_v2.png",
    width = 6, height = 4,
    units = "in", res = 300)

old_par = par(mfrow = c(1, 2))

hist(residuals(mm),
     main = "Male model residuals",
     xlab = "Residuals (log scale)",
     col = "steelblue")

hist(residuals(mf),
     main = "Female model residuals",
     xlab = "Residuals (log scale)",
     col = "tomato")

par(old_par) 
dev.off()