require(segmented)
require(ggplot2)
require(reshape2)
require(lattice)
rm(list = ls())
data("down")

fit.glm <- glm(cases/births~age, weight= births, family=binomial, data=down)
fit.seg <- segmented(fit.glm, seg.Z=~age, psi=25)

test <- davies.test(fit.glm,~age,k=5)

plot(cases/births~age, data=down)
## The Davies test is appropriate for testing for a breakpoint, but it does not appear useful for selecting 
# the number of the joinpoints. (因此仅适用检验序列是否存在突变)
set.seed(1000)
y <- c(rnorm(40, 1, 1), rnorm(80, 3, 2))
x <- seq_along(y)

# x <- c(1:10, 13:22)
# y <- numeric(20)
# ## Create first segment
# y[1:10] <- 20:11 + rnorm(10, 0, 1.5)
# ## Create second segment
# y[11:20] <- seq(11, 15, len=10) + rnorm(10, 0, 1.5)
# ## Plot it
# par(mar=c(4,4,1,1)+0.2)
plot(x,y, pch=16, type = "b")

dati <- data.frame(x, y)
# x <- c(1:10)
# y <- y[1:10]
# summary(lm(y~x))
# ## segmented picecwise检测的突变更加注重线性转折
# dati <- data.frame(x, y)
lm_fit <- lm(y ~x)
#test beta2 significant
davies.test(lm_fit, ~x, k = 5)
segmented(lm_fit, ~x, psi = 10)
