############ DAY 5 #####################


# Non- parametric - 2 dependent samples -----------------------------------

# wilcoxon signed rank test

# import streams

head(streams)

# low sample size, not normally distributed
# water quality of 6 rivers before and after 
# treatment (concentration of any chemical)
attach(streams)
plot(up,down)

abline(a=0,b=1)
# along the line is no effect
# below line -> effect of treatment, above quality got worse

wilcox.test(up, down, mu=0, paired=T, conf.int=T, exact=F)
# highly sign -> reject 0 hyp (no difference)
# difference between two medians is 2.5
detach(streams)

# # parametric version ----------------------------------------------------

# paired t-test
head(fishing)
# fish density before and after removing non native predator

attach(fishing)
boxplot(before, after, ylab="Fish density",
        names=c("before", "after"))

plot(before, after) ; abline(a=0,b=1)
# along the line, locations without effect
# -> 3 cases with lower density after removing,
# the rest has increased density

t.test(before, after,mu=0, paired=T)
detach(fishing)
head(lung)
search()
attach(lung)
plot(Age, LungCap, main="Relation between age and lung capacity",
     col="green", las=1, xlab="Age", ylab="lung cap")

mod=lm(LungCap~Age)
abline(mod, lwd=2, col="grey")
summary(mod)
# formular
# ditribution of residuals
# intercept estimate (y axis at 0) -> not zero, if we care
attributes(mod)
mod$coefficients # -> intercep and slope
mod$fitted.values
mod$fitted.values[1:50]

points(Age, mod$fitted.values, pch=20, col=2)

plot(Age, mod$residuals, pch=20, col="blue")
abline(h=0, lwd=3)

x1=c(1,2,3,4,5)
y1=c(1,2,3,100,200)
plot(x1,y1, xlim=c(0,5), ylim=c(-100,200))
abline(h=0)
mod2=lm(y1~x1); abline(mod2, col=2, lwd=3)
summary(mod2)
points(x1, mod2$fitted.values, pch=20, col="blue", cex=2)
plot(x1, mod2$residuals, pch=20, col="blue")
abline(h=0, lwd=2)

# assumption of the model
# !!!!!!!!!!!!!independence of y values! -> random samples
# !!!!linearity! 
# !!!!!!!!!!!!!!!!!!!!!!!!!!! HOMOSCEDASTICITY -> constand spread
# !!!! normality

par(mfrow=c(2,2))
plot(mod)

shapiro.test(mod$residuals)
par(mfrow=c(1,1))
hist(mod$residuals, freq=F, breaks=20)
lines(density(mod$residuals)) # plots as appendi to show that assumptions are made

# combine with age/lung plot
points(Age[114], LungCap[114], pch=20)
points(Age[293], LungCap[293], pch=20)
points(Age[356], LungCap[356], pch=20)
detach(lung)

#load problems
problems
par(mfrow=c(1,1))
attach(problems)
plot(x,y)
mod1=lm(y~x)
abline(mod1)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)
detach(problems)
head(Decay)
summary(Decay)
attach(Decay)
par(mfrow=c(1,1))
plot(time, amount)
mod3=lm(amount~time)
abline(mod3)
par(mfrow=c(2,2))
plot(mod3)
par(mfrow=c(1,1))

shapiro.test(mod3$residuals)
# -> rejecting 0 hyp
hist(mod3$residuals, freq=F, breaks=20)
lines(density(mod3$residuals))


points(time[1], amount[1], pch=20, cex=1.5)
points(time[5], amount[5], pch=20, cex=1.5)
points(time[30], amount[30], pch=20, cex=1.5)

# lets include a quadratic term
mod4=lm(amount~time+I(time^2))
summary(mod4)
abline(mod4) # not working!
lines(mod4$fitted.values,type="line", lwd=2, lty=3)

par(mfrow=c(2,2))
plot(mod4)
par(mfrow=c(1,2))
plot(time, mod3$residuals, ylim=c(-40,40))
abline(h=0)
plot(time, mod4$residuals, ylim=c(-40,40))
abline(h=0)
par(mfrow=c(1,1))

detach(Decay)
#### EXCERECISE

myData <- women
head(women)

attach(myData)

# 1 - causation
plot(myData)


#2 


mod1=lm(weight~height)

par(mfrow=c(2,2))
plot(mod1)