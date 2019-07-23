#========================================================================#
#   R Users Group: Introduction to linear and non-linear models in R     #
#                                                                        #
# Yuri Niella - PhD candidate (yuri.niella@hdr.mq.edu.au)                #
# Macquarie University - Marine Predator Research Group                  #
#========================================================================#

# Installing packages for workshop:
install.packages("mgcv")
install.packages("visreg")
install.packages("rgl")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("car")
install.packages("agridat")
install.packages("cmocean")

# Loading packages:
library(mgcv)
library(visreg)
library(rgl)
library(ggplot2)
library(ggpubr)
library(car)
library(agridat)
library(cmocean)


                #=========================================#
                ### 1. Single linear Regression Models ####
                #=========================================#

#================================================#
# Relationships between two continuous variables #
#================================================#

# Example dataset 1: car speed and distance 
df <- cars  
summary(df) 

# Visualize general trends between variables:
plot(x=df$speed, y=df$dist, main="Dist ~ Speed", # scatterplot
     xlab="Speed", ylab="Distance")

# Inspect and remove outliers (not really necessary!)
par(mfrow=c(1,2))
boxplot(df$speed, main="Speed"); boxplot(df$dist, main="Distance") # Outliers?
  # Remove outliers:
  df <- subset(df, dist < 120) 
# Check again!
boxplot(df$speed, main="Speed"); boxplot(df$dist, main="Distance") 
dev.off() # Clear plots!

# Check normal distribution of response variable (dependent): Distance 
plot(density(df$dist), main="Distance", lwd=2) +
  polygon(density(df$dist), col="blue") 

# Build linear model
mod <- lm(dist ~ speed, data=df)
summary(mod) # Model diagnostics

  # Inspect residual distribution:
  par(mfrow=c(2,2))
  plot(mod)
  dev.off()

# Plotting linear model: base plot
plot(df$dist ~ df$speed, pch=16, col="gray", 
     xlab="Speed", ylab="Distance")
abline(mod, lwd=2, col="red", lty="dashed") # Model

  # Formula: y = a + bx (or Distance = Intercept + b x Speed)
  summary(mod) 
  
  # Add values to graph:
  text("", x=8, y=80) # Formula
  text("", x=8, y=60) # R²
  

#============================================================#
# Relationships between continuous and categorical variables #
#============================================================#

# Example dataset 2: Professor salary and sex
df <- Salaries[,c(5,6)]
summary(df)

# Check normal distribution of salary variable:
plot(density(df$salary), main="Salary", lwd=2) +
  polygon(density(df$salary), col="red")
  
  # or:
  hist(df$salary)

# Model:
mod <- lm(salary ~ sex, data=df)
summary(mod) # Diagnostics
visreg(mod)  # Plot using visreg package

## Consider "male" as the reference for modelling:
df$sex <- factor(df$sex, levels=c("Male","Female"))
mod <- lm(salary ~ sex, data=df)
summary(mod) # Diagnostics
visreg(mod)  # Plot model


      #=====================================================================#
      ### 2. Multiple regression Models: Generalized linear models (GLM) ####
      #=====================================================================#

# Investigate from a set of "predictors" which influences your response variable
df <- Salaries
summary(df) # 5 "candidate" predictors (independent variables)

### Important: family or error distribution (!)
?glm
# Main families: 
# 1. Gaussian (continuous variable)
# 2. Poisson  (count data)
# 3. Binomial (presence/absence - 0 & 1)

# Investigate for colinearity among continuous candidate predictors: Pearson correlation
cor.test(df$yrs.since.phd,df$yrs.service, method="pearson")
plot(df$yrs.since.phd ~ df$yrs.service)


### MODELLING APPROACH: 

# Check normal distribution:
hist(df$salary) # Gaussian family
hist(log(df$salary)) # Log scale

  # Which family of errors distribution to use?

### MODELLING STARTS! ####
# Identify the first best variable:
mod0 <- glm(salary ~ 1, family="gaussian", data=df) # NULL MODEL!
mod1 <- glm(salary ~ rank, family="gaussian", data=df)
mod2 <- glm(salary ~ discipline, family="gaussian", data=df)
mod3 <- glm(salary ~ yrs.since.phd, family="gaussian", data=df)
mod4 <- glm(salary ~ yrs.service, family="gaussian", data=df)
mod5 <- glm(salary ~ sex, family="gaussian", data=df)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)

aic <- AIC(mod1,mod2,mod3,mod4,mod5)
aic # AIC table
which(aic$AIC == min(aic$AIC)) # Find variable with lowest AIC
anova(mod1,mod0, test="Chisq") # Test significance of new variable
mod1 <- glm(salary ~ rank, family="gaussian", data=df) # First variable!

## Test variable 2:
mod2 <- glm(salary ~ rank + discipline, family="gaussian", data=df)
mod3 <- glm(salary ~ rank + yrs.since.phd, family="gaussian", data=df)
mod4 <- glm(salary ~ rank + yrs.service, family="gaussian", data=df)
mod5 <- glm(salary ~ rank + sex, family="gaussian", data=df)

summary(mod2)
summary(mod3) # Not significant!
summary(mod4) # Not significant!
summary(mod5) # Not significant!

aic <- AIC(mod1,mod2)
aic # AIC table
which(aic$AIC == min(aic$AIC)) # Find variable with lowest AIC
anova(mod1,mod2, test="Chisq") # Test significance of new variable 
mod2 <- glm(salary ~ rank + discipline, family="gaussian", data=df) # New variable

### Model diagnostics
par(mfrow=c(2,2))
plot(mod2)
dev.off()

# PLOT MODEL:
summary(mod2)

par(mfrow=c(1,2))
visreg(mod2, xvar="rank", ylab="", main="Rank")
visreg(mod2, xvar="discipline", ylab="", main="Discipline")
dev.off()

        

        #==============================================================#
        ### 3. Generalized additive models (GAM): non-linear models ####
        #==============================================================#

# When correlation between continuous variables is not simply linear 
# In other words: GAM is a GLM with a smoothing function = s()

# Simulated dataset:
x <- seq(0, pi * 2, 0.1)
sin_x <- sin(x)
y <- sin_x + rnorm(n = length(x), mean = 0, sd = sd(sin_x / 2))
df <- data.frame(y,x)

# Visually investigate trends:
ggplot(df, aes(x, y)) + geom_point() # Clearly a non-linear trend!

# Fit a linear model
linear.mod <- lm(y ~ x, data = df)
summary(linear.mod) # significant! (R²?)
visreg(linear.mod, gg=T) 

# Fit a non-linear model
nonlinear.mod <- gam(y ~ s(x), data=df) # s() smoothing function!
summary(nonlinear.mod) # R²?
visreg(nonlinear.mod, gg=T) + theme_bw()

# Compare linear and non-linear model types
aic <- AIC(linear.mod,nonlinear.mod)
aic # What's best model type?


### Visualize and customize plots with visreg function ####
visreg(nonlinear.mod)

visreg(nonlinear.mod, partial=F, rug=T) # Partial values as tick marks on X axis!

visreg(nonlinear.mod, xlab="X values", ylab="Y values", 
       points=list(pch=16, cex=0.5, col="blue")) # Change points

visreg(nonlinear.mod, xlab="X values", ylab="Y values", 
       points=list(pch=16, cex=0.5, col="blue"),
       line=c(lwd=2, col="darkblue")) # Change line color

visreg(nonlinear.mod, xlab="X values", ylab="Y values", 
       points=list(pch=16, cex=0.5, col="blue"),
       line=c(lwd=2, col="darkblue"), 
       fill.par=c(col="lightskyblue1")) # Change confidence interval color

visreg(nonlinear.mod, xlab="X values", ylab="Y values", 
       points=list(pch=21, cex=1, bg="red"), # Change point type
       line=c(lwd=2, col="darkred"), fill.par=c(col="rosybrown1"), 
       axes=F)          # Omit axis 
axis(1); axis(2, las=0) # Add custom axis


  # visreg + ggplot2!
  visreg(nonlinear.mod, gg=T) 

  visreg(nonlinear.mod, gg=T) + theme_bw() # Change theme option
  
  visreg(nonlinear.mod, gg=T) + theme_bw() +
         scale_x_continuous(breaks=seq(0,6,1)) # Customize X axis
  
  visreg(nonlinear.mod, gg=T,
         line=c(col="black")) +                # Change line colour
    geom_point(pch=17, col="yellow", size=2) + # Customize points
    theme_dark() +                             # Change theme  
    scale_x_continuous(breaks=seq(0,6,1))      # Customize X axis

  
  #==================================================================#
  ### 4. Generalized additive mixed models (GAMM): random effects ####
  #==================================================================#

dat <- lasrosas.corn # Example data from agridat package
summary(dat)

plot(yield ~ bv, data=dat)
mod.lm <- lm(yield ~ bv, data=dat) # Linear
mod.gam <- gam(yield ~ s(bv), data=dat) # Non-linear
AIC(mod.lm, mod.gam) # Investigate best model type


### Compare between fixed and mixed effect model types:

# Fixed
gam.fixed <- gam(yield ~ s(bv), data=dat) 
summary(gam.fixed) # R²?

# Random
gam.random <- gamm(yield ~ s(bv), data=dat, 
            random=list(year=~1)) # Random effect!
summary(gam.random) # gam object!
summary(gam.random$gam) # R²?

  AIC(gam.fixed,gam.random$lme) # Compare by AIC values: best model type?
  

### MODELLING START ####

# Variable 1:
mod0 <- gamm(yield ~ 1, data=dat, random=list(year=~1)) # Null model
mod1 <- gamm(yield ~ s(lat), data=dat, random=list(year=~1)) 
mod2 <- gamm(yield ~ s(long), data=dat, random=list(year=~1)) 
mod3 <- gamm(yield ~ s(nitro), data=dat, random=list(year=~1)) 
mod4 <- gamm(yield ~ topo, data=dat, random=list(year=~1)) 
mod5 <- gamm(yield ~ s(bv), data=dat, random=list(year=~1)) 
mod6 <- gamm(yield ~ rep, data=dat, random=list(year=~1)) 
mod7 <- gamm(yield ~ nf, data=dat, random=list(year=~1)) 

summary(mod1$gam)
summary(mod2$gam)
summary(mod3$gam) 
summary(mod4$gam)
summary(mod5$gam)
summary(mod6$gam) # Not significant!
summary(mod7$gam)

aic <- AIC(mod1$lme,mod2$lme,mod3$lme,mod4$lme,mod5$lme,mod7$lme) # lme object!
which(aic$AIC == min(aic$AIC))
anova(mod0$lme,mod2$lme) # Significant!
mod1 <- gamm(yield ~ s(long), data=dat, random=list(year=~1)) # Variable 1!

# Inspect for colinearity among predictors:
cor.test(dat$long,dat$lat, method="pearson")   # Strong correlation
cor.test(dat$long,dat$nitro, method="pearson") # No correlation
cor.test(dat$long,dat$bv, method="pearson")    # Weak correlation


# Variable 2:
mod2 <- gamm(yield ~ s(long) + s(nitro), data=dat, random=list(year=~1)) 
mod3 <- gamm(yield ~ s(long) + topo, data=dat, random=list(year=~1)) 
mod4 <- gamm(yield ~ s(long) + s(bv), data=dat, random=list(year=~1)) 
mod5 <- gamm(yield ~ s(long) + rep, data=dat, random=list(year=~1)) 
mod6 <- gamm(yield ~ s(long) + nf, data=dat, random=list(year=~1)) 

summary(mod2$gam)
summary(mod3$gam) 
summary(mod4$gam)
summary(mod5$gam) # Not significant!
summary(mod6$gam)

aic <- AIC(mod1$lme,mod2$lme,mod3$lme,mod4$lme,mod6$lme) 
which(aic$AIC == min(aic$AIC))
anova(mod1$lme,mod2$lme) # Significant!
mod2 <- gamm(yield ~ s(long) + s(nitro), data=dat, random=list(year=~1)) # Variable 2!

# Inspect for colinearity among predictors:
cor.test(dat$nitro,dat$bv, method="pearson")   # No correlation


# Variable 3:
mod3 <- gamm(yield ~ s(long) + s(nitro) + topo, data=dat, random=list(year=~1)) 
mod4 <- gamm(yield ~ s(long) + s(nitro) + s(bv), data=dat, random=list(year=~1)) 
mod5 <- gamm(yield ~ s(long) + s(nitro) + nf, data=dat, random=list(year=~1)) 

summary(mod3$gam) 
summary(mod4$gam)
summary(mod5$gam) # Not significant!

aic <- AIC(mod1$lme,mod2$lme,mod3$lme,mod4$lme) 
which(aic$AIC == min(aic$AIC))
anova(mod2$lme,mod3$lme) # Significant!
mod3 <- gamm(yield ~ s(long) + s(nitro) + topo, data=dat, random=list(year=~1)) 


# Variable 4:
mod4 <- gamm(yield ~ s(long) + s(nitro) + topo + s(bv), data=dat, random=list(year=~1)) 

summary(mod4$gam)

aic <- AIC(mod1$lme,mod2$lme,mod3$lme,mod4$lme) 
which(aic$AIC == min(aic$AIC))
anova(mod3$lme,mod4$lme) # Significant!
mod4 <- gamm(yield ~ s(long) + s(nitro) + topo + s(bv), data=dat, random=list(year=~1)) 

  # Residual distribution
  par(mfrow=c(2,2))
  gam.check(mod4$gam)
  dev.off()
  

# Plot mixed effect model:
visreg(mod4$gam) # ?


### Plotting customized mixed effect model (visreg + ggplot2) ####

mod4$gam$data <- dat # Add data to GAM object!

# Default plot:
par(mfrow=c(2,2))
visreg(mod4$gam)
dev.off()


# Customized plot using ggplot:

# Longitude:
plot1 <- visreg(mod4$gam, xvar="long", gg=T,                  # Pick variable (xvar) and use ggplot (gg = T)
                line=c(col="black"), xlab="Longitude",        # Black line + customized X axis label
                type="contrast",                              # Response variable on a transformed scale 
                partial=F, rug=T) +                           # Raw data as tick marks
  ylim(-10,40) + theme_bw() +                                 # Customized Y limit to better zoom in effects
  geom_hline(yintercept = 0, linetype="dashed")               # Add null effect (horizontal line y = 0)
plot1

# Nitrogen:
plot2 <- visreg(mod4$gam, xvar="nitro", gg=T,                  
                line=c(col="black"), xlab="Nitrogen",      
                partial=F, rug=T, type="contrast") +          
  ylim(-6,4) + theme_bw() +                   
  geom_hline(yintercept = 0, linetype="dashed") 
plot2

# Topology (?)
plot3 <- visreg(mod4$gam, xvar="topo", gg=T,                  
                line=c(col="black"), xlab="Topology",      
                partial=T, type="contrast") +          # Show partial values on graph (useful for factors)
  theme_bw() +
  geom_hline(yintercept = 0, linetype="dashed") 
plot3

# BV (??)
plot4 <- visreg(mod4$gam, xvar="bv", gg=T,                  
                line=c(col="black"), xlab="BV",      
                partial=F, rug=T, type="contrast") +          
  ylim(-35,25) + theme_bw() +                   
  geom_hline(yintercept = 0, linetype="dashed") 
plot4


# Plot all effects simultaneously:
ggarrange(plot1,plot2,plot3,plot4,
          nrow=2, ncol=2,
          labels=c("A","B","C","D"))



      #=======================================#
      ### 5. GAMM with interacting effects ####
      #=======================================#

summary(dat) # Latitude and Longitude (geographical distribution)

mod.lat <- gamm(yield ~ s(lat), data=dat, random=list(year=~1))
mod.lon <- gamm(yield ~ s(long), data=dat, random=list(year=~1)) 
mod.lat.lon <- gamm(yield ~ s(lat,long), data=dat, random=list(year=~1)) 

summary(mod.lat$gam)
summary(mod.lon$gam)
summary(mod.lat.lon$gam)

AIC(mod.lat$lme,mod.lon$lme,mod.lat.lon$lme) # Compare models

mod.lat.lon$gam$data <- dat # Add dataset to GAM object


### Visualizing with visreg: 2D and 3D graphs

# Default
visreg2d(mod.lat.lon$gam, xvar="long", yvar="lat",
         xlab="Longitude", ylab="Latitude")

# ggplot
visreg2d(mod.lat.lon$gam, xvar="long", yvar="lat", plot.type="gg") # ggplot

# ggplot + customized color scale:
visreg2d(mod.lat.lon$gam, xvar="long", yvar="lat", plot.type="gg", zlab="Yield",
         color=cmocean('algae')(100)) # Algae palette from cmocean package
         
  # Add isolines:
  visreg2d(mod.lat.lon$gam, xvar="long", yvar="lat", plot.type="gg", zlab="Yield",
           color=cmocean('algae')(100)) +
           geom_contour(aes(z=z), color="black", breaks=seq(40,100,10)) 
  
# 3D plot
visreg2d(mod.lat.lon$gam, xvar="long", yvar="lat", plot.type="persp", # 3D graph: plot.type="persp" 
         phi=20, theta=30, # Rotate the graph!
         xlab="Longitude", ylab="Latitude", zlab="Yield") 

# 3D interactive
visreg2d(mod.lat.lon$gam, xvar="long", yvar="lat", plot.type="rgl",
         xlab="Longitude", ylab="Latitude", zlab="Yield")
