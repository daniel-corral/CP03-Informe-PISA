library(tidyverse)
library(broom) # modelos en df
library(flextable) # Tablas formateadas
library(mgcv) # estimar gam
library(reshape2) # melt
library(ggplot2)
library(tidyverse)
library(broom) # modelos en df
library(flextable) # Tablas formateadas
library(mgcv) # estimar gam
library(reshape2) # melt
mData=read.csv("pisasci2006.csv")
mData = na.omit(mData)
head(mData)
mData1 <- mData %>%
  select(Country, Overall, Interest, Support, Income, Health, Edu, HDI )

ggplot(data = mData1, mapping = aes(x = Overall, y = HDI)) +
  layer(geom = "point",stat = "identity",position = "identity") +
  theme_bw() + theme(legend.key = element_blank())

# Fit a linear model
lm_mod <- lm(Overall~HDI, data = mData1)
width(flextable(tidy(lm_mod)), width = 1.5)

width(flextable(glance(lm_mod)), width = 1.5)

# Visualize the model
termplot(lm_mod, partial.resid = TRUE, se = TRUE)
baseplot1 <- ggplot(data = mData, mapping = aes(x = Overall, y = HDI)) +
  layer(geom = "point",stat = "identity",position = "identity") +
  theme_bw() + theme(legend.key = element_blank())
baseplot1

#Linear splines
knots <- c(14,32,45,65)
mData$x1 <- pmax(0, mData$Interest - knots[1])
mData$x2 <- pmax(0, mData$Support - knots[2])
mData$x3 <- pmax(0, mData$Evidence - knots[3])
mData$x4 <- pmax(0, mData$Explain - knots[4])
mData

lsp <- lm(Overall ~ HDI + x1 + x2 + x3 + x4, data = mData)
summary(lsp)

newdat <- data.frame(HDI = seq(0,1,0.02))
newdat$x1 <- pmax(0, newdat$HDI - knots[1])
newdat$x2 <- pmax(0, newdat$HDI - knots[2])
newdat$x3 <- pmax(0, newdat$HDI - knots[3])
newdat$x4 <- pmax(0, newdat$HDI - knots[4])
newdat$linear <- predict(lsp, newdata = newdat)

#Quadratic splines
qsp <- lm(Overall ~ HDI + I(HDI^2) + I(x1^2) + I(x2^2) + I(x3^2) + I(x4^2), data = mData)
summary(qsp)
newdat$quadratic <- predict(qsp, newdata = newdat)

#Cubic splines
csp <- lm(Overall ~ HDI + I(HDI^2) + I(HDI^3) + I(x1^3) + I(x2^3) + I(x3^3) + I(x4^3), data = mData)
summary(csp)

newdat$cubic <- predict(csp, newdata = newdat)

#Plot splines
newdatMelt <- melt(data          = newdat,
                   id.vars       = c("HDI",paste0("x",1:4)),
                   variable.name = "spline",
                   value.name    = "value")

baseplot1 +
  layer(geom = "line", data = newdatMelt,stat = "identity", position = "identity",
        mapping = aes(x = HDI, y = value, color = spline)) +
  facet_wrap( ~ spline, ncol = 1)

# Fit the model
gam_mod <- gam(Overall ~ s(HDI), data = mData)

# Plot the results
plot(gam_mod, residuals = TRUE, pch = 1)

coef(gam_mod)

# Complexity
# Fit a GAM with 3 basis functions
gam_mod_k3 <- gam(Overall ~ s(HDI, k = 3), data = mData)

# Fit with 20 basis functions
gam_mod_k20 <- gam(Overall ~ s(HDI, k = 20), data = mData)

# Visualize the GAMs

plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)

# Fix the smoothing parameter at 0.1
gam_mod_s1 <- gam(Overall ~ s(HDI), data = mData, sp = 0.01)

# Fix the smoothing parameter at 0.0001
gam_mod_s2 <- gam(Overall ~ s(HDI), data = mData, sp = 0.0001)

# Plot both models
plot(gam_mod_s1, residuals = TRUE, pch = 1)
plot(gam_mod_s2, residuals = TRUE, pch = 1)

gam_mod_sk <- gam(Overall ~ s(HDI, k = 5), data = mData, sp = 0.0001)

# Visualize the model
plot(gam_mod_sk, residuals = TRUE, pch = 1)

# Checking
gam.check(gam_mod_sk)
gam.check(gam_mod_s1)
gam.check(gam_mod_s2)
gam.check(gam_mod_k3)
gam.check(gam_mod_k20)

install.packages("imputeTS")
library(imputeTS)
