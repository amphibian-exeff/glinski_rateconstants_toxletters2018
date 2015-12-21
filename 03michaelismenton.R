
x <- seq(0,10,by=0.1)

curve(2*x/(1+x))

micmen <- function(x,a=2,b=1){
  a*x/(b+x)
}

curve(micmen(x), from=0, to=8, ylim=c(0,4))




#http://stat.ethz.ch/R-manual/R-patched/library/stats/html/nls.html
#https://rpubs.com/RomanL/6752
#http://stats.stackexchange.com/questions/101154/how-to-fit-a-michaelis-menten-function-with-a-random-effect-using-the-nlme-packa

#fake data
mm <- structure(list(S = c(3.6, 1.8, 0.9, 0.45, 0.225, 0.1125, 3.6, 
                           1.8, 0.9, 0.45, 0.225, 0.1125, 3.6, 1.8, 0.9, 0.45, 0.225, 0.1125, 
                           0), v = c(0.004407692, 0.004192308, 0.003553846, 0.002576923, 
                                     0.001661538, 0.001064286, 0.004835714, 0.004671429, 0.0039, 0.002857143, 
                                     0.00175, 0.001057143, 0.004907143, 0.004521429, 0.00375, 0.002764286, 
                                     0.001857143, 0.001121429, 0)), 
                                      .Names = c("S", "v"), class = "data.frame", row.names = c(NA, -19L))

fit a Michaelis-Menten model and draw the result. First part will use a 
package drc and its function drm (dose response model). Second part will 
demonstrate how to fit this model "out of the box" with function nls. The 
code used can be also found in a book by Ritz and Streinbig Nonlinear regression 
with R (see pages 10 and 11).

The model we're trying to fit is a 2 parameter model 
(see page 10 of the above mentioned book) where
f(x,(K,Vm)) = (Vm * x)/ (K + x)

We will use package drm to fit the model and package ggplot2 to draw the result. 
We fit the data using function drm. We are saying that v depends on S, and the 
model that should be fitted is a two parameter Michaelis-Menten model (coded 
in function MM.2). After the model has been fitted, we predict the values in order 
to get a smooth fitted line.
model.drm <- drm (v ~ S, data = mm, fct = MM.2())

mml <- data.frame(S = seq(0, max(mm$S), length.out = 100))
mml$v <- predict(model.drm, newdata = mml)

#plot
ggplot(mm, aes(x = S, y = v)) +
  theme_bw() +
  xlab("Concentration [mM]") +
  ylab("Speed [dE/min]") +
  ggtitle("Michaelis-Menten kinetics") +
  geom_point(alpha = 0.5) +
  geom_line(data = mml, aes(x = S, y = v), colour = "red")

#save as pdf
ggsave("mm.pdf", width = 6, height = 4)


model.nls <- nls(v ~ Vm * S/(K+S), data = mm, 
                 start = list(K = max(mm$v)/2, Vm = max(mm$v)))

summary(model.drm)
summary(model.nls)
