
x <- seq(0,10,by=0.1)

curve(2*x/(1+x))

micmen <- function(x,a=2,b=1){
  a*x/(b+x)
}

curve(micmen(x), from=0, to=8, ylim=c(0,4))
