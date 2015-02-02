
estimate.glm.exp.lambda<-function(x,y){
  obj<-try(glm(y~x, family = Gamma(link = "log")), silent=TRUE)
  if (is(obj, "try-error")) return(NA) else return(summary(obj,dispersion=1)$coef[2])
}
estimate.glm.exp.lambda.intercept<-function(x,y){
  obj<-try(glm(y~x, family = Gamma(link = "log")), silent=TRUE)
  if (is(obj, "try-error")) return(NA) else return(summary(obj,dispersion=1)$coef[1])
}

#atrazine example
#lambda<- estimate.glm.exp.lambda(as.numeric(select_mean_week1[,2]),as.numeric(select_mean_week1[,1]))
#lambda.intercept<- estimate.glm.exp.lambda.intercept(as.numeric(select_mean_week1[,2]),as.numeric(select_mean_week1[,1]))
#lambda1<-format(as.numeric(log(2)/-lambda),digits=3)
#print(lambda1)

pdf(paste(micro.graphics,"data_firstorderfit_scatterplot",".pdf", sep=""))
  par(mfrow=c(3,1))
  print(parents)
  for(parent in parents){
    i=0
    print(parent)
    temp.parent <- micro.group.stats.amphib[which(micro.group.stats.amphib$parent==parent),]
    print(temp.parent)
    parent.analytes <- unique(temp.parent$analyte)
    title_text = ""
    for(analyte in parent.analytes){
      print(parent)
      print(analyte) 
      analytetemp <- micro.group.stats.amphib[which(micro.group.stats.amphib$analyte==analyte),]
      xvalues <- as.numeric(as.character(analytetemp$time))
      points.y <- micro.amphib[which(micro.amphib$analyte==analyte),]$conc
      points.x <- as.numeric(as.character(micro.amphib[which(micro.amphib$analyte==analyte),]$time))
      #first order fit
      lambda<- estimate.glm.exp.lambda(points.x, points.y)
      lambda.intercept<- estimate.glm.exp.lambda.intercept(points.x, points.y)
      lambda1<-format(as.numeric(log(2)/-lambda),digits=3)
      temp_text = paste(analyte, "half-life =", lambda1, "hours")
      print(temp_text)
      title_text = paste(title_text, temp_text, ";")
      #create empty plot if needed
      if(i==0){
        parenttemp <- micro.group.stats.amphib[which(micro.group.stats.amphib$parent==parent),]
        maxconc <- max(points.y) 
        plot(xvalues,analytetemp$ConcMean,type="l", xlim=c(0,48),
             ylim = c(0,maxconc), main= "",col="black", xlab="Hours", ylab="Concentration")
        axis(1,at=xvalues)
        i=1
      }
      #add line for parent metabolite
      if(analyte %in% parents){
        lines(xvalues,analytetemp$ConcMean,type="l",col="red")
        points(points.x, points.y, col = "red")
        curve(exp(lambda.intercept)*exp(x*lambda),add=TRUE,col="orange")
      }else{
        #add line for daughter metabolite
        lines(xvalues,analytetemp$ConcMean,type="l",col="blue")
        points(points.x, points.y, col = "blue")
        curve(exp(lambda.intercept)*exp(x*lambda),add=TRUE,col="green")
      }
    }
    title(title_text)
  }
dev.off()

