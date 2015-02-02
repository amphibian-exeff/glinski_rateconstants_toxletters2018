
R.Version()$version.string
library(dplyr)
library(knitr)
#library(grofit)
library(zoo)

if(Sys.info()[4]=="DC2626UTPURUCKE"){
  microdir <- "C:\\stp_drop\\Dropbox\\amphib_microsomes_glinskietal2015\\RCode\\"
}

if(Sys.info()[4]=="stp-air-3.local"){
  micro.root <- path.expand("~/git/glinski_metabolites/")
  micro.csv.in <- path.expand("~/git/glinski_metabolites/csv_in/")
  micro.csv.out <- path.expand("~/git/glinski_metabolites/csv_out/")
  micro.graphics <- path.expand("~/git/glinski_metabolites/graphics/")
}

file.exists(micro.root)
file.exists(micro.csv.in)

micro <- read.table(paste(micro.csv.in,"Microsomes2.csv",sep=""), header = TRUE, sep = ",")
str(micro)
micro$time <- factor(micro$time)
micro$tank <- factor(micro$tank)
parents <- unique(micro$parent)
analytes <- unique(micro$analyte)

micro
parents
analytes

micro.amphib <- micro[which(micro$matrix=="amphib"),]
micro.soil <- micro[which(micro$matrix=="soil"),]
