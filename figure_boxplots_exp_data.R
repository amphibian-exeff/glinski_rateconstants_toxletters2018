library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
library(grid)

micro <- read.table(paste(micro.csv.in,"exposure_experiment.csv",sep=""), header = TRUE, sep = ",")
micro
str(micro)

micro.amphib <- micro[which(micro$matrix=="amphib"),]
micro.amphib
str(micro.amphib)

unique(micro.amphib$parent)
unique(micro.amphib$analyte)

micro.amphib.atrazine <- micro.amphib[which(micro.amphib$parent=='atrazine'),]
micro.amphib.atrazine$analyte <- factor(micro.amphib.atrazine$analyte,
                                        levels = c('ATZ','DIA','DEA'),ordered = TRUE)
levels(micro.amphib.atrazine$analyte)
micro.amphib.fipronil <- micro.amphib[which(micro.amphib$parent=='fipronil'),]
micro.amphib.fipronil$analyte <- factor(micro.amphib.fipronil$analyte,
                                           levels = c('FIP','F. sulfone'),ordered = TRUE)
levels(micro.amphib.fipronil$analyte)
micro.amphib.triadimefon <- micro.amphib[which(micro.amphib$parent=='triadimefon'),]
micro.amphib.triadimefon$analyte <- factor(micro.amphib.triadimefon$analyte,
                                           levels = c('TDN','TDL A','TDL B'),ordered = TRUE)
levels(micro.amphib.triadimefon$analyte)

p1 <- ggplot(micro.amphib.atrazine, aes(factor(time), conc))
p1 <- p1 + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p1 <- p1 + geom_boxplot(aes(fill=factor(analyte)))
# atrazine red, dia blue, dea green
p1 <- p1 + scale_fill_manual(values=c("red", "blue","chartreuse4"),guide = guide_legend(title = NULL))
p1 <- p1 + theme_bw()
p1 <- p1 + xlab("")
p1 <- p1 + ylab("")
p1 <- p1 + guides(color = guide_legend("Pesticide"))
p1 <- p1 + theme(legend.position=c(.88,.75))
p1 <- p1 + theme(plot.margin = unit(c(0.1,0.1,0.1,0.4), "cm"))
p1 <- p1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p1

p2 <- ggplot(micro.amphib.fipronil, aes(factor(time), conc))
p2 <- p2 + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p2 <- p2 + geom_boxplot(aes(fill = factor(analyte)))
#fip red sulfone blue
p2 <- p2 + scale_fill_manual(values=c("red", "blue"),guide = guide_legend(title = NULL))
p2 <- p2 + theme_bw()
p2 <- p2 + xlab("Exposure Time (h)")
p2 <- p2 + ylab("")
p2 <- p2 + guides(colour = guide_legend("Pesticide"))
p2 <- p2 + theme(axis.title.x=element_text(size = rel(0.9),vjust=2))
p2 <- p2 + theme(legend.position=c(.88,.75))
p2 <- p2 + theme(plot.margin = unit(c(0.1,0.1,0.1,0.4), "cm"))
p2 <- p2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p2


p3 <- ggplot(micro.amphib.triadimefon, aes(factor(time), conc))
p3 <- p3 + theme(axis.title.y=element_text(margin=margin(0,100,0,0)))
p3 <- p3 + geom_boxplot(aes(fill = factor(analyte)))
#tdn red, tdl a blue, tdl b green
p3 <- p3 + scale_fill_manual(values=c("red", "blue", "chartreuse4"),guide = guide_legend(title = NULL))
p3 <- p3 + theme_bw()
p3 <- p3 + xlab("")
p3 <- p3 + ylab(expression(paste("Concentration (",mu, "g ", g^-1,")","\n")))
p3 <- p3 + theme(axis.title.y=element_text(size = rel(0.9),vjust=3))
p3 <- p3 + theme(legend.position=c(.88,.75))
p3 <- p3 + theme(plot.margin = unit(c(0.1,0.1,0.1,0.4), "cm"))
p3 <- p3 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p3

#figure 4
jpeg(paste(micro.graphics,"glinski_fig4.jpg", sep=""),width = 5, height = 8, units = "in",res=300)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p3), ggplotGrob(p2), size = "last"))
dev.off()

#,left=textGrob("Y Axis", rot = 90, vjust = 1)

###

