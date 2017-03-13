micro

str(micro)

micro.amphib
str(micro.amphib)

unique(micro.amphib$analyte)

p <- ggplot(micro.amphib, aes(factor(time), conc))
p <- p + geom_boxplot(aes(color = factor(analyte)))
p <- p + theme_bw()
p <- p + xlab("Dehydration Time (h)")
p <- p + ylab("Concentration (ug/g)")
p <- p + guides(colour = guide_legend("Pesticide"))
p
