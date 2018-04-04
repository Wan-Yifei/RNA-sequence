# draw a scatter plot for log2 fold change
# V1.0.3
library("ggplot2")

## load raw data
rawdata <- read.csv2('LncRNA_all.txt', sep = '\t', row.names = 1)

## calculate the fold change
data1 <- log2(rawdata + 1)
log2fc <- data1$Mod.1_count - data1$Treatment.1_count

## evalute the  Up regulated or down regulated based on threshold
judge <- sapply(log2fc, function(x , threshold = 1){ ## default threshold =1
  if(x > threshold){x = 'up'
  } else if (x < -threshold){x = 'down'
  } else {x = 'none'}
})

## draw the scatter plot
ggplot(data = data1, aes(x=Treatment.1_count, y=Mod.1_count) ) + 
  geom_point(aes(color=judge), alpha=1/2, size=0.8) + ## decide the color relying on 'judge'
  scale_color_manual(values=c("firebrick1", "grey", "forestgreen")) + 
  #geom_smooth(method="lm",se=FALSE) +  ## add trend line
  geom_abline(intercept = 0, slope = 1) ## this trend line is better
  ggtitle('Scatter plot for log2fc')


