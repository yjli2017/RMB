# plot data for every 30 mins
plot_every_30<-function(x,y){
  # x is a table and its three columns are Time, Genotype, Micromovements
  # y is the titlle of the plot
  library(ggplot2)
  # x is in long format
  ggplot(x,aes(Time,Micromovements,color=Genotype)) + 
    geom_line() +
    ggtitle("") +
    scale_x_continuous(breaks=c(0,6,12,18,24)) +
    theme_bw() +
    geom_vline(xintercept=12,color="red",linetype="dotted") +
    ggtitle(y)
}
