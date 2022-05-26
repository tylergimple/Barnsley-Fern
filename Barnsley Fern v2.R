startx<-0
starty<-1

new_pointx<-(startx*0.85)+(starty*0.04)
new_pointy<-(startx*-0.04)+(starty*0.85)+1.6
its<-400
one <- (as.numeric(round(runif((its*1), min = 1, max = 1),0)))
two <- (as.numeric(round(runif((its*73), min = 2, max = 2),0)))
three <- (as.numeric(round(runif((its*13), min = 3, max = 3),0)))
four <- (as.numeric(round(runif((its*11), min = 4, max = 4),0)))
y<-sample(c(one,two,three,four))

z<-as.data.frame(capture.output(for (val in y) {
    if(val == 1){
      new_pointx = 0
      new_pointy = (new_pointy*0.16)
      print(new_pointx)
      print(new_pointy)}
    if(val == 2){
      new_pointx = (new_pointx*0.85)+(new_pointy*0.04)
      new_pointxi=(1.17647058823529*(new_pointx))-(0.0470588235294118*new_pointy)
      new_pointy = (new_pointxi*-0.04)+(new_pointy*0.85)+1.6
      print(new_pointx)
      print(new_pointy)}
    if(val == 3){
      new_pointx = (new_pointx*-0.15)+(new_pointy*0.28)
      new_pointxi = ((-6.66667*new_pointx)+(1.86667*new_pointy))
      new_pointy = (new_pointxi*0.26)+(0.24*new_pointy)+1.6
      print(new_pointx)
      print(new_pointy)}
    if(val == 4){
      new_pointx = (new_pointx*0.2)-(new_pointy*0.26)
      new_pointxi = ((5*new_pointx)+(1.3*new_pointy))
      new_pointy = (new_pointxi*0.23)+(0.2*new_pointy)+0.44
      print(new_pointx)
      print(new_pointy)}
}))
colnames(z)<- c("vals")
cleanz<-as.data.frame(as.numeric(substring(z$vals,4)))
colnames(cleanz)<- c("vals")


grabx<-seq(1, nrow(cleanz), by=2)
xpoint<-as.data.frame(cleanz$vals[c(grabx)])
colnames(xpoint) <- c("xvals")

graby<-seq(2, nrow(cleanz), by=2)
ypoint<-as.data.frame(cleanz$vals[c(graby)])
colnames(ypoint) <- c("yvals")

graph<-cbind(xpoint,ypoint)


ggplot(data = graph, aes(x = xvals, y=yvals)) + geom_point(size=.1, colour="darkgreen") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())