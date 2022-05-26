library(ggplot2)
library(plotly)

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

##Animate
f<-as.data.frame(rep(1, (its*100)))
onex<-as.data.frame(graph$xvals[1:((its*100)/100)])
oney<-as.data.frame(graph$yvals[1:((its*100)/100)])
onez<-as.data.frame(rep(1, (its*100)/100))
twox<-as.data.frame(graph$xvals[1:((its*100)/50)])
twoy<-as.data.frame(graph$yvals[1:((its*100)/50)])
twoz<-as.data.frame(rep(2, (its*100)/50))
thrx<-as.data.frame(graph$xvals[1:((its*100)/30)])
thry<-as.data.frame(graph$yvals[1:((its*100)/30)])
thrz<-as.data.frame(rep(3, (its*100)/30))
fourx<-as.data.frame(graph$xvals[1:((its*100)/25)])
foury<-as.data.frame(graph$yvals[1:((its*100)/25)])
fourz<-as.data.frame(rep(4, (its*100)/25))
fivex<-as.data.frame(graph$xvals[1:((its*100)/20)])
fivey<-as.data.frame(graph$yvals[1:((its*100)/20)])
fivez<-as.data.frame(rep(5, (its*100)/20))
sixx<-as.data.frame(graph$xvals[1:((its*100)/15)])
sixy<-as.data.frame(graph$yvals[1:((its*100)/15)])
sixz<-as.data.frame(rep(6, (its*100)/15))
sevx<-as.data.frame(graph$xvals[1:((its*100)/10)])
sevy<-as.data.frame(graph$yvals[1:((its*100)/10)])
sevz<-as.data.frame(rep(7, (its*100)/10))
eigx<-as.data.frame(graph$xvals[1:((its*100)/5)])
eigy<-as.data.frame(graph$yvals[1:((its*100)/5)])
eigz<-as.data.frame(rep(8, (its*100)/5))
ninx<-as.data.frame(graph$xvals[1:((its*100)/2)])
niny<-as.data.frame(graph$yvals[1:((its*100)/2)])
ninz<-as.data.frame(rep(9, (its*100)/2))
tenx<-as.data.frame(graph$xvals[1:((its*100))])
teny<-as.data.frame(graph$yvals[1:((its*100))])
tenz<-as.data.frame(rep(10, (its*100)/1))
one<-as.data.frame(cbind(onex,oney,onez))
colnames(one) <- c("xvals", "yvals","its")
two<-as.data.frame(cbind(twox,twoy,twoz))
colnames(two) <- c("xvals", "yvals","its")
thr<-as.data.frame(cbind(thrx,thry,thrz))
colnames(thr) <- c("xvals", "yvals","its")
four<-as.data.frame(cbind(fourx,foury,fourz))
colnames(four) <- c("xvals", "yvals","its")
five<-as.data.frame(cbind(fivex,fivey,fivez))
colnames(five) <- c("xvals", "yvals","its")
six<-as.data.frame(cbind(sixx,sixy,sixz))
colnames(six) <- c("xvals", "yvals","its")
sev<-as.data.frame(cbind(sevx,sevy,sevz))
colnames(sev) <- c("xvals", "yvals","its")
eig<-as.data.frame(cbind(eigx,eigy,eigz))
colnames(eig) <- c("xvals", "yvals","its")
nin<-as.data.frame(cbind(ninx,niny,ninz))
colnames(nin) <- c("xvals", "yvals","its")
ten<-as.data.frame(cbind(tenx,teny,tenz))
colnames(ten) <- c("xvals", "yvals","its")
final<-as.data.frame(rbind(one,two,thr,four,five,six,sev,eig,nin,ten))

fig <- final %>%
  plot_ly(
    x = ~xvals, 
    y = ~yvals,
    frame = ~its, 
    text = ~its,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 2.5, color = "green"))
fig
