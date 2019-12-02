# Import simluated cusum data for an aggregated chart(agg) and person-level (pers)
cusum_agg <- read.csv("cusum_agg.csv", stringsAsFactors = FALSE)
cusum_pres<- read.csv("cusum_pers.csv", stringsAsFactors = FALSE)

library(lubridate)
cusum_agg$Period <- factor(cusum_agg$Date)
cusum_agg$Date<-date(as.POSIXct(cusum_agg[[1]], format="%d/%m/%Y"))


#cusum_agg[[1]]<-NULL

cusum_pres$Patient<-cusum_pres[[1]]
cusum_pres[1]<-NULL

cusum_agg$Trigger_n <-as.double(cusum_agg$Trigger)
cusum_agg$Trigger <-as.factor(cusum_agg$Trigger)


cusum_pres$Trigger_n <-as.double(cusum_pres$Trigger)
cusum_pres$Trigger <-as.factor(cusum_pres$Trigger)


cusum_agg$rowno <-as.integer(row.names(cusum_agg))

library(ggplot2)
library(gganimate)

#seq_along(cusum_agg$Period)

# Plot aggregate CUSUM
#a<-

  ggplot(cusum_agg, aes(x=Date, y=Ct))+
  geom_line(col="#51E85E", size=1)+
  geom_point(aes(group=seq_along(Date), shape=Trigger, size=Trigger_n, col=Trigger))+
  geom_hline(aes(yintercept=5.48), col="goldenrod", linetype=2, size=1)+
  ggtitle("Aggreagate Cusum Example")+
  scale_size_continuous(c(0,2))+
  scale_shape_manual(values=c(16,4))+
  scale_colour_manual(values=c("#51E85E", "red"), aesthetics = c("color", "fill"))+
  scale_y_continuous(name="CUSUM value",limits=c(0,6), breaks = seq(0,6,1))+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b-%y",
               expand = expand_scale(0),
               minor_breaks = NULL)+
  guides(size = FALSE)  +
  theme(axis.text.x = element_text(angle=45, hjust = 1))#+
  #transition_reveal(rowno)


anim_save("Aggreagate_cusum.gif", animation = a)


#Plot person-level CUSUM
b<-

  ggplot(cusum_pres, aes(x=Patient, y=Ct))+
  geom_path(col="dodgerblue2", size=1)+
  geom_point(aes(group=seq_along(Patient),shape=Trigger, size=Trigger_n, col=Trigger), fill="#51E85E")+
  geom_hline(aes(yintercept=6.3), col="goldenrod", linetype=2, size=1)+
  ggtitle("Person-level Cusum Example")+
  scale_size_continuous(c(0,2))+
  scale_shape_manual(values=c(16,4))+
  scale_colour_manual(values=c("dodgerblue2","red"))+
  scale_y_continuous(name="CUSUM value",limits=c(0,7), breaks = seq(6))+
  scale_x_continuous(breaks=seq(10,100,10))+
  guides(size = FALSE)+
  #theme(axis.text.x = element_text(angle=45, hjust = 1))
 # transition_reveal(Patient)

anim_save("Person_cusum.gif", animation = b)
