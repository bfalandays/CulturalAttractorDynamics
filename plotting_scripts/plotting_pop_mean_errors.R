#Pop. Signal Entropy
rm(list = ls(all = TRUE))
source('get_color_palettes.R')

setwd('/Users/jbenfalandays/Documents/GitHub/FalandaysSmaldino_model/data/plots')

library(ggplot2)
library(data.table)
pop_signal_entropy=data.table(read.csv('pop_signal_entropys.csv'))
mean_errors=pop_signal_entropy[,.(mean=mean(mean_errors)),by=c("net","POP","iteration","lifespan","error",'crit','attraction','K',"sd_init","run")]
#tmp=rep(1/400,400)
#max_entropy = sum(tmp[tmp>0]*log2(1/tmp[tmp>0]))

plotdata=subset(mean_errors, net == 0 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('POP','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(POP)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(POP)),shape = 'circle',size=2.5)+
  scale_y_continuous('Mean Error') +
  ##geom_hline(yintercept = max_entropy,color='red',linetype='dashed',size=1)+
  ggtitle('Mean Error X Pop. Size')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,13))+
  scale_color_manual(name="Pop. Size",values=c1)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('N_pop_error.png', width =6, height = 4, units ="in")

plotdata=subset(mean_errors, net == 0 & POP ==50 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('lifespan','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(lifespan)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(lifespan)),shape = 'circle',size=2.5)+ scale_y_continuous('Mean Error') +
  #geom_hline(yintercept = max_entropy,color='red',linetype='dashed',size=1)+
  ggtitle('Mean Error X Exp. Lifespan')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,13))+
  scale_color_manual(name="L",values=c2)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('L_pop_error.png', width =6, height = 4, units ="in")

plotdata=subset(mean_errors, net == 0 & POP ==50 & lifespan==10000 & crit ==40000 &  error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('attraction','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(attraction)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(attraction)),shape = 'circle',size=2.5)+ scale_y_continuous('Mean Error') +
  #geom_hline(yintercept = max_entropy,color='red',linetype='dashed',size=1)+
  ggtitle('Mean Error X Prototype Bias in Production')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,13))+
  scale_color_manual(name="Prototype Bias",values=c3)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('A_pop_error.png', width =6, height = 4, units ="in")

plotdata=subset(mean_errors, net == 0 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('error','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(error)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(error)),shape = 'circle',size=2.5)+ scale_y_continuous('Mean Error') +
  #geom_hline(yintercept = max_entropy,color='red',linetype='dashed',size=1)+
  ggtitle('Mean Error X S.D. of Gaussian Noise')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,13))+
  scale_color_manual(name="S.D. of Gaussian Noise",values=c4)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('W_pop_error.png', width =6, height = 4, units ="in")

plotdata=subset(mean_errors, net == 0 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('sd_init','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(sd_init)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(sd_init)),shape = 'circle',size=2.5)+ scale_y_continuous('Mean Error') +
  #geom_hline(yintercept = max_entropy,color='red',linetype='dashed',size=1)+
  ggtitle('Mean Error X Cat. Initial S.D.')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,25))+
  scale_color_manual(name="Cat. Initial S.D.",values=c5)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('SDi_pop_error.png', width =6, height = 4, units ="in")

plotdata=subset(mean_errors, net == 0 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 )
plotdata2=plotdata[,.(mean=mean(mean)),by=c('K','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(K)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(K)),shape = 'circle',size=2.5)+ scale_y_continuous('Mean Error') +
  #geom_hline(yintercept = max_entropy,color='red',linetype='dashed',size=1)+
  ggtitle('Mean Error X Num. Gaussians per Agent')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,20))+
  scale_color_manual(name="K",values=c6)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('K_pop_error.png', width =6, height = 4, units ="in")

plotdata=subset(mean_errors, net == 0 & POP ==50 & lifespan==10000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('crit','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(crit)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(crit)),shape = 'circle',size=2.5)+ scale_y_continuous('Mean Error') +
  #geom_hline(yintercept = max_entropy,color='red',linetype='dashed',size=1)+
  ggtitle('Mean Error X Crit. Period')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,13))+
  scale_color_manual(name="Crit. Period",values=c7,labels=c('2.5K','5K','10K','20K','No Limit'))+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('C_pop_error.png', width =6, height = 4, units ="in")

plotdata=subset(mean_errors, POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('net','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(net)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(net)),shape = 'circle',size=2.5)+ scale_y_continuous('Mean Error') +
  #geom_hline(yintercept = max_entropy,color='red',linetype='dashed',size=1)+
  ggtitle('Mean Error X Network')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,13))+
  scale_color_manual(name="Network",values=c8,labels=c('Fully\nConnected','Realistic Social\nNetwork','Small World\nNetwork','Connected\nCaveman'))+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('Net_pop_error.png', width =6, height = 4, units ="in")
