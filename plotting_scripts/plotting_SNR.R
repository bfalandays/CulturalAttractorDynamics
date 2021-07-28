#Pop. Num Cats
rm(list = ls(all = TRUE))
setwd('/Users/jbenfalandays/Documents/GitHub/FalandaysSmaldino_model/data/plots')
source('get_color_palettes.R')

library(ggplot2)
library(data.table)
library(tidyverse)

pop_num_cats=data.table(read.csv('pop_num_clusts.csv'))
mean_pop_num_cats=pop_num_cats[iteration>=20000,.(mean=mean(K_SC,na.rm=TRUE)),by=c('net','POP','lifespan','sd_init','K','attraction','error','crit','run')]

plotdata=subset(mean_pop_num_cats, net == -1 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('POP')]
ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(POP),y=mean,color=as.factor(POP)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.numeric(POP),y=mean,fill=as.factor(POP)),shape='diamond filled',size=7,show.legend = FALSE)+
  #scale_x_discrete('Pop. Size',labels=c('10','25','50','100','200','400')) +
  scale_y_continuous('Mean S.C.') +
  theme(plot.title = element_text(hjust=.5))+
  ggtitle('Discriminability X Pop. Size')+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,1))+
  scale_color_manual(values=c1)+
  scale_fill_manual(values=c1)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('N_pop_SC.png', width =3, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == -1 & POP ==50 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('lifespan')]
ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(lifespan),y=mean,color=as.factor(lifespan)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.numeric(lifespan),y=mean,fill=as.factor(lifespan)),shape='diamond filled',size=7,show.legend = FALSE)+
  scale_y_continuous('Mean S.C.') +
  theme(plot.title = element_text(hjust=.5))+
  ggtitle('Discriminability X Exp. Lifespan')+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,1))+
  scale_color_manual(values=c2)+
  scale_fill_manual(values=c2)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('L_pop_SC.png', width =3, height = 4, units ="in")

# plotdata=subset(mean_pop_num_cats, net == -1 & POP ==50 & lifespan==10000 & crit ==40000 &  error ==0 & sd_init == 5 & K ==20)
# plotdata2=plotdata[,.(mean=mean(mean)),by=c('attraction','iteration')]
# ggplot()+
#   geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(attraction)),size=1.5,show.legend = FALSE)+
#   geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(attraction)),shape = 'circle',size=2.5)+
#   scale_y_continuous('Mean S.C.') +
#   ggtitle('Discriminability X Prototype Bias in Production')+
#   theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
#   scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
#   coord_cartesian(xlim = c(0, 40000), ylim = c(0,1))+
#   scale_color_manual(name="Prototype Bias",values=c3)+
#   theme(legend.position="bottom",
#         panel.background = element_rect(fill='gray90'),
#         # panel.grid.major = element_line(colour='gray34'),
#         # panel.grid.minor = element_line(colour='gray34'),
#         legend.key = element_rect(fill = "gray90"))+
#   ggsave('A_pop_SC.png', width =6, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == -1 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('error')]
ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(error),y=mean,color=as.factor(error)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.numeric(error),y=mean,fill=as.factor(error)),shape='diamond filled',size=7,show.legend = FALSE)+
  #scale_x_discrete('W',labels=c('0','3','10')) +
  scale_y_continuous('Mean S.C.') +
  theme(plot.title = element_text(hjust=.5))+
  ggtitle('Discriminability X Noise')+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,1))+
  scale_color_manual(values=c4)+
  scale_fill_manual(values=c4)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('W_pop_SC.png', width =3, height = 4, units ="in")

# plotdata=subset(mean_pop_num_cats, net == -1 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & K ==20)
# plotdata2=plotdata[,.(mean=mean(mean)),by=c('sd_init','iteration')]
# ggplot()+
#   geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(sd_init)),size=1.5,show.legend = FALSE)+
#   geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(sd_init)),shape = 'circle',size=2.5)+
#   scale_y_continuous('Mean S.C.') +
#   ggtitle('Discriminability X Cat. Initial S.D.')+
#   theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
#   scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
#   coord_cartesian(xlim = c(0, 40000), ylim = c(0,1))+
#   scale_color_manual(name="Cat. Initial S.D.",values=c5)+
#   theme(legend.position="bottom",
#         panel.background = element_rect(fill='gray90'),
#         # panel.grid.major = element_line(colour='gray34'),
#         # panel.grid.minor = element_line(colour='gray34'),
#         legend.key = element_rect(fill = "gray90"))+
#   ggsave('SDi_pop_SC.png', width =6, height = 4, units ="in")
# 
# plotdata=subset(mean_pop_num_cats, net == -1 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 )
# plotdata2=plotdata[,.(mean=mean(mean)),by=c('K','iteration')]
# ggplot()+
#   geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(K)),size=1.5,show.legend = FALSE)+
#   geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(K)),shape = 'circle',size=2.5)+
#   scale_y_continuous('Mean S.C.') +
#   ggtitle('Discriminability X Num. Gaussians per Agent')+
#   theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
#   scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
#   coord_cartesian(xlim = c(0, 40000), ylim = c(0,1))+
#   scale_color_manual(name="K",values=c6)+
#   theme(legend.position="bottom",
#         panel.background = element_rect(fill='gray90'),
#         # panel.grid.major = element_line(colour='gray34'),
#         # panel.grid.minor = element_line(colour='gray34'),
#         legend.key = element_rect(fill = "gray90"))+
#   ggsave('K_pop_SC.png', width =6, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == -1 & POP ==50 & lifespan==10000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('crit')]
ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(crit),y=mean,color=as.factor(crit)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.numeric(crit),y=mean,fill=as.factor(crit)),shape='diamond filled',size=7,show.legend = FALSE)+
  #scale_x_discrete('Crit. Period Length',labels=c('2.5K','5K','10K','20K','No Limit')) +
  scale_y_continuous('Mean S.C.') +
  theme(plot.title = element_text(hjust=.5))+
  ggtitle('Discriminability X Crit. Period')+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,1))+
  scale_color_manual(values=c7)+
  scale_fill_manual(values=c7)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('C_pop_SC.png', width =3, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('net')]
ggplot()+
  geom_point(data=plotdata, aes(x=as.factor(net),y=mean,color=as.factor(net)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.factor(net),y=mean,fill=as.factor(net)),shape='diamond filled',size=7,show.legend = FALSE)+
  #scale_x_discrete('Network',labels=c('Fully\nConnected','Realistic Social\nNetwork','Small World\nNetwork','Connected\nCaveman')) +
  scale_y_continuous('Mean S.C.') +
  theme(plot.title = element_text(hjust=.5))+
  ggtitle('Discriminability X Network')+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,1))+
  scale_color_manual(values=c8)+
  scale_fill_manual(values=c8)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        axis.text.x = element_text(size=6),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('Net_pop_SC.png', width =3, height = 4, units ="in")
