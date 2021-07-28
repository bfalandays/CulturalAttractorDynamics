#Pop. Num Cats
rm(list = ls(all = TRUE))
setwd('/Users/jbenfalandays/Documents/GitHub/FalandaysSmaldino_model/data/plots')
source('get_color_palettes.R')

library(ggplot2)
library(data.table)
pop_num_cats=data.table(read.csv('pop_num_clusts.csv'))
num_obs_per_group=pop_num_cats[,.(runs=length(unique(run))),by=c('net','POP','lifespan','sd_init','K','attraction','error','crit')]

mean_pop_num_cats=pop_num_cats[,.(mean=mean(num_categories)),by=c('net','POP','lifespan','sd_init','K','attraction','error','crit','iteration','run')]

plotdata=subset(mean_pop_num_cats, net == 0 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('POP','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(POP)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(POP)),shape = 'circle',size=2.5)+
  scale_y_continuous('Optimal # of Categories Found') +
  ggtitle('Optimal Kmeans X Pop. Size')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,50))+
  scale_color_manual(name="Pop. Size",values=c1)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('N_pop_k.png', width =6, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == 0 & POP ==50 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('lifespan','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(lifespan)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(lifespan)),shape = 'circle',size=2.5)+
  scale_y_continuous('Optimal # of Categories Found') +
  ggtitle('Optimal Kmeans X Exp. Lifespan')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,50))+
  scale_color_manual(name="L",values=c2)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('L_pop_k.png', width =6, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == 0 & POP ==50 & lifespan==10000 & crit ==40000 &  error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('attraction','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(attraction)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(attraction)),shape = 'circle',size=2.5)+
  scale_y_continuous('Optimal # of Categories Found') +
  ggtitle('Optimal Kmeans X Prototype Bias in Production')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,50))+
  scale_color_manual(name="Prototype Bias",values=c3)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('A_pop_k.png', width =6, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == 0 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('error','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(error)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(error)),shape = 'circle',size=2.5)+
  scale_y_continuous('Optimal # of Categories Found') +
  ggtitle('Optimal Kmeans X S.D. of Gaussian Noise')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,50))+
  scale_color_manual(name="S.D. of Gaussian Noise",values=c4)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('W_pop_k.png', width =6, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == 0 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('sd_init','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(sd_init)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(sd_init)),shape = 'circle',size=2.5)+
  scale_y_continuous('Optimal # of Categories Found') +
  ggtitle('Optimal Kmeans X Cat. Initial S.D.')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,50))+
  scale_color_manual(name="Cat. Initial S.D.",values=c5)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('SDi_pop_k.png', width =6, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == 0 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 )
plotdata2=plotdata[,.(mean=mean(mean)),by=c('K','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(K)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(K)),shape = 'circle',size=2.5)+
  scale_y_continuous('Optimal # of Categories Found') +
  ggtitle('Optimal Kmeans X Num. Gaussians per Agent')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,50))+
  scale_color_manual(name="K",values=c6)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('K_pop_k.png', width =6, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == 0 & POP ==50 & lifespan==10000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('crit','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(crit)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(crit)),shape = 'circle',size=2.5)+
  scale_y_continuous('Optimal # of Categories Found') +
  ggtitle('Optimal Kmeans X Crit. Period Length')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,50))+
  scale_color_manual(name="Crit. Period",values=c7,labels=c('2.5K','5K','10K','20K','No Limit'))+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('C_pop_k.png', width =6, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('net','iteration')]
ggplot()+
  geom_line(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(net)),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean,color=as.factor(net)),shape = 'circle',size=2.5)+
  scale_y_continuous('Optimal # of Categories Found') +
  ggtitle('Optimal Kmeans X Network')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,50))+
  scale_color_manual(name="Network",values=c8,labels=c('Fully\nConnected','Realistic Social\nNetwork','Small World\nNetwork','Connected\nCaveman'))+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('Net_pop_k.png', width =6, height = 4, units ="in")

