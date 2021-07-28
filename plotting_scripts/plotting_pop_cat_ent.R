#Pop. Num Cats
rm(list = ls(all = TRUE))
setwd('/Users/jbenfalandays/Documents/GitHub/FalandaysSmaldino_model/data/plots')
source('get_color_palettes.R')

library(ggplot2)
library(data.table)
pop_num_cats=data.table(read.csv('pop_num_clusts.csv'))
mean_pop_num_cats=pop_num_cats %>% filter(iteration>=20000) %>% group_by(net,POP,lifespan,sd_init,K,attraction,error,crit,run) %>% dplyr::summarise(mean=mean(K_ent,na.rm=TRUE))

plotdata=subset(mean_pop_num_cats, net == 0 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata %>% group_by(POP) %>% dplyr::summarise(mean=mean(mean))
ggplot()+
  geom_point(data=plotdata, aes(x=as.factor(POP),y=mean,color=as.factor(POP)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.factor(POP),y=mean,fill=as.factor(POP)),shape='diamond filled',size=7,show.legend = FALSE)+
  scale_x_discrete('Pop. Size',labels=c('10','25','50','100','200')) +
  scale_y_continuous('Shannon Entropy') +
  ggtitle('Complexity X Pop. Size')+
  theme(plot.title = element_text(hjust=.5))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,6))+
  scale_color_manual(values=c1)+
  scale_fill_manual(values=c1)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('N_pop_catent.png', width = 3, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == 0 & POP ==50 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata %>% group_by(lifespan) %>% dplyr::summarise(mean=mean(mean))
ggplot()+
  geom_point(data=plotdata, aes(x=as.factor(lifespan),y=mean,color=as.factor(lifespan)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.factor(lifespan),y=mean,fill=as.factor(lifespan)),shape='diamond filled',size=7,show.legend = FALSE)+
  scale_x_discrete('L',labels=c('5000','10000','15000')) +
  scale_y_continuous('Shannon Entropy') +
  ggtitle('Complexity X Exp. Lifespan')+
  theme(plot.title = element_text(hjust=.5))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,6))+
  scale_color_manual(values=c2)+
  scale_fill_manual(values=c2)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('L_pop_catent.png', width = 3, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == 0 & POP ==50 & lifespan==10000 & crit ==40000 &  error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata %>% group_by(attraction) %>% dplyr::summarise(mean=mean(mean))
ggplot()+
  geom_point(data=plotdata, aes(x=as.factor(attraction),y=mean,color=as.factor(attraction)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.factor(attraction),y=mean,fill=as.factor(attraction)),shape='diamond filled',size=7,show.legend = FALSE)+
  scale_x_discrete('Prototype Bias',labels=c('0.0','0.25','0.75','1.0')) +
  scale_y_continuous('Shannon Entropy') +
  ggtitle('Complexity X Prototype Bias')+
  theme(plot.title = element_text(hjust=.5))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,6))+
  scale_color_manual(values=c3)+
  scale_fill_manual(values=c3)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('A_pop_catent.png', width = 3, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == 0 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & sd_init == 5 & K ==20)
plotdata2=plotdata %>% group_by(error) %>% dplyr::summarise(mean=mean(mean))
ggplot()+
  geom_point(data=plotdata, aes(x=as.factor(error),y=mean,color=as.factor(error)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.factor(error),y=mean,fill=as.factor(error)),shape='diamond filled',size=7,show.legend = FALSE)+
  scale_x_discrete('S.D.',labels=c('0','3','10')) +
  scale_y_continuous('Shannon Entropy') +
  ggtitle('Complexity X Noise')+
  theme(plot.title = element_text(hjust=.5))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,6))+
  scale_color_manual(values=c4)+
  scale_fill_manual(values=c4)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('W_pop_catent.png', width = 3, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == 0 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & K ==20)
plotdata2=plotdata %>% group_by(sd_init) %>% dplyr::summarise(mean=mean(mean))
ggplot()+
  geom_point(data=plotdata, aes(x=as.factor(sd_init),y=mean,color=as.factor(sd_init)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.factor(sd_init),y=mean,fill=as.factor(sd_init)),shape='diamond filled',size=7,show.legend = FALSE)+
  scale_x_discrete('S.D.',labels=c('1','5','10')) +
  scale_y_continuous('Shannon Entropy') +
  ggtitle('Complexity X Cat. Initial SD')+
  theme(plot.title = element_text(hjust=.5))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,6))+
  scale_color_manual(values=c5)+
  scale_fill_manual(values=c5)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('SDi_pop_catent.png', width = 3, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == 0 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 )
plotdata2=plotdata %>% group_by(K) %>% dplyr::summarise(mean=mean(mean))
ggplot()+
  geom_point(data=plotdata, aes(x=as.factor(K),y=mean,color=as.factor(K)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.factor(K),y=mean,fill=as.factor(K)),shape='diamond filled',size=7,show.legend = FALSE)+
  scale_x_discrete('K',labels=c('10','20','30')) +
  scale_y_continuous('Shannon Entropy') +
  ggtitle('Complexity X Num. Gauss/Agent')+
  theme(plot.title = element_text(hjust=.5))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,6))+
  scale_color_manual(values=c6)+
  scale_fill_manual(values=c6)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('K_pop_catent.png', width = 3, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, net == 0 & POP ==50 & lifespan==10000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata %>% group_by(crit) %>% dplyr::summarise(mean=mean(mean))
ggplot()+
  geom_point(data=plotdata, aes(x=as.factor(crit),y=mean,color=as.factor(crit)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.factor(crit),y=mean,fill=as.factor(crit)),shape='diamond filled',size=7,show.legend = FALSE)+
  scale_x_discrete('Crit. Period Length',labels=c('2.5K','5K','10K','20K','No Limit')) +
  scale_y_continuous('Shannon Entropy') +
  ggtitle('Complexity X Crit. Period')+
  theme(plot.title = element_text(hjust=.5))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,6))+
  scale_color_manual(values=c7)+
  scale_fill_manual(values=c7)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('C_pop_catent.png', width = 3, height = 4, units ="in")

plotdata=subset(mean_pop_num_cats, POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20)
plotdata2=plotdata %>% group_by(net) %>% dplyr::summarise(mean=mean(mean))
ggplot()+
  geom_point(data=plotdata, aes(x=as.factor(net),y=mean,color=as.factor(net)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.factor(net),y=mean,fill=as.factor(net)),shape='diamond filled',size=7,show.legend = FALSE)+
  scale_x_discrete('Network',labels=c('Fully\nConnected','Realistic Social\nNetwork','Small World\nNetwork','Connected\nCaveman')) +
  scale_y_continuous('Shannon Entropy') +
  ggtitle('Complexity X Network')+
  theme(plot.title = element_text(hjust=.5))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,6))+
  scale_color_manual(values=c8)+
  scale_fill_manual(values=c8)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('Net_pop_catent.png', width = 3, height = 4, units ="in")

