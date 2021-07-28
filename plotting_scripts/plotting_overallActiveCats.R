rm(list = ls(all = TRUE))
setwd('/Users/jbenfalandays/Documents/GitHub/FalandaysSmaldino_model/data/plots')
source('get_color_palettes.R')

library(ggplot2)
library(data.table)
library(tidyverse)

pop_num_cats=data.table(read.csv('pop_num_clusts.csv'))

# pop_num_cats$K_freqs = gsub("\\[|\\]|\\\n", "", pop_num_cats$K_freqs)
# thrsh=.1
# pop_num_cats$num_active=0
# for(i in 1:nrow(pop_num_cats)){
#   print(i)
#   tmp = as.numeric(str_split(pop_num_cats$K_freqs[i]," ")[[1]])
#   tmp = tmp[!is.na(tmp)]
#   pop_num_cats$K_freqs[i] = list(tmp)
#   pop_num_cats$num_active[i]= length(tmp[tmp > .05])
# }
#save(pop_num_cats,file='pop_num_cats.Rda')
load('pop_num_cats.Rda')

########
#mean_pop_num_cats=pop_num_cats[,.(meanTot=mean(num_categories), meanActive=mean(num_active)),by=c('net','POP','lifespan','sd_init','K','attraction','error','crit','run','iteration')]
mean_pop_num_cats=pop_num_cats[,.(meanTot=mean(num_categories)),by=c('net','POP','lifespan','sd_init','K','attraction','error','crit','run','iteration')]
 
plotdata=subset(mean_pop_num_cats, net == -1 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20 & POP ==50)
plotdata2=plotdata[,.(meanTot=mean(meanTot)),by=c('iteration')]
ggplot(data=plotdata2, aes(x=iteration,y=meanTot))+
  geom_line(data=plotdata2, aes(x=iteration,y=meanTot),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=meanTot),shape = 'circle',size=2.5)+
  #geom_line(data=plotdata2, aes(x=iteration,y=meanActive),color='red',size=1.5,show.legend = FALSE)+
  #geom_point(data=plotdata2, aes(x=iteration,y=meanActive),color='red',shape = 'circle',size=2.5)+
  #geom_line(data=plotdata3, aes(x=iteration,y=mean),color='blue',size=1.5,show.legend = FALSE)+
  #geom_point(data=plotdata3, aes(x=iteration,y=mean),color='blue',shape = 'square',size=2.5)+
  scale_y_continuous('# of Categories Detected') +
  ggtitle('Number of Categories Over Time (Baseline)')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,50))+
  #scale_color_manual(name="Pop. Size",values=c1)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('Baseline_#cat_timeseries.png', width =6, height = 4, units ="in")



##########
mean_pop_num_cats=pop_num_cats[iteration>20000,.(meanTot=mean(num_categories)),by=c('net','POP','lifespan','sd_init','K','attraction','error','crit','run')]

plotdata=mean_pop_num_cats %>% filter(net == -1 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20) %>% group_by(POP,run) %>% dplyr::summarise(meanTot=mean(meanTot))
plotdata2=plotdata %>% group_by(POP) %>% dplyr::summarise(mean=mean(meanTot))

ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(POP),y=meanTot,color=as.factor(POP)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.numeric(POP),y=mean,fill=as.factor(POP)),shape='diamond filled',size=7,show.legend = FALSE)+
  #scale_x_discrete('N',labels=c('10','25','50','100','200','400')) +
  scale_x_continuous('N')+
  scale_y_continuous('Number of Categories') +
  ggtitle('Complexity X Pop. Size')+
  theme(plot.title = element_text(hjust=.5))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,50))+
  scale_color_manual(values=c1)+
  scale_fill_manual(values=c1)+
  guides(fill=FALSE)+
  labs(shape='')+
  theme(
    panel.background = element_rect(fill='gray90'),
    legend.position = c(0.25, .87),
    legend.background=element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    #axis.text.x = element_text(size=6),
    legend.text = element_text(size=6),
    # panel.grid.major = element_line(colour='gray34'),
    # panel.grid.minor = element_line(colour='gray34'),
    legend.direction = 'vertical')+
  ggsave('N_pop_#cat.png', width = 3, height = 4, units ="in")


####

plotdata=mean_pop_num_cats %>% filter(net == -1 & POP ==50 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20) %>% group_by(lifespan,run) %>% dplyr::summarise(meanTot=mean(meanTot))
plotdata2=plotdata %>% group_by(lifespan) %>% dplyr::summarise(mean=mean(meanTot))
ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(lifespan),y=meanTot,color=as.factor(lifespan)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.numeric(lifespan),y=mean,fill=as.factor(lifespan)),shape='diamond filled',size=7,show.legend = FALSE)+
  #scale_x_discrete('L',labels=c('5000','10000','15000')) +
  scale_x_continuous('L')+
  scale_y_continuous('Number of Categories') +
  ggtitle('Complexity X Exp. Lifespan')+
  theme(plot.title = element_text(hjust=.5))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,50))+
  scale_color_manual(values=c2)+
  scale_fill_manual(values=c2)+
  guides(fill=FALSE)+
  labs(shape='')+
  theme(
    panel.background = element_rect(fill='gray90'),
    legend.position = c(0.25, .87),
    legend.background=element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    #axis.text.x = element_text(size=6),
    legend.text = element_text(size=6),
    # panel.grid.major = element_line(colour='gray34'),
    # panel.grid.minor = element_line(colour='gray34'),
    legend.direction = 'vertical')+
  ggsave('L_pop_#cat.png', width = 3, height = 4, units ="in")



#####

plotdata=mean_pop_num_cats %>% filter(net == -1 & POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & sd_init == 5 & K ==20) %>% group_by(error,run) %>% dplyr::summarise(meanTot=mean(meanTot))
plotdata2=plotdata %>% group_by(error) %>% dplyr::summarise(mean=mean(meanTot))


ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(error),y=meanTot,color=as.factor(error)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.numeric(error),y=mean,fill=as.factor(error)),shape='diamond filled',size=7,show.legend = FALSE)+
  #scale_x_discrete('W',labels=c('0','3','5','10')) +
  scale_x_continuous('W')+
  scale_y_continuous('Number of Categories') +
  ggtitle('Complexity X Noise')+
  theme(plot.title = element_text(hjust=.5))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,50))+
  scale_fill_manual(values=c4)+
  scale_color_manual(values=c4)+
  guides(fill=FALSE)+
  labs(shape='')+
  theme(
    panel.background = element_rect(fill='gray90'),
    legend.position = c(0.25, .87),
    legend.background=element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    #axis.text.x = element_text(size=6),
    legend.text = element_text(size=6),
    # panel.grid.major = element_line(colour='gray34'),
    # panel.grid.minor = element_line(colour='gray34'),
    legend.direction = 'vertical')+
  ggsave('W_pop_#cat.png', width = 3, height = 4, units ="in")


####

plotdata=mean_pop_num_cats %>% filter(net == -1 & POP ==50 & lifespan==10000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20) %>% group_by(crit,run) %>% dplyr::summarise(meanTot=mean(meanTot))
plotdata2=plotdata %>% group_by(crit) %>% dplyr::summarise(mean=mean(meanTot))

ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(crit),y=meanTot,color=as.factor(crit)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.numeric(crit),y=mean,fill=as.factor(crit)),shape='diamond filled',size=7,show.legend = FALSE)+
  #scale_x_discrete('C',labels=c('2.5K','5K','10K','20K','No Limit')) +
  scale_x_continuous('C')+
  scale_y_continuous('Number of Categories') +
  ggtitle('Complexity X Crit. Period')+
  theme(plot.title = element_text(hjust=.5))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,50))+
  scale_fill_manual(values=c7)+
  scale_color_manual(values=c7)+
  guides(fill=FALSE)+
  labs(shape='')+
  theme(
    panel.background = element_rect(fill='gray90'),
    legend.position = c(0.25, .87),
    legend.background=element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    #axis.text.x = element_text(size=6),
    legend.text = element_text(size=6),
    # panel.grid.major = element_line(colour='gray34'),
    # panel.grid.minor = element_line(colour='gray34'),
    legend.direction = 'vertical')+
  ggsave('C_pop_#cat.png', width = 3, height = 4, units ="in")


####

plotdata=mean_pop_num_cats %>% filter(POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20) %>% group_by(net,run) %>% dplyr::summarise(meanTot=mean(meanTot))
plotdata2=plotdata %>% group_by(net) %>% dplyr::summarise(mean=mean(meanTot))

ggplot()+
  geom_point(data=plotdata, aes(x=as.factor(net),y=meanTot,color=as.factor(net)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.factor(net),y=mean,fill=as.factor(net)),shape='diamond filled',size=7,show.legend = FALSE)+
  scale_x_discrete('Network',labels=c('Fully\nConnected','Realistic Social\nNetwork','Small World\nNetwork','Connected\nCaveman')) +
  scale_y_continuous('Number of Categories') +
  ggtitle('Complexity X Network')+
  theme(plot.title = element_text(hjust=.5))+
  coord_cartesian(ylim = c(0,50))+
  scale_fill_manual(values=c8)+
  scale_color_manual(values=c8)+
  guides(fill=FALSE)+
  labs(shape='')+
  theme(
        panel.background = element_rect(fill='gray90'),
        legend.position = c(0.25, .87),
        legend.background=element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        axis.text.x = element_text(size=6),
        legend.text = element_text(size=6),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.direction = 'vertical')+
  ggsave('Net_pop_#cat.png', width = 3, height = 4, units ="in")

