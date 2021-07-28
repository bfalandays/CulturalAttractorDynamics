rm(list = ls(all = TRUE))

setwd('/Users/jbenfalandays/Documents/GitHub/FalandaysSmaldino_model/data/plots')
source('get_color_palettes.R')

library(ggplot2)
library(data.table)
agent_wass_scores=data.table(read.csv('agent_wass_scores.csv'))
num_obs_per_group=agent_wass_scores[,.(runs=length(unique(run))),by=c('POP','lifespan','sd_init','K','attraction','error','crit','net')]

bins = seq(0,40000,by=250)
counter=0

agent_wass_scores$age_bin=cut(agent_wass_scores$age,breaks = bins,labels=FALSE,include.lowest = TRUE)

hist(subset(agent_wass_scores, iteration==40000)$age)

plotdata=subset(agent_wass_scores, net == -1 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20 & POP ==50)
plotdata2=plotdata[,.(mean=mean(wass)),by=c('iteration')]
ggplot(data=plotdata2, aes(x=iteration,y=mean))+
  geom_line(data=plotdata2, aes(x=iteration,y=mean),size=1.5,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=iteration,y=mean),shape = 'circle',size=2.5)+
  #geom_line(data=plotdata2, aes(x=iteration,y=meanActive),color='red',size=1.5,show.legend = FALSE)+
  #geom_point(data=plotdata2, aes(x=iteration,y=meanActive),color='red',shape = 'circle',size=2.5)+
  #geom_line(data=plotdata3, aes(x=iteration,y=mean),color='blue',size=1.5,show.legend = FALSE)+
  #geom_point(data=plotdata3, aes(x=iteration,y=mean),color='blue',shape = 'square',size=2.5)+
  scale_y_continuous('Mean Nonconformity') +
  ggtitle('Nonconformity Over Time (Baseline)')+
  theme(plot.title = element_text(hjust=.5), axis.title = element_text(size=12))+
  scale_x_continuous('Iteration',breaks = seq(0,40000,by=5000)) +
  coord_cartesian(xlim = c(0, 40000), ylim = c(0,4))+
  #scale_color_manual(name="Pop. Size",values=c1)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('Baseline_agent_emd_timeseries.png', width =6, height = 4, units ="in")

plotdata=agent_wass_scores[POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20 & net ==0 & iteration >= 20000 ,.(mean=mean(wass)),by=c("age_bin")]
plotdata$age_bin2=plotdata$age_bin*250-250
ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(age_bin2),y=mean),color='black',size=2,show.legend = FALSE)+
  scale_x_continuous('Age Bin',breaks=seq(0,40000,by=5000)) +
  scale_y_continuous('Mean Nonconformity') +
  theme(plot.title = element_text(hjust=.5))+
  ggtitle('Nonconformity by Age Bin, T > 20,000')+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,4))+
  #scale_color_brewer(type="seq",palette = 1)+
  #scale_fill_brewer(type="seq",palette = 1)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('agent_wass_byage.png', width = 6, height = 4, units ="in")
  
 #### 

mean_agent_wass_scores=agent_wass_scores[iteration>=20000,.(mean=mean(wass)),by=c("POP","lifespan","error",'crit','attraction','K',"sd_init","run",'net')]

plotdata=subset(mean_agent_wass_scores, lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20 & net == -1 )
plotdata2=plotdata[,.(mean=mean(mean)),by=c('POP')]
#plotdata_init=subset(mean_agent_wass_scores, lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20 & iteration==0 & net == -1)
#plotdata2_init=plotdata_init[,.(mean=mean(mean)),by=c('POP')]
ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(POP),y=mean,color=as.factor(POP)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.numeric(POP),y=mean,fill=as.factor(POP)),shape='diamond filled',size=7,show.legend = FALSE)+
  #geom_point(data=plotdata2_init, aes(x=as.factor(POP),y=mean,color=as.factor(POP)),shape='diamond open',size=7,show.legend = FALSE)+
  #scale_x_discrete('N',labels=c('10','25','50','100','200')) +
  scale_x_continuous('N')+
  scale_y_continuous('Mean Nonconformity') +
  theme(plot.title = element_text(hjust=.5))+
  ggtitle('Nonconformity X Pop. Size')+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,2))+
  scale_color_manual(values=c1)+
  scale_fill_manual(values=c1)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('N_agent_emd.png', width = 3, height = 4, units ="in")

plotdata=subset(mean_agent_wass_scores, POP ==50 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20 & net == -1)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('lifespan')]
#plotdata_init=subset(mean_agent_wass_scores, POP ==50 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20 & iteration==0 & net == -1)
#plotdata2_init=plotdata_init[,.(mean=mean(mean)),by=c('lifespan')]
ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(lifespan),y=mean,color=as.factor(lifespan)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.numeric(lifespan),y=mean,fill=as.factor(lifespan)),shape='diamond filled',size=7,show.legend = FALSE)+
  #geom_point(data=plotdata2_init, aes(x=as.factor(lifespan),y=mean,color=as.factor(lifespan)),shape='diamond open',size=7,show.legend = FALSE)+
  #scale_x_discrete('L',labels=c('5000','10000','15000')) +
  scale_x_continuous('L')+
  scale_y_continuous('Mean Nonconformity') +
  theme(plot.title = element_text(hjust=.5))+
  ggtitle('Nonconformity X Exp. Lifespan')+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,2))+
  scale_color_manual(values=c2)+
  scale_fill_manual(values=c2)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('L_agent_emd.png', width = 3, height = 4, units ="in")

# plotdata=subset(mean_agent_wass_scores, POP ==50 & lifespan==10000 & crit ==40000 &  error ==0 & sd_init == 5 & K ==20  & net == -1)
# plotdata2=plotdata[,.(mean=mean(mean)),by=c('attraction')]
# #plotdata_init=subset(mean_agent_wass_scores, POP ==50 & lifespan==10000 & crit ==40000 &  error ==0 & sd_init == 5 & K ==20 & iteration==0 & net == -1)
# #plotdata2_init=plotdata_init[,.(mean=mean(mean)),by=c('attraction')]
# ggplot()+
#   geom_point(data=plotdata, aes(x=as.factor(attraction),y=mean,color=as.factor(attraction)),size=3, alpha =.7,show.legend = FALSE)+
#   geom_point(data=plotdata2, aes(x=as.factor(attraction),y=mean,fill=as.factor(attraction)),shape='diamond filled',size=7,show.legend = FALSE)+
#   #geom_point(data=plotdata2_init, aes(x=as.factor(attraction),y=mean,color=as.factor(attraction)),shape='diamond open',size=7,show.legend = FALSE)+
#   scale_x_discrete('Prototype Bias',labels=c('0.0','0.25','0.75','1.0')) +
#   scale_y_continuous('Mean Nonconformity') +
#   theme(plot.title = element_text(hjust=.5))+
#   ggtitle('Nonconformity X Prototype Bias')+
#   theme(legend.position="bottom")+
#   coord_cartesian(ylim = c(0,2))+
#   scale_color_manual(values=c3)+
#   scale_fill_manual(values=c3)+
#   theme(legend.position="bottom",
#         panel.background = element_rect(fill='gray90'),
#         # panel.grid.major = element_line(colour='gray34'),
#         # panel.grid.minor = element_line(colour='gray34'),
#         legend.key = element_rect(fill = "gray90"))+
#   ggsave('A_agent_emd.png', width = 3, height = 4, units ="in")

plotdata=subset(mean_agent_wass_scores, POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & sd_init == 5 & K ==20 & net == -1)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('error')]
#plotdata_init=subset(mean_agent_wass_scores, POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & sd_init == 5 & K ==20 & iteration==0 & net == -1)
#plotdata2_init=plotdata_init[,.(mean=mean(mean)),by=c('error')]
ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(error),y=mean,color=as.factor(error)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.numeric(error),y=mean,fill=as.factor(error)),shape='diamond filled',size=7,show.legend = FALSE)+
  #geom_point(data=plotdata2_init, aes(x=as.factor(error),y=mean,color=as.factor(error)),shape='diamond open',size=7,show.legend = FALSE)+
  #scale_x_discrete('W',labels=c('0','3','10')) +
  scale_x_continuous('W')+
  scale_y_continuous('Mean Nonconformity') +
  theme(plot.title = element_text(hjust=.5))+
  ggtitle('Nonconformity X Noise')+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,2))+
  scale_color_manual(values=c4)+
  scale_fill_manual(values=c4)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('W_agent_emd.png', width = 3, height = 4, units ="in")

# plotdata=subset(mean_agent_wass_scores, POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & K ==20 & net == -1)
# plotdata2=plotdata[,.(mean=mean(mean)),by=c('sd_init')]
# #plotdata_init=subset(mean_agent_wass_scores, POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & K ==20 & iteration==0 & net == -1)
# #plotdata2_init=plotdata_init[,.(mean=mean(mean)),by=c('sd_init')]
# ggplot()+
#   geom_point(data=plotdata, aes(x=as.factor(sd_init),y=mean,color=as.factor(sd_init)),size=3, alpha =.7,show.legend = FALSE)+
#   geom_point(data=plotdata2, aes(x=as.factor(sd_init),y=mean,fill=as.factor(sd_init)),shape='diamond filled',size=7,show.legend = FALSE)+
#  # geom_point(data=plotdata2_init, aes(x=as.factor(sd_init),y=mean,color=as.factor(sd_init)),shape='diamond open',size=7,show.legend = FALSE)+
#   scale_x_discrete('S.D.',labels=c('1','5','10')) +
#   scale_y_continuous('Mean Nonconformity') +
#   theme(plot.title = element_text(hjust=.5))+
#   ggtitle('Nonconformity X Cat. Initial S.D.')+
#   theme(legend.position="bottom")+
#   coord_cartesian(ylim = c(0,2))+
#   scale_color_manual(values=c5)+
#   scale_fill_manual(values=c5)+
#   theme(legend.position="bottom",
#         panel.background = element_rect(fill='gray90'),
#         # panel.grid.major = element_line(colour='gray34'),
#         # panel.grid.minor = element_line(colour='gray34'),
#         legend.key = element_rect(fill = "gray90"))+
#   ggsave('SDi_agent_emd.png', width = 3, height = 4, units ="in")

# plotdata=subset(mean_agent_wass_scores, POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5  & net == -1)
# plotdata2=plotdata[,.(mean=mean(mean)),by=c('K')]
# #plotdata_init=subset(mean_agent_wass_scores, POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & iteration==0 & net == -1)
# #plotdata2_init=plotdata_init[,.(mean=mean(mean)),by=c('K')]
# ggplot()+
#   geom_point(data=plotdata, aes(x=as.factor(K),y=mean,color=as.factor(K)),size=3, alpha =.7,show.legend = FALSE)+
#   geom_point(data=plotdata2, aes(x=as.factor(K),y=mean,fill=as.factor(K)),shape='diamond filled',size=7,show.legend = FALSE)+
#   #geom_point(data=plotdata2_init, aes(x=as.factor(K),y=mean,color=as.factor(K)),shape='diamond open',size=7,show.legend = FALSE)+
#   scale_x_discrete('K',labels=c('10','20','30')) +
#   scale_y_continuous('Mean Nonconformity') +
#   theme(plot.title = element_text(hjust=.5))+
#   ggtitle('Nonconformity X Num. Gauss/Agent')+
#   theme(legend.position="bottom")+
#   coord_cartesian(ylim = c(0,2))+
#   scale_color_manual(values=c6)+
#   scale_fill_manual(values=c6)+
#   theme(legend.position="bottom",
#         panel.background = element_rect(fill='gray90'),
#         # panel.grid.major = element_line(colour='gray34'),
#         # panel.grid.minor = element_line(colour='gray34'),
#         legend.key = element_rect(fill = "gray90"))+
#   ggsave('K_agent_emd.png', width = 3, height = 4, units ="in")


plotdata=subset(mean_agent_wass_scores, POP ==50 & lifespan==10000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20 & net == -1)
plotdata2=plotdata[,.(mean=mean(mean)),by=c('crit')]
#plotdata_init=subset(mean_agent_wass_scores, POP ==50 & lifespan==10000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20 & iteration==0 & net == -1)
#plotdata2_init=plotdata_init[,.(mean=mean(mean)),by=c('crit')]
ggplot()+
  geom_point(data=plotdata, aes(x=as.numeric(crit),y=mean,color=as.factor(crit)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.numeric(crit),y=mean,fill=as.factor(crit)),shape='diamond filled',size=7,show.legend = FALSE)+
  #geom_point(data=plotdata2_init, aes(x=as.factor(crit),y=mean,color=as.factor(crit)),shape='diamond open',size=7,show.legend = FALSE)+
  #scale_x_discrete('C',labels=c('2.5K','5K','10K','20K','No Limit')) +
  scale_x_continuous('C')+
  scale_y_continuous('Mean Nonconformity') +
  theme(plot.title = element_text(hjust=.5))+
  ggtitle('Nonconformity X Crit. Period')+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,2))+
  scale_color_manual(values=c7)+
  scale_fill_manual(values=c7)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('C_agent_emd.png', width = 3, height = 4, units ="in")
 
plotdata=subset(mean_agent_wass_scores, POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20  )
plotdata2=plotdata[,.(mean=mean(mean)),by=c('net')]
#plotdata_init=subset(mean_agent_wass_scores, POP ==50 & lifespan==10000 & crit ==40000 & attraction == 1 & error ==0 & sd_init == 5 & K ==20 & iteration==0 )
#plotdata2_init=plotdata_init[,.(mean=mean(mean)),by=c('net')]
ggplot()+
  geom_point(data=plotdata, aes(x=as.factor(net),y=mean,color=as.factor(net)),size=3, alpha =.7,show.legend = FALSE)+
  geom_point(data=plotdata2, aes(x=as.factor(net),y=mean,fill=as.factor(net)),shape='diamond filled',size=7,show.legend = FALSE)+
  #geom_point(data=plotdata2_init, aes(x=as.factor(net),y=mean,color=as.factor(net)),shape='diamond open',size=7,show.legend = FALSE)+
  scale_x_discrete('Network',labels=c('Fully\nConnected','Realistic Social\nNetwork','Small World\nNetwork','Connected\nCaveman')) +
  scale_y_continuous('Mean Nonconformity') +
  theme(plot.title = element_text(hjust=.5))+
  ggtitle('Nonconformity X Network')+
  theme(legend.position="bottom")+
  coord_cartesian(ylim = c(0,3))+
  scale_color_manual(values=c8)+
  scale_fill_manual(values=c8)+
  theme(legend.position="bottom",
        panel.background = element_rect(fill='gray90'),
        # panel.grid.major = element_line(colour='gray34'),
        # panel.grid.minor = element_line(colour='gray34'),
        axis.text.x = element_text(size=6),
        legend.key = element_rect(fill = "gray90"))+
  ggsave('Net_agent_emd.png', width = 3, height = 4, units ="in")

