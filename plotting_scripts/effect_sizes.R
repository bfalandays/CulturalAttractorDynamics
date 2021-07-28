rm(list = ls(all = TRUE))
setwd('/Users/jbenfalandays/Documents/GitHub/FalandaysSmaldino_model/data/plots')
source('get_color_palettes.R')

library(ggplot2)
library(data.table)
library(tidyverse)
source('get_color_palettes.R')


agent_wass_scores=data.table(read.csv('agent_wass_scores.csv'))
mean_agent_wass_scores=agent_wass_scores[iteration>20000,.(nonconformity=mean(wass)),by=c("POP","lifespan","error",'crit','attraction','K',"sd_init","run",'net')]

pop_num_cats=data.table(read.csv('pop_num_clusts.csv'))
mean_pop_num_cats=pop_num_cats[iteration>20000,.(complexity=mean(K_ent,na.rm=TRUE),discriminability=mean(K_SC,na.rm=TRUE)),by=c('net','POP','lifespan','sd_init','K','attraction','error','crit','run')]

pop_wass_scores=data.table(read.csv('pop_wass_scores.csv'))
mean_pop_wass_scores=pop_wass_scores[iteration>20000,.(instability=mean(wass)),by=c('net','POP','lifespan','sd_init','K','attraction','error','crit','run')]

df<-inner_join(mean_agent_wass_scores,mean_pop_num_cats)
df<-inner_join(df, mean_pop_wass_scores)

rm(agent_wass_scores, pop_num_cats, pop_wass_scores, mean_agent_wass_scores, mean_pop_num_cats, mean_pop_wass_scores)

scaled_df<-data.table(scale(df))

m_1<-lm(complexity ~ K + sd_init + attraction + error + crit + lifespan + POP, data = scaled_df)
m_2<-lm(discriminability ~ K + sd_init + attraction + error + crit + lifespan + POP, data = scaled_df)
m_3<-lm(nonconformity ~ K + sd_init + attraction + error + crit + lifespan + POP, data = scaled_df)
m_4<-lm(instability ~ K + sd_init + attraction + error + crit + lifespan + POP, data = scaled_df)

plot_df<-data.frame(complexity=m_1$coefficients, discriminability=m_2$coefficients, nonconformity=m_3$coefficients, instability = m_4$coefficients)

plot_df$param=row.names(plot_df)
plot_df=plot_df[-1,]
plot_df$param=factor(plot_df$param,levels=c('K','sd_init','attraction',"error","crit","lifespan","POP"))
plot_df=data.table(plot_df)

ggplot(plot_df, aes(x=as.factor(param),y=as.numeric(complexity)))+
         geom_point(size=3)

ggplot(plot_df, aes(x=as.factor(param),y=as.numeric(discriminability)))+
  geom_point(size=3)

ggplot(plot_df, aes(x=as.factor(param),y=as.numeric(nonconformity)))+
  geom_point(size=3)

ggplot(plot_df, aes(x=as.factor(param),y=as.numeric(instability)))+
  geom_point(size=3)

library(RColorBrewer)
c1=brewer.pal(n = 9, name = 'Set1')[c(1:5,7,9)]

long <- plot_df %>% gather(measure, value, -c(param))
ggplot(long, aes(x=param, y=value, group=measure, color=param,shape=measure))+
  geom_point(size=4)+
  geom_hline(yintercept = 0,linetype='dashed')+
  theme(plot.title = element_text(hjust=.5))+
  ggtitle('Estimated Effect Sizes by Parameter')+
  scale_color_manual(values=c1, name="Outcome",labels=c('Complexity','Discriminability','Instability','Nonconformity'))+
  scale_x_discrete(name='Parameter',labels=c('K','SDinit','Prototype\nBias','Trans.\nNoise','Crit. Period\nLength','Ex.\nLifespan','Pop.\nSize'))+
  scale_y_continuous(name='Effect Size')+
  scale_shape_manual(name='Outcome',values=c(0,1,2,5),labels=c('Complexity','Discriminability','Instability','Nonconformity'))+
  guides(color=FALSE)+
  ggsave('Scaled_effectsizes.png', width =6, height = 4, units ="in")
  s#geom_point(size=3,alpha=.7)+
  #scale_y_continuous(limits=c(-.6,.6))+
  
