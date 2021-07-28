# import modules
import random
import numpy as np
import pandas as pd
import scipy as sp
from scipy import spatial, stats
#from plotnine import *
#import matplotlib
#import matplotlib.pyplot as pl
from pathlib import Path
#import ot
#import ot.plot
import os
cwd = os.getcwd()
import os.path
from os import path

from gap_statistic import OptimalK
from sklearn.cluster import KMeans
from sklearn import metrics
from sklearn.metrics import pairwise_distances
from cv2 import EMD, DIST_L2

import multiprocessing as mp
import sys

job = int(sys.argv[1])
batch = int(sys.argv[2])
cpus = int(sys.argv[3])


class agent():
    
    def utter(self, attraction, transmission_error):
        
        ps=self.freqs/sum(self.freqs)
        label=np.random.choice(self.labels,1,p=ps)[0]
        
        x_val=np.random.normal(self.xmeans[label],self.xsds[label]*(1-attraction)+transmission_error)
        if x_val<0:
            x_val=0
        elif x_val>100:
            x_val=100
        
        y_val=np.random.normal(self.ymeans[label],self.ysds[label]*(1-attraction)+transmission_error)
        if y_val<0:
            y_val=0
        elif y_val>100:
            y_val=100
        utterance=[x_val,y_val]     

        return utterance

def sample(ag_set,info):
    samples=500
    count=0
    df_active_categories=[]
    df_signal_samples=[]

    for agent in range(len(ag_set)): 
        K=info[3]
        attraction=info[4]
        transmission_error=info[5]
        active_categories=len([n for n in ag_set[agent].freqs if n > 1/K])
        entropy=sum((ag_set[agent].freqs[ag_set[agent].freqs > 0])*np.log2(1/(ag_set[agent].freqs[ag_set[agent].freqs > 0])))
        weighted_sd=(sum(ag_set[agent].xsds*ag_set[agent].freqs)+sum(ag_set[agent].ysds*ag_set[agent].freqs))/2
        age=ag_set[agent].age
        df_active_categories.append(info + [agent, active_categories, entropy, weighted_sd, age])
        for n in range(samples):
            utterance=ag_set[agent].utter(attraction, transmission_error)
            df_signal_samples.append(info + [agent,age] + utterance)

    df_signal_samples=pd.DataFrame(df_signal_samples,columns=["POP","lifespan","sd_init","K","attraction","error","crit","run","iteration","agent","age","x","y"])
    df_active_categories=pd.DataFrame(df_active_categories, columns=["POP","lifespan","sd_init","K","attraction","error","crit","run","iteration","agent", "active_categories","entropy","weighted_sd","age"])

    return df_signal_samples, df_active_categories

def spec_clustering_func(X, k):
    
    gmm = GaussianMixture(n_components=k).fit(X)
    
    # Return the location of each cluster center,
    # and the labels for each point.
    return gmm.means_, gmm.predict(X)

def get_agent_vocab_entropy(paramset):
    return np.unique(df_active_categories[(df_active_categories.POP == paramset[0]) & (df_active_categories.lifespan == paramset[3]) & (df_active_categories.sd_init == paramset[4]) & (df_active_categories.K == paramset[5]) & (df_active_categories.attraction == paramset[6]) & (df_active_categories.error == paramset[7]) & (df_active_categories.crit == paramset[9]) & (df_active_categories.run == paramset[11]) & (df_active_categories.iteration == paramset[12]) & (df_active_categories.agent == paramset[13])].entropy)

def get_2d_hist(cur_data):
    hist, xedges, yedges = np.histogram2d(cur_data.x, cur_data.y, bins=[20,20], range=[[0, 100], [0, 100]], normed=None, weights=None, density=None)
    return (hist/np.sum(hist))
    
def get_entropy(ps):
    return sum(ps[ps > 0]*np.log2(1/ps[ps > 0]))

def hist_to_sig(hist):
    """Convert a 2D array to a signature for cv2.EMD"""
    
    # cv2.EMD requires single-precision, floating-point input
    sig = np.empty((hist.size, 3), dtype=np.float32)
    count = 0
    for i in range(hist.shape[0]):
        for j in range(hist.shape[1]):
            sig[count] = np.array([hist[i,j], i, j])
            count += 1
    return sig

def get_emd(hist1,hist2):

    sig1 = hist_to_sig(hist1)
    sig2 = hist_to_sig(hist2)    

    #dist, _, flow 
    return EMD(sig1, sig2, DIST_L2)

def process_condition(p,d,s,k,a,e,c,net,run):
    if net == -1:
        load_path=outdir+'crit_' + str(c)+ '/pop_' + str(p) + '/doff_' + str(d) + '/sdinit_' + str(s) +'/K_' + str(k) + '/attraction_' + str(a) + '/transmission_error_' + str(e) + '/run_' + str(run) + '/'
    
    if net == 0:
        load_path = outdir + 'real_world_net/run_' + str(run) + '/'

    if net == 1:
        load_path = outdir + 'small_world_net/run_' + str(run) + '/'
       
    if net == 2:
        load_path = outdir + 'connected_caveman_graph/run_' + str(run) + '/'

    if not os.path.isfile(load_path + '/pop_num_clusts.csv'):
        pop_wass_scores=pd.DataFrame()
        pop_signal_entropys=pd.DataFrame()
        pop_num_clusts=pd.DataFrame()
        agent_vocab_entropys=pd.DataFrame()
        agent_wass_scores=pd.DataFrame()
        agent_wass_scores_initial=pd.DataFrame()
        df_active_categories=pd.DataFrame()
        optimalK = OptimalK()
        #optimalK = OptimalK(n_jobs=4, parallel_backend='joblib')
        if [p,d,s,k,a,e,c,net] == [50,10000,5,20,1,0,40000,-1] :
            start=0
        else:
            start=20000
        for i in range(start, 40001, 1000):
            cur_path = load_path + 'agent_set_' + str(i) + '.npy'
            loaded_agents=np.load(cur_path, allow_pickle=True).tolist()
            info = [p,d,s,k,a,e,c,run,i]
            pop_data, df_active_categories_tmp = sample(loaded_agents,info)
            df_active_categories=df_active_categories.append(df_active_categories_tmp)
        
            pop_hist = get_2d_hist(pop_data)
            pop_signal_entropy = get_entropy(pop_hist)
            pop_signal_entropys=pop_signal_entropys.append(pd.DataFrame([info+[pop_signal_entropy]],columns = ["POP","lifespan","sd_init","K","attraction","error","crit","run","iteration","entropy"]))
        
            if i > 0:
                pop_emd, _, flow = get_emd(pop_hist, prev_hist)
                pop_wass_scores=pop_wass_scores.append(pd.DataFrame([info+[pop_emd]],columns = ["POP","lifespan","sd_init","K","attraction","error","crit","run","iteration","wass"]))
        
        
            prev_hist = pop_hist
        
            num_clusts = optimalK(pop_data[["x","y"]], n_refs=5, cluster_array=np.arange(2, 50))
            model = KMeans(num_clusts)
            model.fit(pop_data[["x","y"]])
            counts=np.unique(model.labels_,return_counts=True)
            K_freqs=counts[1]/sum(counts[1])
            K_ent=get_entropy(K_freqs)
            labels = model.labels_
            K_CH=metrics.calinski_harabasz_score(pop_data[["x","y"]],labels)
            K_SC=metrics.silhouette_score(pop_data[["x","y"]], labels, metric='euclidean')
            pop_num_clusts = pop_num_clusts.append(pd.DataFrame([info+[num_clusts]+[K_ent]+[K_freqs]+[K_CH]+[K_SC]],columns = ["POP","lifespan","sd_init","K","attraction","error","crit","run","iteration","num_categories","K_ent","K_freqs","K_CH","K_SC"])) 
                    
            for ag in np.unique(pop_data.agent):
                agent_data = pop_data[pop_data["agent"] == ag]
                age = agent_data.iloc[0,10]
                agent_hist = get_2d_hist(agent_data)
                                            
                pop_data2 = pop_data[pop_data["agent"] != ag]
                pop_hist2 = get_2d_hist(pop_data2)
                
                agent_emd, _, flow = get_emd(agent_hist, pop_hist2)
                agent_wass_scores = agent_wass_scores.append(pd.DataFrame([info+[ag,age,agent_emd]],columns = ["POP","lifespan","sd_init","K","attraction","error","crit","run","iteration","agent","age","wass"]))
                
        df_active_categories.to_csv(load_path + '/df_active_categories.csv')
        agent_wass_scores.to_csv(load_path + '/agent_wass_scores.csv')
        agent_vocab_entropys.to_csv(load_path + '/agent_vocab_entropys.csv')
        pop_signal_entropys.to_csv(load_path + '/pop_signal_entropys.csv')
        pop_wass_scores.to_csv(load_path + '/pop_wass_scores.csv')
        pop_num_clusts.to_csv(load_path + '/pop_num_clusts.csv')

def process_paramlist(condition_number):
    p,d,s,k,a,e,c,net,run = paramlist[condition_number]
    np.random.seed(condition_number)
    process_condition(p,d,s,k,a,e,c,net,run)
    print("finished")


#takes about 20min per run
#17 conditions * 100 runs 
#on 800 cores (max for std.q): 1700/800 * 20 = ~45min
#on 300 cores (max for fast.q): 1700/200 * 20 = ~3hrs
outdir=  cwd + '/data/'

# parameter setting
paramlist=[
    
    #popsize,deathrate,sd_init,K,attraction,error,critical period length, net type
    #p,d,s,k,a,e,c
   # [50,10000,5,20,1,0,40000,-1], #base
   # [50,10000,5,30,1,0,40000,-1], #increase K
   # [50,10000,5,10,1,0,40000,-1], #decrea
   # [50,10000,5,20,0,0,40000,-1], #decrease attraction
   # [50,10000,5,20,.25,0,40000,-1], #decrease attraction
   # [50,10000,5,20,.75,0,40000,-1], #decrease attrac
   # [50,10000,5,20,1,3,40000,-1], #increase error
   # [50,10000,5,20,1,10,40000,-1], #increase e
   # [50,15000,5,20,1,0,40000,-1], #increase lifespan
   # [50,5000,5,20,1,0,40000,-1], #decrease life
   # [50,10000,5,20,1,0,5000,-1], #apply crit
   # [50,10000,5,20,1,0,2500,-1], #apply lower 
   # [10,10000,5,20,1,0,40000,-1], #decrease pop
   # [25,10000,5,20,1,0,40000,-1], #decrease pop
   # [100,10000,5,20,1,0,40000,-1], #increase pop
    
   # [200,10000,5,20,1,0,40000,-1],
   # [50,10000,5,20,1,0,10000,-1],
   # [50,10000,5,20,1,0,20000,-1],

   # [50,10000,10,20,1,0,40000,-1], #increase sd
   # [50,10000,1,20,1,0,40000,-1], #decrease sd
   [50,10000,5,20,1,0,40000,0], #real world net
   [50,10000,5,20,1,0,40000,1], #small world net
   [50,10000,5,20,1,0,40000,2], #connected caveman

]
paramlist = [paramlist[job]]
paramlist = [item + [i] for item in paramlist for i in range(100)]

#job=0
paramlist=paramlist[0+20*batch:20+20*batch]

pool = mp.Pool(cpus)

pool.map(process_paramlist, range(len(paramlist)))
pool.close()
pool.join()

