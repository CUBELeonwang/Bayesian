#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr  5 17:57:42 2021

@author: danlin
"""

from scipy import stats
import arviz as az
import numpy as np
import matplotlib.pyplot as plt
import pymc3 as pm
import seaborn as sns
import pandas as pd
from theano import shared
from sklearn import preprocessing

print('Running on PyMC3 v{}'.format(pm.__version__))



import pandas as pd
import numpy as np
from scipy import stats
import arviz as az
import numpy as np
import matplotlib.pyplot as plt
import pymc3 as pm
import seaborn as sns
import pandas as pd
from theano import shared

fname = 'ESC_3days'
sname = "sep21"

df = pd.read_excel(fname+'.xlsx', sheet_name=sname)

rl = 1

rownum = len(df)

df = df.head(round(len(df)*2/3))
df = df.iloc[1: , :].iloc[::rl, :] # keep the first value, than sample every rl 


az.plot_kde(df['indoor co2 level'].values, rug=True)
#plt.yticks([0], alpha=0);

rownum = len(df)
po = 101325
delt = 5*60  # convert 10min to 600s
v = 230.51 # room volume

obs = np.asarray(df[['indoor co2 level']])[1: (rownum+1)] # indoor CO2 concentration
#N = np.asarray(df[cols[20]])[1: (rownum+1)] # occupancy number
Tin = np.asarray(df[['indoor temperature']])[1: (rownum+1)] + 273.15 # indoor air temperature
To = np.asarray(df[['Outdoor temperature']])[1: (rownum+1)] + 273.15 # outdoor air temperature
Po = np.ones(((rownum-1), 1))*po # outdoor pressure
Pin = Po # indoor pressure
time_mea = df[['time']][1: (rownum+1)]

secN = np.ones(((rownum-1), 1))*delt
C0_ppm = np.asarray(df[['indoor co2 level']])[0: (rownum-1)]# indoor CO2 level, initial value
V = np.ones(((rownum-1), 1))*v # room volumn
#C_o_ppm = np.asarray(df[cols[9]])[1:3] # outdoor CO2 level


with pm.Model() as model_g:
    
    E = pm.Uniform('E', lower=0.002, upper=0.01) # CO2 generation rate by one person (L/s)
    #E = 2.02
    Qo = pm.Uniform ('Qo', lower=0.01, upper=2) # outdoor air ventilation rate for natural 
    #Qo = pm.Uniform ('Qo', lower=1, upper=5) # outdoor air ventilation rate for mechnical
    #Qo = Qo_100 * op
    C_o_ppm =  pm.Uniform ('C_o_ppm', lower=396, upper=416) # outdoor co2 concentration (ppm)
    #C_o_ppm = 449

    N = pm.Uniform('N',lower=5, upper=25) # occupancy number
    sigma = pm.HalfNormal('sigma', sd=200)
    
    # ---------------------------indoor room condition----------------------------
    #C0_ppm <-  as_data(df$`Measurement (ppm)`[1:11]) #218.637 # initial co2 concentration (ppm)
    Mco2 =  44 # CO2 molar mass (g/mol)

    C0 =  C0_ppm * Mco2 / 22.4 * 273 / Tin * Pin / 100 / 1013 # convert initial co2 concentration from ppm to mg/m3
    
    # --------------------------outdoor air properties------------------------------
    C_oa =  C_o_ppm * Mco2 / 22.4 * 273 / To * Po/100 / 1013 #convert outdoor co2 concentration from ppm to mg/m3
    
    # ----------------------CO2 generation rate-----------------------
    Rco2 =  188.92 # CO2 specific heat capacity (J/kg.K)
    Din =  Pin / Rco2 / Tin # calculate indoor co2 density (kg/m3)
    E1 =  E * 1e-3*Din*1e6 # convert generation rate from L/s to mg/s: L/s *1e-3 m3/L * density kg/m3 * 1e6 mg/kg= mg/s
    S = E1 * N / V+ C_oa * Qo/3600#(E1 * N + C_oa *Qo) / V # total CO2 generation rate: summation of people and outdoor airflow  mg/sm3
    
    # ----------------------CO2 deposition rate and total loss rate-----------------------
    L = Qo/3600#(Qo) / V # calculate sum of loss rates due to all sinks (1⁄s)
    μ = (C0 * pm.math.exp(-L * secN) + S / L * (1 - pm.math.exp(-L * secN))) * 22.4 / Mco2 * Tin / 273 * 1013 / Pin*100
    
    y = pm.Normal('y', mu=μ, sd=sigma, observed=obs)
    trace_g = pm.sample(1000, tune=1000, cores=1, random_seed=10086)

pm.trace_to_dataframe(trace_g).to_csv("trace_"+sname+"_"+str(5*rl)+"min.csv")        
summary = az.summary(trace_g)
summary.to_csv(fname+"_summary"+sname+"_"+str(5*rl)+"min.csv")
print(summary)




