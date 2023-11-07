import numpy as np
import pymc3 as pm
import matplotlib.pyplot as plt
import arviz as az

data = np.array([1,1,1,1,0,1,0,0,1,1])

with pm.Model() as coin_model:
    p = pm.Uniform("p", 0, 1)
    likelihood = pm.Bernoulli("likelihood", p, observed=data)
    trace = pm.sample(1000, tune = 1000)

pm.summary(trace)
az.plot_trace(trace, combined = True)