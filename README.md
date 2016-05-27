Scripts
=============

**gen_threads.r**: 

Generates synthetic threads under one of the models (Gomez, Lumbreras...)

It generates a list of trees trees.Rda and a dataframe where, for each post, we have all the information needed to compute its likelihood.

**estimate_params_Gomez2013.r**:

Reproduces Section 3.1 of Gomez 2013 where they check that the estimates are not biased.

**em.r**

Runs the Expectation-Maximization algorithm to estimate the cluster assignments and the parameters $\alpha_k, \beta_k, \tau_k$ of each cluster.