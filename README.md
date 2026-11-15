# Code and Data for Berlind, Cappello, and Madrid Padilla (2025)

This repository contains all necessary code and data to reproduce the simulation studies and applied examples in:

* Berlind, D., Cappello, L. and Madrid Padilla, O.H., (2025). "A Bayesian framework for change-point detection with uncertainty quantification." *arXiv preprint [arXiv:2507.01558](https://arxiv.org/abs/2507.01558)*.

The R package [mich](https://github.com/davis-berlind/mich) implements the Multiple Independent Change-Point (MICH) method described in the paper.

## Simulations

The R script [simulation_functions.R](simulations/simulation_functions.R) contains functions implementing Simulations 1 and 2 of Berlind, Cappello, and Madrid Padilla (2025). The folder [Simulations](simulations) contains the folders [meanvar_sim](simulations/meanvar_sim) and [multi_sim](simulations/multi_sim) corresponding to Simulations 1 and 2 in the paper. Each folder contains:  

1. A .R simulation script that generates a single replicate of the simulation and fits each model included in the study. 
2. A .sh bash script that submits each simulation script to a Univa Grid Engine type job scheduler (we ran our experiments on the [Hoffman2 Cluster](https://www.hoffman2.idre.ucla.edu/About/Mission-and-governance.html) at UCLA). 
3. .R scripts that use the simulation results to generate the plots and tables in the paper.

## Gibbs Sampler

The folder [gibbs_sim](simulations/gibbs_sim) contains .R and .cpp scripts implementing the Gibbs sampler for the mulitvariate MICH model and scripts for the simulation comparing the Gibbs and variational Bayes posteriors (see Appendix S. of Berlind, Cappello, and Madrid Padilla 2025).

## Sensitivity Analysis 

The folder [sensitivity_analysis](sensitivity_analysis) contains .R scripts for performing the sensitivity analysis in Appendix S.7 of Berlind, Cappello, and Madrid Padilla (2025). 

## Ion Channel Data

The folder [ion_channel](ion_channel) contains the porin voltage data [ICdata.csv](ion_channel/ICdata.csv) originally collected by the Steinem lab (Institute of Organic and Biomolecular Chemistry, University of Göttingen), and an .R script that fits the models and generates the plots in Section 6.1 of Berlind, Cappello, and Madrid Padilla (2025). 

## Oil Well Data

The folder [well_log](well_log) contains the Shankle Well Log data [facies_data.csv](well_log/facies_data.csv) originally collected by the Kansas Geological Surcey (Bohling, G. & Dubois, M. 2003) and an .R script that fits the models and generates the plots in Section 6.2 of Berlind, Cappello, and Madrid Padilla (2025). 
