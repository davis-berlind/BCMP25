# Code and Data for Berlind, Cappello, and Madrid Padilla (2025)

This repository contains all necessary code and data to reproduce the simulation studies and applied examples in:

* Berlind, D., Cappello, L. and Madrid Padilla, O.H., (2025). "A Bayesian framework for change-point detection with uncertainty quantification." *arXiv preprint [arXiv:2507.01558](https://arxiv.org/abs/2507.01558)*.

## Simulations

The R script [simulation_functions.R](simulations/simulation_functions.R) implements the data generating processes of Simulations 1, 2, and 3 in Berlind, Cappello, and Madrid Padilla (2025), as well as functions for calculating the Hausdorff distance between any collection of estimated and true change-points and the false positive/negative sensitive location errors (see Appendix D of Berlind, Cappello, and Madrid Padilla (2025)). [Simulations](simulations) also contains the folders [meanvar_sim](simulations/meanvar_sim), [heavy_tailed_dependent_sim](simulations/heavy_tailed_dependent_sim), and [multi_sim](simulations/multi_sim), each corresponding to Simulations 1, 2, and 3. Each folder contains:
  1. A .R simulation script that generates a single replicate of the simulation and fits each model included in the study.
  2. A .sh bash script that submits each simulation script to a Univa Grid Engine type job scheduler (we ran our experiments on the [Hoffman2 Cluster](https://www.hoffman2.idre.ucla.edu/About/Mission-and-governance.html) at UCLA).
  3. .R scripts that use the simulation results to generate the plots and tables in the paper.

## Sensitivity Analysis 

The folder [sensitivity_analysis](sensitivity_analysis) contains .R scripts for performing the sensitiviy analysis in Appendix D.7 of Berlind, Cappello, and Madrid Padilla (2025). [delta_sensitivity.R](sensitivity_analysis/delta_sensitivity.R) evaluates the effect of increasing $\delta$ in the detection rule, [prior_sensitivity.R](sensitivity_analysis/prior_sensitivity.R) evaluates the effect of increasing the prior parameters $\omega_0, u_0, v_0$, and [tol_sensitivity.R](sensitivity_analysis/tol_sensitivity.R) evaluates the effect of increasing $\epsilon$ in the convergence criterion. 

## Ion Channel Data
The folder [ion_channel](ion_channel) contains the porin voltage data [ICdata.csv](ion_channel/ICdata.csv) originally collected by the Steinem lab (Institute of Organic and Biomolecular Chemistry, University of Göttingen), and an .R script that fits the models and generates the plots in Section 5.1 of Berlind, Cappello, and Madrid Padilla (2025). 

## Oil Well Data
The folder [well_log](well_log) contains the Shankle Well Log data [facies_data.csv](well_log/facies_data.csv) originally collected by the Kansas Geological Surcey (Bohling, G. & Dubois, M. 2003) and an .R script that fits the models and generates the plots in Section 5.2 of Berlind, Cappello, and Madrid Padilla (2025). 
