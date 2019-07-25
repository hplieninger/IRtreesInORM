# Simulation Study
 
 - Data were generated using the file `01_generate-data.R`.
   Therein, the function `sim_tree_data()` is used.
 - A plot of the simulated data can be found in the folder `figures/`.
 - Subsequently, all three models in folder `mplus/` were fit using Mplus 7.4.
   - The data-generating model MCN.
   - The proper but misspecified model ECN (Böckenholt, 2012).
   - The improper MCN2 model (which is sometimes also referred to as the APP model, because LaHuis et al. used this term).
 - Model fit and parameter estimates were then analyzed using the script `03_summarize-results.R`.
   They are, for convenience, also saved in the file `sim-results-new.rda`.
 
 
