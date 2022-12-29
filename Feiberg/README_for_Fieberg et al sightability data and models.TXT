# Files are associated with the following paper published in Methods in Ecology and Evolution:


Fieberg, J., Alexander, M., Tse, S,, and K. St. Clair. 2013. Abundance estimation with sightability data: a Bayesian data augmentation approach.  Methods in Ecology and Evolution.




*************************************************
MODELS for JAGS
*************************************************
1.  beta_binom_model.txt = JAGS model for fitting beta binomial model
2.  logistic_t_model.txt = JAGS mdoel for fitting logistic-t model


NOTES:  contact jfieberg@umn.edu for further details or questions regarding the data or programs.  Alternatively, contact john.giudice@state.mn.us for information regarding the moose survey.  



*************************************************
DATA FILES:
*************************************************

------------------------------------------------------
------------------------------------------------------
1.  sight_dat.csv = Sightability survey data:  124 records

  x = visual obstrcution measurements associated with the test trial data used to develop the     sightability model

  z = detection indicator (1 if the group was observed, 0 otherwise)


------------------------------------------------------
------------------------------------------------------
2.  oper_dat.csv:  Operational survey data:  4380 records (includes observed and augmented data for the annual surveys in  2006 and 2007 combined).  Augmented data records have NA (missing) for x, y, q.

  x = visual obstruction measurements

  ym1 = y-1, where y = observed group size

  h = stratum identifier (1, 2, 3 correspond to low, medium and high density strata)

  q = indicator variable that represents whether the group belongs to the study population (equal to 1 for all observed groups and NA for all augmented groups).

  z = detection indicator (equal to 1 if the group was observed during the operational survey and 0 otherwise)

  subunits = unique plot identifier (for all sampled plots).

  yr = year of observation (1 = 2006, 2= 2007)

------------------------------------------------------
------------------------------------------------------


------------------------------------------------------
------------------------------------------------------
3.  plot_dat.csv:  Plot-level information data: 77 records (one for each of the plots sampled in either 2006 or 2007)

  h.plots = stratum to which the plot belonged (1, 2, 3 correspond to low, medium and high density strata)

  yr.plots = year the plot was sampled (1 = 2006, 2= 2007)


------------------------------------------------------
------------------------------------------------------


------------------------------------------------------
------------------------------------------------------
4.  scalar_dat.csv:  Scalars:
  R = number of sightability trials (124)

  Ngroups = number of observed and augmented groups for the 2006 and 2007 annual operational surveys (4380)

  Nsubunits.yr = total number of plots sampled in 2006 and 2007 combined = 77 

  ny1 = number of groups associated with the annual survey in 2006 (year 1) = 2060





