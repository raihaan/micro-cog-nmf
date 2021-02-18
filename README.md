# micro-cog-nmf
This repository contains analysis scripts for NMF and PLS processing of cortical microstructure-cognition relationships.
Scripts are a mix of bash/python/matlab

Workflow is as follows:

1. derivatives/extract_metrics - run extract*py and build*py scripts to extract .txt data to form .mat file per metric
                 and use build*py scripts to build nmf input


2. analysis

	2.1 nmf/stability - define splits to create input matrices for each split, using data from derivatives

	  - run nmf for each split

	  - compute stability for each split/granularity

  - plot results

  nmf - run k=10 nmf. results go in nmf/wb_res

  slope_modelling/ph5-11 (laptop) - slopes_time.age_ph5-11.R does lmer to get cog slopes and intercepts.

                                  - output is intercepts_slopes_398subj_ph5to11.csv

  pls_time.age_ph5-11/k10 - make pls sheets with make_pls_sheets.ipynb

  pls_time.age_ph5-11/k10/int-slope - run pls
