---
title: "Forest-ORE Repository"
author: "MAISSAE HADDOUCHI"
date: "04/11/2021"
output: html_document
---

This repository contains the ForestORE framework implementation and the code, the data files, and the resulting files to the experiments reported in the article “Forest-ORE: Mining Optimal Rule
Ensemble to interpret Random Forest models”. The implementation and the computational work are done using the R language and environment for statistical computing, the Python programming language, and Gurobi Optimizer Software (with free academic licence). All data used are publicly available in UCI Machine Learning repository and Keel data sets repository. The content of this repository is as follows:  
1.  An R file “Functions_Bench_Forest-ORE…R”: includes all the R functions used in this study  
2.  A Python file “Find_OPT_Rules…Py”: contains all the python functions used in this study      
3.  A repository named “data”: contains the data used in the experiments  
4.  A repository named “data_summary”: contains a summary for each data used in the experiments  
5.  A repository named “R_code”: includes an R code file for each data set experimentation  
6.  Repositories named “Rules_results”, “Rules_results_add”, and “Rules_results_abl”: contain all the output files resulting from running the files contained in the “R_code” and “R_code_other_classif” repositories   
7.  A repository named “benchmark_template”: includes the code files used for generating the experimental results (tables and plots) reported in the paper.    
8.  A repository named “benchmark_results”: contains all the output files resulting from running the files contained in the “benchmark_template” repository   

NB: 
In order to run the different files:  
1.  Install and configure Python with R. The following links explain how to install, configure, call Python with RStudio :   https://support.rstudio.com/hc/en-us/articles/360023654474-Installing-and-Configuring-Python-with-RStudio, https://cran.r-project.org/web/packages/reticulate/vignettes/calling_python.html  
2.  Install Gurobi and request a license. The following link explains how to request and use a Free Academic Licence: https://www.gurobi.com/academia/academic-program-and-licenses/. The following link explain how to set environment variables https://www.gurobi.com/documentation/9.1/quickstart_windows/setting_environment_variab.html#subsection:setenvvars  
3.  Create the necessary repositories in order to save the different .csv resulting files (paths used after each “write.csv” command).

The versions of R, Python, Gurobi used during this study are:  
- R version 4.2.2  
- Python version 3.9  
- Gurobi version 9.5.2 Academic free licence  

