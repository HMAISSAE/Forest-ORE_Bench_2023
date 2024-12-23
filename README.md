---
title: "Forest-ORE Repository"
author: "MAISSAE HADDOUCHI"
date: "04/11/2021"
output: html_document
---

This repository contains the implementation of the Forest-ORE framework, including the code, data files, and results for the experiments reported in the article “Forest-ORE: Mining Optimal Rule Ensemble to Interpret Random Forest Models.” The implementation and computational work were carried out using R, Python, and Gurobi Optimizer Software (under a free academic license). All datasets used are publicly available from the UCI Machine Learning repository and the KEEL dataset repository. The content of this repository is organized as follows:
1.  R file “Functions_Bench_Forest-ORE…R”: includes all the R functions used in this study  
2.  Python file “Find_OPT_Rules…Py”: contains all the Python functions used in this study      
3.  Repository named “data”: contains the data used in the experiments  
4.  Repository named “data_summary”: contains a summary for each data used in the experiments  
5.  Repository named “R_code”: includes an R code file for each data set experimentation  
6.  Repositories named “Rules_results”, “Rules_results_add”, and “Rules_results_abl”: contain all the output files resulting from running the files contained in the “R_code” and “R_code_other_classif” repositories   
7.  Repository named “benchmark_template”: includes the code files used to generate the experimental results (tables and plots) reported in the paper.    
8.  Repository named “benchmark_results”: contains all the output files resulting from running the files contained in the “benchmark_template” repository
9.  PDF file named “ForestORE_Supplementary materials”: includes methodological details, extensive results, and additional tables related to the paper.    

NB: To run the different files:  
1.  Install and configure Python with R. The following links explain how to install, configure, call Python with RStudio :   https://support.rstudio.com/hc/en-us/articles/360023654474-Installing-and-Configuring-Python-with-RStudio, https://cran.r-project.org/web/packages/reticulate/vignettes/calling_python.html  
2.  Install Gurobi and request a license. The following link explains how to request and use a Free Academic Licence: https://www.gurobi.com/academia/academic-program-and-licenses/. The following link explain how to set environment variables https://www.gurobi.com/documentation/9.1/quickstart_windows/setting_environment_variab.html#subsection:setenvvars  
3.  Create the necessary repositories in order to save the different .csv resulting files (paths used after each “write.csv” command).

The versions of R, Python, Gurobi used during this study are:  
- R version 4.2.2  
- Python version 3.9  
- Gurobi version 9.5.2 Academic free licence  

