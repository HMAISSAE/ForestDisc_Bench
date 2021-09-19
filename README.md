# ForestDisc_Benchmark

This repository contains the code, the data files, and the resulting files to the benchmark reported in the the article “A novel approach for discretizing continuous attributes based on tree ensemble and moment matching optimization”.This empirical study involves 50 benchmark datasets and six major classification algorithms to compare the performance of ForestDisc against 20 major discretizers. All data used are publicly available in UCI Machine Learning repository and Keel data sets repository. The content of this repository is as follows:
1. A repository for each dataset: It includes an “input” repository (input files) and the output files resulting from running the dataset input files. The input repository contains: 1- the dataset file(s), 2- Two files containing functions codes (of the format:  “Discretization...R”, “dprep...R”) that should be run at first, and the files containing the code for processing the dataset.  
2.	A repository named “All_files”: It includes “B_perf_all” repository ,  “B_datasets” repository, and the output files resulting from running the benchmarking. “B_perf_all” contains all the   .csv files resulting from processing the  50  datasets using 10-fold Monte Carlo cross validation procedure. “B_datasets” contains the description of the datasets used.
3.	A repository named “benchmark_template” includes the code used for generating the average results and plots reported in the paper.  

NB: 
In order to run the different files, the working directory should be renamed to the user working directory( in the different code files). In addition, the necessary repositories should be created (or renamed based on the user needs) in order to save the different .csv resulting files (paths used after each “write.csv” command).
