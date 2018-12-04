# disasters_analysis
Gathering, cleaning and modeling of EMDAT, IPEA and IBGE data regarding disasters and economic indicators
Bear in mind that this is still work in progress.

This is a project i've been developpng with support from the Federal University of Rio de Janeiro and the Civil Defense of Rio de Janeiro. We are trying to analyze data related to disasters impacts (both social and financial) and economic and social indicators.

The idea is to understand how those indicators shape the types of disasters and its impacts on different regions (state level). Do better sewerage and water services infrastructures minimize deaths related to landslides? Does having a greater GDP per capita reduces deaths related to natural disasters? Those are some kinds of questions that are in the scope of this project.

The analysis script is meant to do a cluster analysis of the disasters, in order to classify similar groups. That kind of knowledge will enable the government to develop specific public policies to each area, and have better results. Sadly, conclusions so far are that we don't have enough data. 

In the analysis i used some machine learning algorithms such as Principal Component Analysis (PCA) and Hierarchical Clustering. Please note that the code may be broken in your machine because of our file paths not being the same. I am not actively using GIT, this repository is just a way for me to record my code.

I didn't upload each series of indicators i used, since they are scattered along multiple csv files. That said, i uploaded the final dataset used to do the cluster analysis, which contains all the data from natural disasters in Brazil (Thank you EMDAT!) and all the indicators I acquired online searching on the SIDRA system from IBGE and the IPEADATA website already merged.
