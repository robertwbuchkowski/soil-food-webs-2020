# soil-food-webs
A repository of soil food web theoretical models that calculate the flow of carbon and nitrogen through a soil food web.

Authors: R. Buchkowski and Z Lindo

The repository contains a functions.R file that contains the functions for calculating the soil food web fluxes, stability, as well as functions for correcting stoichiometry and for combining trophic species. These are the main functions used throughout the other scripts.

The file starting_examples.R produces the results presented in Figures 1-3 of our manuscript. It uses functions from the functions.R script and outputs the figures and some additional data. Because many of the analyses can be time intensive, it automatically saves some results in a R data file.

The file newexample.R provides the code for analyzing how lumping together trophic species affects carbon and nitrogen mineralization across gradients of different parameters and food web properties. It uses functions from the functions.R script and outputs the figures and some additional data. Because many of the analyses can be time intensive, it automatically saves some results in a R data file. Using the runthesims = F allows you to avoid running the simulations after you have already generated the data. The data files used in our manuscript are archived on Figshare for datasets that take more than a few hours to create.

The file oribatidmite.R contains the code to recreate the oribatid mite example in Figure 7 of the manuscript. It uses functions from the functions.R script and outputs the graphs Figures 7, S6, and S7. The separate pieces of the figure presented together in the paper were combined outside of R.

The file prettyfoodweb.R provides the code for creating a visualization of each of the food webs using in the newexample.R script and presented in the second supplemental figure.
