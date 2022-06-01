# MS: A savanna puzzle: Elevation drives regionalization patterns of endemic terrestrial vertebrates in the Cerrado

**Researchers involved are: João Paulo dos Santos Vieira-Alencar, Ana Paula Carmignotto, Ricardo J. Sawaya, Luís Fábio Silveira, Paula Hanna Valdujo & Cristiano de Campos Nogueira**

Herein we include R scrips, raw-data and outputs of the paper. This repository was created purely to promote full reproductibility in an easy and transparent way. We also hope that our scripts might stimulate and facilitate further analyses with a similar framework.

To access the `mapar` package mentioned below please visit: [![DOI](https://zenodo.org/badge/316021065.svg)](https://zenodo.org/badge/latestdoi/316021065). Or access its [github](https://github.com/joao-svalencar/mapar) repository.

If you have any doubt, please contact JP Vieira-Alencar at: joaopaulo.valencar@gmail.com

## The repository is organized as follow:
  - R: 
    - C_tets.R: loading objects;
    - D_BioticElements.R: Biotic Elements Analyses. With tests for the first and second predictions of the vicariance model;
    - D_clustering.R: Using the `cdn` function from package `mapar` to choose clustering parameters;
    - D_tets.R: Exploring basic data and modeling elevation~BEs;
    - S_boxplot.R: Organizing data and creating Fig. 3 (elevation boxplot);
    - S_maps.R: Using the `mapar` function from package `mapar` to preview BEs outputs and export .shp files;
    - OBS: Start with script C_tets.R. Comments in the scripts indicates which script to open next;

  - data:
  - outputs:


