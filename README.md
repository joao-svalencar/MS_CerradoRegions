# MS: In search of generality: revised distribution data and regionalization of Cerrado endemic tetrapods

**Researchers involved are: João Paulo dos Santos Vieira-Alencar, Ana Paula Carmignotto, Ricardo J. Sawaya, Luís Fábio Silveira, Paula Hanna Valdujo & Cristiano de Campos Nogueira**

Herein we include R scrips, raw-data and outputs of the paper. This repository was created purely to promote full reproductibility in an easy and transparent way. We also hope that our scripts might stimulate and facilitate further analyses with a similar framework.

To access the `mapar` package mentioned below please visit: [![DOI](https://zenodo.org/badge/316021065.svg)](https://zenodo.org/badge/latestdoi/316021065). Or access its [github](https://github.com/joao-svalencar/mapar) repository.

If you have any doubt, please contact JP Vieira-Alencar at: joaopaulo.valencar@gmail.com

## Analyses reproducibility

This repository incorporates a package version management system facilitated by the `renv` package, ensuring the proper functionality of all functions utilized in the scripts. To leverage this functionality, execute the following code after initializing the Rproj file:

````
#Install the "renv" package
install.packages("renv")

#Restore versions of the utilized packages
renv::restore()
````

**Important Note:!**

The provided code will automatically install the required packages, aligning them with the same versions used during the preparation of the scripts. This ensures the reproducibility of the analyses.

## The repository is organized as follow:
  - MS_CerradoRegions.Rproj: R project for the reproductibility of the analyses;
  - R: 
    - C_tets.R: loading objects;
    - D_BioticElements.R: Biotic Elements Analyses. With tests for the first and second predictions of the vicariance model;
    - D_clustering.R: Using the `cdn` function from package `mapar` to choose clustering parameters;
    - D_tets.R: Exploring basic data and modeling elevation~BEs;
    - S_boxplot.R: Organizing data and creating Fig. 3 (elevation boxplot);
    - S_maps.R: Using the `mapar` function from package `mapar` to preview BEs outputs and export .shp files;
    - OBS: Start with script C_tets.R. Comments in the scripts indicates which script to open next;
  - data:
    - coords.csv: file containing each grid cell centroids coordinates;
    - elevation_full.csv: file containing each species elevational records (including multiple records in the same location);
    - elevation_unique.csv: file containing each species unique elevational records;
    - list.csv: file containing species auxiliary data;
    - sppsample.csv: file containing the information of the intersection of species records and the grid system. Used to create the presence x absence matrix used in the BE analysis (Also available as Supporting Information - S4);
    - shapes:
      - Cerrado.shp (and auxiliary files): shapefile delimiting the Cerrado ecorregion;
      - grid_tet.shp (and auxiliary files): shapefile of the grid system used in the analysis (Also available as Supporting Information - S3);
  - outputs:
    - figures:
      - Fig 1.png (created with QGIS);
      - Fig 2.png (created with QGIS);
      - Fig 3.png (created within scripts);
      - n2_cd015.pdf: file containing a summary preview of all BEs detected (created within scripts);
    - shapes:
      - Area BE "x".shp (and auxiliary files): shapefiles delimiting each BE individually (created within scripts);
      - map.areas.shp (and auxiliary files): Unique shapefile delimiting all BEs (created within scripts);
    - tables:
      - BEs.csv: file containing the output of the clustering analysis (created within scripts);
      - cdn.csv: file containing the output of the `cdn` function (created within scripts);
      - summary.csv: file containing summarized BEs information (created manually ad hoc);
      - SuppInfo_S4_mpa.csv: file containing the presence x absence matrix (created within scripts);
    - tests:
      - chisq_class_nc.txt: chi-square summary for the test of the aggregation of vertebrate classes in the noise component;
      - chisq_class_wnBE.txt: chi-square summary for the test of the aggregation of vertebrate classes in the detected BEs (antagonic to the later);
      - chisq_elev_noNoise.txt: chi-square summary for the test of the aggregation of vertebrate classes in the two elevation categories (full dataset, created within scripts);
      - chisq_elev_restricted.txt: chi-square summary for the test of the aggregation of vertebrate classes in the two elevation categories (only restricted BEs dataset, created within scripts);
      - chisq_tet_genus.txt: chi-square summary for the test of the second prediction of the vicariance model;
      - prabtest.rds: .rds file storing the result of the test (`prabtest`) of the first prediction of the vicariance model;
      - prabtest.txt: summary of the test (`prabtest`) of the first prediction of the vicariance model;