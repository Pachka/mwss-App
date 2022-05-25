--- Application under development ---

## mwss-App: an R-Shiny application to run stochastic simulation of infectious diseases spreading in healthcare systems structured as networked metapopulations

<font size="-2">
   Hammami Pachka<sup>1,2,3,*</sup>, Oodally Ajmal<sup>1,2,3,*</sup>, Reilhac Astrid<sup>4</sup>, Guérineau de Lamérie Guillaume<sup>4</sup>,  Widgren Stefan <sup>5</sup>,  Temime Laura<sup>3,6,¤</sup> and  Opatowski Lulla<sup>1,2,¤</sup></font>

<br>
<sup>1</sup> Anti-infective evasion and pharmacoepidemiology team, Université Paris-Saclay, UVSQ, Inserm, CESP,  Montigny-Le-Bretonneux, France

<sup>2</sup> Epidemiology and Modelling of Antibiotic Evasion (EMAE), Institut Pasteur, Paris, France

<sup>3</sup> Laboratoire de Modélisation,  épidémiologie et surveillance des risques sanitaires (MESuRS), Conservatoire national des arts et métiers, Paris, France

<sup>4</sup> Département d'information médicale, Centre hospitalier Guillaume Régnier, Rennes, France

<sup>5</sup> Department of Disease Control and Epidemiology, National Veterinary Institute, Uppsala, Sweden

<sup>6</sup> PACRI unit, Institut Pasteur, Conservatoire national des arts et métiers, Paris, France
<sup>7</sup>MRC Centre for Global Infectious Disease Analysis, Department of Infectious Disease Epidemiology, Imperial College London, United Kingdom

<sup>*</sup>These authors contributed equally

<sup>¤</sup>These authors contributed equally

</br>

Corresponding author: Hammami Pachka (pachka@hotmail.fr)

<!-- 
## Preprint
Preprint available at: <a href="" target="_blank"> doi: </a> 
-->

## Run mwss-App from your R console or RStudio with one command
Open your R console or RStudio and paste the commands provided below. 
mwss-App will automatically install all required dependencies (R packages).
````
library("shiny")
runGitHub("MESuRS-Lab/mwss-App")
````
The main package used is mwss available in our GitHub page: https://github.com/MESuRS-Lab/mwss

Required Shiny version >= 1.7.1 
### Dependencies
This version of mwss-App was developed on R version 4.1.3 (2022-03-10) with Windows 10 x64 (build 18363) using the following version of dependencies:
````
mwss-App
├── shiny_1.7.1
├── shinyalert_3.0.0
├── shinydashboard_0.7.2 
├── shinyhelper_0.3.2
├── shinyjs_2.1.0
├── shinyTime_1.0.1
├── shinyWidgets_0.7.0
├── DT_0.23
├── data.table_1.14.2
├── SimInf_9.0.0
├── statnet_2019.6
├── igraph_1.3.1
├── network_1.17.2
├── plotly_4.10.0
├── magrittr_2.0.3
├── dplyr_1.0.9
├── knitr_1.39
├── devtools_2.4.3
├── ggplot2_3.3.6
````
The package mwss developed simultaneously should always be up to date with the online version.

````
library(devtools)
install_github("MESuRS-Lab/mwss")
````

## Main contents

This repository contains the source code for the "mwss-App" RShiny application developed using R-programming language.
The RShiny application provides a comprehensive, user-friendly interface to run complex stochastic simulations for the nosocomial spread of Covid-19 in multi-service healthcare systems.

````
mwss-App
├── body
├── data
├── functions
├── header
├── helpfiles
├── www
├── report.Rmd
├── app.R
├── global.R
├── server.R
├── ui.R

````

- **body**
<br>  This folder contains the main ui files shaping the different panels.

- **data**
<br> This folder contains the toydataset and parameters for different Covid-19 variants.

- **functions**
<br> This folder contains the modules.

- **header**
<br> This folder contains the ui file designing the header.

- **helpfiles**
<br> This folder contains the markdown content of help notes.

- **www**
<br> This folder contains the images displayed in the application.

- ** report.Rmd **
<br> This RMarkdown file contains the structure of the report that can be downloaded after runing simulations.

- **R files**
 - app.R
 - global.R
 - server.R
 - ui.R
Those files are baseline files for Rshiny application (read more on: https://shiny.rstudio.com/articles/scoping.html)
