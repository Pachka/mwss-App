# Load and if necessary install packages
#list of packages required
list.of.packages <- c("shiny", "dplyr", "DT",
                      "ggplot2", "statnet", "igraph",
                      "network", "shinydashboard", "shinyjs",
                      "plotly", "magrittr", "SimInf", "data.table", "shinyWidgets",
                      "shinyhelper", "shinyTime", "shinyalert", "knitr", "devtools")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

sapply(list.of.packages, function(pck){
  require(pck, character.only = TRUE)
  })

outdatedpck <- old.packages()
if(TRUE %in% (list.of.packages %in% outdatedpck))
  warning(paste("You might need to update the following packages:", 
                paste(list.of.packages[which(list.of.packages %in% outdatedpck)], collapse = ", ")))

install_github("MESuRS-Lab/mwss", quiet = T)
library("mwss")

# Parameters dataset
source('data/O1.R', local = TRUE)
source('data/O2.R', local = TRUE)

#### Source function
source('functions/buttonsUI.R', local = TRUE)

source('functions/wardEditModule.R', local = TRUE)
source('functions/wardRemoveModule.R', local = TRUE)
source('functions/wardAdd.R', local = TRUE)
source('functions/contactEdit.R', local = TRUE)
source('functions/updateParamsModule.R', local = TRUE)
source('functions/downloadParamsModule.R', local = TRUE)
source('functions/setIMMstateModule.R', local = TRUE)
source('functions/diseasetimelineModule.R', local = TRUE)
source('functions/valueboxoutputModule.R', local = TRUE)
source('functions/plotsoutputModule.R', local = TRUE)
source('functions/resetreactivesModule.R', local = TRUE)
source('functions/synthreportModule.R', local = TRUE)
source('functions/exporttrajModule.R', local = TRUE)
source('functions/loadTestdtModule.R', local = TRUE)



# App Structure function

source('body/presentation.R', local = TRUE)
source('body/structure/tabItemStructure.R', local = TRUE)
source('body/parameters/tabItemParams.R', local = TRUE)
source('body/simulations/tabItemSim.R', local = TRUE)
source('body/about.R', local = TRUE)

source('header/header_ui.R', local = TRUE)
source('body/sidebar_ui.R', local = TRUE)
source('body/body_ui.R', local = TRUE)

