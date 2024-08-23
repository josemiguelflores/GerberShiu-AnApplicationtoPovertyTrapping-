################################################################################################################################################
###################### Article: The Gerber-Shiu Expected Discounted Penalty Function: An Application to Poverty Trapping #######################
######################################## Author: Flores-Contró, José M. ########################################################################
################################################################################################################################################

#########################
### Loading Packages ####
#########################

# We load the packages.

library(tidyverse)
library(DescTools)
library(ggsci)
library(tikzDevice)
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}", "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}")) # OK
library(actuar)

##########################
###### Data Import #######
##########################

# First, we set the working directory to the folder in which the "emc2014_welfare.csv" data file is saved. This data file contains consumption 
# data for households obtained from Burkina Faso's Enquête Multisectorielle Continue (EMC) 2014. 
# See, for example: https://microdata.worldbank.org/index.php/catalog/2538

# We copy the path of the location in which the data file is saved.

Path <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Burkina Faso"

# We set the Path's location as working directory and save it in the WorkingDirectory variable.

WorkingDirectory<- setwd(Path)

# We read the data file (the name of the file is "emc2014_welfare.csv").

emc2014_welfare <- as_tibble(read.csv("emc2014_welfare.csv"))

########################################
########### Data Preparation ########### 
########################################

# We calculate the "Dépense totale du ménage, nominale per capita" and assign it to a new variable: pcdeptotnd. We do this by dividing the 
# "Dépense totale du ménage, nominale" (deptotnd) by the size of the household (hhsize), i.e. pcdeptotnd = deptotnd/hhsize.

emc2014_welfare$pcdeptotnd <- emc2014_welfare$deptotnd/emc2014_welfare$hhsize

# We check the top records for the new calculated variable: pcdeptotnd.

head(emc2014_welfare$pcdeptotnd)

# Now, we create a "flag" variable that will tell us if a household is poor or not. We are going to call this variable: flgdeptotndpoor and if 
# flgdeptotndpoor = 1 then the household is poor, whereas if flgdeptotndpoor = 0 means the household is not poor. Note that, we do this by 
# comparing the expenditure per capita (pcexp) with the poverty line of 153 530 FCFA per capita (appelé seuil absolu de pauvreté monétaire, a 
# été estimé à 153 530 FCFA par tête et au prix courant de Ouagadougou - see, for example: Rapport Enquête multisectorielle continue (EMC) 2014
# Profil de pauvreté et d’inégalités ). If the expenditure per capita (pcexp) is less than zref = 153 530 FCFA, then the household is poor, 
# otherwise, the household is not poor.

emc2014_welfare <- emc2014_welfare %>% mutate(flgdeptotndpoor = ifelse(pcexp <= zref, 1, 0))

# We check the top records for the new calculated variable: flgdeptotndpoor.

head(emc2014_welfare$flgdeptotndpoor)

# We now calculate the number of poor per household. We do this by multiplying the number of members in the household (hhsize) by the "flag" 
# variable previously calculated. We will call this new variable: numdeptotndpoor.

emc2014_welfare$numdeptotndpoor <- emc2014_welfare$flgdeptotndpoor * emc2014_welfare$hhsize

# We check the top records for the new calculated variable: numdeptotndpoor.

head(emc2014_welfare$numdeptotndpoor)

# For those who are poor, we calculate the poverty gap (or income deficit), which is defined as the poverty line less expenditure per capita. We 
# store it in the variable: pcgapdeptotndpoor.

PovertyLine <- 153530

emc2014_welfare <- emc2014_welfare %>% mutate(pcgapdeptotndpoor = ifelse(flgdeptotndpoor == 1, zref - pcexp, NA))

# We create a new data frame that includes a data record (or row) per household. We assign the following name to this data frame: 
# emc2014_welfare2.

emc2014_welfare2 <- as.data.frame(lapply(emc2014_welfare, rep, emc2014_welfare$hhsize))

# Now, we create two more data sets (per each administrative region). This is the approach we follow and it will help to estimate measures for the
# different administrative regions (note that other approaches could be followed to perform the analysis below).

emc2014_welfareBoucleduMouhoun <- emc2014_welfare %>% filter(region == "Boucle du Mouhoun")
emc2014_welfareCascades <- emc2014_welfare %>% filter(region == "Cascade")
emc2014_welfareCentre <- emc2014_welfare %>% filter(region == "Centre")
emc2014_welfareCentreEst <- emc2014_welfare %>% filter(region == "Centre Est")
emc2014_welfareCentreNord <- emc2014_welfare %>% filter(region == "Centre Nord")
emc2014_welfareCentreOuest <- emc2014_welfare %>% filter(region == "Centre Ouest")
emc2014_welfareCentreSud <- emc2014_welfare %>% filter(region == "Centre sud")
emc2014_welfareEst <- emc2014_welfare %>% filter(region == "Est")
emc2014_welfareHautsBassins <- emc2014_welfare %>% filter(region == "Hauts Bassins")
emc2014_welfareNord <- emc2014_welfare %>% filter(region == "Nord")
emc2014_welfarePlateauCentral <- emc2014_welfare %>% filter(region == "Plateau central")
emc2014_welfareSahel <- emc2014_welfare %>% filter(region == "Sahel")
emc2014_welfareSudOuest <- emc2014_welfare %>% filter(region == "Sud Ouest")

emc2014_welfareIndividualBoucleduMouhoun <- emc2014_welfare2 %>% filter(region == "Boucle du Mouhoun")
emc2014_welfareIndividualCascades <- emc2014_welfare2 %>% filter(region == "Cascade")
emc2014_welfareIndividualCentre <- emc2014_welfare2 %>% filter(region == "Centre")
emc2014_welfareIndividualCentreEst <- emc2014_welfare2 %>% filter(region == "Centre Est")
emc2014_welfareIndividualCentreNord <- emc2014_welfare2 %>% filter(region == "Centre Nord")
emc2014_welfareIndividualCentreOuest <- emc2014_welfare2 %>% filter(region == "Centre Ouest")
emc2014_welfareIndividualCentreSud <- emc2014_welfare2 %>% filter(region == "Centre sud")
emc2014_welfareIndividualEst <- emc2014_welfare2 %>% filter(region == "Est")
emc2014_welfareIndividualHautsBassins <- emc2014_welfare2 %>% filter(region == "Hauts Bassins")
emc2014_welfareIndividualNord <- emc2014_welfare2 %>% filter(region == "Nord")
emc2014_welfareIndividualPlateauCentral <- emc2014_welfare2 %>% filter(region == "Plateau central")
emc2014_welfareIndividualSahel <- emc2014_welfare2 %>% filter(region == "Sahel")
emc2014_welfareIndividualSudOuest <- emc2014_welfare2 %>% filter(region == "Sud Ouest")

# Then, we need to remove from all data frames the records with NAs, i.e. those records with people 
# that is not poor. Therefore, we remove the NAs and we save the values of interest (in this case, the pcgapdeptotndpoor) in the variable: 
# pcgapdeptotndpoor. We do this per administrative region for both of the data frames previously created.

pcgapdeptotndpoorBoucleduMouhoun <- emc2014_welfareBoucleduMouhoun$pcgapdeptotndpoor 
pcgapdeptotndpoorBoucleduMouhoun <- pcgapdeptotndpoorBoucleduMouhoun[!is.na(pcgapdeptotndpoorBoucleduMouhoun)]
pcgapdeptotndpoorCascades <- emc2014_welfareCascades$pcgapdeptotndpoor 
pcgapdeptotndpoorCascades <- pcgapdeptotndpoorCascades[!is.na(pcgapdeptotndpoorCascades)]
pcgapdeptotndpoorCentre <- emc2014_welfareCentre$pcgapdeptotndpoor 
pcgapdeptotndpoorCentre <- pcgapdeptotndpoorCentre[!is.na(pcgapdeptotndpoorCentre)]
pcgapdeptotndpoorCentreEst <- emc2014_welfareCentreEst$pcgapdeptotndpoor 
pcgapdeptotndpoorCentreEst <- pcgapdeptotndpoorCentreEst[!is.na(pcgapdeptotndpoorCentreEst)]
pcgapdeptotndpoorCentreNord <- emc2014_welfareCentreNord$pcgapdeptotndpoor 
pcgapdeptotndpoorCentreNord <- pcgapdeptotndpoorCentreNord[!is.na(pcgapdeptotndpoorCentreNord)]
pcgapdeptotndpoorCentreOuest <- emc2014_welfareCentreOuest$pcgapdeptotndpoor 
pcgapdeptotndpoorCentreOuest <- pcgapdeptotndpoorCentreOuest[!is.na(pcgapdeptotndpoorCentreOuest)]
pcgapdeptotndpoorCentreSud <- emc2014_welfareCentreSud$pcgapdeptotndpoor 
pcgapdeptotndpoorCentreSud <- pcgapdeptotndpoorCentreSud[!is.na(pcgapdeptotndpoorCentreSud)]
pcgapdeptotndpoorEst <- emc2014_welfareEst$pcgapdeptotndpoor 
pcgapdeptotndpoorEst <- pcgapdeptotndpoorEst[!is.na(pcgapdeptotndpoorEst)]
pcgapdeptotndpoorHautsBassins <- emc2014_welfareHautsBassins$pcgapdeptotndpoor 
pcgapdeptotndpoorHautsBassins <- pcgapdeptotndpoorHautsBassins[!is.na(pcgapdeptotndpoorHautsBassins)]
pcgapdeptotndpoorNord <- emc2014_welfareNord$pcgapdeptotndpoor 
pcgapdeptotndpoorNord <- pcgapdeptotndpoorNord[!is.na(pcgapdeptotndpoorNord)]
pcgapdeptotndpoorPlateauCentral <- emc2014_welfarePlateauCentral$pcgapdeptotndpoor 
pcgapdeptotndpoorPlateauCentral <- pcgapdeptotndpoorPlateauCentral[!is.na(pcgapdeptotndpoorPlateauCentral)]
pcgapdeptotndpoorSahel <- emc2014_welfareSahel$pcgapdeptotndpoor 
pcgapdeptotndpoorSahel <- pcgapdeptotndpoorSahel[!is.na(pcgapdeptotndpoorSahel)]
pcgapdeptotndpoorSudOuest <- emc2014_welfareSudOuest$pcgapdeptotndpoor 
pcgapdeptotndpoorSudOuest <- pcgapdeptotndpoorSudOuest[!is.na(pcgapdeptotndpoorSudOuest)]

pcgapdeptotndpoorIndividualBoucleduMouhoun <- emc2014_welfareIndividualBoucleduMouhoun$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualBoucleduMouhoun  <- pcgapdeptotndpoorIndividualBoucleduMouhoun[!is.na(pcgapdeptotndpoorIndividualBoucleduMouhoun)]
pcgapdeptotndpoorIndividualCascades <- emc2014_welfareIndividualCascades$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualCascades <- pcgapdeptotndpoorIndividualCascades[!is.na(pcgapdeptotndpoorIndividualCascades)]
pcgapdeptotndpoorIndividualCentre <- emc2014_welfareIndividualCentre$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualCentre <- pcgapdeptotndpoorIndividualCentre[!is.na(pcgapdeptotndpoorIndividualCentre)]
pcgapdeptotndpoorIndividualCentreEst <- emc2014_welfareIndividualCentreEst$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualCentreEst <- pcgapdeptotndpoorIndividualCentreEst[!is.na(pcgapdeptotndpoorIndividualCentreEst)]
pcgapdeptotndpoorIndividualCentreNord <- emc2014_welfareIndividualCentreNord$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualCentreNord <- pcgapdeptotndpoorIndividualCentreNord[!is.na(pcgapdeptotndpoorIndividualCentreNord)]
pcgapdeptotndpoorIndividualCentreOuest <- emc2014_welfareIndividualCentreOuest$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualCentreOuest <- pcgapdeptotndpoorIndividualCentreOuest[!is.na(pcgapdeptotndpoorIndividualCentreOuest)]
pcgapdeptotndpoorIndividualCentreSud <- emc2014_welfareIndividualCentreSud$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualCentreSud <- pcgapdeptotndpoorIndividualCentreSud[!is.na(pcgapdeptotndpoorIndividualCentreSud)]
pcgapdeptotndpoorIndividualEst <- emc2014_welfareIndividualEst$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualEst <- pcgapdeptotndpoorIndividualEst[!is.na(pcgapdeptotndpoorIndividualEst)]
pcgapdeptotndpoorIndividualHautsBassins <- emc2014_welfareIndividualHautsBassins$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualHautsBassins <- pcgapdeptotndpoorIndividualHautsBassins[!is.na(pcgapdeptotndpoorIndividualHautsBassins)]
pcgapdeptotndpoorIndividualNord <- emc2014_welfareIndividualNord$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualNord <- pcgapdeptotndpoorIndividualNord[!is.na(pcgapdeptotndpoorIndividualNord)]
pcgapdeptotndpoorIndividualPlateauCentral <- emc2014_welfareIndividualPlateauCentral$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualPlateauCentral <- pcgapdeptotndpoorIndividualPlateauCentral[!is.na(pcgapdeptotndpoorIndividualPlateauCentral)]
pcgapdeptotndpoorIndividualSahel <- emc2014_welfareIndividualSahel$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualSahel <- pcgapdeptotndpoorIndividualSahel[!is.na(pcgapdeptotndpoorIndividualSahel)]
pcgapdeptotndpoorIndividualSudOuest <- emc2014_welfareIndividualSudOuest$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualSudOuest <- pcgapdeptotndpoorIndividualSudOuest[!is.na(pcgapdeptotndpoorIndividualSudOuest)]

# We check the first ten values for the new calculated variables: pcgapdeptotndpoor.

pcgapdeptotndpoorBoucleduMouhoun[1:10]
pcgapdeptotndpoorCascades[1:10]
pcgapdeptotndpoorCentre[1:10]
pcgapdeptotndpoorCentreEst[1:10]
pcgapdeptotndpoorCentreNord[1:10]
pcgapdeptotndpoorCentreOuest[1:10]
pcgapdeptotndpoorCentreSud[1:10]
pcgapdeptotndpoorEst[1:10]
pcgapdeptotndpoorHautsBassins[1:10]
pcgapdeptotndpoorNord[1:10]
pcgapdeptotndpoorPlateauCentral[1:10]
pcgapdeptotndpoorSahel[1:10]
pcgapdeptotndpoorSudOuest[1:10]

pcgapdeptotndpoorIndividualBoucleduMouhoun[1:10]
pcgapdeptotndpoorIndividualCascades[1:10]
pcgapdeptotndpoorIndividualCentre[1:10]
pcgapdeptotndpoorIndividualCentre[1:10]
pcgapdeptotndpoorIndividualCentreEst[1:10]
pcgapdeptotndpoorIndividualCentreNord[1:10]
pcgapdeptotndpoorIndividualCentreOuest[1:10]
pcgapdeptotndpoorIndividualCentreSud[1:10]
pcgapdeptotndpoorIndividualEst[1:10]
pcgapdeptotndpoorIndividualHautsBassins[1:10]
pcgapdeptotndpoorIndividualNord[1:10]
pcgapdeptotndpoorIndividualPlateauCentral[1:10]
pcgapdeptotndpoorIndividualSahel[1:10]
pcgapdeptotndpoorIndividualSudOuest[1:10]

###################################################
### Calculation of Population (# of households) ###
###################################################

# We calculate the total population in Boucle du Mouhoun (# of households).

TotalPopulationBoucleduMouhoun <- emc2014_welfare %>% count(region) %>% filter(region == "Boucle du Mouhoun") %>% pull(n)
TotalPopulationBoucleduMouhoun

# We calculate the total population in Cascades (# of households).

TotalPopulationCascades <- emc2014_welfare %>% count(region) %>% filter(region == "Cascade") %>% pull(n)
TotalPopulationCascades

# We calculate the total population in Centre (# of households).

TotalPopulationCentre <- emc2014_welfare %>% count(region) %>% filter(region == "Centre") %>% pull(n)
TotalPopulationCentre

# We calculate the total population in Centre-Est (# of households).

TotalPopulationCentreEst <- emc2014_welfare %>% count(region) %>% filter(region == "Centre Est") %>% pull(n)
TotalPopulationCentreEst

# We calculate the total population in Centre-Nord (# of households).

TotalPopulationCentreNord <- emc2014_welfare %>% count(region) %>% filter(region == "Centre Nord") %>% pull(n)
TotalPopulationCentreNord

# We calculate the total population in Centre-Ouest (# of households).

TotalPopulationCentreOuest <- emc2014_welfare %>% count(region) %>% filter(region == "Centre Ouest") %>% pull(n)
TotalPopulationCentreOuest

# We calculate the total population in Centre-Sud (# of households).

TotalPopulationCentreSud <- emc2014_welfare %>% count(region) %>% filter(region == "Centre sud") %>% pull(n)
TotalPopulationCentreSud

# We calculate the total population in Est (# of households).

TotalPopulationEst <- emc2014_welfare %>% count(region) %>% filter(region == "Est") %>% pull(n)
TotalPopulationEst

# We calculate the total population in Hauts-Bassins (# of households).

TotalPopulationHautsBassins <- emc2014_welfare %>% count(region) %>% filter(region == "Hauts Bassins") %>% pull(n)
TotalPopulationHautsBassins

# We calculate the total population in Nord (# of households).

TotalPopulationNord <- emc2014_welfare %>% count(region) %>% filter(region == "Nord") %>% pull(n)
TotalPopulationNord

# We calculate the total population in Plateau Central (# of households).

TotalPopulationPlateauCentral <- emc2014_welfare %>% count(region) %>% filter(region == "Plateau central") %>% pull(n)
TotalPopulationPlateauCentral

# We calculate the total population in Sahel (# of households).

TotalPopulationSahel <- emc2014_welfare %>% count(region) %>% filter(region == "Sahel") %>% pull(n)
TotalPopulationSahel

# We calculate the total population in Sud-Ouest (# of households).

TotalPopulationSudOuest <- emc2014_welfare %>% count(region) %>% filter(region == "Sud Ouest") %>% pull(n)
TotalPopulationSudOuest

##########################################################
#### Calculation of Poor Population (# of households) ####
##########################################################

# We calculate the total number of poor households in Boucle du Mouhoun (# of households).

TotalPoorBoucleduMouhoun <- length(pcgapdeptotndpoorBoucleduMouhoun)
TotalPoorBoucleduMouhoun

# We calculate the total number of poor households in Cascades (# of households).

TotalPoorCascades <- length(pcgapdeptotndpoorCascades)
TotalPoorCascades

# We calculate the total number of poor households in Centre (# of households).

TotalPoorCentre <- length(pcgapdeptotndpoorCentre)
TotalPoorCentre

# We calculate the total number of poor households in Centre-Est (# of households).

TotalPoorCentreEst <- length(pcgapdeptotndpoorCentreEst)
TotalPoorCentreEst

# We calculate the total number of poor households in Centre-Nord (# of households).

TotalPoorCentreNord <- length(pcgapdeptotndpoorCentreNord)
TotalPoorCentreNord

# We calculate the total number of poor households in Centre-Ouest (# of households).

TotalPoorCentreOuest <- length(pcgapdeptotndpoorCentreOuest)
TotalPoorCentreOuest

# We calculate the total number of poor households in Centre-Sud (# of households).

TotalPoorCentreSud <- length(pcgapdeptotndpoorCentreSud)
TotalPoorCentreSud

# We calculate the total number of poor households in Est (# of households).

TotalPoorEst <- length(pcgapdeptotndpoorEst)
TotalPoorEst

# We calculate the total number of poor households in Hauts-Bassins (# of households).

TotalPoorHautsBassins <- length(pcgapdeptotndpoorHautsBassins)
TotalPoorHautsBassins

# We calculate the total number of poor households in Nord (# of households).

TotalPoorNord <- length(pcgapdeptotndpoorNord)
TotalPoorNord

# We calculate the total number of poor households in Plateau Central (# of households).

TotalPoorPlateauCentral <- length(pcgapdeptotndpoorPlateauCentral)
TotalPoorPlateauCentral

# We calculate the total number of poor households in Sahel (# of households).

TotalPoorSahel <- length(pcgapdeptotndpoorSahel)
TotalPoorSahel

# We calculate the total number of poor households in Sud-Ouest (# of households).

TotalPoorSudOuest <- length(pcgapdeptotndpoorSudOuest)
TotalPoorSudOuest

####################################################
### Calculation of Population (# of individuals) ###
####################################################

# We calculate the total population in Boucle du Mouhoun (# of individuals).

TotalPopulationIndividualBoucleduMouhoun <- emc2014_welfare2 %>% count(region) %>% filter(region == "Boucle du Mouhoun") %>% pull(n)
TotalPopulationIndividualBoucleduMouhoun

# We calculate the total population in Cascades (# of individuals).

TotalPopulationIndividualCascades <- emc2014_welfare2 %>% count(region) %>% filter(region == "Cascade") %>% pull(n)
TotalPopulationIndividualCascades

# We calculate the total population in Centre (# of individuals).

TotalPopulationIndividualCentre <- emc2014_welfare2 %>% count(region) %>% filter(region == "Centre") %>% pull(n)
TotalPopulationIndividualCentre

# We calculate the total population in Centre-Est (# of individuals).

TotalPopulationIndividualCentreEst <- emc2014_welfare2 %>% count(region) %>% filter(region == "Centre Est") %>% pull(n)
TotalPopulationIndividualCentreEst

# We calculate the total population in Centre-Nord (# of individuals).

TotalPopulationIndividualCentreNord <- emc2014_welfare2 %>% count(region) %>% filter(region == "Centre Nord") %>% pull(n)
TotalPopulationIndividualCentreNord

# We calculate the total population in Centre-Ouest (# of individuals).

TotalPopulationIndividualCentreOuest <- emc2014_welfare2 %>% count(region) %>% filter(region == "Centre Ouest") %>% pull(n)
TotalPopulationIndividualCentreOuest

# We calculate the total population in Centre-Sud (# of individuals).

TotalPopulationIndividualCentreSud <- emc2014_welfare2 %>% count(region) %>% filter(region == "Centre sud") %>% pull(n)
TotalPopulationIndividualCentreSud

# We calculate the total population in Est (# of individuals).

TotalPopulationIndividualEst <- emc2014_welfare2 %>% count(region) %>% filter(region == "Est") %>% pull(n)
TotalPopulationIndividualEst

# We calculate the total population in Hauts-Bassins (# of individuals).

TotalPopulationIndividualHautsBassins <- emc2014_welfare2 %>% count(region) %>% filter(region == "Hauts Bassins") %>% pull(n)
TotalPopulationIndividualHautsBassins

# We calculate the total population in Nord (# of individuals).

TotalPopulationIndividualNord <- emc2014_welfare2 %>% count(region) %>% filter(region == "Nord") %>% pull(n)
TotalPopulationIndividualNord

# We calculate the total population in Plateau Central (# of individuals).

TotalPopulationIndividualPlateauCentral <- emc2014_welfare2 %>% count(region) %>% filter(region == "Plateau central") %>% pull(n)
TotalPopulationIndividualPlateauCentral

# We calculate the total population in Sahel (# of individuals).

TotalPopulationIndividualSahel <- emc2014_welfare2 %>% count(region) %>% filter(region == "Sahel") %>% pull(n)
TotalPopulationIndividualSahel

# We calculate the total population in Sud-Ouest (# of individuals).

TotalPopulationIndividualSudOuest <- emc2014_welfare2 %>% count(region) %>% filter(region == "Sud Ouest") %>% pull(n)
TotalPopulationIndividualSudOuest

############################################################
##### Calculation of Poor Population (# of individuals) ####
############################################################

# We calculate the total number of poor households in Boucle du Mouhoun (# of individuals).

TotalPoorIndividualBoucleduMouhoun <- length(pcgapdeptotndpoorIndividualBoucleduMouhoun)
TotalPoorIndividualBoucleduMouhoun

# We calculate the total number of poor households in Cascades (# of individuals).

TotalPoorIndividualCascades <- length(pcgapdeptotndpoorIndividualCascades)
TotalPoorIndividualCascades

# We calculate the total number of poor households in Centre (# of individuals).

TotalPoorIndividualCentre <- length(pcgapdeptotndpoorIndividualCentre)
TotalPoorIndividualCentre

# We calculate the total number of poor households in Centre-Est (# of individuals).

TotalPoorIndividualCentreEst <- length(pcgapdeptotndpoorIndividualCentreEst)
TotalPoorIndividualCentreEst

# We calculate the total number of poor households in Centre-Nord (# of individuals).

TotalPoorIndividualCentreNord <- length(pcgapdeptotndpoorIndividualCentreNord)
TotalPoorIndividualCentreNord

# We calculate the total number of poor households in Centre-Ouest (# of individuals).

TotalPoorIndividualCentreOuest <- length(pcgapdeptotndpoorIndividualCentreOuest)
TotalPoorIndividualCentreOuest

# We calculate the total number of poor households in Centre-Sud (# of individuals).

TotalPoorIndividualCentreSud <- length(pcgapdeptotndpoorIndividualCentreSud)
TotalPoorIndividualCentreSud

# We calculate the total number of poor households in Est (# of individuals).

TotalPoorIndividualEst <- length(pcgapdeptotndpoorIndividualEst)
TotalPoorIndividualEst

# We calculate the total number of poor households in Hauts-Bassins (# of individuals).

TotalPoorIndividualHautsBassins <- length(pcgapdeptotndpoorIndividualHautsBassins)
TotalPoorIndividualHautsBassins

# We calculate the total number of poor households in Nord (# of individuals).

TotalPoorIndividualNord <- length(pcgapdeptotndpoorIndividualNord)
TotalPoorIndividualNord

# We calculate the total number of poor households in Plateau Central (# of individuals).

TotalPoorIndividualPlateauCentral <- length(pcgapdeptotndpoorIndividualPlateauCentral)
TotalPoorIndividualPlateauCentral

# We calculate the total number of poor households in Sahel (# of individuals).

TotalPoorIndividualSahel <- length(pcgapdeptotndpoorIndividualSahel)
TotalPoorIndividualSahel

# We calculate the total number of poor households in Sud-Ouest (# of individuals).

TotalPoorIndividualSudOuest <- length(pcgapdeptotndpoorIndividualSudOuest)
TotalPoorIndividualSudOuest

##################################################
## Estimation of Parameters  (# of individuals) ##
##################################################

# We define a function to print values with two decimals.

percent2 <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...))
}

########### Boucle du Mouhoun ##########

# For Boucle du Mouhoun, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun <- TotalPoorIndividualBoucleduMouhoun/(TotalPoorIndividualBoucleduMouhoun * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualBoucleduMouhoun)))
sprintf("Maximum Likelihood Estimator of α for Boucle du Mouhoun: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun))

# For Boucle du Mouhoun, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun <- (PovertyLine - mean(pcgapdeptotndpoorIndividualBoucleduMouhoun))/mean(pcgapdeptotndpoorIndividualBoucleduMouhoun)
sprintf("Method of Moments Estimator of α for Boucle du Mouhoun: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun))

########### Cascades ##########

# For Cascades, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades <- TotalPoorIndividualCascades/(TotalPoorIndividualCascades * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualCascades)))
sprintf("Maximum Likelihood Estimator of α for Cascades: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades))

# For Cascades, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades <- (PovertyLine - mean(pcgapdeptotndpoorIndividualCascades))/mean(pcgapdeptotndpoorIndividualCascades)
sprintf("Method of Moments Estimator of α for Cascades: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades))

########### Centre ##########

# For Centre, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre <- TotalPoorIndividualCentre/(TotalPoorIndividualCentre * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualCentre)))
sprintf("Maximum Likelihood Estimator of α for Centre: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre))

# For Centre, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre <- (PovertyLine - mean(pcgapdeptotndpoorIndividualCentre))/mean(pcgapdeptotndpoorIndividualCentre)
sprintf("Method of Moments Estimator of α for Centre: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre))

########### Centre-Est ##########

# For Centre-Est, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst <- TotalPoorIndividualCentreEst/(TotalPoorIndividualCentreEst * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualCentreEst)))
sprintf("Maximum Likelihood Estimator of α for Centre-Est: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst))

# For Centre-Est, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst <- (PovertyLine - mean(pcgapdeptotndpoorIndividualCentreEst))/mean(pcgapdeptotndpoorIndividualCentreEst)
sprintf("Method of Moments Estimator of α for Centre-Est: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst))

########### Centre-Nord ##########

# For Centre-Nord, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord <- TotalPoorIndividualCentreNord/(TotalPoorIndividualCentreNord * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualCentreNord)))
sprintf("Maximum Likelihood Estimator of α for Centre-Nord: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord))

# For Centre-Nord, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord <- (PovertyLine - mean(pcgapdeptotndpoorIndividualCentreNord))/mean(pcgapdeptotndpoorIndividualCentreNord)
sprintf("Method of Moments Estimator of α for Centre-Nord: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord))

########### Centre-Ouest ##########

# For Centre-Ouest, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest <- TotalPoorIndividualCentreOuest/(TotalPoorIndividualCentreOuest * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualCentreOuest)))
sprintf("Maximum Likelihood Estimator of α for Centre-Ouest: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest))

# For Centre-Ouest, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest <- (PovertyLine - mean(pcgapdeptotndpoorIndividualCentreOuest))/mean(pcgapdeptotndpoorIndividualCentreOuest)
sprintf("Method of Moments Estimator of α for Centre-Ouest: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest))

########### Centre-Sud ##########

# For Centre-Sud, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud <- TotalPoorIndividualCentreSud/(TotalPoorIndividualCentreSud * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualCentreSud)))
sprintf("Maximum Likelihood Estimator of α for Centre-Sud: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud))

# For Centre-Sud, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud <- (PovertyLine - mean(pcgapdeptotndpoorIndividualCentreSud))/mean(pcgapdeptotndpoorIndividualCentreSud)
sprintf("Method of Moments Estimator of α for Centre-Sud: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud))

########### Est ##########

# For Est, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst <- TotalPoorIndividualEst/(TotalPoorIndividualEst * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualEst)))
sprintf("Maximum Likelihood Estimator of α for Est: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst))

# For Est, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualEst <- (PovertyLine - mean(pcgapdeptotndpoorIndividualEst))/mean(pcgapdeptotndpoorIndividualEst)
sprintf("Method of Moments Estimator of α for Est: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualEst))

########### Hauts-Bassins ##########

# For Hauts-Bassins, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins <- TotalPoorIndividualHautsBassins/(TotalPoorIndividualHautsBassins * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualHautsBassins)))
sprintf("Maximum Likelihood Estimator of α for Hauts-Bassins: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins))

# For Est, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins <- (PovertyLine - mean(pcgapdeptotndpoorIndividualHautsBassins))/mean(pcgapdeptotndpoorIndividualHautsBassins)
sprintf("Method of Moments Estimator of α for Hauts-Bassins: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins))

########### Nord ##########

# For Nord, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord <- TotalPoorIndividualNord/(TotalPoorIndividualNord * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualNord)))
sprintf("Maximum Likelihood Estimator of α for Nord: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord))

# For Nord, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualNord <- (PovertyLine - mean(pcgapdeptotndpoorIndividualNord))/mean(pcgapdeptotndpoorIndividualNord)
sprintf("Method of Moments Estimator of α for Nord: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualNord))

########### Plateau Central ##########

# For Plateau Central, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral <- TotalPoorIndividualPlateauCentral/(TotalPoorIndividualPlateauCentral * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualPlateauCentral)))
sprintf("Maximum Likelihood Estimator of α for Plateau Central: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral))

# For Plateau Central, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral <- (PovertyLine - mean(pcgapdeptotndpoorIndividualPlateauCentral))/mean(pcgapdeptotndpoorIndividualPlateauCentral)
sprintf("Method of Moments Estimator of α for Plateau Central: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral))

########### Sahel ##########

# For Sahel, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel <- TotalPoorIndividualSahel/(TotalPoorIndividualSahel * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualSahel)))
sprintf("Maximum Likelihood Estimator of α for Sahel: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel))

# For Sahel, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel <- (PovertyLine - mean(pcgapdeptotndpoorIndividualSahel))/mean(pcgapdeptotndpoorIndividualSahel)
sprintf("Method of Moments Estimator of α for Sahel: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel))

########### Sud-Ouest ##########

# For Sud-Ouest, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest <- TotalPoorIndividualSudOuest/(TotalPoorIndividualSudOuest * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualSudOuest)))
sprintf("Maximum Likelihood Estimator of α for Sud-Ouest: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest))

# For Sud-Ouest, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest <- (PovertyLine - mean(pcgapdeptotndpoorIndividualSudOuest))/mean(pcgapdeptotndpoorIndividualSudOuest)
sprintf("Method of Moments Estimator of α for Sud-Ouest: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest))

##################################################
################## KS-TESTS ######################
##################################################

# We define the cumulative distribution function of the generalised beta distribution 1 (GB1). We call this function: ppbeta.

ppbeta <- function(x, shape1, shape2, PovertyLine) {y <- x/PovertyLine
return(pbeta(y, shape1, shape2))}

# Define a function to compute the empirical cumulative distribution function (ECDF). We call this function: ecdf_func.

ecdf_func <- function(data) { 
  Length <- length(data) 
  sorted <- sort(data) 
  
  ecdf <- rep(0, Length) 
  for (i in 1:Length) { 
    ecdf[i] <- sum(sorted <= data[i]) / Length 
  } 
  return(ecdf) 
} 


# We create a function DStatisticEstimation that will estimate the D statistic as per pp. 530 from (tied observations):

# Hollander, M. and D. A. Wolfe. Nonparametric Statistical Methods, Second Edition. Wiley Series in Probability and Statistics. 
# United States of America.

DStatisticEstimation <- function(ecdf, tcdf) {
  term1 <- abs(ecdf - tcdf)
  term2 <- vector()
  Mprime <- vector()
  for (i in 1:length(ecdf)) {
    if (i==1){
      term2[i] <- abs(0 - tcdf[1])
    }else{
      term2[i] <- abs(ecdf[i - 1] - tcdf[i])
    }
  }
  for (i in 1:length(ecdf)) {
    Mprime[i] <- max(term1[i], term2[i])
  }
  return(max(Mprime))
}

# Now, we create a function PValueEstimation that will simulate the p-value.

PValueEstimation <- function(data, D, simulations) { set.seed(1) # Set seed for replication.
  count <- vector()
  n <- length(unique(data))
  X <- seq(from = 1, to = n)/n
  Xm <- seq(from = 0, to = n - 1)/n
  for (i in 1:simulations){
    uniforms <- sort(runif(n))
    simD <- max(c(X - uniforms, uniforms - Xm))
    if (simD > D){
      count[i] <- 1
    }
    else{
      count[i] <- 0
    }
  }
  return(mean(count))
}

########### Boucle du Mouhoun ##########

ecdfBoucleduMouhoun <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualBoucleduMouhoun)))

# Maximum Likelihood Estimator (MLE) - KS test for Boucle du Mouhoun: 
  
tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBoucleduMouhoun)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorBoucleduMouhoun <- DStatisticEstimation(ecdfBoucleduMouhoun, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Boucle du Mouhoun: %s", MprimeMaximumLikelihoodEstimatorBoucleduMouhoun)

PValueMaximumLikelihoodEstimatorBoucleduMouhoun <- PValueEstimation(pcgapdeptotndpoorIndividualBoucleduMouhoun, MprimeMaximumLikelihoodEstimatorBoucleduMouhoun, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Boucle du Mouhoun: %s", PValueMaximumLikelihoodEstimatorBoucleduMouhoun)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBoucleduMouhoun, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Boucle du Mouhoun:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBoucleduMouhoun)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorBoucleduMouhoun <- DStatisticEstimation(ecdfBoucleduMouhoun, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Boucle du Mouhoun: %s", MprimeMethodofMomentsEstimatorBoucleduMouhoun)

PValueMethodofMomentsEstimatorBoucleduMouhoun <- PValueEstimation(pcgapdeptotndpoorIndividualBoucleduMouhoun, MprimeMethodofMomentsEstimatorBoucleduMouhoun, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Boucle du Mouhoun: %s", PValueMethodofMomentsEstimatorBoucleduMouhoun)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBoucleduMouhoun, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine) # two-sided, exact

########### Cascades ##########

ecdfCascades <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualCascades)))

# Maximum Likelihood Estimator (MLE) - KS test for Cascades: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCascades)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorCascades <- DStatisticEstimation(ecdfCascades, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Cascades: %s", MprimeMaximumLikelihoodEstimatorCascades)

PValueMaximumLikelihoodEstimatorCascades <- PValueEstimation(pcgapdeptotndpoorIndividualCascades, MprimeMaximumLikelihoodEstimatorCascades, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Cascades: %s", PValueMaximumLikelihoodEstimatorCascades)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCascades, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Cascades:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCascades <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCascades)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorCascades <- DStatisticEstimation(ecdfCascades, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCascades)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Cascades: %s", MprimeMethodofMomentsEstimatorCascades)

PValueMethodofMomentsEstimatorCascades <- PValueEstimation(pcgapdeptotndpoorIndividualCascades, MprimeMethodofMomentsEstimatorCascades, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Cascades: %s", PValueMethodofMomentsEstimatorCascades)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCascades, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine) # two-sided, exact

########### Centre ##########

ecdfCentre <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualCentre)))

# Maximum Likelihood Estimator (MLE) - KS test for Centre: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCentre)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorCentre <- DStatisticEstimation(ecdfCentre, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Centre: %s", MprimeMaximumLikelihoodEstimatorCentre)

PValueMaximumLikelihoodEstimatorCentre <- PValueEstimation(pcgapdeptotndpoorIndividualCentre, MprimeMaximumLikelihoodEstimatorCentre, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Centre: %s", PValueMaximumLikelihoodEstimatorCentre)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCentre, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Centre:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCentre <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCentre)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorCentre <- DStatisticEstimation(ecdfCentre, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCentre)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Centre: %s", MprimeMethodofMomentsEstimatorCentre)

PValueMethodofMomentsEstimatorCentre <- PValueEstimation(pcgapdeptotndpoorIndividualCentre, MprimeMethodofMomentsEstimatorCentre, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Centre: %s", PValueMethodofMomentsEstimatorCentre)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCentre, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine) # two-sided, exact

########### Centre-Est ##########

ecdfCentreEst <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualCentreEst)))

# Maximum Likelihood Estimator (MLE) - KS test for Centre-Est: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCentreEst)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorCentreEst <- DStatisticEstimation(ecdfCentreEst, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Centre-Est: %s", MprimeMaximumLikelihoodEstimatorCentreEst)

PValueMaximumLikelihoodEstimatorCentreEst <- PValueEstimation(pcgapdeptotndpoorIndividualCentreEst, MprimeMaximumLikelihoodEstimatorCentreEst, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Centre-Est: %s", PValueMaximumLikelihoodEstimatorCentreEst)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCentreEst, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Centre-Est:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCentreEst)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorCentreEst <- DStatisticEstimation(ecdfCentreEst, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Centre-Est: %s", MprimeMethodofMomentsEstimatorCentreEst)

PValueMethodofMomentsEstimatorCentreEst <- PValueEstimation(pcgapdeptotndpoorIndividualCentreEst, MprimeMethodofMomentsEstimatorCentreEst, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Centre-Est: %s", PValueMethodofMomentsEstimatorCentreEst)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCentreEst, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine) # two-sided, exact

########### Centre-Nord ##########

ecdfCentreNord <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualCentreNord)))

# Maximum Likelihood Estimator (MLE) - KS test for Centre-Nord: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCentreNord)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorCentreNord <- DStatisticEstimation(ecdfCentreNord, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Centre-Nord: %s", MprimeMaximumLikelihoodEstimatorCentreNord)

PValueMaximumLikelihoodEstimatorCentreNord <- PValueEstimation(pcgapdeptotndpoorIndividualCentreNord, MprimeMaximumLikelihoodEstimatorCentreNord, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Centre-Nord: %s", PValueMaximumLikelihoodEstimatorCentreNord)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCentreNord, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Centre-Nord:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCentreNord)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorCentreNord <- DStatisticEstimation(ecdfCentreNord, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Centre-Nord: %s", MprimeMethodofMomentsEstimatorCentreNord)

PValueMethodofMomentsEstimatorCentreNord <- PValueEstimation(pcgapdeptotndpoorIndividualCentreNord, MprimeMethodofMomentsEstimatorCentreNord, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Centre-Nord: %s", PValueMethodofMomentsEstimatorCentreNord)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCentreNord, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine) # two-sided, exact

########### Centre-Ouest ##########

ecdfCentreOuest <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualCentreOuest)))

# Maximum Likelihood Estimator (MLE) - KS test for Centre-Ouest: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCentreOuest)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorCentreOuest <- DStatisticEstimation(ecdfCentreOuest, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Centre-Ouest: %s", MprimeMaximumLikelihoodEstimatorCentreOuest)

PValueMaximumLikelihoodEstimatorCentreOuest <- PValueEstimation(pcgapdeptotndpoorIndividualCentreOuest, MprimeMaximumLikelihoodEstimatorCentreOuest, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Centre-Ouest: %s", PValueMaximumLikelihoodEstimatorCentreOuest)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCentreOuest, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Centre-Ouest:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCentreOuest)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorCentreOuest <- DStatisticEstimation(ecdfCentreOuest, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Centre-Ouest: %s", MprimeMethodofMomentsEstimatorCentreOuest)

PValueMethodofMomentsEstimatorCentreOuest <- PValueEstimation(pcgapdeptotndpoorIndividualCentreOuest, MprimeMethodofMomentsEstimatorCentreOuest, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Centre-Ouest: %s", PValueMethodofMomentsEstimatorCentreOuest)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCentreOuest, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine) # two-sided, exact

########### Centre-Sud ##########

ecdfCentreSud <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualCentreSud)))

# Maximum Likelihood Estimator (MLE) - KS test for Centre-Sud: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCentreSud)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorCentreSud <- DStatisticEstimation(ecdfCentreSud, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Centre-Sud: %s", MprimeMaximumLikelihoodEstimatorCentreSud)

PValueMaximumLikelihoodEstimatorCentreSud <- PValueEstimation(pcgapdeptotndpoorIndividualCentreSud, MprimeMaximumLikelihoodEstimatorCentreSud, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Centre-Sud: %s", PValueMaximumLikelihoodEstimatorCentreSud)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCentreSud, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Centre-Sud:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCentreSud)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorCentreSud <- DStatisticEstimation(ecdfCentreSud, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Centre-Sud: %s", MprimeMethodofMomentsEstimatorCentreSud)

PValueMethodofMomentsEstimatorCentreSud <- PValueEstimation(pcgapdeptotndpoorIndividualCentreSud, MprimeMethodofMomentsEstimatorCentreSud, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Centre-Sud: %s", PValueMethodofMomentsEstimatorCentreSud)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCentreSud, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine) # two-sided, exact

########### Est ##########

ecdfEst <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualEst)))

# Maximum Likelihood Estimator (MLE) - KS test for Est: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualEst)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorEst <- DStatisticEstimation(ecdfEst, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Est: %s", MprimeMaximumLikelihoodEstimatorEst)

PValueMaximumLikelihoodEstimatorEst <- PValueEstimation(pcgapdeptotndpoorIndividualEst, MprimeMaximumLikelihoodEstimatorEst, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Est: %s", PValueMaximumLikelihoodEstimatorEst)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorEst, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Est:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualEst <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualEst)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorEst <- DStatisticEstimation(ecdfEst, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualEst)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Est: %s", MprimeMethodofMomentsEstimatorEst)

PValueMethodofMomentsEstimatorEst <- PValueEstimation(pcgapdeptotndpoorIndividualEst, MprimeMethodofMomentsEstimatorEst, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Est: %s", PValueMethodofMomentsEstimatorEst)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorEst, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine) # two-sided, exact

########### Hauts-Bassins ##########

ecdfHautsBassins <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualHautsBassins)))

# Maximum Likelihood Estimator (MLE) - KS test for Hauts-Bassins: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualHautsBassins)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorHautsBassins <- DStatisticEstimation(ecdfHautsBassins, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Hauts-Bassins: %s", MprimeMaximumLikelihoodEstimatorHautsBassins)

PValueMaximumLikelihoodEstimatorHautsBassins <- PValueEstimation(pcgapdeptotndpoorIndividualHautsBassins, MprimeMaximumLikelihoodEstimatorHautsBassins, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Hauts-Bassins: %s", PValueMaximumLikelihoodEstimatorHautsBassins)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorHautsBassins, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Hauts-Bassins:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualHautsBassins)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorHautsBassins <- DStatisticEstimation(ecdfHautsBassins, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Hauts-Bassins: %s", MprimeMethodofMomentsEstimatorHautsBassins)

PValueMethodofMomentsEstimatorHautsBassins <- PValueEstimation(pcgapdeptotndpoorIndividualHautsBassins, MprimeMethodofMomentsEstimatorHautsBassins, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Hauts-Bassins: %s", PValueMethodofMomentsEstimatorHautsBassins)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorHautsBassins, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine) # two-sided, exact

########### Nord ##########

ecdfNord <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualNord)))

# Maximum Likelihood Estimator (MLE) - KS test for Nord: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualNord)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorNord <- DStatisticEstimation(ecdfNord, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Nord: %s", MprimeMaximumLikelihoodEstimatorNord)

PValueMaximumLikelihoodEstimatorNord <- PValueEstimation(pcgapdeptotndpoorIndividualNord, MprimeMaximumLikelihoodEstimatorNord, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Nord: %s", PValueMaximumLikelihoodEstimatorNord)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorNord, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Nord:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualNord <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualNord)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorNord <- DStatisticEstimation(ecdfNord, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualNord)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Nord: %s", MprimeMethodofMomentsEstimatorNord)

PValueMethodofMomentsEstimatorNord <- PValueEstimation(pcgapdeptotndpoorIndividualNord, MprimeMethodofMomentsEstimatorNord, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Nord: %s", PValueMethodofMomentsEstimatorNord)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorNord, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine) # two-sided, exact

########### Plateau Central ##########

ecdfPlateauCentral <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualPlateauCentral)))

# Maximum Likelihood Estimator (MLE) - KS test for Plateau Central: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualPlateauCentral)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorPlateauCentral <- DStatisticEstimation(ecdfPlateauCentral, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Plateau Central: %s", MprimeMaximumLikelihoodEstimatorPlateauCentral)

PValueMaximumLikelihoodEstimatorPlateauCentral <- PValueEstimation(pcgapdeptotndpoorIndividualPlateauCentral, MprimeMaximumLikelihoodEstimatorPlateauCentral, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Plateau Central: %s", PValueMaximumLikelihoodEstimatorPlateauCentral)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorPlateauCentral, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Plateau Central:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualPlateauCentral)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorPlateauCentral <- DStatisticEstimation(ecdfPlateauCentral, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Plateau Central: %s", MprimeMethodofMomentsEstimatorPlateauCentral)

PValueMethodofMomentsEstimatorPlateauCentral <- PValueEstimation(pcgapdeptotndpoorIndividualPlateauCentral, MprimeMethodofMomentsEstimatorPlateauCentral, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Plateau Central: %s", PValueMethodofMomentsEstimatorPlateauCentral)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorPlateauCentral, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine) # two-sided, exact

########### Sahel ##########

ecdfSahel <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualSahel)))

# Maximum Likelihood Estimator (MLE) - KS test for Sahel: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualSahel)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorSahel <- DStatisticEstimation(ecdfSahel, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Sahel: %s", MprimeMaximumLikelihoodEstimatorSahel)

PValueMaximumLikelihoodEstimatorSahel <- PValueEstimation(pcgapdeptotndpoorIndividualSahel, MprimeMaximumLikelihoodEstimatorSahel, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Sahel: %s", PValueMaximumLikelihoodEstimatorSahel)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorSahel, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Sahel:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualSahel <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualSahel)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorSahel <- DStatisticEstimation(ecdfSahel, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualSahel)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Sahel: %s", MprimeMethodofMomentsEstimatorSahel)

PValueMethodofMomentsEstimatorSahel <- PValueEstimation(pcgapdeptotndpoorIndividualSahel, MprimeMethodofMomentsEstimatorSahel, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Sahel: %s", PValueMethodofMomentsEstimatorSahel)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorSahel, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine) # two-sided, exact

########### Sud-Ouest ##########

ecdfSudOuest <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualSudOuest)))

# Maximum Likelihood Estimator (MLE) - KS test for Sud-Ouest: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualSudOuest)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorSudOuest <- DStatisticEstimation(ecdfSudOuest, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Sud-Ouest: %s", MprimeMaximumLikelihoodEstimatorSudOuest)

PValueMaximumLikelihoodEstimatorSudOuest <- PValueEstimation(pcgapdeptotndpoorIndividualSudOuest, MprimeMaximumLikelihoodEstimatorSudOuest, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Sud-Ouest: %s", PValueMaximumLikelihoodEstimatorSudOuest)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorSudOuest, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Sud-Ouest:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualSudOuest)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorSudOuest <- DStatisticEstimation(ecdfSudOuest, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Sud-Ouest: %s", MprimeMethodofMomentsEstimatorSudOuest)

PValueMethodofMomentsEstimatorSudOuest <- PValueEstimation(pcgapdeptotndpoorIndividualSudOuest, MprimeMethodofMomentsEstimatorSudOuest, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Sud-Ouest: %s", PValueMethodofMomentsEstimatorSudOuest)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorSudOuest, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine) # two-sided, exact

###################################################
################## R-SQUARED ######################
###################################################

# Define a function to compute the ECDF
percent4 <- function(x, digits = 4, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...))
}

########### Boucle du Mouhoun ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Boucle du Mouhoun: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBoucleduMouhoun, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBoucleduMouhoun)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBoucleduMouhoun, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBoucleduMouhoun)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBoucleduMouhoun, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBoucleduMouhoun))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Boucle du Mouhoun: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBoucleduMouhoun, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBoucleduMouhoun)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBoucleduMouhoun, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBoucleduMouhoun)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBoucleduMouhoun, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBoucleduMouhoun))^2))))

########### Cascades ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Cascades: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCascades, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCascades)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCascades, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCascades)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCascades, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCascades))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Cascades: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCascades, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCascades)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCascades, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCascades)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCascades, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCascades))^2))))

########### Centre ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Centre: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCentre, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentre)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCentre, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentre)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCentre, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCentre))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Centre: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCentre, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentre)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCentre, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentre)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCentre, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCentre))^2))))

########### Centre-Est ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Centre-Est: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCentreEst, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreEst)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCentreEst, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreEst)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCentreEst, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCentreEst))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Centre-Est: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCentreEst, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreEst)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCentreEst, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreEst)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCentreEst, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCentreEst))^2))))

########### Centre-Nord ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Centre-Nord: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCentreNord, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreNord)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCentreNord, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreNord)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCentreNord, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCentreNord))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Centre-Nord: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCentreNord, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreNord)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCentreNord, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreNord)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCentreNord, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCentreNord))^2))))

########### Centre-Ouest ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Centre-Ouest: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCentreOuest, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreOuest)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCentreOuest, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreOuest)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCentreOuest, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCentreOuest))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Centre-Ouest: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCentreOuest, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreOuest)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCentreOuest, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreOuest)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCentreOuest, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCentreOuest))^2))))

########### Centre-Sud ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Centre-Sud: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCentreSud, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreSud)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCentreSud, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreSud)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCentreSud, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCentreSud))^2))))

sprintf("Method of Moments Estimator R-Squared for Centre-Sud: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCentreSud, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreSud)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCentreSud, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCentreSud)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCentreSud, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCentreSud))^2))))

########### Est ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Est: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualEst, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualEst)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualEst, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualEst)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualEst, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualEst))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Est: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualEst, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualEst)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualEst, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualEst)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualEst, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualEst))^2))))

########### Hauts-Bassins ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Hauts-Bassins: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualHautsBassins, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualHautsBassins)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualHautsBassins, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualHautsBassins)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualHautsBassins, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualHautsBassins))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Hauts-Bassins: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualHautsBassins, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualHautsBassins)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualHautsBassins, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualHautsBassins)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualHautsBassins, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualHautsBassins))^2))))

########### Nord ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Nord: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualNord, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualNord)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualNord, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualNord)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualNord, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualNord))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Nord: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualNord, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualNord)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualNord, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualNord)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualNord, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualNord))^2))))

########### Plateau Central ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Plateau Central: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualPlateauCentral, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualPlateauCentral)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualPlateauCentral, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualPlateauCentral)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualPlateauCentral, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualPlateauCentral))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Plateau Central: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualPlateauCentral, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualPlateauCentral)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualPlateauCentral, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualPlateauCentral)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualPlateauCentral, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualPlateauCentral))^2))))

########### Sahel ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Sahel: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualSahel, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSahel)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualSahel, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSahel)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualSahel, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualSahel))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Sahel: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualSahel, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSahel)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualSahel, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSahel)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualSahel, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualSahel))^2))))

########### Sud-Ouest ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Sud-Ouest: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualSudOuest, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSudOuest)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualSudOuest, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSudOuest)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualSudOuest, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualSudOuest))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Sud-Ouest: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualSudOuest, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSudOuest)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualSudOuest, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSudOuest)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualSudOuest, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualSudOuest))^2))))

######################################################################################
################## Poverty Indicators: Poverty Gap Index (FGT1) ######################
######################################################################################

# Define a function to compute the ECDF
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# Define a function to compute the ECDF
percent3 <- function(x, digits = 3, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...))
}

########### Boucle du Mouhoun ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Boucle du Mouhoun: %s", percent3(sum((pcgapdeptotndpoorIndividualBoucleduMouhoun/PovertyLine)) * (1/TotalPopulationIndividualBoucleduMouhoun)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Boucle du Mouhoun: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun)) * TotalPoorIndividualBoucleduMouhoun)/TotalPopulationIndividualBoucleduMouhoun))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Boucle du Mouhoun: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun)) * TotalPoorIndividualBoucleduMouhoun)/TotalPopulationIndividualBoucleduMouhoun))

########### Cascades ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Cascades: %s", percent3(sum((pcgapdeptotndpoorIndividualCascades/PovertyLine)) * (1/TotalPopulationIndividualCascades)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Cascades: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades)) * TotalPoorIndividualCascades)/TotalPopulationIndividualCascades))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Cascades: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades)) * TotalPoorIndividualCascades)/TotalPopulationIndividualCascades))

########### Centre ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Centre: %s", percent3(sum((pcgapdeptotndpoorIndividualCentre/PovertyLine)) * (1/TotalPopulationIndividualCentre)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Centre: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre)) * TotalPoorIndividualCentre)/TotalPopulationIndividualCentre))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Centre: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre)) * TotalPoorIndividualCentre)/TotalPopulationIndividualCentre))

########### Centre-Est ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Centre-Est: %s", percent3(sum((pcgapdeptotndpoorIndividualCentreEst/PovertyLine)) * (1/TotalPopulationIndividualCentreEst)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Centre-Est: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst)) * TotalPoorIndividualCentreEst)/TotalPopulationIndividualCentreEst))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Centre-Est: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst)) * TotalPoorIndividualCentreEst)/TotalPopulationIndividualCentreEst))

########### Centre-Nord ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Centre-Nord: %s", percent3(sum((pcgapdeptotndpoorIndividualCentreNord/PovertyLine)) * (1/TotalPopulationIndividualCentreNord)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Centre-Nord: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord)) * TotalPoorIndividualCentreNord)/TotalPopulationIndividualCentreNord))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Centre-Nord: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord)) * TotalPoorIndividualCentreNord)/TotalPopulationIndividualCentreNord))

########### Centre-Ouest ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Centre-Ouest: %s", percent3(sum((pcgapdeptotndpoorIndividualCentreOuest/PovertyLine)) * (1/TotalPopulationIndividualCentreOuest)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Centre-Ouest: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest)) * TotalPoorIndividualCentreOuest)/TotalPopulationIndividualCentreOuest))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Centre-Ouest: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest)) * TotalPoorIndividualCentreOuest)/TotalPopulationIndividualCentreOuest))

########### Centre-Sud ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Centre-Sud: %s", percent3(sum((pcgapdeptotndpoorIndividualCentreSud/PovertyLine)) * (1/TotalPopulationIndividualCentreSud)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Centre-Sud: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud)) * TotalPoorIndividualCentreSud)/TotalPopulationIndividualCentreSud))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Centre-Sud: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud)) * TotalPoorIndividualCentreSud)/TotalPopulationIndividualCentreSud))

########### Est ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Est: %s", percent3(sum((pcgapdeptotndpoorIndividualEst/PovertyLine)) * (1/TotalPopulationIndividualEst)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Est: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst)) * TotalPoorIndividualEst)/TotalPopulationIndividualEst))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Est: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualEst)) * TotalPoorIndividualEst)/TotalPopulationIndividualEst))

########### Hauts-Bassins ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Hauts-Bassins: %s", percent3(sum((pcgapdeptotndpoorIndividualHautsBassins/PovertyLine)) * (1/TotalPopulationIndividualHautsBassins)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Hauts-Bassins: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins)) * TotalPoorIndividualHautsBassins)/TotalPopulationIndividualHautsBassins))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Hauts-Bassins: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins)) * TotalPoorIndividualHautsBassins)/TotalPopulationIndividualHautsBassins))

########### Nord ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Nord: %s", percent3(sum((pcgapdeptotndpoorIndividualNord/PovertyLine)) * (1/TotalPopulationIndividualNord)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Nord: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord)) * TotalPoorIndividualNord)/TotalPopulationIndividualNord))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Nord: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualNord)) * TotalPoorIndividualNord)/TotalPopulationIndividualNord))

########### Plateau Central ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Plateau Central: %s", percent3(sum((pcgapdeptotndpoorIndividualPlateauCentral/PovertyLine)) * (1/TotalPopulationIndividualPlateauCentral)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Plateau Central: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral)) * TotalPoorIndividualPlateauCentral)/TotalPopulationIndividualPlateauCentral))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Plateau Central: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral)) * TotalPoorIndividualPlateauCentral)/TotalPopulationIndividualPlateauCentral))

########### Sahel ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Sahel: %s", percent3(sum((pcgapdeptotndpoorIndividualSahel/PovertyLine)) * (1/TotalPopulationIndividualSahel)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Sahel: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel)) * TotalPoorIndividualSahel)/TotalPopulationIndividualSahel))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Sahel: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel)) * TotalPoorIndividualSahel)/TotalPopulationIndividualSahel))

########### Sud-Ouest ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Sud-Ouest: %s", percent3(sum((pcgapdeptotndpoorIndividualSudOuest/PovertyLine)) * (1/TotalPopulationIndividualSudOuest)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Sud-Ouest: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest)) * TotalPoorIndividualSudOuest)/TotalPopulationIndividualSudOuest))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Sud-Ouest: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest)) * TotalPoorIndividualSudOuest)/TotalPopulationIndividualSudOuest))

#######################################################################################
################### Poverty Indicators: Poverty Severity Index (FGT2) #################
#######################################################################################

########### Boucle du Mouhoun ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Boucle du Mouhoun: %s", percent3(sum((pcgapdeptotndpoorIndividualBoucleduMouhoun/PovertyLine)^2) * (1/TotalPopulationIndividualBoucleduMouhoun)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Boucle du Mouhoun: %s", percent3((TotalPoorIndividualBoucleduMouhoun * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun + 1)))/PovertyLine^2/TotalPopulationIndividualBoucleduMouhoun))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Boucle du Mouhoun: %s", percent3((TotalPoorIndividualBoucleduMouhoun * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun + 1)))/PovertyLine^2/TotalPopulationIndividualBoucleduMouhoun))

########### Cascades ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Cascades: %s", percent3(sum((pcgapdeptotndpoorIndividualCascades/PovertyLine)^2) * (1/TotalPopulationIndividualCascades)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Cascades: %s", percent3((TotalPoorIndividualCascades * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades + 1)))/PovertyLine^2/TotalPopulationIndividualCascades))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Cascades: %s", percent3((TotalPoorIndividualCascades * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades + 1)))/PovertyLine^2/TotalPopulationIndividualCascades))

########### Centre ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Centre: %s", percent3(sum((pcgapdeptotndpoorIndividualCentre/PovertyLine)^2) * (1/TotalPopulationIndividualCentre)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Centre: %s", percent3((TotalPoorIndividualCentre * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre + 1)))/PovertyLine^2/TotalPopulationIndividualCentre))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Centre: %s", percent3((TotalPoorIndividualCentre * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre + 1)))/PovertyLine^2/TotalPopulationIndividualCentre))

########### Centre-Est ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Centre-Est: %s", percent3(sum((pcgapdeptotndpoorIndividualCentreEst/PovertyLine)^2) * (1/TotalPopulationIndividualCentreEst)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Centre-Est: %s", percent3((TotalPoorIndividualCentreEst * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst + 1)))/PovertyLine^2/TotalPopulationIndividualCentreEst))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Centre-Est: %s", percent3((TotalPoorIndividualCentreEst * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst + 1)))/PovertyLine^2/TotalPopulationIndividualCentreEst))

########### Centre-Nord ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Centre-Nord: %s", percent3(sum((pcgapdeptotndpoorIndividualCentreNord/PovertyLine)^2) * (1/TotalPopulationIndividualCentreNord)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Centre-Nord: %s", percent3((TotalPoorIndividualCentreNord * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord + 1)))/PovertyLine^2/TotalPopulationIndividualCentreNord))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Centre-Nord: %s", percent3((TotalPoorIndividualCentreNord * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord + 1)))/PovertyLine^2/TotalPopulationIndividualCentreNord))

########### Centre-Ouest ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Centre-Ouest: %s", percent3(sum((pcgapdeptotndpoorIndividualCentreOuest/PovertyLine)^2) * (1/TotalPopulationIndividualCentreOuest)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Centre-Ouest: %s", percent3((TotalPoorIndividualCentreOuest * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest + 1)))/PovertyLine^2/TotalPopulationIndividualCentreOuest))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Centre-Ouest: %s", percent3((TotalPoorIndividualCentreOuest * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest + 1)))/PovertyLine^2/TotalPopulationIndividualCentreOuest))

########### Centre-Sud ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Centre-Sud: %s", percent3(sum((pcgapdeptotndpoorIndividualCentreSud/PovertyLine)^2) * (1/TotalPopulationIndividualCentreSud)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Centre-Sud: %s", percent3((TotalPoorIndividualCentreSud * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud + 1)))/PovertyLine^2/TotalPopulationIndividualCentreSud))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Centre-Sud: %s", percent3((TotalPoorIndividualCentreSud * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud + 1)))/PovertyLine^2/TotalPopulationIndividualCentreSud))

########### Est ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Est: %s", percent3(sum((pcgapdeptotndpoorIndividualEst/PovertyLine)^2) * (1/TotalPopulationIndividualEst)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Est: %s", percent3((TotalPoorIndividualEst * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst + 1)))/PovertyLine^2/TotalPopulationIndividualEst))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Est: %s", percent3((TotalPoorIndividualEst * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualEst + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualEst + 1)))/PovertyLine^2/TotalPopulationIndividualEst))

########### Hauts-Bassins ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Hauts-Bassins: %s", percent3(sum((pcgapdeptotndpoorIndividualHautsBassins/PovertyLine)^2) * (1/TotalPopulationIndividualHautsBassins)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Hauts-Bassins: %s", percent3((TotalPoorIndividualHautsBassins * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins + 1)))/PovertyLine^2/TotalPopulationIndividualHautsBassins))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Hauts-Bassins: %s", percent3((TotalPoorIndividualHautsBassins * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins + 1)))/PovertyLine^2/TotalPopulationIndividualHautsBassins))

########### Nord ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Nord: %s", percent3(sum((pcgapdeptotndpoorIndividualNord/PovertyLine)^2) * (1/TotalPopulationIndividualNord)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Nord: %s", percent3((TotalPoorIndividualNord * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord + 1)))/PovertyLine^2/TotalPopulationIndividualNord))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Nord: %s", percent3((TotalPoorIndividualNord * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualNord + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualNord + 1)))/PovertyLine^2/TotalPopulationIndividualNord))

########### Plateau Central ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Plateau Central: %s", percent3(sum((pcgapdeptotndpoorIndividualPlateauCentral/PovertyLine)^2) * (1/TotalPopulationIndividualPlateauCentral)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Plateau Central: %s", percent3((TotalPoorIndividualPlateauCentral * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral + 1)))/PovertyLine^2/TotalPopulationIndividualPlateauCentral))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Plateau Central: %s", percent3((TotalPoorIndividualPlateauCentral * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral + 1)))/PovertyLine^2/TotalPopulationIndividualPlateauCentral))

########### Sahel ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Sahel: %s", percent3(sum((pcgapdeptotndpoorIndividualSahel/PovertyLine)^2) * (1/TotalPopulationIndividualSahel)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Sahel: %s", percent3((TotalPoorIndividualSahel * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel + 1)))/PovertyLine^2/TotalPopulationIndividualSahel))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Sahel: %s", percent3((TotalPoorIndividualSahel * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel + 1)))/PovertyLine^2/TotalPopulationIndividualSahel))

########### Sud-Ouest ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Sud-Ouest: %s", percent3(sum((pcgapdeptotndpoorIndividualSudOuest/PovertyLine)^2) * (1/TotalPopulationIndividualSudOuest)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Sud-Ouest: %s", percent3((TotalPoorIndividualSudOuest * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest + 1)))/PovertyLine^2/TotalPopulationIndividualSudOuest))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Sud-Ouest: %s", percent3((TotalPoorIndividualSudOuest * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest + 1)))/PovertyLine^2/TotalPopulationIndividualSudOuest))

##################################################
########### Histogram vs. Density Plots ##########
##################################################

# We define a new variable where we are going to save the location where we would like to save the graphs we produce.
# This new variable is going to be called: GraphPathGraphPathHistograms.
GraphPathHistograms <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Burkina Faso/Graphs/Administrative Region/New Plots/Individual Level/Histograms"

# We set the GraphPathGraphPathHistograms's location as working directory and save it in the WorkingDirectory variable. Again, this is 
# the location were the graphs are going to be saved.
WorkingDirectory <- setwd(GraphPathHistograms)

# We set the labels of the ticks to non-scientific notation.
options(scipen = 10000)

# Set up a vector with different colors.
MyColors <- pal_jco()(10)

# Set up a vector with the expressions for the plot's legend.
my.expressions.hist <- vector()
my.expressions.hist <- c("Empirical Data", "$\\hat{\\alpha}_{\\scaleto{MLE}{2pt}}$", "$\\hat{\\alpha}_{\\scaleto{MME}{2pt}}$")

# We define a function for the density of the Generalized Beta Distribution 1 (GB1). We call this function: ddbeta.
ddbeta <- function(y, alpha, PovertyLine) {(alpha * (PovertyLine - y)^(alpha - 1))/PovertyLine^alpha}

########### Boucle du Mouhoun ##########

# We generate the .tex file
FileNameHistogramBoucleduMouhoun <- paste0('HistogramDefititatTrapping', 'BoucleduMouhoun.tex')
tikz(FileNameHistogramBoucleduMouhoun, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualBoucleduMouhoun, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000025), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualBoucleduMouhoun), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualBoucleduMouhoun), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Cascades ##########

# We generate the .tex file
FileNameHistogramCascades <- paste0('HistogramDefititatTrapping', 'Cascades.tex')
tikz(FileNameHistogramCascades, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualCascades, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000030), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCascades), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCascades), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Centre ##########

# We generate the .tex file
FileNameHistogramCentre <- paste0('HistogramDefititatTrapping', 'Centre.tex')
tikz(FileNameHistogramCentre, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualCentre, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000025), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCentre), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCentre), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Centre-Est ##########

# We generate the .tex file
FileNameHistogramCentreEst <- paste0('HistogramDefititatTrapping', 'CentreEst.tex')
tikz(FileNameHistogramCentreEst, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualCentreEst, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000020), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCentreEst), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCentreEst), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Centre-Nord ##########

# We generate the .tex file
FileNameHistogramCentreNord <- paste0('HistogramDefititatTrapping', 'CentreNord.tex')
tikz(FileNameHistogramCentreNord, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualCentreNord, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000025), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCentreNord), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCentreNord), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Centre-Ouest ##########

# We generate the .tex file
FileNameHistogramCentreOuest <- paste0('HistogramDefititatTrapping', 'CentreOuest.tex')
tikz(FileNameHistogramCentreOuest, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualCentreOuest, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000025), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCentreOuest), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCentreOuest), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Centre-Sud ##########

# We generate the .tex file
FileNameHistogramCentreSud <- paste0('HistogramDefititatTrapping', 'CentreSud.tex')
tikz(FileNameHistogramCentreSud, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualCentreSud, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000025), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCentreSud), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCentreSud), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Est ##########

# We generate the .tex file
FileNameHistogramEst <- paste0('HistogramDefititatTrapping', 'Est.tex')
tikz(FileNameHistogramEst, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualEst, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000025), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualEst), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualEst, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualEst), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Hauts-Bassins ##########

# We generate the .tex file
FileNameHistogramHautsBassins <- paste0('HistogramDefititatTrapping', 'HautsBassins.tex')
tikz(FileNameHistogramHautsBassins, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualHautsBassins, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000025), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualHautsBassins), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualHautsBassins), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Nord ##########

# We generate the .tex file
FileNameHistogramNord <- paste0('HistogramDefititatTrapping', 'Nord.tex')
tikz(FileNameHistogramNord, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualNord, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000020), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualNord), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualNord, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualNord), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Plateau Central ##########

# We generate the .tex file
FileNameHistogramPlateauCentral <- paste0('HistogramDefititatTrapping', 'PlateauCentral.tex')
tikz(FileNameHistogramPlateauCentral, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualPlateauCentral, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000025), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualPlateauCentral), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualPlateauCentral), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Sahel ##########

# We generate the .tex file
FileNameHistogramSahel <- paste0('HistogramDefititatTrapping', 'Sahel.tex')
tikz(FileNameHistogramSahel, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualSahel, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000030), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualSahel), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualSahel), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Sud-Ouest ##########

# We generate the .tex file
FileNameHistogramSudOuest <- paste0('HistogramDefititatTrapping', 'SudOuest.tex')
tikz(FileNameHistogramSudOuest, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualSudOuest, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000025), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualSudOuest), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualSudOuest), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

##################################################
################## QQ-Plots ######################
##################################################

# We define a new variable where we are going to save the location where we would like to save the graphs we produce.
# This new variable is going to be called: GraphPathQQPlots.
GraphPathQQPlots <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Burkina Faso/Graphs/Administrative Region/New Plots/Individual Level/QQ-Plots"

# We set the GraphPathQQPlots's location as working directory and save it in the WorkingDirectory variable. Again, this is 
# the location were the graphs are going to be saved.
WorkingDirectory <- setwd(GraphPathQQPlots)

# Set up a vector with the expressions for the plot's legend.
my.expressions.qq <- vector()
my.expressions.qq <- c("$\\hat{\\alpha}_{\\scaleto{MLE}{2pt}}$", "$\\hat{\\alpha}_{\\scaleto{MME}{2pt}}$")

########### Boucle du Mouhoun ##########

# We generate a vector with the sample quantiles.
samplequantilesBoucleduMouhoun = quantile(pcgapdeptotndpoorIndividualBoucleduMouhoun, probs = seq(0, 1, length = 1000))
unname(samplequantilesBoucleduMouhoun)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorBoucleduMouhoun <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorBoucleduMouhoun

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorBoucleduMouhoun <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorBoucleduMouhoun

# We generate the .tex file
FileNameQQPlotBoucleduMouhoun <- paste0('QQPlotDefititatTrapping', 'BoucleduMouhoun.tex')
tikz(FileNameQQPlotBoucleduMouhoun, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorBoucleduMouhoun, samplequantilesBoucleduMouhoun, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorBoucleduMouhoun, samplequantilesBoucleduMouhoun, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Cascades ##########

# We generate a vector with the sample quantiles.
samplequantilesCascades = quantile(pcgapdeptotndpoorIndividualCascades, probs = seq(0, 1, length = 1000))
unname(samplequantilesCascades)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorCascades <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorCascades

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorCascades <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorCascades

# We generate the .tex file
FileNameQQPlotCascades <- paste0('QQPlotDefititatTrapping', 'Cascades.tex')
tikz(FileNameQQPlotCascades, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorCascades, samplequantilesCascades, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorCascades, samplequantilesCascades, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Centre ##########

# We generate a vector with the sample quantiles.
samplequantilesCentre = quantile(pcgapdeptotndpoorIndividualCentre, probs = seq(0, 1, length = 1000))
unname(samplequantilesCentre)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorCentre <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorCentre

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorCentre <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorCentre

# We generate the .tex file
FileNameQQPlotCentre <- paste0('QQPlotDefititatTrapping', 'Centre.tex')
tikz(FileNameQQPlotCentre, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorCentre, samplequantilesCentre, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorCentre, samplequantilesCentre, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Centre-Est ##########

# We generate a vector with the sample quantiles.
samplequantilesCentreEst = quantile(pcgapdeptotndpoorIndividualCentreEst, probs = seq(0, 1, length = 1000))
unname(samplequantilesCentreEst)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorCentreEst <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorCentreEst

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorCentreEst <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorCentreEst

# We generate the .tex file
FileNameQQPlotCentreEst <- paste0('QQPlotDefititatTrapping', 'CentreEst.tex')
tikz(FileNameQQPlotCentreEst, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorCentreEst, samplequantilesCentreEst, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorCentreEst, samplequantilesCentreEst, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Centre-Nord ##########

# We generate a vector with the sample quantiles.
samplequantilesCentreNord = quantile(pcgapdeptotndpoorIndividualCentreNord, probs = seq(0, 1, length = 1000))
unname(samplequantilesCentreNord)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorCentreNord <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorCentreNord

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorCentreNord <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorCentreNord

# We generate the .tex file
FileNameQQPlotCentreNord <- paste0('QQPlotDefititatTrapping', 'CentreNord.tex')
tikz(FileNameQQPlotCentreNord, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorCentreNord, samplequantilesCentreNord, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorCentreNord, samplequantilesCentreNord, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Centre-Ouest ##########

# We generate a vector with the sample quantiles.
samplequantilesCentreOuest = quantile(pcgapdeptotndpoorIndividualCentreOuest, probs = seq(0, 1, length = 1000))
unname(samplequantilesCentreOuest)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorCentreOuest <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorCentreOuest

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorCentreOuest <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorCentreOuest

# We generate the .tex file
FileNameQQPlotCentreOuest <- paste0('QQPlotDefititatTrapping', 'CentreOuest.tex')
tikz(FileNameQQPlotCentreOuest, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorCentreOuest, samplequantilesCentreOuest, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorCentreOuest, samplequantilesCentreOuest, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Centre-Sud ##########

# We generate a vector with the sample quantiles.
samplequantilesCentreSud = quantile(pcgapdeptotndpoorIndividualCentreSud, probs = seq(0, 1, length = 1000))
unname(samplequantilesCentreSud)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorCentreSud <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorCentreSud

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorCentreSud <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorCentreSud

# We generate the .tex file
FileNameQQPlotCentreSud <- paste0('QQPlotDefititatTrapping', 'CentreSud.tex')
tikz(FileNameQQPlotCentreSud, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorCentreSud, samplequantilesCentreSud, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorCentreSud, samplequantilesCentreSud, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Est ##########

# We generate a vector with the sample quantiles.
samplequantilesEst = quantile(pcgapdeptotndpoorIndividualEst, probs = seq(0, 1, length = 1000))
unname(samplequantilesEst)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorEst <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorEst

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorEst <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualEst, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorEst

# We generate the .tex file
FileNameQQPlotEst <- paste0('QQPlotDefititatTrapping', 'Est.tex')
tikz(FileNameQQPlotEst, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorEst, samplequantilesEst, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorEst, samplequantilesEst, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Hauts-Bassins ##########

# We generate a vector with the sample quantiles.
samplequantilesHautsBassins = quantile(pcgapdeptotndpoorIndividualHautsBassins, probs = seq(0, 1, length = 1000))
unname(samplequantilesHautsBassins)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorHautsBassins <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorHautsBassins

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorHautsBassins <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorHautsBassins

# We generate the .tex file
FileNameQQPlotHautsBassins <- paste0('QQPlotDefititatTrapping', 'HautsBassins.tex')
tikz(FileNameQQPlotHautsBassins, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorHautsBassins, samplequantilesHautsBassins, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorHautsBassins, samplequantilesHautsBassins, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Nord ##########

# We generate a vector with the sample quantiles.
samplequantilesNord = quantile(pcgapdeptotndpoorIndividualNord, probs = seq(0, 1, length = 1000))
unname(samplequantilesNord)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorNord <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorNord

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorNord <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNord, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorNord

# We generate the .tex file
FileNameQQPlotNord <- paste0('QQPlotDefititatTrapping', 'Nord.tex')
tikz(FileNameQQPlotNord, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorNord, samplequantilesNord, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorNord, samplequantilesNord, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Plateau Central ##########

# We generate a vector with the sample quantiles.
samplequantilesPlateauCentral = quantile(pcgapdeptotndpoorIndividualPlateauCentral, probs = seq(0, 1, length = 1000))
unname(samplequantilesPlateauCentral)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorPlateauCentral <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorPlateauCentral

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorPlateauCentral <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorPlateauCentral

# We generate the .tex file
FileNameQQPlotPlateauCentral <- paste0('QQPlotDefititatTrapping', 'PlateauCentral.tex')
tikz(FileNameQQPlotPlateauCentral, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorPlateauCentral, samplequantilesPlateauCentral, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorPlateauCentral, samplequantilesPlateauCentral, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Sahel ##########

# We generate a vector with the sample quantiles.
samplequantilesSahel = quantile(pcgapdeptotndpoorIndividualSahel, probs = seq(0, 1, length = 1000))
unname(samplequantilesSahel)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorSahel <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorSahel

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorSahel <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorSahel

# We generate the .tex file
FileNameQQPlotSahel <- paste0('QQPlotDefititatTrapping', 'Sahel.tex')
tikz(FileNameQQPlotSahel, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorSahel, samplequantilesSahel, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorSahel, samplequantilesSahel, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Sud-Ouest ##########

# We generate a vector with the sample quantiles.
samplequantilesSudOuest = quantile(pcgapdeptotndpoorIndividualSudOuest, probs = seq(0, 1, length = 1000))
unname(samplequantilesSudOuest)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorSudOuest <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorSudOuest

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorSudOuest <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorSudOuest

# We generate the .tex file
FileNameQQPlotSudOuest <- paste0('QQPlotDefititatTrapping', 'SudOuest.tex')
tikz(FileNameQQPlotSudOuest, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorSudOuest, samplequantilesSudOuest, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorSudOuest, samplequantilesSudOuest, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

##################################################
################## CDF-Plots #####################
##################################################

# We define a new variable where we are going to save the location where we would like to save the graphs we produce.
# This new variable is going to be called: GraphPathCDFPlots.
GraphPathCDFPlots <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Burkina Faso/Graphs/Administrative Region/New Plots/Individual Level/CDF-Plots"

# We set the GraphPathCDFPlots's location as working directory and save it in the WorkingDirectory variable. Again, this is 
# the location were the graphs are going to be saved.
WorkingDirectory <- setwd(GraphPathCDFPlots)

# Set up a vector with the expressions for the plot's legend.
my.expressions.cdf <- vector()
my.expressions.cdf <- c("Empirical Data", "$\\hat{\\alpha}_{\\scaleto{MLE}{2pt}}$", "$\\hat{\\alpha}_{\\scaleto{MME}{2pt}}$")

########### Boucle du Mouhoun ##########

# We generate the .tex file
FileNameCDFPlotBoucleduMouhoun <- paste0('CDFPlotDefititatTrapping', 'TotalPoorBoucleduMouhoun.tex')
tikz(FileNameCDFPlotBoucleduMouhoun, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualBoucleduMouhoun), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Cascades ##########

# We generate the .tex file
FileNameCDFPlotCascades <- paste0('CDFPlotDefititatTrapping', 'TotalPoorCascades.tex')
tikz(FileNameCDFPlotCascades, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualCascades), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Centre ##########

# We generate the .tex file
FileNameCDFPlotCentre <- paste0('CDFPlotDefititatTrapping', 'TotalPoorCentre.tex')
tikz(FileNameCDFPlotCentre, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualCentre), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Centre-Est ##########

# We generate the .tex file
FileNameCDFPlotCentreEst <- paste0('CDFPlotDefititatTrapping', 'TotalPoorCentreEst.tex')
tikz(FileNameCDFPlotCentreEst, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualCentreEst), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Centre-Nord ##########

# We generate the .tex file
FileNameCDFPlotCentreNord <- paste0('CDFPlotDefititatTrapping', 'TotalPoorCentreNord.tex')
tikz(FileNameCDFPlotCentreNord, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualCentreNord), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Centre-Ouest ##########

# We generate the .tex file
FileNameCDFPlotCentreOuest <- paste0('CDFPlotDefititatTrapping', 'TotalPoorCentreOuest.tex')
tikz(FileNameCDFPlotCentreOuest, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualCentreOuest), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Centre-Sud ##########

# We generate the .tex file
FileNameCDFPlotCentreSud <- paste0('CDFPlotDefititatTrapping', 'TotalPoorCentreSud.tex')
tikz(FileNameCDFPlotCentreSud, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualCentreSud), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Est ##########

# We generate the .tex file
FileNameCDFPlotEst <- paste0('CDFPlotDefititatTrapping', 'TotalPoorEst.tex')
tikz(FileNameCDFPlotEst, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualEst), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Hauts-Bassins ##########

# We generate the .tex file
FileNameCDFPlotHautsBassins <- paste0('CDFPlotDefititatTrapping', 'TotalPoorHautsBassins.tex')
tikz(FileNameCDFPlotHautsBassins, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualHautsBassins), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Nord ##########

# We generate the .tex file
FileNameCDFPlotNord <- paste0('CDFPlotDefititatTrapping', 'TotalPoorNord.tex')
tikz(FileNameCDFPlotNord, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualNord), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Plateau Central ##########

# We generate the .tex file
FileNameCDFPlotPlateauCentral <- paste0('CDFPlotDefititatTrapping', 'TotalPoorPlateauCentral.tex')
tikz(FileNameCDFPlotPlateauCentral, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualPlateauCentral), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Sahel ##########

# We generate the .tex file
FileNameCDFPlotSahel <- paste0('CDFPlotDefititatTrapping', 'TotalPoorSahel.tex')
tikz(FileNameCDFPlotSahel, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualSahel), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Sud-Ouest ##########

# We generate the .tex file
FileNameCDFPlotSudOuest <- paste0('CDFPlotDefititatTrapping', 'TotalPoorSudOuest.tex')
tikz(FileNameCDFPlotSudOuest, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualSudOuest), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

##################################################
################## PP-Plots ######################
##################################################

# We define a new variable where we are going to save the location where we would like to save the graphs we produce.
# This new variable is going to be called: GraphPathPPPlots.
GraphPathPPPlots <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Burkina Faso/Graphs/Administrative Region/New Plots/Individual Level/PP-Plots"

# We set the GraphPathPPPlots's location as working directory and save it in the WorkingDirectory variable. Again, this is 
# the location were the graphs are going to be saved.
WorkingDirectory <- setwd(GraphPathPPPlots)

# Set up a vector with the expressions for the plot's legend.
my.expressions.pp <- vector()
my.expressions.pp <- c("$\\hat{\\alpha}_{\\scaleto{MLE}{2pt}}$", "$\\hat{\\alpha}_{\\scaleto{MME}{2pt}}$")

########### Boucle du Mouhoun ##########

# We generate the .tex file
FileNamePPPlotBoucleduMouhoun <- paste0('PPPlotDefititatTrapping', 'TotalPoorBoucleduMouhoun.tex')
tikz(FileNamePPPlotBoucleduMouhoun, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorBoucleduMouhoun <- ppbeta(pcgapdeptotndpoorIndividualBoucleduMouhoun, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorBoucleduMouhoun <- ppbeta(pcgapdeptotndpoorIndividualBoucleduMouhoun, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBoucleduMouhoun, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualBoucleduMouhoun)), sort(probDistMaximumLikelihoodEstimatorBoucleduMouhoun), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualBoucleduMouhoun)), sort(probDistMethodofMomentsEstimatorBoucleduMouhoun), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Cascades ##########

# We generate the .tex file
FileNamePPPlotCascades <- paste0('PPPlotDefititatTrapping', 'TotalPoorCascades.tex')
tikz(FileNamePPPlotCascades, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorCascades <- ppbeta(pcgapdeptotndpoorIndividualCascades, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorCascades <- ppbeta(pcgapdeptotndpoorIndividualCascades, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCascades, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualCascades)), sort(probDistMaximumLikelihoodEstimatorCascades), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualCascades)), sort(probDistMethodofMomentsEstimatorCascades), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Centre ##########

# We generate the .tex file
FileNamePPPlotCentre <- paste0('PPPlotDefititatTrapping', 'TotalPoorCentre.tex')
tikz(FileNamePPPlotCentre, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorCentre <- ppbeta(pcgapdeptotndpoorIndividualCentre, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorCentre <- ppbeta(pcgapdeptotndpoorIndividualCentre, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualCentre)), sort(probDistMaximumLikelihoodEstimatorCentre), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualCentre)), sort(probDistMethodofMomentsEstimatorCentre), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Centre-Est ##########

# We generate the .tex file
FileNamePPPlotCentreEst <- paste0('PPPlotDefititatTrapping', 'TotalPoorCentreEst.tex')
tikz(FileNamePPPlotCentreEst, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorCentreEst <- ppbeta(pcgapdeptotndpoorIndividualCentreEst, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorCentreEst <- ppbeta(pcgapdeptotndpoorIndividualCentreEst, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreEst, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualCentreEst)), sort(probDistMaximumLikelihoodEstimatorCentreEst), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualCentreEst)), sort(probDistMethodofMomentsEstimatorCentreEst), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Centre-Nord ##########

# We generate the .tex file
FileNamePPPlotCentreNord <- paste0('PPPlotDefititatTrapping', 'TotalPoorCentreNord.tex')
tikz(FileNamePPPlotCentreNord, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorCentreNord <- ppbeta(pcgapdeptotndpoorIndividualCentreNord, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorCentreNord <- ppbeta(pcgapdeptotndpoorIndividualCentreNord, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualCentreNord)), sort(probDistMaximumLikelihoodEstimatorCentreNord), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualCentreNord)), sort(probDistMethodofMomentsEstimatorCentreNord), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Centre-Ouest ##########

# We generate the .tex file
FileNamePPPlotCentreOuest <- paste0('PPPlotDefititatTrapping', 'TotalPoorCentreOuest.tex')
tikz(FileNamePPPlotCentreOuest, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorCentreOuest <- ppbeta(pcgapdeptotndpoorIndividualCentreOuest, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorCentreOuest <- ppbeta(pcgapdeptotndpoorIndividualCentreOuest, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreOuest, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualCentreOuest)), sort(probDistMaximumLikelihoodEstimatorCentreOuest), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualCentreOuest)), sort(probDistMethodofMomentsEstimatorCentreOuest), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Centre-Sud ##########

# We generate the .tex file
FileNamePPPlotCentreSud <- paste0('PPPlotDefititatTrapping', 'TotalPoorCentreSud.tex')
tikz(FileNamePPPlotCentreSud, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorCentreSud <- ppbeta(pcgapdeptotndpoorIndividualCentreSud, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorCentreSud <- ppbeta(pcgapdeptotndpoorIndividualCentreSud, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualCentreSud)), sort(probDistMaximumLikelihoodEstimatorCentreSud), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualCentreSud)), sort(probDistMethodofMomentsEstimatorCentreSud), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Est ##########

# We generate the .tex file
FileNamePPPlotEst <- paste0('PPPlotDefititatTrapping', 'TotalPoorEst.tex')
tikz(FileNamePPPlotEst, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorEst <- ppbeta(pcgapdeptotndpoorIndividualEst, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorEst <- ppbeta(pcgapdeptotndpoorIndividualEst, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualEst, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualEst)), sort(probDistMaximumLikelihoodEstimatorEst), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualEst)), sort(probDistMethodofMomentsEstimatorEst), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Hauts-Bassins ##########

# We generate the .tex file
FileNamePPPlotHautsBassins <- paste0('PPPlotDefititatTrapping', 'TotalPoorHautsBassins.tex')
tikz(FileNamePPPlotHautsBassins, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorHautsBassins <- ppbeta(pcgapdeptotndpoorIndividualHautsBassins, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorHautsBassins <- ppbeta(pcgapdeptotndpoorIndividualHautsBassins, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualHautsBassins)), sort(probDistMaximumLikelihoodEstimatorHautsBassins), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualHautsBassins)), sort(probDistMethodofMomentsEstimatorHautsBassins), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Nord ##########

# We generate the .tex file
FileNamePPPlotNord <- paste0('PPPlotDefititatTrapping', 'TotalPoorNord.tex')
tikz(FileNamePPPlotNord, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorNord <- ppbeta(pcgapdeptotndpoorIndividualNord, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorNord <- ppbeta(pcgapdeptotndpoorIndividualNord, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNord, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualNord)), sort(probDistMaximumLikelihoodEstimatorNord), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualNord)), sort(probDistMethodofMomentsEstimatorNord), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Plateau Central ##########

# We generate the .tex file
FileNamePPPlotPlateauCentral <- paste0('PPPlotDefititatTrapping', 'TotalPoorPlateauCentral.tex')
tikz(FileNamePPPlotPlateauCentral, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorPlateauCentral <- ppbeta(pcgapdeptotndpoorIndividualPlateauCentral, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorPlateauCentral <- ppbeta(pcgapdeptotndpoorIndividualPlateauCentral, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPlateauCentral, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualPlateauCentral)), sort(probDistMaximumLikelihoodEstimatorPlateauCentral), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualPlateauCentral)), sort(probDistMethodofMomentsEstimatorPlateauCentral), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Sahel ##########

# We generate the .tex file
FileNamePPPlotSahel <- paste0('PPPlotDefititatTrapping', 'TotalPoorSahel.tex')
tikz(FileNamePPPlotSahel, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorSahel <- ppbeta(pcgapdeptotndpoorIndividualSahel, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorSahel <- ppbeta(pcgapdeptotndpoorIndividualSahel, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualSahel)), sort(probDistMaximumLikelihoodEstimatorSahel), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualSahel)), sort(probDistMethodofMomentsEstimatorSahel), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Sud-Ouest ##########

# We generate the .tex file
FileNamePPPlotSudOuest <- paste0('PPPlotDefititatTrapping', 'TotalPoorSudOuest.tex')
tikz(FileNamePPPlotSudOuest, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorSudOuest <- ppbeta(pcgapdeptotndpoorIndividualSudOuest, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorSudOuest <- ppbeta(pcgapdeptotndpoorIndividualSudOuest, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudOuest, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualSudOuest)), sort(probDistMaximumLikelihoodEstimatorSudOuest), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualSudOuest)), sort(probDistMethodofMomentsEstimatorSudOuest), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

##############################################################
################## Sensitivity Analysis ######################
##############################################################

# We define a new variable where we are going to save the location where we would like to save the graphs we produce.
# This new variable is going to be called: GraphPathSensitivityPlots.
GraphPathSensitivityPlots <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Burkina Faso/Graphs/Administrative Region/New Plots/Individual Level/SensitivityPlots"

# We set the GraphPathPPPlots's location as working directory and save it in the WorkingDirectory variable. Again, this is 
# the location were the graphs are going to be saved.
WorkingDirectory <- setwd(GraphPathSensitivityPlots)

# Set up a vector with different types of lines for each region.

MyLines <-seq(from = 1, to = 6, by = 1)

# Set up a vector with the expressions for the plot's legend.

my.expressions.sens <- c("Centre", "Centre-Nord", "Centre-Sud", "Hauts-Bassins", "Sahel")

# We generate the .tex file
FileNameSensitivityPovertyGapIndex <- paste0('SensitivityPovertyGapIndexAlpha', '.tex')
tikz(FileNameSensitivityPovertyGapIndex, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
alpha <- seq(0.001, 6, length = 1000)
plot(alpha, ((1/(1 + alpha)) * TotalPoorIndividualCentre)/TotalPopulationIndividualCentre, xaxs = "i", yaxs = "i", xlim= c(0, max(alpha)), ylim= c(0, 0.5), xlab = "$\\alpha$", ylab = "Poverty Gap Index ($FGT_{1}$)", cex.lab = 1, lty = MyLines[1], lwd = 2, col = MyColors[1], cex.lab = 1, type = "l", cex = 0.5, xaxt = "n", yaxt = "n")
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre, ((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre)) * TotalPoorIndividualCentre)/TotalPopulationIndividualCentre, col = MyColors[1], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre, ((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre)) * TotalPoorIndividualCentre)/TotalPopulationIndividualCentre, col = MyColors[1], type = "p", pch = 2)
lines(alpha, ((1/(1 + alpha)) * TotalPoorIndividualCentreNord)/TotalPopulationIndividualCentreNord, lty = MyLines[2], lwd = 2, col = MyColors[2])
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord, ((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord)) * TotalPoorIndividualCentreNord)/TotalPopulationIndividualCentreNord, col = MyColors[2], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord, ((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord)) * TotalPoorIndividualCentreNord)/TotalPopulationIndividualCentreNord, col = MyColors[2], type = "p", pch = 2)
lines(alpha, ((1/(1 + alpha)) * TotalPoorIndividualCentreSud)/TotalPopulationIndividualCentreSud, lty = MyLines[3], lwd = 2, col = MyColors[3])
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud, ((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud)) * TotalPoorIndividualCentreSud)/TotalPopulationIndividualCentreSud, col = MyColors[3], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud, ((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud)) * TotalPoorIndividualCentreSud)/TotalPopulationIndividualCentreSud, col = MyColors[3], type = "p", pch = 2)
lines(alpha, ((1/(1 + alpha)) * TotalPoorIndividualHautsBassins)/TotalPopulationIndividualHautsBassins, lty = MyLines[4], lwd = 2, col = MyColors[4])
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins, ((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins)) * TotalPoorIndividualHautsBassins)/TotalPopulationIndividualHautsBassins, col = MyColors[4], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins, ((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins)) * TotalPoorIndividualHautsBassins)/TotalPopulationIndividualHautsBassins, col = MyColors[4], type = "p", pch = 2)
lines(alpha, ((1/(1 + alpha)) * TotalPoorIndividualSahel)/TotalPopulationIndividualSahel, lty = MyLines[5], lwd = 2, col = MyColors[5])
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel, ((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel)) * TotalPoorIndividualSahel)/TotalPopulationIndividualSahel, col = MyColors[5], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel, ((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel)) * TotalPoorIndividualSahel)/TotalPopulationIndividualSahel, col = MyColors[5], type = "p", pch = 2)
axis(side = 1, at = c(0, 1, 2, 3, 4, 5, 6), labels = c(0, 1, 2, 3, 4, 5, 6), cex.axis = 0.7)
axis(side = 2, at = c(0, 0.10, 0.20, 0.30, 0.40, 0.50), labels = c(0, 0.10, 0.20, 0.30, 0.40, 0.50), cex.axis = 0.7)
legend("topright", inset = 0.02, legend = my.expressions.sens, lty = MyLines, lwd = 3, col = MyColors, cex = 0.8)
dev.off()

# We generate the .tex file
FileNameSensitivityPovertySeverityIndex <- paste0('SensitivityPovertySeverityIndexAlpha', '.tex')
tikz(FileNameSensitivityPovertySeverityIndex, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(alpha, (TotalPoorIndividualCentre * (2 * PovertyLine^2)/((alpha + 2) * (alpha + 1)))/PovertyLine^2/TotalPopulationIndividualCentre, xaxs = "i", yaxs = "i", xlim= c(0, max(alpha)), ylim= c(0, 0.5), xlab = "$\\alpha$", ylab = "Poverty Severity Index ($FGT_{2}$)", cex.lab = 1, lty = MyLines[1], lwd = 2, col = MyColors[1], cex.lab = 1, type = "l", cex = 0.5, xaxt = "n", yaxt = "n")
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre, (TotalPoorIndividualCentre * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentre + 1)))/PovertyLine^2/TotalPopulationIndividualCentre, col = MyColors[1], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre, (TotalPoorIndividualCentre * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualCentre + 1)))/PovertyLine^2/TotalPopulationIndividualCentre, col = MyColors[1], type = "p", pch = 2)
lines(alpha, (TotalPoorIndividualCentreNord * (2 * PovertyLine^2)/((alpha + 2) * (alpha + 1)))/PovertyLine^2/TotalPopulationIndividualCentreNord, xaxs = "i", yaxs = "i", xlim= c(0, max(alpha)), ylim= c(0, 0.5), xlab = "$\\alpha$", ylab = "Poverty Severity Index ($FGT_{2}$)", cex.lab = 1, lty = MyLines[2], lwd = 2, col = MyColors[2], cex.lab = 1, type = "l", cex = 0.5, xaxt = "n", yaxt = "n")
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord, (TotalPoorIndividualCentreNord * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreNord + 1)))/PovertyLine^2/TotalPopulationIndividualCentreNord, col = MyColors[2], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord, (TotalPoorIndividualCentreNord * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreNord + 1)))/PovertyLine^2/TotalPopulationIndividualCentreNord, col = MyColors[2], type = "p", pch = 2)
lines(alpha, (TotalPoorIndividualCentreSud * (2 * PovertyLine^2)/((alpha + 2) * (alpha + 1)))/PovertyLine^2/TotalPopulationIndividualCentreSud, xaxs = "i", yaxs = "i", xlim= c(0, max(alpha)), ylim= c(0, 0.5), xlab = "$\\alpha$", ylab = "Poverty Severity Index ($FGT_{2}$)", cex.lab = 1, lty = MyLines[3], lwd = 2, col = MyColors[3], cex.lab = 1, type = "l", cex = 0.5, xaxt = "n", yaxt = "n")
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud, (TotalPoorIndividualCentreSud * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCentreSud + 1)))/PovertyLine^2/TotalPopulationIndividualCentreSud, col = MyColors[3], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud, (TotalPoorIndividualCentreSud * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualCentreSud + 1)))/PovertyLine^2/TotalPopulationIndividualCentreSud, col = MyColors[3], type = "p", pch = 2)
lines(alpha, (TotalPoorIndividualHautsBassins * (2 * PovertyLine^2)/((alpha + 2) * (alpha + 1)))/PovertyLine^2/TotalPopulationIndividualHautsBassins, xaxs = "i", yaxs = "i", xlim= c(0, max(alpha)), ylim= c(0, 0.5), xlab = "$\\alpha$", ylab = "Poverty Severity Index ($FGT_{2}$)", cex.lab = 1, lty = MyLines[4], lwd = 2, col = MyColors[4], cex.lab = 1, type = "l", cex = 0.5, xaxt = "n", yaxt = "n")
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins, (TotalPoorIndividualHautsBassins * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautsBassins + 1)))/PovertyLine^2/TotalPopulationIndividualHautsBassins, col = MyColors[4], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins, (TotalPoorIndividualHautsBassins * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualHautsBassins + 1)))/PovertyLine^2/TotalPopulationIndividualHautsBassins, col = MyColors[4], type = "p", pch = 2)
lines(alpha, (TotalPoorIndividualSahel * (2 * PovertyLine^2)/((alpha + 2) * (alpha + 1)))/PovertyLine^2/TotalPopulationIndividualSahel, xaxs = "i", yaxs = "i", xlim= c(0, max(alpha)), ylim= c(0, 0.5), xlab = "$\\alpha$", ylab = "Poverty Severity Index ($FGT_{2}$)", cex.lab = 1, lty = MyLines[5], lwd = 2, col = MyColors[5], cex.lab = 1, type = "l", cex = 0.5, xaxt = "n", yaxt = "n")
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel, (TotalPoorIndividualSahel * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSahel + 1)))/PovertyLine^2/TotalPopulationIndividualSahel, col = MyColors[5], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel, (TotalPoorIndividualSahel * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualSahel + 1)))/PovertyLine^2/TotalPopulationIndividualSahel, col = MyColors[5], type = "p", pch = 2)
axis(side = 1, at = c(0, 1, 2, 3, 4, 5, 6), labels = c(0, 1, 2, 3, 4, 5, 6), cex.axis = 0.7)
axis(side = 2, at = c(0, 0.10, 0.20, 0.30, 0.40, 0.50), labels = c(0, 0.10, 0.20, 0.30, 0.40, 0.50), cex.axis = 0.7)
legend("topright", inset = 0.02, legend = my.expressions.sens, lty = MyLines, lwd = 3, col = MyColors, cex = 0.8)
dev.off()
