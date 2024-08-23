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

# Now, we create two more data sets (per each area of residence). This is the approach we follow and it will help to estimate measures for the
# different areas of residence (note that other approaches could be followed to perform the analysis below).

emc2014_welfareRural <- emc2014_welfare %>% filter(milieu == "Rural")
emc2014_welfareUrban <- emc2014_welfare %>% filter(milieu == "Urbain")

emc2014_welfareIndividualRural <- emc2014_welfare2 %>% filter(milieu == "Rural")
emc2014_welfareIndividualUrban <- emc2014_welfare2 %>% filter(milieu == "Urbain")

# Then, we need to remove from all data frames the records with NAs, i.e. those records with people 
# that is not poor. Therefore, we remove the NAs and we save the values of interest (in this case, the pcgapdeptotndpoor) in the variable: 
# pcgapdeptotndpoor. We do this per area of residence for both of the data frames previously created.

pcgapdeptotndpoorBurkinaFaso <- emc2014_welfare$pcgapdeptotndpoor 
pcgapdeptotndpoorBurkinaFaso <- pcgapdeptotndpoorBurkinaFaso[!is.na(pcgapdeptotndpoorBurkinaFaso)]
pcgapdeptotndpoorRural <- emc2014_welfareRural$pcgapdeptotndpoor 
pcgapdeptotndpoorRural <- pcgapdeptotndpoorRural[!is.na(pcgapdeptotndpoorRural)]
pcgapdeptotndpoorUrban <- emc2014_welfareUrban$pcgapdeptotndpoor 
pcgapdeptotndpoorUrban <- pcgapdeptotndpoorUrban[!is.na(pcgapdeptotndpoorUrban)]

pcgapdeptotndpoorIndividualBurkinaFaso <- emc2014_welfare2$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualBurkinaFaso <- pcgapdeptotndpoorIndividualBurkinaFaso[!is.na(pcgapdeptotndpoorIndividualBurkinaFaso)]
pcgapdeptotndpoorIndividualRural <- emc2014_welfareIndividualRural$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualRural <- pcgapdeptotndpoorIndividualRural[!is.na(pcgapdeptotndpoorIndividualRural)]
pcgapdeptotndpoorIndividualUrban <- emc2014_welfareIndividualUrban$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualUrban <- pcgapdeptotndpoorIndividualUrban[!is.na(pcgapdeptotndpoorIndividualUrban)]

# We check the first ten values for the new calculated variables: pcgapdeptotndpoor.

pcgapdeptotndpoorBurkinaFaso[1:10]
pcgapdeptotndpoorRural[1:10]
pcgapdeptotndpoorUrban[1:10]

pcgapdeptotndpoorIndividualBurkinaFaso[1:10]
pcgapdeptotndpoorIndividualRural[1:10]
pcgapdeptotndpoorIndividualUrban[1:10]

###################################################
### Calculation of Population (# of households) ###
###################################################

# We calculate the total population in Burkina Faso (# of households).

TotalPopulation <- emc2014_welfare %>% count(milieu) %>% filter(milieu == "Rural") %>% pull(n) + emc2014_welfare %>% count(milieu) %>% filter(milieu == "Urbain") %>% pull(n)
TotalPopulation

# We calculate the total population in the area of residence: Rural  (# of households).

TotalPopulationRural <- emc2014_welfare %>% count(milieu) %>% filter(milieu == "Rural") %>% pull(n)
TotalPopulationRural

# We calculate the total population in the area of residence: Urban  (# of households).

TotalPopulationUrban <- emc2014_welfare %>% count(milieu) %>% filter(milieu == "Urbain") %>% pull(n)
TotalPopulationUrban

##########################################################
#### Calculation of Poor Population (# of households) ####
##########################################################

# We calculate the total number of poor households in Burkina Faso (# of households).

TotalPoorBurkinaFaso <- length(pcgapdeptotndpoorBurkinaFaso)
TotalPoorBurkinaFaso

# We calculate the total number of poor households in the area of residence: Rural  (# of households).

TotalPoorRural <- length(pcgapdeptotndpoorRural)
TotalPoorRural

# We calculate the total number of poor households in the area of residence: Urban  (# of households).

TotalPoorUrban <- length(pcgapdeptotndpoorUrban)
TotalPoorUrban

####################################################
### Calculation of Population (# of individuals) ###
####################################################

# We calculate the total population in Burkina Faso (# of individuals).

TotalPopulationIndividualBurkinaFaso <- emc2014_welfare2 %>% count(milieu) %>% filter(milieu == "Rural") %>% pull(n) + emc2014_welfare2 %>% count(milieu) %>% filter(milieu == "Urbain") %>% pull(n) 
TotalPopulationIndividualBurkinaFaso

# We calculate the total population in the area of residence: Rural  (# of individuals).

TotalPopulationIndividualRural <- emc2014_welfare2 %>% count(milieu) %>% filter(milieu == "Rural") %>% pull(n)
TotalPopulationIndividualRural

# We calculate the total population in the area of residence: Urban  (# of individuals).

TotalPopulationIndividualUrban <- emc2014_welfare2 %>% count(milieu) %>% filter(milieu == "Urbain") %>% pull(n)
TotalPopulationIndividualUrban

############################################################
##### Calculation of Poor Population (# of individuals) ####
############################################################

# We calculate the total number of poor households in Burkina Faso (# of individuals).

TotalPoorIndividualBurkinaFaso <- length(pcgapdeptotndpoorIndividualBurkinaFaso)
TotalPoorIndividualBurkinaFaso

# We calculate the total number of poor households in the area of residence: Rural (# of individuals).

TotalPoorIndividualRural <- length(pcgapdeptotndpoorIndividualRural)
TotalPoorIndividualRural

# We calculate the total number of poor households in the area of residence: Urban (# of individuals).

TotalPoorIndividualUrban <- length(pcgapdeptotndpoorIndividualUrban)
TotalPoorIndividualUrban

##################################################
## Estimation of Parameters  (# of individuals) ##
##################################################

# We define a function to print values with two decimals.

percent2 <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...))
}

########### Burkina Faso ##########

# For Burkina Faso, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso <- TotalPoorIndividualBurkinaFaso/(TotalPoorIndividualBurkinaFaso * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualBurkinaFaso)))
sprintf("Maximum Likelihood Estimator of α for Burkina Faso: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso))

# For Burkina Faso, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso <- (PovertyLine - mean(pcgapdeptotndpoorIndividualBurkinaFaso))/mean(pcgapdeptotndpoorIndividualBurkinaFaso)
sprintf("Method of Moments Estimator of α for Burkina Faso: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso))

########### Rural ##########

# For Rural, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural <- TotalPoorIndividualRural/(TotalPoorIndividualRural * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualRural)))
sprintf("Maximum Likelihood Estimator of α for Rural: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural))

# For Rural, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualRural <- (PovertyLine - mean(pcgapdeptotndpoorIndividualRural))/mean(pcgapdeptotndpoorIndividualRural)
sprintf("Method of Moments Estimator of α for Rural: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualRural))

########### Urban ##########

# For Urban, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban <- TotalPoorIndividualUrban/(TotalPoorIndividualUrban * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualUrban)))
sprintf("Maximum Likelihood Estimator of α for Urban: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban))

# For Urban, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban <- (PovertyLine - mean(pcgapdeptotndpoorIndividualUrban))/mean(pcgapdeptotndpoorIndividualUrban)
sprintf("Method of Moments Estimator of α for Urban: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban))

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

########### Burkina Faso ##########

ecdfBurkinaFaso <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualBurkinaFaso)))

# Maximum Likelihood Estimator (MLE) - KS test for Burkina Faso: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBurkinaFaso)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorBurkinaFaso <- DStatisticEstimation(ecdfBurkinaFaso, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Burkina Faso: %s", MprimeMaximumLikelihoodEstimatorBurkinaFaso)

PValueMaximumLikelihoodEstimatorBurkinaFaso <- PValueEstimation(pcgapdeptotndpoorIndividualBurkinaFaso, MprimeMaximumLikelihoodEstimatorBurkinaFaso, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Burkina Faso: %s", PValueMaximumLikelihoodEstimatorBurkinaFaso)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBurkinaFaso, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Burkina Faso:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBurkinaFaso)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorBurkinaFaso <- DStatisticEstimation(ecdfBurkinaFaso, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Burkina Faso: %s", MprimeMethodofMomentsEstimatorBurkinaFaso)

PValueMethodofMomentsEstimatorBurkinaFaso <- PValueEstimation(pcgapdeptotndpoorIndividualBurkinaFaso, MprimeMethodofMomentsEstimatorBurkinaFaso, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Burkina Faso: %s", PValueMethodofMomentsEstimatorBurkinaFaso)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBurkinaFaso, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine) # two-sided, exact

########### Rural ##########

ecdfRural <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualRural)))

# Maximum Likelihood Estimator (MLE) - KS test for Rural: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualRural)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorRural <- DStatisticEstimation(ecdfRural, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Rural: %s", MprimeMaximumLikelihoodEstimatorRural)

PValueMaximumLikelihoodEstimatorRural <- PValueEstimation(pcgapdeptotndpoorIndividualRural, MprimeMaximumLikelihoodEstimatorRural, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Rural: %s", PValueMaximumLikelihoodEstimatorRural)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorRural, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Rural:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualRural <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualRural)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorRural <- DStatisticEstimation(ecdfRural, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualRural)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Rural: %s", MprimeMethodofMomentsEstimatorRural)

PValueMethodofMomentsEstimatorRural <- PValueEstimation(pcgapdeptotndpoorIndividualRural, MprimeMethodofMomentsEstimatorRural, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Rural: %s", PValueMethodofMomentsEstimatorRural)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorRural, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) # two-sided, exact

########### Urban ##########

ecdfUrban <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualUrban)))

# Maximum Likelihood Estimator (MLE) - KS test for Urban: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualUrban)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorUrban <- DStatisticEstimation(ecdfUrban, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Urban: %s", MprimeMaximumLikelihoodEstimatorUrban)

PValueMaximumLikelihoodEstimatorUrban <- PValueEstimation(pcgapdeptotndpoorIndividualUrban, MprimeMaximumLikelihoodEstimatorUrban, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Urban: %s", PValueMaximumLikelihoodEstimatorUrban)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorUrban, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Urban:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualUrban <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualUrban)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorUrban <- DStatisticEstimation(ecdfUrban, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualUrban)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Urban: %s", MprimeMethodofMomentsEstimatorUrban)

PValueMethodofMomentsEstimatorUrban <- PValueEstimation(pcgapdeptotndpoorIndividualUrban, MprimeMethodofMomentsEstimatorUrban, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Urban: %s", PValueMethodofMomentsEstimatorUrban)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorUrban, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) # two-sided, exact

###################################################
################## R-SQUARED ######################
###################################################

# Define a function to compute the ECDF
percent4 <- function(x, digits = 4, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...))
}

########### Burkina Faso ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Burkina Faso: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBurkinaFaso, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBurkinaFaso)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBurkinaFaso, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBurkinaFaso)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBurkinaFaso, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBurkinaFaso))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Burkina Faso: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBurkinaFaso, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBurkinaFaso)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBurkinaFaso, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBurkinaFaso)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBurkinaFaso, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBurkinaFaso))^2))))

########### Rural ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Rural: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualRural)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualRural)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualRural))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Rural: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualRural)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualRural)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualRural))^2))))

########### Urban ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Urban: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualUrban)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualUrban)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualUrban))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Urban: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualUrban)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualUrban)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualUrban))^2))))

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

########### Burkina Faso ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Burkina Faso: %s", percent3(sum((pcgapdeptotndpoorIndividualBurkinaFaso/PovertyLine)) * (1/TotalPopulationIndividualBurkinaFaso)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Burkina Faso: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso)) * TotalPoorIndividualBurkinaFaso)/TotalPopulationIndividualBurkinaFaso))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Burkina Faso: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso)) * TotalPoorIndividualBurkinaFaso)/TotalPopulationIndividualBurkinaFaso))

########### Rural ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Rural: %s", percent3(sum((pcgapdeptotndpoorIndividualRural/PovertyLine)) * (1/TotalPopulationIndividualRural)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Rural: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural)) * TotalPoorIndividualRural)/TotalPopulationIndividualRural))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Rural: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualRural)) * TotalPoorIndividualRural)/TotalPopulationIndividualRural))

########### Urban ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Urban: %s", percent3(sum((pcgapdeptotndpoorIndividualUrban/PovertyLine)) * (1/TotalPopulationIndividualUrban)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Urban: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban)) * TotalPoorIndividualUrban)/TotalPopulationIndividualUrban))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Urban: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban)) * TotalPoorIndividualUrban)/TotalPopulationIndividualUrban))

#######################################################################################
################### Poverty Indicators: Poverty Severity Index (FGT2) #################
#######################################################################################

########### Burkina Faso ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Burkina Faso: %s", percent3(sum((pcgapdeptotndpoorIndividualBurkinaFaso/PovertyLine)^2) * (1/TotalPopulationIndividualBurkinaFaso)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Burkina Faso: %s", percent3((TotalPoorIndividualBurkinaFaso * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso + 1)))/PovertyLine^2/TotalPopulationIndividualBurkinaFaso))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Burkina Faso: %s", percent3((TotalPoorIndividualBurkinaFaso * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso + 1)))/PovertyLine^2/TotalPopulationIndividualBurkinaFaso))

########### Rural ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Rural: %s", percent3(sum((pcgapdeptotndpoorIndividualRural/PovertyLine)^2) * (1/TotalPopulationIndividualRural)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Rural: %s", percent3((TotalPoorIndividualRural * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural + 1)))/PovertyLine^2/TotalPopulationIndividualRural))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Rural: %s", percent3((TotalPoorIndividualRural * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualRural + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualRural + 1)))/PovertyLine^2/TotalPopulationIndividualRural))

########### Urban ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Urban: %s", percent3(sum((pcgapdeptotndpoorIndividualUrban/PovertyLine)^2) * (1/TotalPopulationIndividualUrban)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Urban: %s", percent3((TotalPoorIndividualUrban * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban + 1)))/PovertyLine^2/TotalPopulationIndividualUrban))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Urban: %s", percent3((TotalPoorIndividualUrban * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban + 1)))/PovertyLine^2/TotalPopulationIndividualUrban))

##################################################
########### Histogram vs. Density Plots ##########
##################################################

# We define a new variable where we are going to save the location where we would like to save the graphs we produce.
# This new variable is going to be called: GraphPathGraphPathHistograms.
GraphPathHistograms <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Burkina Faso/Graphs/New Plots/Individual Level/Histograms"

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

########### Burkina Faso ##########

# We generate the .tex file
FileNameHistogramBurkinaFaso <- paste0('HistogramDefititatTrapping', 'TotalPoorBurkinaFaso.tex')
tikz(FileNameHistogramBurkinaFaso, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualBurkinaFaso, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000025), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorBurkinaFaso), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorBurkinaFaso), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Rural ##########

# We generate the .tex file
FileNameHistogramRural <- paste0('HistogramDefititatTrapping', 'TotalPoorRural.tex')
tikz(FileNameHistogramRural, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualRural, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000025), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorRural), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorRural), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Urban ##########

# We generate the .tex file
FileNameHistogramUrban <- paste0('HistogramDefititatTrapping', 'TotalPoorUrban.tex')
tikz(FileNameHistogramUrban, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualUrban, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 160000), ylim = c(0, 0.000025), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, cex.axis = 0.7)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorUrban), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorUrban), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
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
GraphPathQQPlots <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Burkina Faso/Graphs/New Plots/Individual Level/QQ-Plots"

# We set the GraphPathQQPlots's location as working directory and save it in the WorkingDirectory variable. Again, this is 
# the location were the graphs are going to be saved.
WorkingDirectory <- setwd(GraphPathQQPlots)

# Set up a vector with the expressions for the plot's legend.
my.expressions.qq <- vector()
my.expressions.qq <- c("$\\hat{\\alpha}_{\\scaleto{MLE}{2pt}}$", "$\\hat{\\alpha}_{\\scaleto{MME}{2pt}}$")

########### Burkina Faso ##########

# We generate a vector with the sample quantiles.
samplequantilesBurkinaFaso = quantile(pcgapdeptotndpoorIndividualBurkinaFaso, probs = seq(0, 1, length = 1000))
unname(samplequantilesBurkinaFaso)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorBurkinaFaso <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorBurkinaFaso

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorBurkinaFaso <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorBurkinaFaso

# We generate the .tex file
FileNameQQPlotBurkinaFaso <- paste0('QQPlotDefititatTrapping', 'TotalPoorBurkinaFaso.tex')
tikz(FileNameQQPlotBurkinaFaso, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorBurkinaFaso, samplequantilesBurkinaFaso, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorBurkinaFaso, samplequantilesBurkinaFaso, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Rural ##########

# We generate a vector with the sample quantiles.
samplequantilesRural = quantile(pcgapdeptotndpoorIndividualRural, probs = seq(0, 1, length = 1000))
unname(samplequantilesRural)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorRural <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorRural

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorRural <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorRural

# We generate the .tex file
FileNameQQPlotRural <- paste0('QQPlotDefititatTrapping', 'TotalPoorRural.tex')
tikz(FileNameQQPlotRural, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorRural, samplequantilesRural, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorRural, samplequantilesRural, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Urban ##########

# We generate a vector with the sample quantiles.
samplequantilesUrban = quantile(pcgapdeptotndpoorIndividualUrban, probs = seq(0, 1, length = 1000))
unname(samplequantilesUrban)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorUrban <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorUrban

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorUrban <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorUrban

# We generate the .tex file
FileNameQQPlotUrban <- paste0('QQPlotDefititatTrapping', 'TotalPoorUrban.tex')
tikz(FileNameQQPlotUrban, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorUrban, samplequantilesUrban, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 160000), ylim = c(0, 160000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n")
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
axis(side = 2, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','), cex.axis = 0.7)
lines(seq(0, 160000, length = 1000), seq(0, 160000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorUrban, samplequantilesUrban, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
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
GraphPathCDFPlots <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Burkina Faso/Graphs/New Plots/Individual Level/CDF-Plots"

# We set the GraphPathCDFPlots's location as working directory and save it in the WorkingDirectory variable. Again, this is 
# the location were the graphs are going to be saved.
WorkingDirectory <- setwd(GraphPathCDFPlots)

# Set up a vector with the expressions for the plot's legend.
my.expressions.cdf <- vector()
my.expressions.cdf <- c("Empirical Data", "$\\hat{\\alpha}_{\\scaleto{MLE}{2pt}}$", "$\\hat{\\alpha}_{\\scaleto{MME}{2pt}}$")

########### Burkina Faso ##########

# We generate the .tex file
FileNameCDFPlotBurkinaFaso <- paste0('CDFPlotDefititatTrapping', 'TotalPoorBurkinaFaso.tex')
tikz(FileNameCDFPlotBurkinaFaso, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualBurkinaFaso), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 0.5, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Rural ##########

# We generate the .tex file
FileNameCDFPlotRural <- paste0('CDFPlotDefititatTrapping', 'TotalPoorRural.tex')
tikz(FileNameCDFPlotRural, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualRural), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 0.5, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Urban ##########

# We generate the .tex file
FileNameCDFPlotUrban <- paste0('CDFPlotDefititatTrapping', 'TotalPoorUrban.tex')
tikz(FileNameCDFPlotUrban, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualUrban), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 160000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, xaxt = "n", cex = 0.1)
axis(side = 1, at = c(0, 40000, 80000, 120000, 160000), labels = formatC(c(0, 40000, 80000, 120000, 160000), format = "d", big.mark = ','))
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2)
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
GraphPathPPPlots <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Burkina Faso/Graphs/New Plots/Individual Level/PP-Plots"

# We set the GraphPathPPPlots's location as working directory and save it in the WorkingDirectory variable. Again, this is 
# the location were the graphs are going to be saved.
WorkingDirectory <- setwd(GraphPathPPPlots)

# Set up a vector with the expressions for the plot's legend.
my.expressions.pp <- vector()
my.expressions.pp <- c("$\\hat{\\alpha}_{\\scaleto{MLE}{2pt}}$", "$\\hat{\\alpha}_{\\scaleto{MME}{2pt}}$")

########### Burkina Faso ##########

FileNamePPPlotBurkinaFaso <- paste0('PPPlotDefititatTrapping', 'TotalPoorBurkinaFaso.tex')
tikz(FileNamePPPlotBurkinaFaso, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorBurkinaFaso <- ppbeta(pcgapdeptotndpoorIndividualBurkinaFaso, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorBurkinaFaso <- ppbeta(pcgapdeptotndpoorIndividualBurkinaFaso, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBurkinaFaso, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualBurkinaFaso)), sort(probDistMaximumLikelihoodEstimatorBurkinaFaso), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualBurkinaFaso)), sort(probDistMethodofMomentsEstimatorBurkinaFaso), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Rural ##########

FileNamePPPlotRural <- paste0('PPPlotDefititatTrapping', 'TotalPoorRural.tex')
tikz(FileNamePPPlotRural, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorRural <- ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorRural <- ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualRural)), sort(probDistMaximumLikelihoodEstimatorRural), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualRural)), sort(probDistMethodofMomentsEstimatorRural), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Urban ##########

FileNamePPPlotUrban <- paste0('PPPlotDefititatTrapping', 'TotalPoorUrban.tex')
tikz(FileNamePPPlotUrban, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorUrban <- ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorUrban <- ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualUrban)), sort(probDistMaximumLikelihoodEstimatorUrban), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualUrban)), sort(probDistMethodofMomentsEstimatorUrban), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()
