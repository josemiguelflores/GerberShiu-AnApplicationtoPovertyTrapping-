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

# First, we set the working directory to the folder in which the "ehcvm_welfare_civ2018.csv" data file is saved. This data file contains consumption 
# data for households obtained from Ivory Coast's Enquête Harmonisée sur les Conditions de Vie des Ménages Continue (EHCVM) 2018-2019. 
# See, for example: https://microdata.worldbank.org/index.php/catalog/4292

# We copy the path of the location in which the data file is saved.

Path <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Ivory Coast"

# We set the Path's location as working directory and save it in the WorkingDirectory variable.

WorkingDirectory<- setwd(Path)

# We read the data file (the name of the file is "ehcvm_welfare_civ2018.csv").

ehcvm_welfare_civ2018 <- as_tibble(read.csv("ehcvm_welfare_civ2018.csv"))

########################################
########### Data Preparation ########### 
########################################

# Now, we create a "flag" variable that will tell us if a household is poor or not. We are going to call this variable: flgdeptotndpoor and if 
# flgdeptotndpoor = 1 then the household is poor, whereas if flgdeptotndpoor = 0 means the household is not poor. Note that, we do this by 
# comparing the expenditure per capita (pcexp) with the poverty line of 345,520 CFA per capita (appelé seuil absolu de pauvreté monétaire, a 
# été estimé à 345,520 CFA par personne et par an - see, for example: Note Synthetique Sur les Resultats de Pauvrete - EHCVM 2018-2019.
# If the expenditure per capita (pcexp) is less than zref = 345,520 CFA, then the household is poor, otherwise, the household is not poor.

ehcvm_welfare_civ2018 <- ehcvm_welfare_civ2018 %>% mutate(flgdeptotndpoor = ifelse(pcexp <= zref, 1, 0))

# We check the top records for the new calculated variable: flgdeptotndpoor.

head(ehcvm_welfare_civ2018$flgdeptotndpoor)

# We now calculate the number of poor per household. We do this by multiplying the number of members in the household (hhsize) by the "flag" 
# variable previously calculated. We will call this new variable: numdeptotndpoor.

ehcvm_welfare_civ2018$numdeptotndpoor <- ehcvm_welfare_civ2018$flgdeptotndpoor * ehcvm_welfare_civ2018$hhsize

# We check the top records for the new calculated variable: numdeptotndpoor.

head(ehcvm_welfare_civ2018$numdeptotndpoor)

# For those who are poor, we calculate the poverty gap (or income deficit), which is defined as the poverty line less expenditure per capita. We 
# store it in the variable: pcgapdeptotndpoor.

PovertyLine <- 345520.34375

ehcvm_welfare_civ2018 <- ehcvm_welfare_civ2018 %>% mutate(pcgapdeptotndpoor = ifelse(flgdeptotndpoor == 1, zref - pcexp, NA))

# We create a new data frame that includes a data record (or row) per household. We assign the following name to this data frame: 
# ehcvm_welfare_civ20182.

ehcvm_welfare_civ20182 <- as.data.frame(lapply(ehcvm_welfare_civ2018, rep, ehcvm_welfare_civ2018$hhsize))

# Now, we create two more data sets (per each area of residence). This is the approach we follow and it will help to estimate measures for the
# different areas of residence (note that other approaches could be followed to perform the analysis below).

ehcvm_welfare_civ2018Rural <- ehcvm_welfare_civ2018 %>% filter(milieu == 1)
ehcvm_welfare_civ2018Urban <- ehcvm_welfare_civ2018 %>% filter(milieu == 2)

ehcvm_welfare_civ2018IndividualRural <- ehcvm_welfare_civ20182 %>% filter(milieu == 1)
ehcvm_welfare_civ2018IndividualUrban <- ehcvm_welfare_civ20182 %>% filter(milieu == 2)

# Then, we need to remove from all data frames the records with NAs, i.e. those records with people 
# that is not poor. Therefore, we remove the NAs and we save the values of interest (in this case, the pcgapdeptotndpoor) in the variable: 
# pcgapdeptotndpoor. We do this per area of residence for both of the data frames previously created.

pcgapdeptotndpoorIvoryCoast <- ehcvm_welfare_civ2018$pcgapdeptotndpoor 
pcgapdeptotndpoorIvoryCoast <- pcgapdeptotndpoorIvoryCoast[!is.na(pcgapdeptotndpoorIvoryCoast)]
pcgapdeptotndpoorRural <- ehcvm_welfare_civ2018Rural$pcgapdeptotndpoor 
pcgapdeptotndpoorRural <- pcgapdeptotndpoorRural[!is.na(pcgapdeptotndpoorRural)]
pcgapdeptotndpoorUrban <- ehcvm_welfare_civ2018Urban$pcgapdeptotndpoor 
pcgapdeptotndpoorUrban <- pcgapdeptotndpoorUrban[!is.na(pcgapdeptotndpoorUrban)]

pcgapdeptotndpoorIndividualIvoryCoast <- ehcvm_welfare_civ20182$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualIvoryCoast <- pcgapdeptotndpoorIndividualIvoryCoast[!is.na(pcgapdeptotndpoorIndividualIvoryCoast)]
pcgapdeptotndpoorIndividualRural <- ehcvm_welfare_civ2018IndividualRural$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualRural <- pcgapdeptotndpoorIndividualRural[!is.na(pcgapdeptotndpoorIndividualRural)]
pcgapdeptotndpoorIndividualUrban <- ehcvm_welfare_civ2018IndividualUrban$pcgapdeptotndpoor
pcgapdeptotndpoorIndividualUrban <- pcgapdeptotndpoorIndividualUrban[!is.na(pcgapdeptotndpoorIndividualUrban)]

# We check the first ten values for the new calculated variables: pcgapdeptotndpoor.

pcgapdeptotndpoorIvoryCoast[1:10]
pcgapdeptotndpoorRural[1:10]
pcgapdeptotndpoorUrban[1:10]

pcgapdeptotndpoorIndividualIvoryCoast[1:10]
pcgapdeptotndpoorIndividualRural[1:10]
pcgapdeptotndpoorIndividualUrban[1:10]

###################################################
### Calculation of Population (# of households) ###
###################################################

# We calculate the total population in Ivory Coast (# of households).

TotalPopulation <- ehcvm_welfare_civ2018 %>% count(milieu) %>% filter(milieu == 1) %>% pull(n) + ehcvm_welfare_civ2018 %>% count(milieu) %>% filter(milieu == 2) %>% pull(n)
TotalPopulation

# We calculate the total population in the area of residence: Urban  (# of households).

TotalPopulationUrban <- ehcvm_welfare_civ2018 %>% count(milieu) %>% filter(milieu == 1) %>% pull(n)
TotalPopulationUrban

# We calculate the total population in the area of residence: Rural  (# of households).

TotalPopulationRural <- ehcvm_welfare_civ2018 %>% count(milieu) %>% filter(milieu == 2) %>% pull(n)
TotalPopulationRural

##########################################################
#### Calculation of Poor Population (# of households) ####
##########################################################

# We calculate the total number of poor households in Ivory Coast (# of households).

TotalPoorIvoryCoast <- length(pcgapdeptotndpoorIvoryCoast)
TotalPoorIvoryCoast

# We calculate the total number of poor households in the area of residence: Urban  (# of households).

TotalPoorUrban <- length(pcgapdeptotndpoorUrban)
TotalPoorUrban

# We calculate the total number of poor households in the area of residence: Rural  (# of households).

TotalPoorRural <- length(pcgapdeptotndpoorRural)
TotalPoorRural

####################################################
### Calculation of Population (# of individuals) ###
####################################################

# We calculate the total population in Ivory Coast (# of individuals).

TotalPopulationIndividualIvoryCoast <- ehcvm_welfare_civ20182 %>% count(milieu) %>% filter(milieu == 1) %>% pull(n) + ehcvm_welfare_civ20182 %>% count(milieu) %>% filter(milieu == 2) %>% pull(n) 
TotalPopulationIndividualIvoryCoast

# We calculate the total population in the area of residence: Urban  (# of individuals).

TotalPopulationIndividualUrban <- ehcvm_welfare_civ20182 %>% count(milieu) %>% filter(milieu == 1) %>% pull(n)
TotalPopulationIndividualUrban

# We calculate the total population in the area of residence: Rural  (# of individuals).

TotalPopulationIndividualRural <- ehcvm_welfare_civ20182 %>% count(milieu) %>% filter(milieu == 2) %>% pull(n)
TotalPopulationIndividualRural

############################################################
##### Calculation of Poor Population (# of individuals) ####
############################################################

# We calculate the total number of poor households in Ivory Coast (# of individuals).

TotalPoorIndividualIvoryCoast <- length(pcgapdeptotndpoorIndividualIvoryCoast)
TotalPoorIndividualIvoryCoast

# We calculate the total number of poor households in the area of residence: Urban (# of individuals).

TotalPoorIndividualUrban <- length(pcgapdeptotndpoorIndividualUrban)
TotalPoorIndividualUrban

# We calculate the total number of poor households in the area of residence: Rural (# of individuals).

TotalPoorIndividualRural <- length(pcgapdeptotndpoorIndividualRural)
TotalPoorIndividualRural

##################################################
## Estimation of Parameters  (# of individuals) ##
##################################################

# We define a function to print values with two decimals.

percent2 <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...))
}

########### Ivory Coast ##########

# For Ivory Coast, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast <- TotalPoorIndividualIvoryCoast/(TotalPoorIndividualIvoryCoast * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualIvoryCoast)))
sprintf("Maximum Likelihood Estimator of α for Ivory Coast: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast))

# For Ivory Coast, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast <- (PovertyLine - mean(pcgapdeptotndpoorIndividualIvoryCoast))/mean(pcgapdeptotndpoorIndividualIvoryCoast)
sprintf("Method of Moments Estimator of α for Ivory Coast: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast))

########### Urban ##########

# For Urban, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban <- TotalPoorIndividualUrban/(TotalPoorIndividualUrban * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualUrban)))
sprintf("Maximum Likelihood Estimator of α for Urban: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban))

# For Urban, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban <- (PovertyLine - mean(pcgapdeptotndpoorIndividualUrban))/mean(pcgapdeptotndpoorIndividualUrban)
sprintf("Method of Moments Estimator of α for Urban: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban))

########### Rural ##########

# For Rural, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural <- TotalPoorIndividualRural/(TotalPoorIndividualRural * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualRural)))
sprintf("Maximum Likelihood Estimator of α for Rural: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural))

# For Rural, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualRural <- (PovertyLine - mean(pcgapdeptotndpoorIndividualRural))/mean(pcgapdeptotndpoorIndividualRural)
sprintf("Method of Moments Estimator of α for Rural: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualRural))

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

########### Ivory Coast ##########

ecdfIvoryCoast <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualIvoryCoast)))

# Maximum Likelihood Estimator (MLE) - KS test for Ivory Coast: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualIvoryCoast)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorIvoryCoast <- DStatisticEstimation(ecdfIvoryCoast, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Ivory Coast: %s", MprimeMaximumLikelihoodEstimatorIvoryCoast)

PValueMaximumLikelihoodEstimatorIvoryCoast <- PValueEstimation(pcgapdeptotndpoorIndividualIvoryCoast, MprimeMaximumLikelihoodEstimatorIvoryCoast, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Ivory Coast: %s", PValueMaximumLikelihoodEstimatorIvoryCoast)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorIvoryCoast, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Ivory Coast:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualIvoryCoast)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorIvoryCoast <- DStatisticEstimation(ecdfIvoryCoast, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Ivory Coast: %s", MprimeMethodofMomentsEstimatorIvoryCoast)

PValueMethodofMomentsEstimatorIvoryCoast <- PValueEstimation(pcgapdeptotndpoorIndividualIvoryCoast, MprimeMethodofMomentsEstimatorIvoryCoast, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Ivory Coast: %s", PValueMethodofMomentsEstimatorIvoryCoast)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorIvoryCoast, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine) # two-sided, exact

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

###################################################
################## R-SQUARED ######################
###################################################

# Define a function to compute the ECDF
percent4 <- function(x, digits = 4, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...))
}

########### Ivory Coast ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Ivory Coast: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualIvoryCoast, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualIvoryCoast)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualIvoryCoast, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualIvoryCoast)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualIvoryCoast, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualIvoryCoast))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Ivory Coast: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualIvoryCoast, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualIvoryCoast)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualIvoryCoast, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualIvoryCoast)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualIvoryCoast, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualIvoryCoast))^2))))

########### Urban ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Urban: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualUrban)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualUrban)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualUrban))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Urban: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualUrban)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualUrban)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualUrban, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualUrban))^2))))


########### Rural ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Rural: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualRural)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualRural)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualRural))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Rural: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualRural)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualRural)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualRural, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualRural))^2))))

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

########### Ivory Coast ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Ivory Coast: %s", percent3(sum((pcgapdeptotndpoorIndividualIvoryCoast/PovertyLine)) * (1/TotalPopulationIndividualIvoryCoast)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Ivory Coast: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast)) * TotalPoorIndividualIvoryCoast)/TotalPopulationIndividualIvoryCoast))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Ivory Coast: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast)) * TotalPoorIndividualIvoryCoast)/TotalPopulationIndividualIvoryCoast))

########### Urban ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Urban: %s", percent3(sum((pcgapdeptotndpoorIndividualUrban/PovertyLine)) * (1/TotalPopulationIndividualUrban)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Urban: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban)) * TotalPoorIndividualUrban)/TotalPopulationIndividualUrban))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Urban: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban)) * TotalPoorIndividualUrban)/TotalPopulationIndividualUrban))

########### Rural ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Rural: %s", percent3(sum((pcgapdeptotndpoorIndividualRural/PovertyLine)) * (1/TotalPopulationIndividualRural)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Rural: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural)) * TotalPoorIndividualRural)/TotalPopulationIndividualRural))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Rural: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualRural)) * TotalPoorIndividualRural)/TotalPopulationIndividualRural))

#######################################################################################
################### Poverty Indicators: Poverty Severity Index (FGT2) #################
#######################################################################################

########### Ivory Coast ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Ivory Coast: %s", percent3(sum((pcgapdeptotndpoorIndividualIvoryCoast/PovertyLine)^2) * (1/TotalPopulationIndividualIvoryCoast)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Ivory Coast: %s", percent3((TotalPoorIndividualIvoryCoast * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast + 1)))/PovertyLine^2/TotalPopulationIndividualIvoryCoast))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Ivory Coast: %s", percent3((TotalPoorIndividualIvoryCoast * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast + 1)))/PovertyLine^2/TotalPopulationIndividualIvoryCoast))

########### Urban ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Urban: %s", percent3(sum((pcgapdeptotndpoorIndividualUrban/PovertyLine)^2) * (1/TotalPopulationIndividualUrban)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Urban: %s", percent3((TotalPoorIndividualUrban * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban + 1)))/PovertyLine^2/TotalPopulationIndividualUrban))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Urban: %s", percent3((TotalPoorIndividualUrban * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban + 1)))/PovertyLine^2/TotalPopulationIndividualUrban))

########### Rural ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Rural: %s", percent3(sum((pcgapdeptotndpoorIndividualRural/PovertyLine)^2) * (1/TotalPopulationIndividualRural)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Rural: %s", percent3((TotalPoorIndividualRural * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural + 1)))/PovertyLine^2/TotalPopulationIndividualRural))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Rural: %s", percent3((TotalPoorIndividualRural * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualRural + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualRural + 1)))/PovertyLine^2/TotalPopulationIndividualRural))

##################################################
########### Histogram vs. Density Plots ##########
##################################################

# We define a new variable where we are going to save the location where we would like to save the graphs we produce.
# This new variable is going to be called: GraphPathGraphPathHistograms.
GraphPathHistograms <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Ivory Coast/Graphs/Regions/Individual Level/Histograms"

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

########### Ivory Coast ##########

# We generate the .tex file
FileNameHistogramIvoryCoast <- paste0('HistogramDefititatTrapping', 'IvoryCoast.tex')
tikz(FileNameHistogramIvoryCoast, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualIvoryCoast, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualIvoryCoast), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualIvoryCoast), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Urban ##########

# We generate the .tex file
FileNameHistogramUrban <- paste0('HistogramDefititatTrapping', 'Urban.tex')
tikz(FileNameHistogramUrban, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualUrban, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualUrban), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualUrban), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Rural ##########

# We generate the .tex file
FileNameHistogramRural <- paste0('HistogramDefititatTrapping', 'Rural.tex')
tikz(FileNameHistogramRural, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualRural, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000010), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualRural), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualRural), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
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
GraphPathQQPlots <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Ivory Coast/Graphs/Regions/Individual Level/QQ-Plots"

# We set the GraphPathQQPlots's location as working directory and save it in the WorkingDirectory variable. Again, this is 
# the location were the graphs are going to be saved.
WorkingDirectory <- setwd(GraphPathQQPlots)

# Set up a vector with the expressions for the plot's legend.
my.expressions.qq <- vector()
my.expressions.qq <- c("$\\hat{\\alpha}_{\\scaleto{MLE}{2pt}}$", "$\\hat{\\alpha}_{\\scaleto{MME}{2pt}}$")

########### Ivory Coast ##########

# We generate a vector with the sample quantiles.
samplequantilesIvoryCoast = quantile(pcgapdeptotndpoorIndividualIvoryCoast, probs = seq(0, 1, length = 1000))
unname(samplequantilesIvoryCoast)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorIvoryCoast <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorIvoryCoast

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorIvoryCoast <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorIvoryCoast

# We generate the .tex file
FileNameQQPlotIvoryCoast <- paste0('QQPlotDefititatTrapping', 'IvoryCoast.tex')
tikz(FileNameQQPlotIvoryCoast, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorIvoryCoast, samplequantilesIvoryCoast, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorIvoryCoast, samplequantilesIvoryCoast, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
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
FileNameQQPlotUrban <- paste0('QQPlotDefititatTrapping', 'Urban.tex')
tikz(FileNameQQPlotUrban, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorUrban, samplequantilesUrban, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorUrban, samplequantilesUrban, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
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
FileNameQQPlotRural <- paste0('QQPlotDefititatTrapping', 'Rural.tex')
tikz(FileNameQQPlotRural, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorRural, samplequantilesRural, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorRural, samplequantilesRural, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
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
GraphPathCDFPlots <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Ivory Coast/Graphs/Regions/Individual Level/CDF-Plots"

# We set the GraphPathCDFPlots's location as working directory and save it in the WorkingDirectory variable. Again, this is 
# the location were the graphs are going to be saved.
WorkingDirectory <- setwd(GraphPathCDFPlots)

# Set up a vector with the expressions for the plot's legend.
my.expressions.cdf <- vector()
my.expressions.cdf <- c("Empirical Data", "$\\hat{\\alpha}_{\\scaleto{MLE}{2pt}}$", "$\\hat{\\alpha}_{\\scaleto{MME}{2pt}}$")

########### Ivory Coast ##########

# We generate the .tex file
FileNameCDFPlotIvoryCoast <- paste0('CDFPlotDefititatTrapping', 'TotalPoorIvoryCoast.tex')
tikz(FileNameCDFPlotIvoryCoast, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualIvoryCoast), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
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
plot(ecdf(pcgapdeptotndpoorIndividualUrban), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualUrban, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
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
plot(ecdf(pcgapdeptotndpoorIndividualRural), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualRural, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
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
GraphPathPPPlots <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Ivory Coast/Graphs/Regions/Individual Level/PP-Plots"

# We set the GraphPathPPPlots's location as working directory and save it in the WorkingDirectory variable. Again, this is 
# the location were the graphs are going to be saved.
WorkingDirectory <- setwd(GraphPathPPPlots)

# Set up a vector with the expressions for the plot's legend.
my.expressions.pp <- vector()
my.expressions.pp <- c("$\\hat{\\alpha}_{\\scaleto{MLE}{2pt}}$", "$\\hat{\\alpha}_{\\scaleto{MME}{2pt}}$")

########### Ivory Coast ##########

# We generate the .tex file
FileNamePPPlotIvoryCoast <- paste0('PPPlotDefititatTrapping', 'TotalPoorIvoryCoast.tex')
tikz(FileNamePPPlotIvoryCoast, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorIvoryCoast <- ppbeta(pcgapdeptotndpoorIndividualIvoryCoast, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorIvoryCoast <- ppbeta(pcgapdeptotndpoorIndividualIvoryCoast, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIvoryCoast, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualIvoryCoast)), sort(probDistMaximumLikelihoodEstimatorIvoryCoast), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualIvoryCoast)), sort(probDistMethodofMomentsEstimatorIvoryCoast), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
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

# We generate the .tex file
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

########### Rural ##########

# We generate the .tex file
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
