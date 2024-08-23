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

# Now, we create two more data sets (per each administrative region). This is the approach we follow and it will help to estimate measures for the
# different administrative regions (note that other approaches could be followed to perform the analysis below).

ehcvm_welfare_civ2018AutonomedAbidjan <- ehcvm_welfare_civ2018 %>% filter(region == 1)
ehcvm_welfare_civ2018HautSassandra <- ehcvm_welfare_civ2018 %>% filter(region == 2)
ehcvm_welfare_civ2018Poro <- ehcvm_welfare_civ2018 %>% filter(region == 3)
ehcvm_welfare_civ2018Gbeke <- ehcvm_welfare_civ2018 %>% filter(region == 4)
ehcvm_welfare_civ2018IndenieDjuablin <- ehcvm_welfare_civ2018 %>% filter(region == 5)
ehcvm_welfare_civ2018Tonkpi <- ehcvm_welfare_civ2018 %>% filter(region == 6)
ehcvm_welfare_civ2018Yamoussoukro <- ehcvm_welfare_civ2018 %>% filter(region == 7)
ehcvm_welfare_civ2018Gontougo <- ehcvm_welfare_civ2018 %>% filter(region == 8)
ehcvm_welfare_civ2018SanPedro <- ehcvm_welfare_civ2018 %>% filter(region == 9)
ehcvm_welfare_civ2018Kabadougou <- ehcvm_welfare_civ2018 %>% filter(region == 10)
ehcvm_welfare_civ2018NZi <- ehcvm_welfare_civ2018 %>% filter(region == 11)
ehcvm_welfare_civ2018Marahoue <- ehcvm_welfare_civ2018 %>% filter(region == 12)
ehcvm_welfare_civ2018SudComoe <- ehcvm_welfare_civ2018 %>% filter(region == 13)
ehcvm_welfare_civ2018Worodougou <- ehcvm_welfare_civ2018 %>% filter(region == 14)
ehcvm_welfare_civ2018LôhDjiboua <- ehcvm_welfare_civ2018 %>% filter(region == 15)
ehcvm_welfare_civ2018AgnebyTiassa <- ehcvm_welfare_civ2018 %>% filter(region == 16)
ehcvm_welfare_civ2018GÔh <- ehcvm_welfare_civ2018 %>% filter(region == 17)
ehcvm_welfare_civ2018Cavally <- ehcvm_welfare_civ2018 %>% filter(region == 18)
ehcvm_welfare_civ2018Bafing <- ehcvm_welfare_civ2018 %>% filter(region == 19)
ehcvm_welfare_civ2018Bagoue <- ehcvm_welfare_civ2018 %>% filter(region == 20)
ehcvm_welfare_civ2018Belier <- ehcvm_welfare_civ2018 %>% filter(region == 21)
ehcvm_welfare_civ2018Bere <- ehcvm_welfare_civ2018 %>% filter(region == 22)
ehcvm_welfare_civ2018Bounkani <- ehcvm_welfare_civ2018 %>% filter(region == 23)
ehcvm_welfare_civ2018Folon <- ehcvm_welfare_civ2018 %>% filter(region == 24)
ehcvm_welfare_civ2018GbÔkle <- ehcvm_welfare_civ2018 %>% filter(region == 25)
ehcvm_welfare_civ2018GrandsPonts <- ehcvm_welfare_civ2018 %>% filter(region == 26)
ehcvm_welfare_civ2018Guemon <- ehcvm_welfare_civ2018 %>% filter(region == 27)
ehcvm_welfare_civ2018Hambol <- ehcvm_welfare_civ2018 %>% filter(region == 28)
ehcvm_welfare_civ2018Iffou <- ehcvm_welfare_civ2018 %>% filter(region == 29)
ehcvm_welfare_civ2018LaMe <- ehcvm_welfare_civ2018 %>% filter(region == 30)
ehcvm_welfare_civ2018Nawa <- ehcvm_welfare_civ2018 %>% filter(region == 31)
ehcvm_welfare_civ2018Tchologo <- ehcvm_welfare_civ2018 %>% filter(region == 32)
ehcvm_welfare_civ2018Moronou <- ehcvm_welfare_civ2018 %>% filter(region == 33)

ehcvm_welfare_civ2018IndividualAutonomedAbidjan <- ehcvm_welfare_civ20182 %>% filter(region == 1)
ehcvm_welfare_civ2018IndividualHautSassandra <- ehcvm_welfare_civ20182 %>% filter(region == 2)
ehcvm_welfare_civ2018IndividualPoro <- ehcvm_welfare_civ20182 %>% filter(region == 3)
ehcvm_welfare_civ2018IndividualGbeke <- ehcvm_welfare_civ20182 %>% filter(region == 4)
ehcvm_welfare_civ2018IndividualIndenieDjuablin <- ehcvm_welfare_civ20182 %>% filter(region == 5)
ehcvm_welfare_civ2018IndividualTonkpi <- ehcvm_welfare_civ20182 %>% filter(region == 6)
ehcvm_welfare_civ2018IndividualYamoussoukro <- ehcvm_welfare_civ20182 %>% filter(region == 7)
ehcvm_welfare_civ2018IndividualGontougo <- ehcvm_welfare_civ20182 %>% filter(region == 8)
ehcvm_welfare_civ2018IndividualSanPedro <- ehcvm_welfare_civ20182 %>% filter(region == 9)
ehcvm_welfare_civ2018IndividualKabadougou <- ehcvm_welfare_civ20182 %>% filter(region == 10)
ehcvm_welfare_civ2018IndividualNZi <- ehcvm_welfare_civ20182 %>% filter(region == 11)
ehcvm_welfare_civ2018IndividualMarahoue <- ehcvm_welfare_civ20182 %>% filter(region == 12)
ehcvm_welfare_civ2018IndividualSudComoe <- ehcvm_welfare_civ20182 %>% filter(region == 13)
ehcvm_welfare_civ2018IndividualWorodougou <- ehcvm_welfare_civ20182 %>% filter(region == 14)
ehcvm_welfare_civ2018IndividualLôhDjiboua <- ehcvm_welfare_civ20182 %>% filter(region == 15)
ehcvm_welfare_civ2018IndividualAgnebyTiassa <- ehcvm_welfare_civ20182 %>% filter(region == 16)
ehcvm_welfare_civ2018IndividualGÔh <- ehcvm_welfare_civ20182 %>% filter(region == 17)
ehcvm_welfare_civ2018IndividualCavally <- ehcvm_welfare_civ20182 %>% filter(region == 18)
ehcvm_welfare_civ2018IndividualBafing <- ehcvm_welfare_civ20182 %>% filter(region == 19)
ehcvm_welfare_civ2018IndividualBagoue  <- ehcvm_welfare_civ20182 %>% filter(region == 20)
ehcvm_welfare_civ2018IndividualBelier <- ehcvm_welfare_civ20182 %>% filter(region == 21)
ehcvm_welfare_civ2018IndividualBere <- ehcvm_welfare_civ20182 %>% filter(region == 22)
ehcvm_welfare_civ2018IndividualBounkani <- ehcvm_welfare_civ20182 %>% filter(region == 23)
ehcvm_welfare_civ2018IndividualFolon <- ehcvm_welfare_civ20182 %>% filter(region == 24)
ehcvm_welfare_civ2018IndividualGbÔkle <- ehcvm_welfare_civ20182 %>% filter(region == 25)
ehcvm_welfare_civ2018IndividualGrandsPonts <- ehcvm_welfare_civ20182 %>% filter(region == 26)
ehcvm_welfare_civ2018IndividualGuemon <- ehcvm_welfare_civ20182 %>% filter(region == 27)
ehcvm_welfare_civ2018IndividualHambol <- ehcvm_welfare_civ20182 %>% filter(region == 28)
ehcvm_welfare_civ2018IndividualIffou <- ehcvm_welfare_civ20182 %>% filter(region == 29)
ehcvm_welfare_civ2018IndividualLaMe <- ehcvm_welfare_civ20182 %>% filter(region == 30)
ehcvm_welfare_civ2018IndividualNawa <- ehcvm_welfare_civ20182 %>% filter(region == 31)
ehcvm_welfare_civ2018IndividualTchologo <- ehcvm_welfare_civ20182 %>% filter(region == 32)
ehcvm_welfare_civ2018IndividualMoronou <- ehcvm_welfare_civ20182 %>% filter(region == 33)

# Then, we need to remove from all data frames the records with NAs, i.e. those records with people 
# that is not poor. Therefore, we remove the NAs and we save the values of interest (in this case, the pcgapdeptotndpoor) in the variable: 
# pcgapdeptotndpoor. We do this per administrative region for both of the data frames previously created.

pcgapdeptotndpoorAutonomedAbidjan <- ehcvm_welfare_civ2018AutonomedAbidjan$pcgapdeptotndpoor 
pcgapdeptotndpoorAutonomedAbidjan <- pcgapdeptotndpoorAutonomedAbidjan[!is.na(pcgapdeptotndpoorAutonomedAbidjan)]
pcgapdeptotndpoorHautSassandra <- ehcvm_welfare_civ2018HautSassandra$pcgapdeptotndpoor 
pcgapdeptotndpoorHautSassandra <- pcgapdeptotndpoorHautSassandra[!is.na(pcgapdeptotndpoorHautSassandra)]
pcgapdeptotndpoorPoro <- ehcvm_welfare_civ2018Poro$pcgapdeptotndpoor 
pcgapdeptotndpoorPoro <- pcgapdeptotndpoorPoro[!is.na(pcgapdeptotndpoorPoro)]
pcgapdeptotndpoorGbeke <- ehcvm_welfare_civ2018Gbeke$pcgapdeptotndpoor 
pcgapdeptotndpoorGbeke <- pcgapdeptotndpoorGbeke[!is.na(pcgapdeptotndpoorGbeke)]
pcgapdeptotndpoorIndenieDjuablin <- ehcvm_welfare_civ2018IndenieDjuablin$pcgapdeptotndpoor
pcgapdeptotndpoorIndenieDjuablin <- pcgapdeptotndpoorIndenieDjuablin[!is.na(pcgapdeptotndpoorIndenieDjuablin)]
pcgapdeptotndpoorTonkpi <- ehcvm_welfare_civ2018Tonkpi$pcgapdeptotndpoor
pcgapdeptotndpoorTonkpi <- pcgapdeptotndpoorTonkpi[!is.na(pcgapdeptotndpoorTonkpi)]
pcgapdeptotndpoorYamoussoukro <- ehcvm_welfare_civ2018Yamoussoukro$pcgapdeptotndpoor
pcgapdeptotndpoorYamoussoukro <- pcgapdeptotndpoorYamoussoukro[!is.na(pcgapdeptotndpoorYamoussoukro)]
pcgapdeptotndpoorGontougo <- ehcvm_welfare_civ2018Gontougo$pcgapdeptotndpoor
pcgapdeptotndpoorGontougo <- pcgapdeptotndpoorGontougo[!is.na(pcgapdeptotndpoorGontougo)]
pcgapdeptotndpoorSanPedro <- ehcvm_welfare_civ2018SanPedro$pcgapdeptotndpoor
pcgapdeptotndpoorSanPedro <- pcgapdeptotndpoorSanPedro[!is.na(pcgapdeptotndpoorSanPedro)]
pcgapdeptotndpoorKabadougou <- ehcvm_welfare_civ2018Kabadougou$pcgapdeptotndpoor
pcgapdeptotndpoorKabadougou <- pcgapdeptotndpoorKabadougou[!is.na(pcgapdeptotndpoorKabadougou)]
pcgapdeptotndpoorNZi <- ehcvm_welfare_civ2018NZi$pcgapdeptotndpoor
pcgapdeptotndpoorNZi <- pcgapdeptotndpoorNZi[!is.na(pcgapdeptotndpoorNZi)]
pcgapdeptotndpoorMarahoue <- ehcvm_welfare_civ2018Marahoue$pcgapdeptotndpoor
pcgapdeptotndpoorMarahoue <- pcgapdeptotndpoorMarahoue[!is.na(pcgapdeptotndpoorMarahoue)]
pcgapdeptotndpoorSudComoe <- ehcvm_welfare_civ2018SudComoe$pcgapdeptotndpoor
pcgapdeptotndpoorSudComoe <- pcgapdeptotndpoorSudComoe[!is.na(pcgapdeptotndpoorSudComoe)]
pcgapdeptotndpoorWorodougou <- ehcvm_welfare_civ2018Worodougou$pcgapdeptotndpoor
pcgapdeptotndpoorWorodougou <- pcgapdeptotndpoorWorodougou[!is.na(pcgapdeptotndpoorWorodougou)]
pcgapdeptotndpoorLôhDjiboua <- ehcvm_welfare_civ2018LôhDjiboua$pcgapdeptotndpoor
pcgapdeptotndpoorLôhDjiboua <- pcgapdeptotndpoorLôhDjiboua[!is.na(pcgapdeptotndpoorLôhDjiboua)]
pcgapdeptotndpoorAgnebyTiassa <- ehcvm_welfare_civ2018AgnebyTiassa$pcgapdeptotndpoor
pcgapdeptotndpoorAgnebyTiassa <- pcgapdeptotndpoorAgnebyTiassa[!is.na(pcgapdeptotndpoorAgnebyTiassa)]
pcgapdeptotndpoorGÔh <- ehcvm_welfare_civ2018GÔh$pcgapdeptotndpoor
pcgapdeptotndpoorGÔh <- pcgapdeptotndpoorGÔh[!is.na(pcgapdeptotndpoorGÔh)]
pcgapdeptotndpoorCavally <- ehcvm_welfare_civ2018Cavally$pcgapdeptotndpoor
pcgapdeptotndpoorCavally <- pcgapdeptotndpoorCavally[!is.na(pcgapdeptotndpoorCavally)]
pcgapdeptotndpoorBafing <- ehcvm_welfare_civ2018Bafing$pcgapdeptotndpoor
pcgapdeptotndpoorBafing <- pcgapdeptotndpoorBafing[!is.na(pcgapdeptotndpoorBafing)]
pcgapdeptotndpoorBagoue <- ehcvm_welfare_civ2018Bagoue$pcgapdeptotndpoor
pcgapdeptotndpoorBagoue <- pcgapdeptotndpoorBagoue[!is.na(pcgapdeptotndpoorBagoue)]
pcgapdeptotndpoorBelier <- ehcvm_welfare_civ2018Belier$pcgapdeptotndpoor
pcgapdeptotndpoorBelier <- pcgapdeptotndpoorBelier[!is.na(pcgapdeptotndpoorBelier)]
pcgapdeptotndpoorBere <- ehcvm_welfare_civ2018Bere$pcgapdeptotndpoor
pcgapdeptotndpoorBere <- pcgapdeptotndpoorBere[!is.na(pcgapdeptotndpoorBere)]
pcgapdeptotndpoorBounkani <- ehcvm_welfare_civ2018Bounkani$pcgapdeptotndpoor
pcgapdeptotndpoorBounkani <- pcgapdeptotndpoorBounkani[!is.na(pcgapdeptotndpoorBounkani)]
pcgapdeptotndpoorFolon <- ehcvm_welfare_civ2018Folon$pcgapdeptotndpoor
pcgapdeptotndpoorFolon <- pcgapdeptotndpoorFolon[!is.na(pcgapdeptotndpoorFolon)]
pcgapdeptotndpoorGbÔkle <- ehcvm_welfare_civ2018GbÔkle$pcgapdeptotndpoor
pcgapdeptotndpoorGbÔkle <- pcgapdeptotndpoorGbÔkle[!is.na(pcgapdeptotndpoorGbÔkle)]
pcgapdeptotndpoorGrandsPonts <- ehcvm_welfare_civ2018GrandsPonts$pcgapdeptotndpoor
pcgapdeptotndpoorGrandsPonts <- pcgapdeptotndpoorGrandsPonts[!is.na(pcgapdeptotndpoorGrandsPonts)]
pcgapdeptotndpoorGuemon <- ehcvm_welfare_civ2018Guemon$pcgapdeptotndpoor
pcgapdeptotndpoorGuemon <- pcgapdeptotndpoorGuemon[!is.na(pcgapdeptotndpoorGuemon)]
pcgapdeptotndpoorHambol <- ehcvm_welfare_civ2018Hambol$pcgapdeptotndpoor
pcgapdeptotndpoorHambol <- pcgapdeptotndpoorHambol[!is.na(pcgapdeptotndpoorHambol)]
pcgapdeptotndpoorIffou <- ehcvm_welfare_civ2018Iffou$pcgapdeptotndpoor
pcgapdeptotndpoorIffou <- pcgapdeptotndpoorIffou[!is.na(pcgapdeptotndpoorIffou)]
pcgapdeptotndpoorLaMe<- ehcvm_welfare_civ2018LaMe$pcgapdeptotndpoor
pcgapdeptotndpoorLaMe <- pcgapdeptotndpoorLaMe[!is.na(pcgapdeptotndpoorLaMe)]
pcgapdeptotndpoorNawa <- ehcvm_welfare_civ2018Nawa$pcgapdeptotndpoor
pcgapdeptotndpoorNawa <- pcgapdeptotndpoorNawa[!is.na(pcgapdeptotndpoorNawa)]
pcgapdeptotndpoorTchologo <- ehcvm_welfare_civ2018Tchologo$pcgapdeptotndpoor
pcgapdeptotndpoorTchologo <- pcgapdeptotndpoorTchologo[!is.na(pcgapdeptotndpoorTchologo)]
pcgapdeptotndpoorMoronou <- ehcvm_welfare_civ2018Moronou$pcgapdeptotndpoor
pcgapdeptotndpoorMoronou <- pcgapdeptotndpoorMoronou[!is.na(pcgapdeptotndpoorMoronou)]

pcgapdeptotndpoorIndividualAutonomedAbidjan <- ehcvm_welfare_civ2018IndividualAutonomedAbidjan$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualAutonomedAbidjan  <- pcgapdeptotndpoorIndividualAutonomedAbidjan[!is.na(pcgapdeptotndpoorIndividualAutonomedAbidjan)]
pcgapdeptotndpoorIndividualHautSassandra <- ehcvm_welfare_civ2018IndividualHautSassandra$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualHautSassandra  <- pcgapdeptotndpoorIndividualHautSassandra[!is.na(pcgapdeptotndpoorIndividualHautSassandra)]
pcgapdeptotndpoorIndividualPoro <- ehcvm_welfare_civ2018IndividualPoro$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualPoro  <- pcgapdeptotndpoorIndividualPoro[!is.na(pcgapdeptotndpoorIndividualPoro)]
pcgapdeptotndpoorIndividualGbeke <- ehcvm_welfare_civ2018IndividualGbeke$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualGbeke  <- pcgapdeptotndpoorIndividualGbeke[!is.na(pcgapdeptotndpoorIndividualGbeke)]
pcgapdeptotndpoorIndividualIndenieDjuablin <- ehcvm_welfare_civ2018IndividualIndenieDjuablin$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualIndenieDjuablin  <- pcgapdeptotndpoorIndividualIndenieDjuablin[!is.na(pcgapdeptotndpoorIndividualIndenieDjuablin)]
pcgapdeptotndpoorIndividualTonkpi <- ehcvm_welfare_civ2018IndividualTonkpi$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualTonkpi  <- pcgapdeptotndpoorIndividualTonkpi[!is.na(pcgapdeptotndpoorIndividualTonkpi)]
pcgapdeptotndpoorIndividualYamoussoukro <- ehcvm_welfare_civ2018IndividualYamoussoukro$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualYamoussoukro  <- pcgapdeptotndpoorIndividualYamoussoukro[!is.na(pcgapdeptotndpoorIndividualYamoussoukro)]
pcgapdeptotndpoorIndividualGontougo <- ehcvm_welfare_civ2018IndividualGontougo$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualGontougo  <- pcgapdeptotndpoorIndividualGontougo[!is.na(pcgapdeptotndpoorIndividualGontougo)]
pcgapdeptotndpoorIndividualSanPedro <- ehcvm_welfare_civ2018IndividualSanPedro$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualSanPedro  <- pcgapdeptotndpoorIndividualSanPedro[!is.na(pcgapdeptotndpoorIndividualSanPedro)]
pcgapdeptotndpoorIndividualKabadougou <- ehcvm_welfare_civ2018IndividualKabadougou$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualKabadougou  <- pcgapdeptotndpoorIndividualKabadougou[!is.na(pcgapdeptotndpoorIndividualKabadougou)]
pcgapdeptotndpoorIndividualNZi <- ehcvm_welfare_civ2018IndividualNZi$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualNZi  <- pcgapdeptotndpoorIndividualNZi[!is.na(pcgapdeptotndpoorIndividualNZi)]
pcgapdeptotndpoorIndividualMarahoue <- ehcvm_welfare_civ2018IndividualMarahoue$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualMarahoue  <- pcgapdeptotndpoorIndividualMarahoue[!is.na(pcgapdeptotndpoorIndividualMarahoue)]
pcgapdeptotndpoorIndividualSudComoe <- ehcvm_welfare_civ2018IndividualSudComoe$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualSudComoe  <- pcgapdeptotndpoorIndividualSudComoe[!is.na(pcgapdeptotndpoorIndividualSudComoe)]
pcgapdeptotndpoorIndividualWorodougou <- ehcvm_welfare_civ2018IndividualWorodougou$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualWorodougou  <- pcgapdeptotndpoorIndividualWorodougou[!is.na(pcgapdeptotndpoorIndividualWorodougou)]
pcgapdeptotndpoorIndividualLôhDjiboua <- ehcvm_welfare_civ2018IndividualLôhDjiboua$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualLôhDjiboua  <- pcgapdeptotndpoorIndividualLôhDjiboua[!is.na(pcgapdeptotndpoorIndividualLôhDjiboua)]
pcgapdeptotndpoorIndividualAgnebyTiassa <- ehcvm_welfare_civ2018IndividualAgnebyTiassa$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualAgnebyTiassa  <- pcgapdeptotndpoorIndividualAgnebyTiassa[!is.na(pcgapdeptotndpoorIndividualAgnebyTiassa)]
pcgapdeptotndpoorIndividualGÔh <- ehcvm_welfare_civ2018IndividualGÔh$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualGÔh  <- pcgapdeptotndpoorIndividualGÔh[!is.na(pcgapdeptotndpoorIndividualGÔh)]
pcgapdeptotndpoorIndividualCavally <- ehcvm_welfare_civ2018IndividualCavally$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualCavally  <- pcgapdeptotndpoorIndividualCavally[!is.na(pcgapdeptotndpoorIndividualCavally)]
pcgapdeptotndpoorIndividualBafing <- ehcvm_welfare_civ2018IndividualBafing$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualBafing  <- pcgapdeptotndpoorIndividualBafing[!is.na(pcgapdeptotndpoorIndividualBafing)]
pcgapdeptotndpoorIndividualBagoue <- ehcvm_welfare_civ2018IndividualBagoue$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualBagoue  <- pcgapdeptotndpoorIndividualBagoue[!is.na(pcgapdeptotndpoorIndividualBagoue)]
pcgapdeptotndpoorIndividualBelier <- ehcvm_welfare_civ2018IndividualBelier$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualBelier  <- pcgapdeptotndpoorIndividualBelier[!is.na(pcgapdeptotndpoorIndividualBelier)]
pcgapdeptotndpoorIndividualBere <- ehcvm_welfare_civ2018IndividualBere$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualBere  <- pcgapdeptotndpoorIndividualBere[!is.na(pcgapdeptotndpoorIndividualBere)]
pcgapdeptotndpoorIndividualBounkani <- ehcvm_welfare_civ2018IndividualBounkani$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualBounkani  <- pcgapdeptotndpoorIndividualBounkani[!is.na(pcgapdeptotndpoorIndividualBounkani)]
pcgapdeptotndpoorIndividualFolon <- ehcvm_welfare_civ2018IndividualFolon$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualFolon  <- pcgapdeptotndpoorIndividualFolon[!is.na(pcgapdeptotndpoorIndividualFolon)]
pcgapdeptotndpoorIndividualGbÔkle <- ehcvm_welfare_civ2018IndividualGbÔkle$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualGbÔkle  <- pcgapdeptotndpoorIndividualGbÔkle[!is.na(pcgapdeptotndpoorIndividualGbÔkle)]
pcgapdeptotndpoorIndividualGrandsPonts <- ehcvm_welfare_civ2018IndividualGrandsPonts$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualGrandsPonts  <- pcgapdeptotndpoorIndividualGrandsPonts[!is.na(pcgapdeptotndpoorIndividualGrandsPonts)]
pcgapdeptotndpoorIndividualGuemon <- ehcvm_welfare_civ2018IndividualGuemon$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualGuemon  <- pcgapdeptotndpoorIndividualGuemon[!is.na(pcgapdeptotndpoorIndividualGuemon)]
pcgapdeptotndpoorIndividualHambol <- ehcvm_welfare_civ2018IndividualHambol$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualHambol  <- pcgapdeptotndpoorIndividualHambol[!is.na(pcgapdeptotndpoorIndividualHambol)]
pcgapdeptotndpoorIndividualIffou <- ehcvm_welfare_civ2018IndividualIffou$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualIffou  <- pcgapdeptotndpoorIndividualIffou[!is.na(pcgapdeptotndpoorIndividualIffou)]
pcgapdeptotndpoorIndividualLaMe <- ehcvm_welfare_civ2018IndividualLaMe$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualLaMe  <- pcgapdeptotndpoorIndividualLaMe[!is.na(pcgapdeptotndpoorIndividualLaMe)]
pcgapdeptotndpoorIndividualNawa <- ehcvm_welfare_civ2018IndividualNawa$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualNawa  <- pcgapdeptotndpoorIndividualNawa[!is.na(pcgapdeptotndpoorIndividualNawa)]
pcgapdeptotndpoorIndividualTchologo <- ehcvm_welfare_civ2018IndividualTchologo$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualTchologo  <- pcgapdeptotndpoorIndividualTchologo[!is.na(pcgapdeptotndpoorIndividualTchologo)]
pcgapdeptotndpoorIndividualMoronou <- ehcvm_welfare_civ2018IndividualMoronou$pcgapdeptotndpoor 
pcgapdeptotndpoorIndividualMoronou  <- pcgapdeptotndpoorIndividualMoronou[!is.na(pcgapdeptotndpoorIndividualMoronou)]

# We check the first ten values for the new calculated variables: pcgapdeptotndpoor.

pcgapdeptotndpoorAutonomedAbidjan[1:10]
pcgapdeptotndpoorHautSassandra[1:10]
pcgapdeptotndpoorPoro[1:10]
pcgapdeptotndpoorGbeke[1:10]
pcgapdeptotndpoorIndenieDjuablin[1:10]
pcgapdeptotndpoorTonkpi[1:10]
pcgapdeptotndpoorYamoussoukro[1:10]
pcgapdeptotndpoorGontougo[1:10]
pcgapdeptotndpoorSanPedro[1:10]
pcgapdeptotndpoorKabadougou[1:10]
pcgapdeptotndpoorNZi[1:10]
pcgapdeptotndpoorMarahoue[1:10]
pcgapdeptotndpoorSudComoe[1:10]
pcgapdeptotndpoorWorodougou[1:10]
pcgapdeptotndpoorLôhDjiboua[1:10]
pcgapdeptotndpoorAgnebyTiassa[1:10]
pcgapdeptotndpoorGÔh[1:10]
pcgapdeptotndpoorCavally[1:10]
pcgapdeptotndpoorBafing[1:10]
pcgapdeptotndpoorBagoue[1:10]
pcgapdeptotndpoorBelier[1:10]
pcgapdeptotndpoorBere[1:10]
pcgapdeptotndpoorBounkani[1:10]
pcgapdeptotndpoorFolon[1:10]
pcgapdeptotndpoorGbÔkle[1:10]
pcgapdeptotndpoorGrandsPonts[1:10]
pcgapdeptotndpoorGuemon[1:10]
pcgapdeptotndpoorHambol[1:10]
pcgapdeptotndpoorIffou[1:10]
pcgapdeptotndpoorLaMe[1:10]
pcgapdeptotndpoorNawa[1:10]
pcgapdeptotndpoorTchologo[1:10]
pcgapdeptotndpoorMoronou[1:10]

pcgapdeptotndpoorIndividualAutonomedAbidjan[1:10]
pcgapdeptotndpoorIndividualHautSassandra[1:10]
pcgapdeptotndpoorIndividualPoro[1:10]
pcgapdeptotndpoorIndividualGbeke[1:10]
pcgapdeptotndpoorIndividualIndenieDjuablin[1:10]
pcgapdeptotndpoorIndividualTonkpi[1:10]
pcgapdeptotndpoorIndividualYamoussoukro[1:10]
pcgapdeptotndpoorIndividualGontougo[1:10]
pcgapdeptotndpoorIndividualSanPedro[1:10]
pcgapdeptotndpoorIndividualKabadougou[1:10]
pcgapdeptotndpoorIndividualNZi[1:10]
pcgapdeptotndpoorIndividualMarahoue[1:10]
pcgapdeptotndpoorIndividualSudComoe[1:10]
pcgapdeptotndpoorIndividualWorodougou[1:10]
pcgapdeptotndpoorIndividualLôhDjiboua[1:10]
pcgapdeptotndpoorIndividualAgnebyTiassa[1:10]
pcgapdeptotndpoorIndividualGÔh[1:10]
pcgapdeptotndpoorIndividualCavally[1:10]
pcgapdeptotndpoorIndividualBafing[1:10]
pcgapdeptotndpoorIndividualBagoue[1:10]
pcgapdeptotndpoorIndividualBelier[1:10]
pcgapdeptotndpoorIndividualBere[1:10]
pcgapdeptotndpoorIndividualBounkani[1:10]
pcgapdeptotndpoorIndividualFolon[1:10]
pcgapdeptotndpoorIndividualGbÔkle[1:10]
pcgapdeptotndpoorIndividualGrandsPonts[1:10]
pcgapdeptotndpoorIndividualGuemon[1:10]
pcgapdeptotndpoorIndividualHambol[1:10]
pcgapdeptotndpoorIndividualIffou[1:10]
pcgapdeptotndpoorIndividualLaMe[1:10]
pcgapdeptotndpoorIndividualNawa[1:10]
pcgapdeptotndpoorIndividualTchologo[1:10]
pcgapdeptotndpoorIndividualMoronou[1:10]

###################################################
### Calculation of Population (# of households) ###
###################################################

# We calculate the total population in Autonome d'Abidjan (# of households).

TotalPopulationAutonomedAbidjan <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 1) %>% pull(n)
TotalPopulationAutonomedAbidjan

# We calculate the total population in Haut-Sassandra (# of households).

TotalPopulationHautSassandra <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 2) %>% pull(n)
TotalPopulationHautSassandra

# We calculate the total population in Poro (# of households).

TotalPopulationPoro <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 3) %>% pull(n)
TotalPopulationPoro

# We calculate the total population in Gbeke (# of households).

TotalPopulationGbeke <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 4) %>% pull(n)
TotalPopulationGbeke

# We calculate the total population in Indenie-Djuablin (# of households).

TotalPopulationIndenieDjuablin <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 5) %>% pull(n)
TotalPopulationIndenieDjuablin

# We calculate the total population in Tonkpi (# of households).

TotalPopulationTonkpi <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 6) %>% pull(n)
TotalPopulationTonkpi

# We calculate the total population in Yamoussoukro (# of households).

TotalPopulationYamoussoukro <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 7) %>% pull(n)
TotalPopulationYamoussoukro

# We calculate the total population in Gontougo (# of households).

TotalPopulationGontougo <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 8) %>% pull(n)
TotalPopulationGontougo

# We calculate the total population in San-Pedro (# of households).

TotalPopulationSanPedro <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 9) %>% pull(n)
TotalPopulationSanPedro

# We calculate the total population in Kabadougou (# of households).

TotalPopulationKabadougou <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 10) %>% pull(n)
TotalPopulationKabadougou

# We calculate the total population in N'Zi (# of households).

TotalPopulationNZi <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 11) %>% pull(n)
TotalPopulationNZi

# We calculate the total population in Marahoue (# of households).

TotalPopulationMarahoue <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 12) %>% pull(n)
TotalPopulationMarahoue

# We calculate the total population in Sud-Comoe (# of households).

TotalPopulationSudComoe <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 13) %>% pull(n)
TotalPopulationSudComoe

# We calculate the total population in Worodougou (# of households).

TotalPopulationWorodougou <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 14) %>% pull(n)
TotalPopulationWorodougou

# We calculate the total population in Lôh-Djiboua (# of households).

TotalPopulationLôhDjiboua <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 15) %>% pull(n)
TotalPopulationLôhDjiboua

# We calculate the total population in Agneby-Tiassa (# of households).

TotalPopulationAgnebyTiassa <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 16) %>% pull(n)
TotalPopulationAgnebyTiassa

# We calculate the total population in GÔh (# of households).

TotalPopulationGÔh <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 17) %>% pull(n)
TotalPopulationGÔh

# We calculate the total population in Cavally (# of households).

TotalPopulationCavally <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 18) %>% pull(n)
TotalPopulationCavally

# We calculate the total population in Bafing (# of households).

TotalPopulationBafing <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 19) %>% pull(n)
TotalPopulationBafing

# We calculate the total population in Bagoue (# of households).

TotalPopulationBagoue <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 20) %>% pull(n)
TotalPopulationBagoue

# We calculate the total population in Belier (# of households).

TotalPopulationBelier <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 21) %>% pull(n)
TotalPopulationBelier

# We calculate the total population in Bere (# of households).

TotalPopulationBere <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 22) %>% pull(n)
TotalPopulationBere

# We calculate the total population in Bounkani (# of households).

TotalPopulationBounkani<- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 23) %>% pull(n)
TotalPopulationBounkani

# We calculate the total population in Folon (# of households).

TotalPopulationFolon<- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 24) %>% pull(n)
TotalPopulationFolon

# We calculate the total population in GbÔkle (# of households).

TotalPopulationGbÔkle <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 25) %>% pull(n)
TotalPopulationGbÔkle

# We calculate the total population in Grands-Ponts (# of households).

TotalPopulationGrandsPonts <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 26) %>% pull(n)
TotalPopulationGrandsPonts

# We calculate the total population in Guemon (# of households).

TotalPopulationGuemon <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 27) %>% pull(n)
TotalPopulationGuemon

# We calculate the total population in Hambol (# of households).

TotalPopulationHambol <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 28) %>% pull(n)
TotalPopulationHambol

# We calculate the total population in Iffou (# of households).

TotalPopulationIffou <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 29) %>% pull(n)
TotalPopulationIffou

# We calculate the total population in La Me (# of households).

TotalPopulationLaMe <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 30) %>% pull(n)
TotalPopulationLaMe

# We calculate the total population in Nawa (# of households).

TotalPopulationNawa <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 31) %>% pull(n)
TotalPopulationNawa

# We calculate the total population in Tchologo (# of households).

TotalPopulationTchologo <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 32) %>% pull(n)
TotalPopulationTchologo

# We calculate the total population in Moronou (# of households).

TotalPopulationMoronou <- ehcvm_welfare_civ2018 %>% count(region) %>% filter(region == 33) %>% pull(n)
TotalPopulationMoronou

##########################################################
#### Calculation of Poor Population (# of households) ####
##########################################################

# We calculate the total number of poor households in Autonome d'Abidjan (# of households).

TotalPoorAutonomedAbidjan <- length(pcgapdeptotndpoorAutonomedAbidjan)
TotalPoorAutonomedAbidjan

# We calculate the total number of poor households in Haut-Sassandra (# of households).

TotalPoorHautSassandra <- length(pcgapdeptotndpoorHautSassandra)
TotalPoorHautSassandra

# We calculate the total number of poor households in Poro (# of households).

TotalPoorPoro <- length(pcgapdeptotndpoorPoro)
TotalPoorPoro

# We calculate the total number of poor households in Gbeke (# of households).

TotalPoorGbeke <- length(pcgapdeptotndpoorGbeke)
TotalPoorGbeke

# We calculate the total number of poor households in Indenie-Djuablin (# of households).

TotalPoorIndenieDjuablin <- length(pcgapdeptotndpoorIndenieDjuablin)
TotalPoorIndenieDjuablin

# We calculate the total number of poor households in Tonkpi (# of households).

TotalPoorTonkpi <- length(pcgapdeptotndpoorTonkpi)
TotalPoorTonkpi

# We calculate the total number of poor households in Yamoussoukro (# of households).

TotalPoorYamoussoukro <- length(pcgapdeptotndpoorYamoussoukro)
TotalPoorYamoussoukro

# We calculate the total number of poor households in Gontougo (# of households).

TotalPoorGontougo <- length(pcgapdeptotndpoorGontougo)
TotalPoorGontougo

# We calculate the total number of poor households in San-Pedro (# of households).

TotalPoorSanPedro <- length(pcgapdeptotndpoorSanPedro)
TotalPoorSanPedro

# We calculate the total number of poor households in Kabadougou (# of households).

TotalPoorKabadougou <- length(pcgapdeptotndpoorKabadougou)
TotalPoorKabadougou

# We calculate the total number of poor households in N'Zi (# of households).

TotalPoorNZi <- length(pcgapdeptotndpoorNZi)
TotalPoorNZi

# We calculate the total number of poor households in Marahoue (# of households).

TotalPoorMarahoue <- length(pcgapdeptotndpoorMarahoue)
TotalPoorMarahoue

# We calculate the total number of poor households in Sud-Comoe (# of households).

TotalPoorSudComoe <- length(pcgapdeptotndpoorSudComoe)
TotalPoorSudComoe

# We calculate the total number of poor households in Worodougou (# of households).

TotalPoorWorodougou <- length(pcgapdeptotndpoorWorodougou)
TotalPoorWorodougou

# We calculate the total number of poor households in Lôh-Djiboua (# of households).

TotalPoorLôhDjiboua <- length(pcgapdeptotndpoorLôhDjiboua)
TotalPoorLôhDjiboua

# We calculate the total number of poor households in Agneby-Tiassa (# of households).

TotalPoorAgnebyTiassa <- length(pcgapdeptotndpoorAgnebyTiassa)
TotalPoorAgnebyTiassa

# We calculate the total number of poor households in GÔh (# of households).

TotalPoorGÔh <- length(pcgapdeptotndpoorGÔh)
TotalPoorGÔh

# We calculate the total number of poor households in Cavally (# of households).

TotalPoorCavally <- length(pcgapdeptotndpoorCavally)
TotalPoorCavally

# We calculate the total number of poor households in Bafing (# of households).

TotalPoorBafing <- length(pcgapdeptotndpoorBafing)
TotalPoorBafing

# We calculate the total number of poor households in Bagoue (# of households).

TotalPoorBagoue <- length(pcgapdeptotndpoorBagoue)
TotalPoorBagoue

# We calculate the total number of poor households in Belier (# of households).

TotalPoorBelier <- length(pcgapdeptotndpoorBelier)
TotalPoorBelier

# We calculate the total number of poor households in Bere (# of households).

TotalPoorBere <- length(pcgapdeptotndpoorBere)
TotalPoorBere

# We calculate the total number of poor households in Bounkani (# of households).

TotalPoorBounkani <- length(pcgapdeptotndpoorBounkani)
TotalPoorBounkani

# We calculate the total number of poor households in Folon (# of households).

TotalPoorFolon <- length(pcgapdeptotndpoorFolon)
TotalPoorFolon

# We calculate the total number of poor households in GbÔkle (# of households).

TotalPoorGbÔkle <- length(pcgapdeptotndpoorGbÔkle)
TotalPoorGbÔkle

# We calculate the total number of poor households in Grands-Ponts (# of households).

TotalPoorGrandsPonts <- length(pcgapdeptotndpoorGrandsPonts)
TotalPoorGrandsPonts

# We calculate the total number of poor households in Guemon (# of households).

TotalPoorGuemon <- length(pcgapdeptotndpoorGuemon)
TotalPoorGuemon

# We calculate the total number of poor households in Hambol (# of households).

TotalPoorHambol <- length(pcgapdeptotndpoorHambol)
TotalPoorHambol

# We calculate the total number of poor households in Iffou (# of households).

TotalPoorIffou <- length(pcgapdeptotndpoorIffou)
TotalPoorIffou

# We calculate the total number of poor households in La Me (# of households).

TotalPoorLaMe <- length(pcgapdeptotndpoorLaMe)
TotalPoorLaMe

# We calculate the total number of poor households in Nawa (# of households).

TotalPoorNawa <- length(pcgapdeptotndpoorNawa)
TotalPoorNawa

# We calculate the total number of poor households in Tchologo (# of households).

TotalPoorTchologo <- length(pcgapdeptotndpoorTchologo)
TotalPoorTchologo

# We calculate the total number of poor households in Moronou (# of households).

TotalPoorMoronou <- length(pcgapdeptotndpoorMoronou)
TotalPoorMoronou

####################################################
### Calculation of Population (# of individuals) ###
####################################################

# We calculate the total population in Autonome d'Abidjan (# of individuals).

TotalPopulationIndividualAutonomedAbidjan <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 1) %>% pull(n)
TotalPopulationIndividualAutonomedAbidjan

# We calculate the total population in Haut-Sassandra (# of individuals).

TotalPopulationIndividualHautSassandra <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 2) %>% pull(n)
TotalPopulationIndividualHautSassandra

# We calculate the total population in Poro (# of individuals).

TotalPopulationIndividualPoro <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 3) %>% pull(n)
TotalPopulationIndividualPoro

# We calculate the total population in Gbeke (# of individuals).

TotalPopulationIndividualGbeke <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 4) %>% pull(n)
TotalPopulationIndividualGbeke

# We calculate the total population in Indenie-Djuablin (# of individuals).

TotalPopulationIndividualIndenieDjuablin <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 5) %>% pull(n)
TotalPopulationIndividualIndenieDjuablin

# We calculate the total population in Tonkpi (# of individuals).

TotalPopulationIndividualTonkpi <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 6) %>% pull(n)
TotalPopulationIndividualTonkpi

# We calculate the total population in Yamoussoukro (# of individuals).

TotalPopulationIndividualYamoussoukro <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 7) %>% pull(n)
TotalPopulationIndividualYamoussoukro

# We calculate the total population in Gontougo (# of individuals).

TotalPopulationIndividualGontougo <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 8) %>% pull(n)
TotalPopulationIndividualGontougo

# We calculate the total population in San-Pedro (# of individuals).

TotalPopulationIndividualSanPedro <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 9) %>% pull(n)
TotalPopulationIndividualSanPedro

# We calculate the total population in Kabadougou (# of individuals).

TotalPopulationIndividualKabadougou <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 10) %>% pull(n)
TotalPopulationIndividualKabadougou

# We calculate the total population in N'Zi (# of individuals).

TotalPopulationIndividualNZi <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 11) %>% pull(n)
TotalPopulationIndividualNZi

# We calculate the total population in Marahoue (# of individuals).

TotalPopulationIndividualMarahoue <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 12) %>% pull(n)
TotalPopulationIndividualMarahoue

# We calculate the total population in Sud-Comoe (# of individuals).

TotalPopulationIndividualSudComoe <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 13) %>% pull(n)
TotalPopulationIndividualSudComoe

# We calculate the total population in Worodougou (# of individuals).

TotalPopulationIndividualWorodougou <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 14) %>% pull(n)
TotalPopulationIndividualWorodougou

# We calculate the total population in Lôh-Djiboua (# of individuals).

TotalPopulationIndividualLôhDjiboua <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 15) %>% pull(n)
TotalPopulationIndividualLôhDjiboua

# We calculate the total population in Agneby-Tiassa (# of individuals).

TotalPopulationIndividualAgnebyTiassa <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 16) %>% pull(n)
TotalPopulationIndividualAgnebyTiassa

# We calculate the total population in GÔh (# of individuals).

TotalPopulationIndividualGÔh <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 17) %>% pull(n)
TotalPopulationIndividualGÔh

# We calculate the total population in Cavally (# of individuals).

TotalPopulationIndividualCavally <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 18) %>% pull(n)
TotalPopulationIndividualCavally

# We calculate the total population in Bafing (# of individuals).

TotalPopulationIndividualBafing <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 19) %>% pull(n)
TotalPopulationIndividualBafing

# We calculate the total population in Bagoue (# of individuals).

TotalPopulationIndividualBagoue <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 20) %>% pull(n)
TotalPopulationIndividualBagoue

# We calculate the total population in Belier (# of individuals).

TotalPopulationIndividualBelier <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 21) %>% pull(n)
TotalPopulationIndividualBelier

# We calculate the total population in Bere (# of individuals).

TotalPopulationIndividualBere <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 22) %>% pull(n)
TotalPopulationIndividualBere

# We calculate the total population in Bounkani (# of individuals).

TotalPopulationIndividualBounkani <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 23) %>% pull(n)
TotalPopulationIndividualBounkani

# We calculate the total population in Folon (# of individuals).

TotalPopulationIndividualFolon <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 24) %>% pull(n)
TotalPopulationIndividualFolon

# We calculate the total population in GbÔkle (# of individuals).

TotalPopulationIndividualGbÔkle <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 25) %>% pull(n)
TotalPopulationIndividualGbÔkle

# We calculate the total population in Grands-Ponts (# of individuals).

TotalPopulationIndividualGrandsPonts <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 26) %>% pull(n)
TotalPopulationIndividualGrandsPonts

# We calculate the total population in Guemon (# of individuals).

TotalPopulationIndividualGuemon <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 27) %>% pull(n)
TotalPopulationIndividualGuemon

# We calculate the total population in Hambol (# of individuals).

TotalPopulationIndividualHambol <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 28) %>% pull(n)
TotalPopulationIndividualHambol

# We calculate the total population in Iffou (# of individuals).

TotalPopulationIndividualIffou <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 29) %>% pull(n)
TotalPopulationIndividualIffou

# We calculate the total population in La Me (# of individuals).

TotalPopulationIndividualLaMe <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 30) %>% pull(n)
TotalPopulationIndividualLaMe

# We calculate the total population in Nawa (# of individuals).

TotalPopulationIndividualNawa <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 31) %>% pull(n)
TotalPopulationIndividualNawa

# We calculate the total population in Tchologo (# of individuals).

TotalPopulationIndividualTchologo <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 32) %>% pull(n)
TotalPopulationIndividualTchologo

# We calculate the total population in Moronou (# of individuals).

TotalPopulationIndividualMoronou <- ehcvm_welfare_civ20182 %>% count(region) %>% filter(region == 33) %>% pull(n)
TotalPopulationIndividualMoronou

############################################################
##### Calculation of Poor Population (# of individuals) ####
############################################################

# We calculate the total number of poor households in Autonome d'Abidjan (# of individuals).

TotalPoorIndividualAutonomedAbidjan <- length(pcgapdeptotndpoorIndividualAutonomedAbidjan)
TotalPoorIndividualAutonomedAbidjan

# We calculate the total number of poor households in Haut-Sassandra (# of individuals).

TotalPoorIndividualHautSassandra <- length(pcgapdeptotndpoorIndividualHautSassandra)
TotalPoorIndividualHautSassandra

# We calculate the total number of poor households in Poro (# of individuals).

TotalPoorIndividualPoro <- length(pcgapdeptotndpoorIndividualPoro)
TotalPoorIndividualPoro

# We calculate the total number of poor households in Gbeke (# of individuals).

TotalPoorIndividualGbeke <- length(pcgapdeptotndpoorIndividualGbeke)
TotalPoorIndividualGbeke

# We calculate the total number of poor households in Indenie-Djuablin (# of individuals).

TotalPoorIndividualIndenieDjuablin <- length(pcgapdeptotndpoorIndividualIndenieDjuablin)
TotalPoorIndividualIndenieDjuablin

# We calculate the total number of poor households in Tonkpi (# of individuals).

TotalPoorIndividualTonkpi <- length(pcgapdeptotndpoorIndividualTonkpi)
TotalPoorIndividualTonkpi

# We calculate the total number of poor households in Yamoussoukro (# of individuals).

TotalPoorIndividualYamoussoukro <- length(pcgapdeptotndpoorIndividualYamoussoukro)
TotalPoorIndividualYamoussoukro

# We calculate the total number of poor households in Gontougo (# of individuals).

TotalPoorIndividualGontougo <- length(pcgapdeptotndpoorIndividualGontougo)
TotalPoorIndividualGontougo

# We calculate the total number of poor households in San-Pedro (# of individuals).

TotalPoorIndividualSanPedro <- length(pcgapdeptotndpoorIndividualSanPedro)
TotalPoorIndividualSanPedro

# We calculate the total number of poor households in Kabadougou (# of individuals).

TotalPoorIndividualKabadougou <- length(pcgapdeptotndpoorIndividualKabadougou)
TotalPoorIndividualKabadougou

# We calculate the total number of poor households in N'Zi (# of individuals).

TotalPoorIndividualNZi <- length(pcgapdeptotndpoorIndividualNZi)
TotalPoorIndividualNZi

# We calculate the total number of poor households in Marahoue (# of individuals).

TotalPoorIndividualMarahoue <- length(pcgapdeptotndpoorIndividualMarahoue)
TotalPoorIndividualMarahoue

# We calculate the total number of poor households in Sud-Comoe (# of individuals).

TotalPoorIndividualSudComoe <- length(pcgapdeptotndpoorIndividualSudComoe)
TotalPoorIndividualSudComoe

# We calculate the total number of poor households in Worodougou (# of individuals).

TotalPoorIndividualWorodougou <- length(pcgapdeptotndpoorIndividualWorodougou)
TotalPoorIndividualWorodougou

# We calculate the total number of poor households in Lôh-Djiboua (# of individuals).

TotalPoorIndividualLôhDjiboua <- length(pcgapdeptotndpoorIndividualLôhDjiboua)
TotalPoorIndividualLôhDjiboua

# We calculate the total number of poor households in Agneby-Tiassa (# of individuals).

TotalPoorIndividualAgnebyTiassa <- length(pcgapdeptotndpoorIndividualAgnebyTiassa)
TotalPoorIndividualAgnebyTiassa

# We calculate the total number of poor households in GÔh (# of individuals).

TotalPoorIndividualGÔh <- length(pcgapdeptotndpoorIndividualGÔh)
TotalPoorIndividualGÔh

# We calculate the total number of poor households in Cavally (# of individuals).

TotalPoorIndividualCavally <- length(pcgapdeptotndpoorIndividualCavally)
TotalPoorIndividualCavally

# We calculate the total number of poor households in Bafing (# of individuals).

TotalPoorIndividualBafing <- length(pcgapdeptotndpoorIndividualBafing)
TotalPoorIndividualBafing

# We calculate the total number of poor households in Bagoue (# of individuals).

TotalPoorIndividualBagoue <- length(pcgapdeptotndpoorIndividualBagoue)
TotalPoorIndividualBagoue

# We calculate the total number of poor households in Belier (# of individuals).

TotalPoorIndividualBelier <- length(pcgapdeptotndpoorIndividualBelier)
TotalPoorIndividualBelier

# We calculate the total number of poor households in Bere (# of individuals).

TotalPoorIndividualBere <- length(pcgapdeptotndpoorIndividualBere)
TotalPoorIndividualBere

# We calculate the total number of poor households in Bounkani (# of individuals).

TotalPoorIndividualBounkani <- length(pcgapdeptotndpoorIndividualBounkani)
TotalPoorIndividualBounkani

# We calculate the total number of poor households in Folon (# of individuals).

TotalPoorIndividualFolon <- length(pcgapdeptotndpoorIndividualFolon)
TotalPoorIndividualFolon

# We calculate the total number of poor households in GbÔkle (# of individuals).

TotalPoorIndividualGbÔkle <- length(pcgapdeptotndpoorIndividualGbÔkle)
TotalPoorIndividualGbÔkle

# We calculate the total number of poor households in Grands-Ponts (# of individuals).

TotalPoorIndividualGrandsPonts <- length(pcgapdeptotndpoorIndividualGrandsPonts)
TotalPoorIndividualGrandsPonts

# We calculate the total number of poor households in Guemon (# of individuals).

TotalPoorIndividualGuemon <- length(pcgapdeptotndpoorIndividualGuemon)
TotalPoorIndividualGuemon

# We calculate the total number of poor households in Hambol (# of individuals).

TotalPoorIndividualHambol <- length(pcgapdeptotndpoorIndividualHambol)
TotalPoorIndividualHambol

# We calculate the total number of poor households in Iffou (# of individuals).

TotalPoorIndividualIffou <- length(pcgapdeptotndpoorIndividualIffou)
TotalPoorIndividualIffou

# We calculate the total number of poor households in La Me (# of individuals).

TotalPoorIndividualLaMe <- length(pcgapdeptotndpoorIndividualLaMe)
TotalPoorIndividualLaMe

# We calculate the total number of poor households in Nawa (# of individuals).

TotalPoorIndividualNawa <- length(pcgapdeptotndpoorIndividualNawa)
TotalPoorIndividualNawa

# We calculate the total number of poor households in Tchologo (# of individuals).

TotalPoorIndividualTchologo <- length(pcgapdeptotndpoorIndividualTchologo)
TotalPoorIndividualTchologo

# We calculate the total number of poor households in Moronou (# of individuals).

TotalPoorIndividualMoronou <- length(pcgapdeptotndpoorIndividualMoronou)
TotalPoorIndividualMoronou

##################################################
## Estimation of Parameters  (# of individuals) ##
##################################################

# We define a function to print values with two decimals.

percent2 <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...))
}

########### Autonome d'Abidjan ##########

# For Autonome d'Abidjan, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan <- TotalPoorIndividualAutonomedAbidjan/(TotalPoorIndividualAutonomedAbidjan * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualAutonomedAbidjan)))
sprintf("Maximum Likelihood Estimator of α for Autonome d'Abidjan: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan))

# For Autonome d'Abidjan, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan <- (PovertyLine - mean(pcgapdeptotndpoorIndividualAutonomedAbidjan))/mean(pcgapdeptotndpoorIndividualAutonomedAbidjan)
sprintf("Method of Moments Estimator of α for Autonome d'Abidjan: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan))

########### Haut-Sassandra ##########

# For Haut-Sassandra, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra <- TotalPoorIndividualHautSassandra/(TotalPoorIndividualHautSassandra * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualHautSassandra)))
sprintf("Maximum Likelihood Estimator of α for Haut-Sassandra: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra))

# For Haut-Sassandra, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra <- (PovertyLine - mean(pcgapdeptotndpoorIndividualHautSassandra))/mean(pcgapdeptotndpoorIndividualHautSassandra)
sprintf("Method of Moments Estimator of α for Haut-Sassandra: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra))

########### Poro ##########

# For Poro, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro <- TotalPoorIndividualPoro/(TotalPoorIndividualPoro * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualPoro)))
sprintf("Maximum Likelihood Estimator of α for Poro: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro))

# For Poro, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro <- (PovertyLine - mean(pcgapdeptotndpoorIndividualPoro))/mean(pcgapdeptotndpoorIndividualPoro)
sprintf("Method of Moments Estimator of α for Poro: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro))

########### Gbeke ##########

# For Gbeke, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke <- TotalPoorIndividualGbeke/(TotalPoorIndividualGbeke * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualGbeke)))
sprintf("Maximum Likelihood Estimator of α for Gbeke: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke))

# For Gbeke, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke <- (PovertyLine - mean(pcgapdeptotndpoorIndividualGbeke))/mean(pcgapdeptotndpoorIndividualGbeke)
sprintf("Method of Moments Estimator of α for Gbeke: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke))

########### Indenie-Djuablin ##########

# For Indenie-Djuablin, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin <- TotalPoorIndividualIndenieDjuablin/(TotalPoorIndividualIndenieDjuablin * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualIndenieDjuablin)))
sprintf("Maximum Likelihood Estimator of α for Indenie-Djuablin: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin))

# For Indenie-Djuablin, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin <- (PovertyLine - mean(pcgapdeptotndpoorIndividualIndenieDjuablin))/mean(pcgapdeptotndpoorIndividualIndenieDjuablin)
sprintf("Method of Moments Estimator of α for Indenie-Djuablin: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin))

########### Tonkpi ##########

# For Tonkpi, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi <- TotalPoorIndividualTonkpi/(TotalPoorIndividualTonkpi * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualTonkpi)))
sprintf("Maximum Likelihood Estimator of α for Tonkpi: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi))

# For Tonkpi, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi <- (PovertyLine - mean(pcgapdeptotndpoorIndividualTonkpi))/mean(pcgapdeptotndpoorIndividualTonkpi)
sprintf("Method of Moments Estimator of α for Tonkpi: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi))

########### Yamoussoukro ##########

# For Yamoussoukro, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro <- TotalPoorIndividualYamoussoukro/(TotalPoorIndividualYamoussoukro * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualYamoussoukro)))
sprintf("Maximum Likelihood Estimator of α for Yamoussoukro: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro))

# For Yamoussoukro, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro <- (PovertyLine - mean(pcgapdeptotndpoorIndividualYamoussoukro))/mean(pcgapdeptotndpoorIndividualYamoussoukro)
sprintf("Method of Moments Estimator of α for Yamoussoukro: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro))

########### Gontougo ##########

# For Gontougo, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo <- TotalPoorIndividualGontougo/(TotalPoorIndividualGontougo * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualGontougo)))
sprintf("Maximum Likelihood Estimator of α for Gontougo: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo))

# For Gontougo, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo <- (PovertyLine - mean(pcgapdeptotndpoorIndividualGontougo))/mean(pcgapdeptotndpoorIndividualGontougo)
sprintf("Method of Moments Estimator of α for Gontougo: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo))

########### San-Pedro ##########

# For San-Pedro, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro <- TotalPoorIndividualSanPedro/(TotalPoorIndividualSanPedro * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualSanPedro)))
sprintf("Maximum Likelihood Estimator of α for San-Pedro: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro))

# For San-Pedro, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro <- (PovertyLine - mean(pcgapdeptotndpoorIndividualSanPedro))/mean(pcgapdeptotndpoorIndividualSanPedro)
sprintf("Method of Moments Estimator of α for San-Pedro: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro))

########### Kabadougou ##########

# For Kabadougou, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou <- TotalPoorIndividualKabadougou/(TotalPoorIndividualKabadougou * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualKabadougou)))
sprintf("Maximum Likelihood Estimator of α for Kabadougou: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou))

# For Kabadougou, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou <- (PovertyLine - mean(pcgapdeptotndpoorIndividualKabadougou))/mean(pcgapdeptotndpoorIndividualKabadougou)
sprintf("Method of Moments Estimator of α for Kabadougou: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou))

########### N'Zi ##########

# For N'Zi, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi <- TotalPoorIndividualNZi/(TotalPoorIndividualNZi * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualNZi)))
sprintf("Maximum Likelihood Estimator of α for N'Zi: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi))

# For N'Zi, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi <- (PovertyLine - mean(pcgapdeptotndpoorIndividualNZi))/mean(pcgapdeptotndpoorIndividualNZi)
sprintf("Method of Moments Estimator of α for N'Zi: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi))

########### Marahoue ##########

# For Marahoue, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue <- TotalPoorIndividualMarahoue/(TotalPoorIndividualMarahoue * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualMarahoue)))
sprintf("Maximum Likelihood Estimator of α for Marahoue: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue))

# For Marahoue, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue <- (PovertyLine - mean(pcgapdeptotndpoorIndividualMarahoue))/mean(pcgapdeptotndpoorIndividualMarahoue)
sprintf("Method of Moments Estimator of α for Marahoue: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue))

########### Sud-Comoe ##########

# For Sud-Comoe, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe <- TotalPoorIndividualSudComoe/(TotalPoorIndividualSudComoe * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualSudComoe)))
sprintf("Maximum Likelihood Estimator of α for Sud-Comoe: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe))

# For Sud-Comoe, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe <- (PovertyLine - mean(pcgapdeptotndpoorIndividualSudComoe))/mean(pcgapdeptotndpoorIndividualSudComoe)
sprintf("Method of Moments Estimator of α for Sud-Comoe: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe))

########### Worodougou ##########

# For Worodougou, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou <- TotalPoorIndividualWorodougou/(TotalPoorIndividualWorodougou * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualWorodougou)))
sprintf("Maximum Likelihood Estimator of α for Worodougou: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou))

# For Worodougou, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou <- (PovertyLine - mean(pcgapdeptotndpoorIndividualWorodougou))/mean(pcgapdeptotndpoorIndividualWorodougou)
sprintf("Method of Moments Estimator of α for Worodougou: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou))

########### Lôh-Djiboua ##########

# For Lôh-Djiboua, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua <- TotalPoorIndividualLôhDjiboua/(TotalPoorIndividualLôhDjiboua * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualLôhDjiboua)))
sprintf("Maximum Likelihood Estimator of α for Lôh-Djiboua: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua))

# For Lôh-Djiboua, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua <- (PovertyLine - mean(pcgapdeptotndpoorIndividualLôhDjiboua))/mean(pcgapdeptotndpoorIndividualLôhDjiboua)
sprintf("Method of Moments Estimator of α for Lôh-Djiboua: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua))

########### Agneby-Tiassa ##########

# For Agneby-Tiassa, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa <- TotalPoorIndividualAgnebyTiassa/(TotalPoorIndividualAgnebyTiassa * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualAgnebyTiassa)))
sprintf("Maximum Likelihood Estimator of α for Agneby-Tiassa: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa))

# For Agneby-Tiassa, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa <- (PovertyLine - mean(pcgapdeptotndpoorIndividualAgnebyTiassa))/mean(pcgapdeptotndpoorIndividualAgnebyTiassa)
sprintf("Method of Moments Estimator of α for Agneby-Tiassa: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa))

########### GÔh ##########

# For GÔh, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh <- TotalPoorIndividualGÔh/(TotalPoorIndividualGÔh * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualGÔh)))
sprintf("Maximum Likelihood Estimator of α for GÔh: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh))

# For GÔh, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh <- (PovertyLine - mean(pcgapdeptotndpoorIndividualGÔh))/mean(pcgapdeptotndpoorIndividualGÔh)
sprintf("Method of Moments Estimator of α for GÔh: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh))

########### Cavally ##########

# For Cavally, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally <- TotalPoorIndividualCavally/(TotalPoorIndividualCavally * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualCavally)))
sprintf("Maximum Likelihood Estimator of α for Cavally: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally))

# For Cavally, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally <- (PovertyLine - mean(pcgapdeptotndpoorIndividualCavally))/mean(pcgapdeptotndpoorIndividualCavally)
sprintf("Method of Moments Estimator of α for Cavally: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally))

########### Bafing ##########

# For Bafing, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing <- TotalPoorIndividualBafing/(TotalPoorIndividualBafing * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualBafing)))
sprintf("Maximum Likelihood Estimator of α for Bafing: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing))

# For Bafing, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing <- (PovertyLine - mean(pcgapdeptotndpoorIndividualBafing))/mean(pcgapdeptotndpoorIndividualBafing)
sprintf("Method of Moments Estimator of α for Bafing: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing))

########### Bagoue ##########

# For Bagoue, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue <- TotalPoorIndividualBagoue/(TotalPoorIndividualBagoue * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualBagoue)))
sprintf("Maximum Likelihood Estimator of α for Bagoue: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue))

# For Bagoue, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue <- (PovertyLine - mean(pcgapdeptotndpoorIndividualBagoue))/mean(pcgapdeptotndpoorIndividualBagoue)
sprintf("Method of Moments Estimator of α for Bagoue: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue))

########### Belier ##########

# For Belier, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier <- TotalPoorIndividualBelier/(TotalPoorIndividualBelier * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualBelier)))
sprintf("Maximum Likelihood Estimator of α for Belier: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier))

# For Belier, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier <- (PovertyLine - mean(pcgapdeptotndpoorIndividualBelier))/mean(pcgapdeptotndpoorIndividualBelier)
sprintf("Method of Moments Estimator of α for Belier: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier))

########### Bere ##########

# For Bere, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere <- TotalPoorIndividualBere/(TotalPoorIndividualBere * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualBere)))
sprintf("Maximum Likelihood Estimator of α for Bere: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere))

# For Bere, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualBere <- (PovertyLine - mean(pcgapdeptotndpoorIndividualBere))/mean(pcgapdeptotndpoorIndividualBere)
sprintf("Method of Moments Estimator of α for Bere: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualBere))

########### Bounkani ##########

# For Bounkani, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani <- TotalPoorIndividualBounkani/(TotalPoorIndividualBounkani * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualBounkani)))
sprintf("Maximum Likelihood Estimator of α for Bounkani: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani))

# For Bounkani, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani <- (PovertyLine - mean(pcgapdeptotndpoorIndividualBounkani))/mean(pcgapdeptotndpoorIndividualBounkani)
sprintf("Method of Moments Estimator of α for Bounkani: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani))

########### Folon ##########

# For Folon, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon <- TotalPoorIndividualFolon/(TotalPoorIndividualFolon * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualFolon)))
sprintf("Maximum Likelihood Estimator of α for Folon: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon))

# For Folon, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon <- (PovertyLine - mean(pcgapdeptotndpoorIndividualFolon))/mean(pcgapdeptotndpoorIndividualFolon)
sprintf("Method of Moments Estimator of α for Folon: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon))

########### GbÔkle ##########

# For GbÔkle, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle <- TotalPoorIndividualGbÔkle/(TotalPoorIndividualGbÔkle * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualGbÔkle)))
sprintf("Maximum Likelihood Estimator of α for GbÔkle: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle))

# For GbÔkle, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle <- (PovertyLine - mean(pcgapdeptotndpoorIndividualGbÔkle))/mean(pcgapdeptotndpoorIndividualGbÔkle)
sprintf("Method of Moments Estimator of α for GbÔkle: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle))

########### Grands-Ponts ##########

# For Grands-Ponts, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts <- TotalPoorIndividualGrandsPonts/(TotalPoorIndividualGrandsPonts * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualGrandsPonts)))
sprintf("Maximum Likelihood Estimator of α for Grands-Ponts: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts))

# For Grands-Ponts, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts <- (PovertyLine - mean(pcgapdeptotndpoorIndividualGrandsPonts))/mean(pcgapdeptotndpoorIndividualGrandsPonts)
sprintf("Method of Moments Estimator of α for Grands-Ponts: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts))

########### Guemon ##########

# For Guemon, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon <- TotalPoorIndividualGuemon/(TotalPoorIndividualGuemon * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualGuemon)))
sprintf("Maximum Likelihood Estimator of α for Guemon: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon))

# For Guemon, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon <- (PovertyLine - mean(pcgapdeptotndpoorIndividualGuemon))/mean(pcgapdeptotndpoorIndividualGuemon)
sprintf("Method of Moments Estimator of α for Guemon: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon))

########### Hambol ##########

# For Hambol, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol <- TotalPoorIndividualHambol/(TotalPoorIndividualHambol * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualHambol)))
sprintf("Maximum Likelihood Estimator of α for Hambol: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol))

# For Hambol, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol <- (PovertyLine - mean(pcgapdeptotndpoorIndividualHambol))/mean(pcgapdeptotndpoorIndividualHambol)
sprintf("Method of Moments Estimator of α for Hambol: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol))

########### Iffou ##########

# For Iffou, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou <- TotalPoorIndividualIffou/(TotalPoorIndividualIffou * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualIffou)))
sprintf("Maximum Likelihood Estimator of α for Iffou: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou))

# For Iffou, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou <- (PovertyLine - mean(pcgapdeptotndpoorIndividualIffou))/mean(pcgapdeptotndpoorIndividualIffou)
sprintf("Method of Moments Estimator of α for Iffou: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou))

########### La Me ##########

# For La Me, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe <- TotalPoorIndividualLaMe/(TotalPoorIndividualLaMe * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualLaMe)))
sprintf("Maximum Likelihood Estimator of α for La Me: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe))

# For La Me, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe <- (PovertyLine - mean(pcgapdeptotndpoorIndividualLaMe))/mean(pcgapdeptotndpoorIndividualLaMe)
sprintf("Method of Moments Estimator of α for La Me: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe))

########### Nawa ##########

# For Nawa, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa <- TotalPoorIndividualNawa/(TotalPoorIndividualNawa * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualNawa)))
sprintf("Maximum Likelihood Estimator of α for Nawa: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa))

# For Nawa, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa <- (PovertyLine - mean(pcgapdeptotndpoorIndividualNawa))/mean(pcgapdeptotndpoorIndividualNawa)
sprintf("Method of Moments Estimator of α for Nawa: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa))

########### Tchologo ##########

# For Tchologo, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo <- TotalPoorIndividualTchologo/(TotalPoorIndividualTchologo * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualTchologo)))
sprintf("Maximum Likelihood Estimator of α for Tchologo: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo))

# For Tchologo, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo <- (PovertyLine - mean(pcgapdeptotndpoorIndividualTchologo))/mean(pcgapdeptotndpoorIndividualTchologo)
sprintf("Method of Moments Estimator of α for Tchologo: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo))

########### Moronou ##########

# For Moronou, we calculate the maximum likelihood estimator for α.

MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou <- TotalPoorIndividualMoronou/(TotalPoorIndividualMoronou * log(PovertyLine) - sum(log(PovertyLine - pcgapdeptotndpoorIndividualMoronou)))
sprintf("Maximum Likelihood Estimator of α for Moronou: %s", percent2(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou))

# For Moronou, we calculate the method of moments estimator for α.

MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou <- (PovertyLine - mean(pcgapdeptotndpoorIndividualMoronou))/mean(pcgapdeptotndpoorIndividualMoronou)
sprintf("Method of Moments Estimator of α for Moronou: %s", percent2(MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou))

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

DStatisticEstimation <- function(ecdf, tcdf) { set.seed(1) # Set seed for replication.
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

########### Autonome d'Abidjan ##########

ecdfAutonomedAbidjan <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualAutonomedAbidjan)))

# Maximum Likelihood Estimator (MLE) - KS test for Autonome d'Abidjan: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualAutonomedAbidjan)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorAutonomedAbidjan <- DStatisticEstimation(ecdfAutonomedAbidjan, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Autonome d'Abidjan: %s", MprimeMaximumLikelihoodEstimatorAutonomedAbidjan)

PValueMaximumLikelihoodEstimatorAutonomedAbidjan <- PValueEstimation(pcgapdeptotndpoorIndividualAutonomedAbidjan, MprimeMaximumLikelihoodEstimatorAutonomedAbidjan, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Autonome d'Abidjan: %s", PValueMaximumLikelihoodEstimatorAutonomedAbidjan)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorAutonomedAbidjan, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Autonome d'Abidjan:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualAutonomedAbidjan)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorAutonomedAbidjan <- DStatisticEstimation(ecdfAutonomedAbidjan, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Autonome d'Abidjan: %s", MprimeMethodofMomentsEstimatorAutonomedAbidjan)

PValueMethodofMomentsEstimatorAutonomedAbidjan <- PValueEstimation(pcgapdeptotndpoorIndividualAutonomedAbidjan, MprimeMethodofMomentsEstimatorAutonomedAbidjan, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Autonome d'Abidjan: %s", PValueMethodofMomentsEstimatorAutonomedAbidjan)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorAutonomedAbidjan, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine) # two-sided, exact

########### Haut-Sassandra ##########

ecdfHautSassandra <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualHautSassandra)))

# Maximum Likelihood Estimator (MLE) - KS test for Haut-Sassandra: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualHautSassandra)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorHautSassandra <- DStatisticEstimation(ecdfHautSassandra, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Haut-Sassandra: %s", MprimeMaximumLikelihoodEstimatorHautSassandra)

PValueMaximumLikelihoodEstimatorHautSassandra <- PValueEstimation(pcgapdeptotndpoorIndividualHautSassandra, MprimeMaximumLikelihoodEstimatorHautSassandra, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Haut-Sassandra: %s", PValueMaximumLikelihoodEstimatorHautSassandra)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorHautSassandra, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Haut-Sassandra:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualHautSassandra)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorHautSassandra <- DStatisticEstimation(ecdfHautSassandra, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Haut-Sassandra: %s", MprimeMethodofMomentsEstimatorHautSassandra)

PValueMethodofMomentsEstimatorHautSassandra <- PValueEstimation(pcgapdeptotndpoorIndividualHautSassandra, MprimeMethodofMomentsEstimatorHautSassandra, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Haut-Sassandra: %s", PValueMethodofMomentsEstimatorHautSassandra)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorHautSassandra, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine) # two-sided, exact

########### Poro ##########

ecdfPoro <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualPoro)))

# Maximum Likelihood Estimator (MLE) - KS test for Poro: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualPoro)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorPoro <- DStatisticEstimation(ecdfPoro, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Poro: %s", MprimeMaximumLikelihoodEstimatorPoro)

PValueMaximumLikelihoodEstimatorPoro <- PValueEstimation(pcgapdeptotndpoorIndividualPoro, MprimeMaximumLikelihoodEstimatorPoro, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Poro: %s", PValueMaximumLikelihoodEstimatorPoro)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorPoro, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Poro:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualPoro <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualPoro)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorPoro <- DStatisticEstimation(ecdfPoro, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualPoro)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Poro: %s", MprimeMethodofMomentsEstimatorPoro)

PValueMethodofMomentsEstimatorPoro <- PValueEstimation(pcgapdeptotndpoorIndividualPoro, MprimeMethodofMomentsEstimatorPoro, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Poro: %s", PValueMethodofMomentsEstimatorPoro)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorPoro, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine) # two-sided, exact

########### Gbeke ##########

ecdfGbeke <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualGbeke)))

# Maximum Likelihood Estimator (MLE) - KS test for Gbeke: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualGbeke)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorGbeke <- DStatisticEstimation(ecdfGbeke, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Gbeke: %s", MprimeMaximumLikelihoodEstimatorGbeke)

PValueMaximumLikelihoodEstimatorGbeke <- PValueEstimation(pcgapdeptotndpoorIndividualGbeke, MprimeMaximumLikelihoodEstimatorGbeke, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Gbeke: %s", PValueMaximumLikelihoodEstimatorGbeke)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorGbeke, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Gbeke:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualGbeke)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorGbeke <- DStatisticEstimation(ecdfGbeke, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Gbeke: %s", MprimeMethodofMomentsEstimatorGbeke)

PValueMethodofMomentsEstimatorGbeke <- PValueEstimation(pcgapdeptotndpoorIndividualGbeke, MprimeMethodofMomentsEstimatorGbeke, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Gbeke: %s", PValueMethodofMomentsEstimatorGbeke)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorGbeke, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine) # two-sided, exact

########### Indenie-Djuablin ##########

ecdfIndenieDjuablin <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualIndenieDjuablin)))

# Maximum Likelihood Estimator (MLE) - KS test for Indenie-Djuablin: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualIndenieDjuablin)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorIndenieDjuablin <- DStatisticEstimation(ecdfIndenieDjuablin, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Indenie-Djuablin: %s", MprimeMaximumLikelihoodEstimatorIndenieDjuablin)

PValueMaximumLikelihoodEstimatorIndenieDjuablin <- PValueEstimation(pcgapdeptotndpoorIndividualIndenieDjuablin, MprimeMaximumLikelihoodEstimatorIndenieDjuablin, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Indenie-Djuablin: %s", PValueMaximumLikelihoodEstimatorIndenieDjuablin)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorIndenieDjuablin, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Indenie-Djuablin:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualIndenieDjuablin)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorIndenieDjuablin <- DStatisticEstimation(ecdfIndenieDjuablin, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Indenie-Djuablin: %s", MprimeMethodofMomentsEstimatorIndenieDjuablin)

PValueMethodofMomentsEstimatorIndenieDjuablin <- PValueEstimation(pcgapdeptotndpoorIndividualIndenieDjuablin, MprimeMethodofMomentsEstimatorIndenieDjuablin, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Indenie-Djuablin: %s", PValueMethodofMomentsEstimatorIndenieDjuablin)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorIndenieDjuablin, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine) # two-sided, exact

########### Tonkpi ##########

ecdfTonkpi <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualTonkpi)))

# Maximum Likelihood Estimator (MLE) - KS test for Tonkpi: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualTonkpi)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorTonkpi <- DStatisticEstimation(ecdfTonkpi, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Tonkpi: %s", MprimeMaximumLikelihoodEstimatorTonkpi)

PValueMaximumLikelihoodEstimatorTonkpi <- PValueEstimation(pcgapdeptotndpoorIndividualTonkpi, MprimeMaximumLikelihoodEstimatorTonkpi, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Tonkpi: %s", PValueMaximumLikelihoodEstimatorTonkpi)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorTonkpi, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Tonkpi:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualTonkpi)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorTonkpi <- DStatisticEstimation(ecdfTonkpi, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Tonkpi: %s", MprimeMethodofMomentsEstimatorTonkpi)

PValueMethodofMomentsEstimatorTonkpi <- PValueEstimation(pcgapdeptotndpoorIndividualTonkpi, MprimeMethodofMomentsEstimatorTonkpi, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Tonkpi: %s", PValueMethodofMomentsEstimatorTonkpi)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorTonkpi, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine) # two-sided, exact

########### Yamoussoukro ##########

ecdfYamoussoukro <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualYamoussoukro)))

# Maximum Likelihood Estimator (MLE) - KS test for Yamoussoukro: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualYamoussoukro)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorYamoussoukro <- DStatisticEstimation(ecdfYamoussoukro, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Yamoussoukro: %s", MprimeMaximumLikelihoodEstimatorYamoussoukro)

PValueMaximumLikelihoodEstimatorYamoussoukro <- PValueEstimation(pcgapdeptotndpoorIndividualYamoussoukro, MprimeMaximumLikelihoodEstimatorYamoussoukro, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Yamoussoukro: %s", PValueMaximumLikelihoodEstimatorYamoussoukro)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorYamoussoukro, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Yamoussoukro:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualYamoussoukro)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorYamoussoukro <- DStatisticEstimation(ecdfYamoussoukro, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Yamoussoukro: %s", MprimeMethodofMomentsEstimatorYamoussoukro)

PValueMethodofMomentsEstimatorYamoussoukro <- PValueEstimation(pcgapdeptotndpoorIndividualYamoussoukro, MprimeMethodofMomentsEstimatorYamoussoukro, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Yamoussoukro: %s", PValueMethodofMomentsEstimatorYamoussoukro)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorYamoussoukro, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine) # two-sided, exact

########### Gontougo ##########

ecdfGontougo <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualGontougo)))

# Maximum Likelihood Estimator (MLE) - KS test for Gontougo: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualGontougo)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorGontougo <- DStatisticEstimation(ecdfGontougo, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Gontougo: %s", MprimeMaximumLikelihoodEstimatorGontougo)

PValueMaximumLikelihoodEstimatorGontougo <- PValueEstimation(pcgapdeptotndpoorIndividualGontougo, MprimeMaximumLikelihoodEstimatorGontougo, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Gontougo: %s", PValueMaximumLikelihoodEstimatorGontougo)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorGontougo, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Gontougo:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualGontougo)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorGontougo <- DStatisticEstimation(ecdfGontougo, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Gontougo: %s", MprimeMethodofMomentsEstimatorGontougo)

PValueMethodofMomentsEstimatorGontougo <- PValueEstimation(pcgapdeptotndpoorIndividualGontougo, MprimeMethodofMomentsEstimatorGontougo, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Gontougo: %s", PValueMethodofMomentsEstimatorGontougo)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorGontougo, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine) # two-sided, exact

########### San-Pedro ##########

ecdfSanPedro <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualSanPedro)))

# Maximum Likelihood Estimator (MLE) - KS test for San-Pedro: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualSanPedro)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorSanPedro <- DStatisticEstimation(ecdfSanPedro, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for San-Pedro: %s", MprimeMaximumLikelihoodEstimatorSanPedro)

PValueMaximumLikelihoodEstimatorSanPedro <- PValueEstimation(pcgapdeptotndpoorIndividualSanPedro, MprimeMaximumLikelihoodEstimatorSanPedro, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for San-Pedro: %s", PValueMaximumLikelihoodEstimatorSanPedro)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorSanPedro, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for San-Pedro:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualSanPedro)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorSanPedro <- DStatisticEstimation(ecdfSanPedro, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro)

sprintf("Method of Moments Estimator (MME) - KS D statistic for San-Pedro: %s", MprimeMethodofMomentsEstimatorSanPedro)

PValueMethodofMomentsEstimatorSanPedro <- PValueEstimation(pcgapdeptotndpoorIndividualSanPedro, MprimeMethodofMomentsEstimatorSanPedro, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for San-Pedro: %s", PValueMethodofMomentsEstimatorSanPedro)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorSanPedro, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine) # two-sided, exact

########### Kabadougou ##########

ecdfKabadougou <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualKabadougou)))

# Maximum Likelihood Estimator (MLE) - KS test for Kabadougou: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualKabadougou)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorKabadougou <- DStatisticEstimation(ecdfKabadougou, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Kabadougou: %s", MprimeMaximumLikelihoodEstimatorKabadougou)

PValueMaximumLikelihoodEstimatorKabadougou <- PValueEstimation(pcgapdeptotndpoorIndividualKabadougou, MprimeMaximumLikelihoodEstimatorKabadougou, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Kabadougou: %s", PValueMaximumLikelihoodEstimatorKabadougou)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorKabadougou, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Kabadougou:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualKabadougou)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorKabadougou <- DStatisticEstimation(ecdfKabadougou, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Kabadougou: %s", MprimeMethodofMomentsEstimatorKabadougou)

PValueMethodofMomentsEstimatorKabadougou <- PValueEstimation(pcgapdeptotndpoorIndividualKabadougou, MprimeMethodofMomentsEstimatorKabadougou, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Kabadougou: %s", PValueMethodofMomentsEstimatorKabadougou)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorKabadougou, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine) # two-sided, exact

########### N'Zi ##########

ecdfNZi <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualNZi)))

# Maximum Likelihood Estimator (MLE) - KS test for N'Zi: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualNZi)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorNZi <- DStatisticEstimation(ecdfNZi, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for N'Zi: %s", MprimeMaximumLikelihoodEstimatorNZi)

PValueMaximumLikelihoodEstimatorNZi <- PValueEstimation(pcgapdeptotndpoorIndividualNZi, MprimeMaximumLikelihoodEstimatorNZi, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for N'Zi: %s", PValueMaximumLikelihoodEstimatorNZi)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorNZi, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for N'Zi:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualNZi <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualNZi)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorNZi <- DStatisticEstimation(ecdfNZi, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualNZi)

sprintf("Method of Moments Estimator (MME) - KS D statistic for N'Zi: %s", MprimeMethodofMomentsEstimatorNZi)

PValueMethodofMomentsEstimatorNZi <- PValueEstimation(pcgapdeptotndpoorIndividualNZi, MprimeMethodofMomentsEstimatorNZi, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for N'Zi: %s", PValueMethodofMomentsEstimatorNZi)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorNZi, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine) # two-sided, exact

########### Marahoue ##########

ecdfMarahoue <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualMarahoue)))

# Maximum Likelihood Estimator (MLE) - KS test for Marahoue: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualMarahoue)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorMarahoue <- DStatisticEstimation(ecdfMarahoue, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Marahoue: %s", MprimeMaximumLikelihoodEstimatorMarahoue)

PValueMaximumLikelihoodEstimatorMarahoue <- PValueEstimation(pcgapdeptotndpoorIndividualMarahoue, MprimeMaximumLikelihoodEstimatorMarahoue, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Marahoue: %s", PValueMaximumLikelihoodEstimatorMarahoue)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorMarahoue, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Marahoue:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualMarahoue)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorMarahoue <- DStatisticEstimation(ecdfMarahoue, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Marahoue: %s", MprimeMethodofMomentsEstimatorMarahoue)

PValueMethodofMomentsEstimatorMarahoue <- PValueEstimation(pcgapdeptotndpoorIndividualMarahoue, MprimeMethodofMomentsEstimatorMarahoue, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Marahoue: %s", PValueMethodofMomentsEstimatorMarahoue)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorMarahoue, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine) # two-sided, exact

########### Sud-Comoe ##########

ecdfSudComoe <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualSudComoe)))

# Maximum Likelihood Estimator (MLE) - KS test for Sud-Comoe: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualSudComoe)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorSudComoe <- DStatisticEstimation(ecdfSudComoe, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Sud-Comoe: %s", MprimeMaximumLikelihoodEstimatorSudComoe)

PValueMaximumLikelihoodEstimatorSudComoe <- PValueEstimation(pcgapdeptotndpoorIndividualSudComoe, MprimeMaximumLikelihoodEstimatorSudComoe, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Sud-Comoe: %s", PValueMaximumLikelihoodEstimatorSudComoe)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorSudComoe, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Sud-Comoe:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualSudComoe)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorSudComoe <- DStatisticEstimation(ecdfSudComoe, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Sud-Comoe: %s", MprimeMethodofMomentsEstimatorSudComoe)

PValueMethodofMomentsEstimatorSudComoe <- PValueEstimation(pcgapdeptotndpoorIndividualSudComoe, MprimeMethodofMomentsEstimatorSudComoe, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Sud-Comoe: %s", PValueMethodofMomentsEstimatorSudComoe)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorSudComoe, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine) # two-sided, exact

########### Worodougou ##########

ecdfWorodougou <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualWorodougou)))

# Maximum Likelihood Estimator (MLE) - KS test for Worodougou: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualWorodougou)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorWorodougou <- DStatisticEstimation(ecdfWorodougou, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Worodougou: %s", MprimeMaximumLikelihoodEstimatorWorodougou)

PValueMaximumLikelihoodEstimatorWorodougou <- PValueEstimation(pcgapdeptotndpoorIndividualWorodougou, MprimeMaximumLikelihoodEstimatorWorodougou, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Worodougou: %s", PValueMaximumLikelihoodEstimatorWorodougou)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorWorodougou, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Worodougou:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualWorodougou)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorWorodougou <- DStatisticEstimation(ecdfWorodougou, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Worodougou: %s", MprimeMethodofMomentsEstimatorWorodougou)

PValueMethodofMomentsEstimatorWorodougou <- PValueEstimation(pcgapdeptotndpoorIndividualWorodougou, MprimeMethodofMomentsEstimatorWorodougou, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Worodougou: %s", PValueMethodofMomentsEstimatorWorodougou)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorWorodougou, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine) # two-sided, exact

########### Lôh-Djiboua ##########

ecdfLôhDjiboua <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualLôhDjiboua)))

# Maximum Likelihood Estimator (MLE) - KS test for Lôh-Djiboua: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualLôhDjiboua)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorLôhDjiboua <- DStatisticEstimation(ecdfLôhDjiboua, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Lôh-Djiboua: %s", MprimeMaximumLikelihoodEstimatorLôhDjiboua)

PValueMaximumLikelihoodEstimatorLôhDjiboua <- PValueEstimation(pcgapdeptotndpoorIndividualLôhDjiboua, MprimeMaximumLikelihoodEstimatorLôhDjiboua, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Lôh-Djiboua: %s", PValueMaximumLikelihoodEstimatorLôhDjiboua)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorLôhDjiboua, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Lôh-Djiboua:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualLôhDjiboua)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorLôhDjiboua <- DStatisticEstimation(ecdfLôhDjiboua, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Lôh-Djiboua: %s", MprimeMethodofMomentsEstimatorLôhDjiboua)

PValueMethodofMomentsEstimatorLôhDjiboua <- PValueEstimation(pcgapdeptotndpoorIndividualLôhDjiboua, MprimeMethodofMomentsEstimatorLôhDjiboua, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Lôh-Djiboua: %s", PValueMethodofMomentsEstimatorLôhDjiboua)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorLôhDjiboua, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine) # two-sided, exact

########### Agneby-Tiassa ##########

ecdfAgnebyTiassa <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualAgnebyTiassa)))

# Maximum Likelihood Estimator (MLE) - KS test for Agneby-Tiassa: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualAgnebyTiassa)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorAgnebyTiassa <- DStatisticEstimation(ecdfAgnebyTiassa, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Agneby-Tiassa: %s", MprimeMaximumLikelihoodEstimatorAgnebyTiassa)

PValueMaximumLikelihoodEstimatorAgnebyTiassa <- PValueEstimation(pcgapdeptotndpoorIndividualAgnebyTiassa, MprimeMaximumLikelihoodEstimatorAgnebyTiassa, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Agneby-Tiassa: %s", PValueMaximumLikelihoodEstimatorAgnebyTiassa)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorAgnebyTiassa, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Agneby-Tiassa:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualAgnebyTiassa)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorAgnebyTiassa <- DStatisticEstimation(ecdfAgnebyTiassa, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Agneby-Tiassa: %s", MprimeMethodofMomentsEstimatorAgnebyTiassa)

PValueMethodofMomentsEstimatorAgnebyTiassa <- PValueEstimation(pcgapdeptotndpoorIndividualAgnebyTiassa, MprimeMethodofMomentsEstimatorAgnebyTiassa, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Agneby-Tiassa: %s", PValueMethodofMomentsEstimatorAgnebyTiassa)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorAgnebyTiassa, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine) # two-sided, exact

########### GÔh ##########

ecdfGÔh <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualGÔh)))

# Maximum Likelihood Estimator (MLE) - KS test for GÔh: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualGÔh)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorGÔh <- DStatisticEstimation(ecdfGÔh, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for GÔh: %s", MprimeMaximumLikelihoodEstimatorGÔh)

PValueMaximumLikelihoodEstimatorGÔh <- PValueEstimation(pcgapdeptotndpoorIndividualGÔh, MprimeMaximumLikelihoodEstimatorGÔh, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for GÔh: %s", PValueMaximumLikelihoodEstimatorGÔh)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorGÔh, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for GÔh:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualGÔh)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorGÔh <- DStatisticEstimation(ecdfGÔh, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh)

sprintf("Method of Moments Estimator (MME) - KS D statistic for GÔh: %s", MprimeMethodofMomentsEstimatorGÔh)

PValueMethodofMomentsEstimatorGÔh <- PValueEstimation(pcgapdeptotndpoorIndividualGÔh, MprimeMethodofMomentsEstimatorGÔh, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for GÔh: %s", PValueMethodofMomentsEstimatorGÔh)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorGÔh, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine) # two-sided, exact

########### Cavally ##########

ecdfCavally <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualCavally)))

# Maximum Likelihood Estimator (MLE) - KS test for Cavally: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCavally)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorCavally <- DStatisticEstimation(ecdfCavally, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Cavally: %s", MprimeMaximumLikelihoodEstimatorCavally)

PValueMaximumLikelihoodEstimatorCavally <- PValueEstimation(pcgapdeptotndpoorIndividualCavally, MprimeMaximumLikelihoodEstimatorCavally, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Cavally: %s", PValueMaximumLikelihoodEstimatorCavally)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCavally, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Cavally:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCavally <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualCavally)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorCavally <- DStatisticEstimation(ecdfCavally, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualCavally)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Cavally: %s", MprimeMethodofMomentsEstimatorCavally)

PValueMethodofMomentsEstimatorCavally <- PValueEstimation(pcgapdeptotndpoorIndividualCavally, MprimeMethodofMomentsEstimatorCavally, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Cavally: %s", PValueMethodofMomentsEstimatorCavally)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorCavally, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine) # two-sided, exact

########### Bafing ##########

ecdfBafing <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualBafing)))

# Maximum Likelihood Estimator (MLE) - KS test for Bafing: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBafing)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorBafing <- DStatisticEstimation(ecdfBafing, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Bafing: %s", MprimeMaximumLikelihoodEstimatorBafing)

PValueMaximumLikelihoodEstimatorBafing <- PValueEstimation(pcgapdeptotndpoorIndividualBafing, MprimeMaximumLikelihoodEstimatorBafing, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Bafing: %s", PValueMaximumLikelihoodEstimatorBafing)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBafing, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Bafing:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBafing <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBafing)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorBafing <- DStatisticEstimation(ecdfBafing, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBafing)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Bafing: %s", MprimeMethodofMomentsEstimatorBafing)

PValueMethodofMomentsEstimatorBafing <- PValueEstimation(pcgapdeptotndpoorIndividualBafing, MprimeMethodofMomentsEstimatorBafing, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Bafing: %s", PValueMethodofMomentsEstimatorBafing)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBafing, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine) # two-sided, exact

########### Bagoue ##########

ecdfBagoue <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualBagoue)))

# Maximum Likelihood Estimator (MLE) - KS test for Bagoue: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBagoue)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorBagoue <- DStatisticEstimation(ecdfBagoue, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Bagoue: %s", MprimeMaximumLikelihoodEstimatorBagoue)

PValueMaximumLikelihoodEstimatorBagoue <- PValueEstimation(pcgapdeptotndpoorIndividualBagoue, MprimeMaximumLikelihoodEstimatorBagoue, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Bagoue: %s", PValueMaximumLikelihoodEstimatorBagoue)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBagoue, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Bagoue:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBagoue)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorBagoue <- DStatisticEstimation(ecdfBagoue, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Bagoue: %s", MprimeMethodofMomentsEstimatorBagoue)

PValueMethodofMomentsEstimatorBagoue <- PValueEstimation(pcgapdeptotndpoorIndividualBagoue, MprimeMethodofMomentsEstimatorBagoue, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Bagoue: %s", PValueMethodofMomentsEstimatorBagoue)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBagoue, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine) # two-sided, exact

########### Belier ##########

ecdfBelier <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualBelier)))

# Maximum Likelihood Estimator (MLE) - KS test for Belier: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBelier)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorBelier <- DStatisticEstimation(ecdfBelier, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Belier: %s", MprimeMaximumLikelihoodEstimatorBelier)

PValueMaximumLikelihoodEstimatorBelier <- PValueEstimation(pcgapdeptotndpoorIndividualBelier, MprimeMaximumLikelihoodEstimatorBelier, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Belier: %s", PValueMaximumLikelihoodEstimatorBelier)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBelier, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Belier:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBelier <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBelier)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorBelier <- DStatisticEstimation(ecdfBelier, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBelier)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Belier: %s", MprimeMethodofMomentsEstimatorBelier)

PValueMethodofMomentsEstimatorBelier <- PValueEstimation(pcgapdeptotndpoorIndividualBelier, MprimeMethodofMomentsEstimatorBelier, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Belier: %s", PValueMethodofMomentsEstimatorBelier)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBelier, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine) # two-sided, exact

########### Bere ##########

ecdfBere <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualBere)))

# Maximum Likelihood Estimator (MLE) - KS test for Bere: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBere)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorBere <- DStatisticEstimation(ecdfBere, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Bere: %s", MprimeMaximumLikelihoodEstimatorBere)

PValueMaximumLikelihoodEstimatorBere <- PValueEstimation(pcgapdeptotndpoorIndividualBere, MprimeMaximumLikelihoodEstimatorBere, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Bere: %s", PValueMaximumLikelihoodEstimatorBere)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBere, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Bere:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBere <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBere)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorBere <- DStatisticEstimation(ecdfBere, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBere)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Bere: %s", MprimeMethodofMomentsEstimatorBere)

PValueMethodofMomentsEstimatorBere <- PValueEstimation(pcgapdeptotndpoorIndividualBere, MprimeMethodofMomentsEstimatorBere, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Bere: %s", PValueMethodofMomentsEstimatorBere)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBere, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine) # two-sided, exact

########### Bounkani ##########

ecdfBounkani <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualBounkani)))

# Maximum Likelihood Estimator (MLE) - KS test for Bounkani: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBounkani)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorBounkani <- DStatisticEstimation(ecdfBounkani, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Bounkani: %s", MprimeMaximumLikelihoodEstimatorBounkani)

PValueMaximumLikelihoodEstimatorBounkani <- PValueEstimation(pcgapdeptotndpoorIndividualBounkani, MprimeMaximumLikelihoodEstimatorBounkani, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Bounkani: %s", PValueMaximumLikelihoodEstimatorBounkani)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBounkani, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Bounkani:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualBounkani)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorBounkani <- DStatisticEstimation(ecdfBounkani, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Bounkani: %s", MprimeMethodofMomentsEstimatorBounkani)

PValueMethodofMomentsEstimatorBounkani <- PValueEstimation(pcgapdeptotndpoorIndividualBounkani, MprimeMethodofMomentsEstimatorBounkani, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Bounkani: %s", PValueMethodofMomentsEstimatorBounkani)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorBounkani, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine) # two-sided, exact

########### Folon ##########

ecdfFolon <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualFolon)))

# Maximum Likelihood Estimator (MLE) - KS test for Folon: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualFolon)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorFolon <- DStatisticEstimation(ecdfFolon, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Folon: %s", MprimeMaximumLikelihoodEstimatorFolon)

PValueMaximumLikelihoodEstimatorFolon <- PValueEstimation(pcgapdeptotndpoorIndividualFolon, MprimeMaximumLikelihoodEstimatorFolon, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Folon: %s", PValueMaximumLikelihoodEstimatorFolon)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorFolon, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Folon:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualFolon <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualFolon)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorFolon <- DStatisticEstimation(ecdfFolon, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualFolon)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Folon: %s", MprimeMethodofMomentsEstimatorFolon)

PValueMethodofMomentsEstimatorFolon <- PValueEstimation(pcgapdeptotndpoorIndividualFolon, MprimeMethodofMomentsEstimatorFolon, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Folon: %s", PValueMethodofMomentsEstimatorFolon)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorFolon, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine) # two-sided, exact

########### GbÔkle ##########

ecdfGbÔkle <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualGbÔkle)))

# Maximum Likelihood Estimator (MLE) - KS test for GbÔkle: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualGbÔkle)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorGbÔkle <- DStatisticEstimation(ecdfGbÔkle, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for GbÔkle: %s", MprimeMaximumLikelihoodEstimatorGbÔkle)

PValueMaximumLikelihoodEstimatorGbÔkle <- PValueEstimation(pcgapdeptotndpoorIndividualGbÔkle, MprimeMaximumLikelihoodEstimatorGbÔkle, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for GbÔkle: %s", PValueMaximumLikelihoodEstimatorGbÔkle)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorGbÔkle, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for GbÔkle:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualGbÔkle)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorGbÔkle <- DStatisticEstimation(ecdfGbÔkle, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle)

sprintf("Method of Moments Estimator (MME) - KS D statistic for GbÔkle: %s", MprimeMethodofMomentsEstimatorGbÔkle)

PValueMethodofMomentsEstimatorGbÔkle <- PValueEstimation(pcgapdeptotndpoorIndividualGbÔkle, MprimeMethodofMomentsEstimatorGbÔkle, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for GbÔkle: %s", PValueMethodofMomentsEstimatorGbÔkle)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorGbÔkle, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine) # two-sided, exact

########### Grands-Ponts ##########

ecdfGrandsPonts <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualGrandsPonts)))

# Maximum Likelihood Estimator (MLE) - KS test for Grands-Ponts: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualGrandsPonts)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorGrandsPonts <- DStatisticEstimation(ecdfGrandsPonts, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Grands-Ponts: %s", MprimeMaximumLikelihoodEstimatorGrandsPonts)

PValueMaximumLikelihoodEstimatorGrandsPonts <- PValueEstimation(pcgapdeptotndpoorIndividualGrandsPonts, MprimeMaximumLikelihoodEstimatorGrandsPonts, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Grands-Ponts: %s", PValueMaximumLikelihoodEstimatorGrandsPonts)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorGrandsPonts, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Grands-Ponts:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualGrandsPonts)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorGrandsPonts <- DStatisticEstimation(ecdfGrandsPonts, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Grands-Ponts: %s", MprimeMethodofMomentsEstimatorGrandsPonts)

PValueMethodofMomentsEstimatorGrandsPonts <- PValueEstimation(pcgapdeptotndpoorIndividualGrandsPonts, MprimeMethodofMomentsEstimatorGrandsPonts, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Grands-Ponts: %s", PValueMethodofMomentsEstimatorGrandsPonts)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorGrandsPonts, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine) # two-sided, exact

########### Guemon ##########

ecdfGuemon <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualGuemon)))

# Maximum Likelihood Estimator (MLE) - KS test for Guemon: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualGuemon)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorGuemon <- DStatisticEstimation(ecdfGuemon, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Guemon: %s", MprimeMaximumLikelihoodEstimatorGuemon)

PValueMaximumLikelihoodEstimatorGuemon <- PValueEstimation(pcgapdeptotndpoorIndividualGuemon, MprimeMaximumLikelihoodEstimatorGuemon, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Guemon: %s", PValueMaximumLikelihoodEstimatorGuemon)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorGuemon, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Guemon:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualGuemon)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorGuemon <- DStatisticEstimation(ecdfGuemon, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Guemon: %s", MprimeMethodofMomentsEstimatorGuemon)

PValueMethodofMomentsEstimatorGuemon <- PValueEstimation(pcgapdeptotndpoorIndividualGuemon, MprimeMethodofMomentsEstimatorGuemon, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Guemon: %s", PValueMethodofMomentsEstimatorGuemon)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorGuemon, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine) # two-sided, exact

########### Hambol ##########

ecdfHambol <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualHambol)))

# Maximum Likelihood Estimator (MLE) - KS test for Hambol: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualHambol)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorHambol <- DStatisticEstimation(ecdfHambol, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Hambol: %s", MprimeMaximumLikelihoodEstimatorHambol)

PValueMaximumLikelihoodEstimatorHambol <- PValueEstimation(pcgapdeptotndpoorIndividualHambol, MprimeMaximumLikelihoodEstimatorHambol, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Hambol: %s", PValueMaximumLikelihoodEstimatorHambol)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorHambol, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Hambol:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualHambol <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualHambol)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorHambol <- DStatisticEstimation(ecdfHambol, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualHambol)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Hambol: %s", MprimeMethodofMomentsEstimatorHambol)

PValueMethodofMomentsEstimatorHambol <- PValueEstimation(pcgapdeptotndpoorIndividualHambol, MprimeMethodofMomentsEstimatorHambol, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Hambol: %s", PValueMethodofMomentsEstimatorHambol)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorHambol, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine) # two-sided, exact

########### Iffou ##########

ecdfIffou <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualIffou)))

# Maximum Likelihood Estimator (MLE) - KS test for Iffou: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualIffou)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorIffou <- DStatisticEstimation(ecdfIffou, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Iffou: %s", MprimeMaximumLikelihoodEstimatorIffou)

PValueMaximumLikelihoodEstimatorIffou <- PValueEstimation(pcgapdeptotndpoorIndividualIffou, MprimeMaximumLikelihoodEstimatorIffou, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Iffou: %s", PValueMaximumLikelihoodEstimatorIffou)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorIffou, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Iffou:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualIffou <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualIffou)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorIffou <- DStatisticEstimation(ecdfIffou, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualIffou)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Iffou: %s", MprimeMethodofMomentsEstimatorIffou)

PValueMethodofMomentsEstimatorIffou <- PValueEstimation(pcgapdeptotndpoorIndividualIffou, MprimeMethodofMomentsEstimatorIffou, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Iffou: %s", PValueMethodofMomentsEstimatorIffou)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorIffou, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine) # two-sided, exact

########### La Me ##########

ecdfLaMe <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualLaMe)))

# Maximum Likelihood Estimator (MLE) - KS test for La Me: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualLaMe)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorLaMe <- DStatisticEstimation(ecdfLaMe, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for La Me: %s", MprimeMaximumLikelihoodEstimatorLaMe)

PValueMaximumLikelihoodEstimatorLaMe <- PValueEstimation(pcgapdeptotndpoorIndividualLaMe, MprimeMaximumLikelihoodEstimatorLaMe, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for La Me: %s", PValueMaximumLikelihoodEstimatorLaMe)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorLaMe, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for La Me:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualLaMe)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorLaMe <- DStatisticEstimation(ecdfLaMe, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe)

sprintf("Method of Moments Estimator (MME) - KS D statistic for La Me: %s", MprimeMethodofMomentsEstimatorLaMe)

PValueMethodofMomentsEstimatorLaMe <- PValueEstimation(pcgapdeptotndpoorIndividualLaMe, MprimeMethodofMomentsEstimatorLaMe, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for La Me: %s", PValueMethodofMomentsEstimatorLaMe)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorLaMe, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine) # two-sided, exact

########### Nawa ##########

ecdfNawa <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualNawa)))

# Maximum Likelihood Estimator (MLE) - KS test for Nawa: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualNawa)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorNawa <- DStatisticEstimation(ecdfNawa, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Nawa: %s", MprimeMaximumLikelihoodEstimatorNawa)

PValueMaximumLikelihoodEstimatorNawa <- PValueEstimation(pcgapdeptotndpoorIndividualNawa, MprimeMaximumLikelihoodEstimatorNawa, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Nawa: %s", PValueMaximumLikelihoodEstimatorNawa)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorNawa, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Nawa:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualNawa <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualNawa)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorNawa <- DStatisticEstimation(ecdfNawa, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualNawa)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Nawa: %s", MprimeMethodofMomentsEstimatorNawa)

PValueMethodofMomentsEstimatorNawa <- PValueEstimation(pcgapdeptotndpoorIndividualNawa, MprimeMethodofMomentsEstimatorNawa, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Nawa: %s", PValueMethodofMomentsEstimatorNawa)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorNawa, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine) # two-sided, exact

########### Tchologo ##########

ecdfTchologo <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualTchologo)))

# Maximum Likelihood Estimator (MLE) - KS test for Tchologo: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualTchologo)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorTchologo <- DStatisticEstimation(ecdfTchologo, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Tchologo: %s", MprimeMaximumLikelihoodEstimatorTchologo)

PValueMaximumLikelihoodEstimatorTchologo <- PValueEstimation(pcgapdeptotndpoorIndividualTchologo, MprimeMaximumLikelihoodEstimatorTchologo, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Tchologo: %s", PValueMaximumLikelihoodEstimatorTchologo)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorTchologo, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Tchologo:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualTchologo)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorTchologo <- DStatisticEstimation(ecdfTchologo, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Tchologo: %s", MprimeMethodofMomentsEstimatorTchologo)

PValueMethodofMomentsEstimatorTchologo <- PValueEstimation(pcgapdeptotndpoorIndividualTchologo, MprimeMethodofMomentsEstimatorTchologo, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Tchologo: %s", PValueMethodofMomentsEstimatorTchologo)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorTchologo, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) # two-sided, exact

########### Moronou ##########

ecdfMoronou <- ecdf_func(sort(unique(pcgapdeptotndpoorIndividualMoronou)))

# Maximum Likelihood Estimator (MLE) - KS test for Moronou: 

tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualMoronou)),  shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine)

MprimeMaximumLikelihoodEstimatorMoronou <- DStatisticEstimation(ecdfMoronou, tcdfMaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou)

sprintf("Maximum Likelihood Estimator (MLE) - KS D statistic for Moronou: %s", MprimeMaximumLikelihoodEstimatorMoronou)

PValueMaximumLikelihoodEstimatorMoronou <- PValueEstimation(pcgapdeptotndpoorIndividualMoronou, MprimeMaximumLikelihoodEstimatorMoronou, 10000)

sprintf("Maximum Likelihood Estimator (MLE) - KS p-value for Moronou: %s", PValueMaximumLikelihoodEstimatorMoronou)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorMoronou, "ppbeta", shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine) # two-sided, exact

# Method of Moments Estimator (MME) - KS test for Moronou:

tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou <- ppbeta(sort(unique(pcgapdeptotndpoorIndividualMoronou)),  shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine)

MprimeMethodofMomentsEstimatorMoronou <- DStatisticEstimation(ecdfMoronou, tcdfMethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou)

sprintf("Method of Moments Estimator (MME) - KS D statistic for Moronou: %s", MprimeMethodofMomentsEstimatorMoronou)

PValueMethodofMomentsEstimatorMoronou <- PValueEstimation(pcgapdeptotndpoorIndividualMoronou, MprimeMethodofMomentsEstimatorMoronou, 10000)

sprintf("Method of Moments Estimator (MME) - KS p-value for Moronou: %s", PValueMethodofMomentsEstimatorMoronou)

# We compare the results we obtain from our functions DStatisticEstimation and PValueEstimation with those we can obtain using 
# the ks.test function in R (from the stats package).

ks.test(pcgapdeptotndpoorMoronou, "ppbeta", shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine) # two-sided, exact

###################################################
################## R-SQUARED ######################
###################################################

# Define a function to compute the ECDF
percent4 <- function(x, digits = 4, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...))
}

########### Autonome d'Abidjan ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Autonome d'Abidjan: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualAutonomedAbidjan, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualAutonomedAbidjan)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualAutonomedAbidjan, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualAutonomedAbidjan)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualAutonomedAbidjan, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualAutonomedAbidjan))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Autonome d'Abidjan: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualAutonomedAbidjan, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualAutonomedAbidjan)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualAutonomedAbidjan, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualAutonomedAbidjan)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualAutonomedAbidjan, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualAutonomedAbidjan))^2))))

########### Haut-Sassandra ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Haut-Sassandra: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualHautSassandra, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualHautSassandra)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualHautSassandra, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualHautSassandra)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualHautSassandra, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualHautSassandra))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Haut-Sassandra: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualHautSassandra, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualHautSassandra)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualHautSassandra, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualHautSassandra)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualHautSassandra, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualHautSassandra))^2))))

########### Poro ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Poro: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualPoro, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualPoro)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualPoro, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualPoro)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualPoro, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualPoro))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Poro: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualPoro, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualPoro)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualPoro, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualPoro)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualPoro, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualPoro))^2))))

########### Gbeke ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Gbeke: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualGbeke, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGbeke)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualGbeke, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGbeke)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualGbeke, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualGbeke))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Gbeke: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualGbeke, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGbeke)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualGbeke, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGbeke)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualGbeke, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualGbeke))^2))))

########### Indenie-Djuablin ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Indenie-Djuablin: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualIndenieDjuablin, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualIndenieDjuablin)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualIndenieDjuablin, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualIndenieDjuablin)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualIndenieDjuablin, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualIndenieDjuablin))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Indenie-Djuablin: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualIndenieDjuablin, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualIndenieDjuablin)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualIndenieDjuablin, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualIndenieDjuablin)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualIndenieDjuablin, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualIndenieDjuablin))^2))))

########### Tonkpi ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Tonkpi: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualTonkpi, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualTonkpi)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualTonkpi, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualTonkpi)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualTonkpi, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualTonkpi))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Tonkpi: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualTonkpi, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualTonkpi)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualTonkpi, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualTonkpi)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualTonkpi, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualTonkpi))^2))))

########### Yamoussoukro ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Yamoussoukro: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualYamoussoukro, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualYamoussoukro)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualYamoussoukro, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualYamoussoukro)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualYamoussoukro, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualYamoussoukro))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Yamoussoukro: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualYamoussoukro, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualYamoussoukro)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualYamoussoukro, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualYamoussoukro)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualYamoussoukro, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualYamoussoukro))^2))))

########### Gontougo ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Gontougo: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualGontougo, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGontougo)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualGontougo, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGontougo)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualGontougo, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualGontougo))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Gontougo: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualGontougo, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGontougo)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualGontougo, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGontougo)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualGontougo, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualGontougo))^2))))

########### San-Pedro ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for San-Pedro: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualSanPedro, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSanPedro)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualSanPedro, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSanPedro)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualSanPedro, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualSanPedro))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for San-Pedro: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualSanPedro, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSanPedro)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualSanPedro, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSanPedro)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualSanPedro, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualSanPedro))^2))))

########### Kabadougou ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Kabadougou: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualKabadougou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualKabadougou)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualKabadougou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualKabadougou)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualKabadougou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualKabadougou))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Kabadougou: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualKabadougou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualKabadougou)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualKabadougou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualKabadougou)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualKabadougou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualKabadougou))^2))))

########### N'Zi ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for N'Zi: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualNZi, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualNZi)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualNZi, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualNZi)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualNZi, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualNZi))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for N'Zi: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualNZi, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualNZi)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualNZi, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualNZi)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualNZi, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualNZi))^2))))

########### Marahoue ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Marahoue: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualMarahoue, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualMarahoue)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualMarahoue, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualMarahoue)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualMarahoue, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualMarahoue))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Marahoue: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualMarahoue, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualMarahoue)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualMarahoue, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualMarahoue)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualMarahoue, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualMarahoue))^2))))

########### Sud-Comoe ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Sud-Comoe: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualSudComoe, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSudComoe)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualSudComoe, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSudComoe)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualSudComoe, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualSudComoe))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Sud-Comoe: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualSudComoe, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSudComoe)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualSudComoe, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualSudComoe)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualSudComoe, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualSudComoe))^2))))

########### Worodougou ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Worodougou: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualWorodougou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualWorodougou)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualWorodougou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualWorodougou)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualWorodougou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualWorodougou))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Worodougou: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualWorodougou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualWorodougou)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualWorodougou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualWorodougou)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualWorodougou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualWorodougou))^2))))

########### Lôh-Djiboua ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Lôh-Djiboua: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualLôhDjiboua, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualLôhDjiboua)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualLôhDjiboua, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualLôhDjiboua)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualLôhDjiboua, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualLôhDjiboua))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Lôh-Djiboua: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualLôhDjiboua, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualLôhDjiboua)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualLôhDjiboua, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualLôhDjiboua)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualLôhDjiboua, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualLôhDjiboua))^2))))

########### Agneby-Tiassa ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Agneby-Tiassa: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualAgnebyTiassa, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualAgnebyTiassa)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualAgnebyTiassa, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualAgnebyTiassa)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualAgnebyTiassa, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualAgnebyTiassa))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Agneby-Tiassa: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualAgnebyTiassa, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualAgnebyTiassa)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualAgnebyTiassa, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualAgnebyTiassa)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualAgnebyTiassa, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualAgnebyTiassa))^2))))

########### GÔh ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for GÔh: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualGÔh, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGÔh)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualGÔh, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGÔh)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualGÔh, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualGÔh))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for GÔh: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualGÔh, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGÔh)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualGÔh, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGÔh)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualGÔh, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualGÔh))^2))))

########### Cavally ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Cavally: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCavally, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCavally)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCavally, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCavally)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCavally, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCavally))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Cavally: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualCavally, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCavally)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualCavally, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualCavally)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualCavally, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualCavally))^2))))

########### Bafing ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Bafing: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBafing, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBafing)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBafing, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBafing)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBafing, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBafing))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Bafing: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBafing, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBafing)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBafing, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBafing)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBafing, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBafing))^2))))

########### Bagoue ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Bagoue: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBagoue, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBagoue)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBagoue, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBagoue)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBagoue, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBagoue))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Bagoue: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBagoue, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBagoue)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBagoue, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBagoue)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBagoue, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBagoue))^2))))

########### Belier ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Belier: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBelier, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBelier)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBelier, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBelier)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBelier, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBelier))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Belier: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBelier, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBelier)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBelier, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBelier)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBelier, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBelier))^2))))

########### Bere ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Bere: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBere, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBere)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBere, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBere)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBere, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBere))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Bere: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBere, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBere)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBere, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBere)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBere, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBere))^2))))

########### Bounkani ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Bounkani: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBounkani, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBounkani)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBounkani, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBounkani)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBounkani, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBounkani))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Bounkani: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualBounkani, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBounkani)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualBounkani, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualBounkani)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualBounkani, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualBounkani))^2))))

########### Folon ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Folon: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualFolon, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualFolon)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualFolon, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualFolon)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualFolon, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualFolon))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Folon: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualFolon, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualFolon)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualFolon, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualFolon)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualFolon, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualFolon))^2))))

########### GbÔkle ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for GbÔkle: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualGbÔkle, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGbÔkle)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualGbÔkle, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGbÔkle)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualGbÔkle, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualGbÔkle))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for GbÔkle: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualGbÔkle, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGbÔkle)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualGbÔkle, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGbÔkle)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualGbÔkle, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualGbÔkle))^2))))

########### Grands-Ponts ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Grands-Ponts: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualGrandsPonts, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGrandsPonts)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualGrandsPonts, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGrandsPonts)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualGrandsPonts, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualGrandsPonts))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Grands-Ponts: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualGrandsPonts, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGrandsPonts)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualGrandsPonts, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGrandsPonts)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualGrandsPonts, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualGrandsPonts))^2))))

########### Guemon ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Guemon: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualGuemon, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGuemon)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualGuemon, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGuemon)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualGuemon, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualGuemon))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Guemon: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualGuemon, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGuemon)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualGuemon, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualGuemon)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualGuemon, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualGuemon))^2))))

########### Hambol ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Hambol: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualHambol, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualHambol)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualHambol, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualHambol)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualHambol, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualHambol))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Hambol: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualHambol, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualHambol)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualHambol, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualHambol)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualHambol, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualHambol))^2))))

########### Iffou ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Iffou: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualIffou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualIffou)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualIffou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualIffou)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualIffou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualIffou))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Iffou: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualIffou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualIffou)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualIffou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualIffou)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualIffou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualIffou))^2))))

########### La Me ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for La Me: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualLaMe, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualLaMe)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualLaMe, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualLaMe)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualLaMe, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualLaMe))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for La Me: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualLaMe, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualLaMe)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualLaMe, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualLaMe)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualLaMe, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualLaMe))^2))))

########### Nawa ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Nawa: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualNawa, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualNawa)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualNawa, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualNawa)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualNawa, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualNawa))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Nawa: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualNawa, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualNawa)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualNawa, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualNawa)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualNawa, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualNawa))^2))))

########### Tchologo ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Tchologo: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualTchologo)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualTchologo)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualTchologo))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Tchologo: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualTchologo)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualTchologo)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualTchologo))^2))))

########### Tchologo ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Tchologo: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualTchologo)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualTchologo)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualTchologo))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Tchologo: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualTchologo)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualTchologo)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualTchologo))^2))))

########### Moronou ##########

sprintf("Maximum Likelihood Estimator (MLE) - R-Squared for Moronou: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualMoronou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualMoronou)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualMoronou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualMoronou)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualMoronou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualMoronou))^2))))

sprintf("Method of Moments Estimator (MME) - R-Squared for Moronou: %s", percent4(sum((ppbeta(pcgapdeptotndpoorIndividualMoronou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualMoronou)))^2)/(sum((ppbeta(pcgapdeptotndpoorIndividualMoronou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine) - mean(ecdf_func(pcgapdeptotndpoorIndividualMoronou)))^2) + sum((ppbeta(pcgapdeptotndpoorIndividualMoronou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine) - ecdf_func(pcgapdeptotndpoorIndividualMoronou))^2))))

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

########### Autonome d'Abidjan ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Autonome d'Abidjan: %s", percent3(sum((pcgapdeptotndpoorIndividualAutonomedAbidjan/PovertyLine)) * (1/TotalPopulationIndividualAutonomedAbidjan)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Autonome d'Abidjan: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan)) * TotalPoorIndividualAutonomedAbidjan)/TotalPopulationIndividualAutonomedAbidjan))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Autonome d'Abidjan: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan)) * TotalPoorIndividualAutonomedAbidjan)/TotalPopulationIndividualAutonomedAbidjan))

########### Haut-Sassandra ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Haut-Sassandra: %s", percent3(sum((pcgapdeptotndpoorIndividualHautSassandra/PovertyLine)) * (1/TotalPopulationIndividualHautSassandra)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Haut-Sassandra: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra)) * TotalPoorIndividualHautSassandra)/TotalPopulationIndividualHautSassandra))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Haut-Sassandra: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra)) * TotalPoorIndividualHautSassandra)/TotalPopulationIndividualHautSassandra))

########### Poro ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Poro: %s", percent3(sum((pcgapdeptotndpoorIndividualPoro/PovertyLine)) * (1/TotalPopulationIndividualPoro)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Poro: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro)) * TotalPoorIndividualPoro)/TotalPopulationIndividualPoro))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Poro: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro)) * TotalPoorIndividualPoro)/TotalPopulationIndividualPoro))

########### Gbeke ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Gbeke: %s", percent3(sum((pcgapdeptotndpoorIndividualGbeke/PovertyLine)) * (1/TotalPopulationIndividualGbeke)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Gbeke: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke)) * TotalPoorIndividualGbeke)/TotalPopulationIndividualGbeke))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Gbeke: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke)) * TotalPoorIndividualGbeke)/TotalPopulationIndividualGbeke))

########### Indenie-Djuablin ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Indenie-Djuablin: %s", percent3(sum((pcgapdeptotndpoorIndividualIndenieDjuablin/PovertyLine)) * (1/TotalPopulationIndividualIndenieDjuablin)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Indenie-Djuablin: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin)) * TotalPoorIndividualIndenieDjuablin)/TotalPopulationIndividualIndenieDjuablin))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Indenie-Djuablin: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin)) * TotalPoorIndividualIndenieDjuablin)/TotalPopulationIndividualIndenieDjuablin))

########### Tonkpi ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Tonkpi: %s", percent3(sum((pcgapdeptotndpoorIndividualTonkpi/PovertyLine)) * (1/TotalPopulationIndividualTonkpi)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Tonkpi: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi)) * TotalPoorIndividualTonkpi)/TotalPopulationIndividualTonkpi))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Tonkpi: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi)) * TotalPoorIndividualTonkpi)/TotalPopulationIndividualTonkpi))

########### Yamoussoukro ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Yamoussoukro: %s", percent3(sum((pcgapdeptotndpoorIndividualYamoussoukro/PovertyLine)) * (1/TotalPopulationIndividualYamoussoukro)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Yamoussoukro: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro)) * TotalPoorIndividualYamoussoukro)/TotalPopulationIndividualYamoussoukro))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Yamoussoukro: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro)) * TotalPoorIndividualYamoussoukro)/TotalPopulationIndividualYamoussoukro))

########### Gontougo ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Gontougo: %s", percent3(sum((pcgapdeptotndpoorIndividualGontougo/PovertyLine)) * (1/TotalPopulationIndividualGontougo)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Gontougo: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo)) * TotalPoorIndividualGontougo)/TotalPopulationIndividualGontougo))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Gontougo: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo)) * TotalPoorIndividualGontougo)/TotalPopulationIndividualGontougo))

########### San-Pedro ##########

sprintf("Direct - Poverty Gap Index (FGT1) in San-Pedro: %s", percent3(sum((pcgapdeptotndpoorIndividualSanPedro/PovertyLine)) * (1/TotalPopulationIndividualSanPedro)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in San-Pedro: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro)) * TotalPoorIndividualSanPedro)/TotalPopulationIndividualSanPedro))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in San-Pedro: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro)) * TotalPoorIndividualSanPedro)/TotalPopulationIndividualSanPedro))

########### Kabadougou ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Kabadougou: %s", percent3(sum((pcgapdeptotndpoorIndividualKabadougou/PovertyLine)) * (1/TotalPopulationIndividualKabadougou)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Kabadougou: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou)) * TotalPoorIndividualKabadougou)/TotalPopulationIndividualKabadougou))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Kabadougou: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou)) * TotalPoorIndividualKabadougou)/TotalPopulationIndividualKabadougou))

########### N'Zi ##########

sprintf("Direct - Poverty Gap Index (FGT1) in N'Zi: %s", percent3(sum((pcgapdeptotndpoorIndividualNZi/PovertyLine)) * (1/TotalPopulationIndividualNZi)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in N'Zi: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi)) * TotalPoorIndividualNZi)/TotalPopulationIndividualNZi))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in N'Zi: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi)) * TotalPoorIndividualNZi)/TotalPopulationIndividualNZi))

########### Marahoue ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Marahoue: %s", percent3(sum((pcgapdeptotndpoorIndividualMarahoue/PovertyLine)) * (1/TotalPopulationIndividualMarahoue)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Marahoue: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue)) * TotalPoorIndividualMarahoue)/TotalPopulationIndividualMarahoue))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Marahoue: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue)) * TotalPoorIndividualMarahoue)/TotalPopulationIndividualMarahoue))

########### Sud-Comoe ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Sud-Comoe: %s", percent3(sum((pcgapdeptotndpoorIndividualSudComoe/PovertyLine)) * (1/TotalPopulationIndividualSudComoe)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Sud-Comoe: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe)) * TotalPoorIndividualSudComoe)/TotalPopulationIndividualSudComoe))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Sud-Comoe: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe)) * TotalPoorIndividualSudComoe)/TotalPopulationIndividualSudComoe))

########### Worodougou ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Worodougou: %s", percent3(sum((pcgapdeptotndpoorIndividualWorodougou/PovertyLine)) * (1/TotalPopulationIndividualWorodougou)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Worodougou: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou)) * TotalPoorIndividualWorodougou)/TotalPopulationIndividualWorodougou))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Worodougou: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou)) * TotalPoorIndividualWorodougou)/TotalPopulationIndividualWorodougou))

########### Lôh-Djiboua ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Lôh-Djiboua: %s", percent3(sum((pcgapdeptotndpoorIndividualLôhDjiboua/PovertyLine)) * (1/TotalPopulationIndividualLôhDjiboua)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Lôh-Djiboua: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua)) * TotalPoorIndividualLôhDjiboua)/TotalPopulationIndividualLôhDjiboua))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Lôh-Djiboua: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua)) * TotalPoorIndividualLôhDjiboua)/TotalPopulationIndividualLôhDjiboua))

########### Agneby-Tiassa ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Agneby-Tiassa: %s", percent3(sum((pcgapdeptotndpoorIndividualAgnebyTiassa/PovertyLine)) * (1/TotalPopulationIndividualAgnebyTiassa)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Agneby-Tiassa: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa)) * TotalPoorIndividualAgnebyTiassa)/TotalPopulationIndividualAgnebyTiassa))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Agneby-Tiassa: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa)) * TotalPoorIndividualAgnebyTiassa)/TotalPopulationIndividualAgnebyTiassa))

########### GÔh ##########

sprintf("Direct - Poverty Gap Index (FGT1) in GÔh: %s", percent3(sum((pcgapdeptotndpoorIndividualGÔh/PovertyLine)) * (1/TotalPopulationIndividualGÔh)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in GÔh: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh)) * TotalPoorIndividualGÔh)/TotalPopulationIndividualGÔh))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in GÔh: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh)) * TotalPoorIndividualGÔh)/TotalPopulationIndividualGÔh))

########### Cavally ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Cavally: %s", percent3(sum((pcgapdeptotndpoorIndividualCavally/PovertyLine)) * (1/TotalPopulationIndividualCavally)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Cavally: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally)) * TotalPoorIndividualCavally)/TotalPopulationIndividualCavally))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Cavally: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally)) * TotalPoorIndividualCavally)/TotalPopulationIndividualCavally))

########### Bafing ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Bafing: %s", percent3(sum((pcgapdeptotndpoorIndividualBafing/PovertyLine)) * (1/TotalPopulationIndividualBafing)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Bafing: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing)) * TotalPoorIndividualBafing)/TotalPopulationIndividualBafing))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Bafing: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing)) * TotalPoorIndividualBafing)/TotalPopulationIndividualBafing))

########### Bagoue ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Bagoue: %s", percent3(sum((pcgapdeptotndpoorIndividualBagoue/PovertyLine)) * (1/TotalPopulationIndividualBagoue)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Bagoue: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue)) * TotalPoorIndividualBagoue)/TotalPopulationIndividualBagoue))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Bagoue: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue)) * TotalPoorIndividualBagoue)/TotalPopulationIndividualBagoue))

########### Belier ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Belier: %s", percent3(sum((pcgapdeptotndpoorIndividualBelier/PovertyLine)) * (1/TotalPopulationIndividualBelier)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Belier: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier)) * TotalPoorIndividualBelier)/TotalPopulationIndividualBelier))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Belier: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier)) * TotalPoorIndividualBelier)/TotalPopulationIndividualBelier))

########### Bere ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Bere: %s", percent3(sum((pcgapdeptotndpoorIndividualBere/PovertyLine)) * (1/TotalPopulationIndividualBere)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Bere: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere)) * TotalPoorIndividualBere)/TotalPopulationIndividualBere))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Bere: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualBere)) * TotalPoorIndividualBere)/TotalPopulationIndividualBere))

########### Bounkani ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Bounkani: %s", percent3(sum((pcgapdeptotndpoorIndividualBounkani/PovertyLine)) * (1/TotalPopulationIndividualBounkani)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Bounkani: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani)) * TotalPoorIndividualBounkani)/TotalPopulationIndividualBounkani))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Bounkani: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani)) * TotalPoorIndividualBounkani)/TotalPopulationIndividualBounkani))

########### Folon ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Folon: %s", percent3(sum((pcgapdeptotndpoorIndividualFolon/PovertyLine)) * (1/TotalPopulationIndividualFolon)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Folon: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon)) * TotalPoorIndividualFolon)/TotalPopulationIndividualFolon))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Folon: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon)) * TotalPoorIndividualFolon)/TotalPopulationIndividualFolon))

########### GbÔkle ##########

sprintf("Direct - Poverty Gap Index (FGT1) in GbÔkle: %s", percent3(sum((pcgapdeptotndpoorIndividualGbÔkle/PovertyLine)) * (1/TotalPopulationIndividualGbÔkle)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in GbÔkle: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle)) * TotalPoorIndividualGbÔkle)/TotalPopulationIndividualGbÔkle))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in GbÔkle: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle)) * TotalPoorIndividualGbÔkle)/TotalPopulationIndividualGbÔkle))

########### Grands-Ponts ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Grands-Ponts: %s", percent3(sum((pcgapdeptotndpoorIndividualGrandsPonts/PovertyLine)) * (1/TotalPopulationIndividualGrandsPonts)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Grands-Ponts: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts)) * TotalPoorIndividualGrandsPonts)/TotalPopulationIndividualGrandsPonts))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Grands-Ponts: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts)) * TotalPoorIndividualGrandsPonts)/TotalPopulationIndividualGrandsPonts))

########### Guemon ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Guemon: %s", percent3(sum((pcgapdeptotndpoorIndividualGuemon/PovertyLine)) * (1/TotalPopulationIndividualGuemon)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Guemon: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon)) * TotalPoorIndividualGuemon)/TotalPopulationIndividualGuemon))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Guemon: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon)) * TotalPoorIndividualGuemon)/TotalPopulationIndividualGuemon))

########### Hambol ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Hambol: %s", percent3(sum((pcgapdeptotndpoorIndividualHambol/PovertyLine)) * (1/TotalPopulationIndividualHambol)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Hambol: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol)) * TotalPoorIndividualHambol)/TotalPopulationIndividualHambol))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Hambol: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol)) * TotalPoorIndividualHambol)/TotalPopulationIndividualHambol))

########### Iffou ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Iffou: %s", percent3(sum((pcgapdeptotndpoorIndividualIffou/PovertyLine)) * (1/TotalPopulationIndividualIffou)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Iffou: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou)) * TotalPoorIndividualIffou)/TotalPopulationIndividualIffou))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Iffou: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou)) * TotalPoorIndividualIffou)/TotalPopulationIndividualIffou))

########### La Me ##########

sprintf("Direct - Poverty Gap Index (FGT1) in La Me: %s", percent3(sum((pcgapdeptotndpoorIndividualLaMe/PovertyLine)) * (1/TotalPopulationIndividualLaMe)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in La Me: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe)) * TotalPoorIndividualLaMe)/TotalPopulationIndividualLaMe))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in La Me: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe)) * TotalPoorIndividualLaMe)/TotalPopulationIndividualLaMe))

########### Nawa ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Nawa: %s", percent3(sum((pcgapdeptotndpoorIndividualNawa/PovertyLine)) * (1/TotalPopulationIndividualNawa)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Nawa: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa)) * TotalPoorIndividualNawa)/TotalPopulationIndividualNawa))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Nawa: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa)) * TotalPoorIndividualNawa)/TotalPopulationIndividualNawa))

########### Tchologo ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Tchologo: %s", percent3(sum((pcgapdeptotndpoorIndividualTchologo/PovertyLine)) * (1/TotalPopulationIndividualTchologo)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Tchologo: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo)) * TotalPoorIndividualTchologo)/TotalPopulationIndividualTchologo))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Tchologo: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo)) * TotalPoorIndividualTchologo)/TotalPopulationIndividualTchologo))

########### Moronou ##########

sprintf("Direct - Poverty Gap Index (FGT1) in Moronou: %s", percent3(sum((pcgapdeptotndpoorIndividualMoronou/PovertyLine)) * (1/TotalPopulationIndividualMoronou)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Gap Index (FGT1) in Moronou: %s", percent3(((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou)) * TotalPoorIndividualMoronou)/TotalPopulationIndividualMoronou))

sprintf("Method of Moments Estimator (MME) - Poverty Gap Index (FGT1) in Moronou: %s", percent3(((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou)) * TotalPoorIndividualMoronou)/TotalPopulationIndividualMoronou))

#######################################################################################
################### Poverty Indicators: Poverty Severity Index (FGT2) #################
#######################################################################################

########### Autonome d'Abidjan ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Autonome d'Abidjan: %s", percent3(sum((pcgapdeptotndpoorIndividualAutonomedAbidjan/PovertyLine)^2) * (1/TotalPopulationIndividualAutonomedAbidjan)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Autonome d'Abidjan: %s", percent3((TotalPoorIndividualAutonomedAbidjan * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan + 1)))/PovertyLine^2/TotalPopulationIndividualAutonomedAbidjan))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Autonome d'Abidjan: %s", percent3((TotalPoorIndividualAutonomedAbidjan * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan + 1)))/PovertyLine^2/TotalPopulationIndividualAutonomedAbidjan))

########### Haut-Sassandra ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Haut-Sassandra: %s", percent3(sum((pcgapdeptotndpoorIndividualHautSassandra/PovertyLine)^2) * (1/TotalPopulationIndividualHautSassandra)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Haut-Sassandra: %s", percent3((TotalPoorIndividualHautSassandra * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra + 1)))/PovertyLine^2/TotalPopulationIndividualHautSassandra))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Haut-Sassandra: %s", percent3((TotalPoorIndividualHautSassandra * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra + 1)))/PovertyLine^2/TotalPopulationIndividualHautSassandra))

########### Poro ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Poro: %s", percent3(sum((pcgapdeptotndpoorIndividualPoro/PovertyLine)^2) * (1/TotalPopulationIndividualPoro)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Poro: %s", percent3((TotalPoorIndividualPoro * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro + 1)))/PovertyLine^2/TotalPopulationIndividualPoro))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Poro: %s", percent3((TotalPoorIndividualPoro * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro + 1)))/PovertyLine^2/TotalPopulationIndividualPoro))

########### Gbeke ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Gbeke: %s", percent3(sum((pcgapdeptotndpoorIndividualGbeke/PovertyLine)^2) * (1/TotalPopulationIndividualGbeke)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Gbeke: %s", percent3((TotalPoorIndividualGbeke * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke + 1)))/PovertyLine^2/TotalPopulationIndividualGbeke))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Gbeke: %s", percent3((TotalPoorIndividualGbeke * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke + 1)))/PovertyLine^2/TotalPopulationIndividualGbeke))

########### Indenie-Djuablin ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Indenie-Djuablin: %s", percent3(sum((pcgapdeptotndpoorIndividualIndenieDjuablin/PovertyLine)^2) * (1/TotalPopulationIndividualIndenieDjuablin)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Indenie-Djuablin: %s", percent3((TotalPoorIndividualIndenieDjuablin * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin + 1)))/PovertyLine^2/TotalPopulationIndividualIndenieDjuablin))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Indenie-Djuablin: %s", percent3((TotalPoorIndividualIndenieDjuablin * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin + 1)))/PovertyLine^2/TotalPopulationIndividualIndenieDjuablin))

########### Tonkpi ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Tonkpi: %s", percent3(sum((pcgapdeptotndpoorIndividualTonkpi/PovertyLine)^2) * (1/TotalPopulationIndividualTonkpi)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Tonkpi: %s", percent3((TotalPoorIndividualTonkpi * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi + 1)))/PovertyLine^2/TotalPopulationIndividualTonkpi))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Tonkpi: %s", percent3((TotalPoorIndividualTonkpi * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi + 1)))/PovertyLine^2/TotalPopulationIndividualTonkpi))

########### Yamoussoukro ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Yamoussoukro: %s", percent3(sum((pcgapdeptotndpoorIndividualYamoussoukro/PovertyLine)^2) * (1/TotalPopulationIndividualYamoussoukro)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Yamoussoukro: %s", percent3((TotalPoorIndividualYamoussoukro * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro + 1)))/PovertyLine^2/TotalPopulationIndividualYamoussoukro))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Yamoussoukro: %s", percent3((TotalPoorIndividualYamoussoukro * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro + 1)))/PovertyLine^2/TotalPopulationIndividualYamoussoukro))

########### Gontougo ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Gontougo: %s", percent3(sum((pcgapdeptotndpoorIndividualGontougo/PovertyLine)^2) * (1/TotalPopulationIndividualGontougo)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Gontougo: %s", percent3((TotalPoorIndividualGontougo * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo + 1)))/PovertyLine^2/TotalPopulationIndividualGontougo))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Gontougo: %s", percent3((TotalPoorIndividualGontougo * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo + 1)))/PovertyLine^2/TotalPopulationIndividualGontougo))

########### San-Pedro ##########

sprintf("Direct - Poverty Severity Index (FGT2) in San-Pedro: %s", percent3(sum((pcgapdeptotndpoorIndividualSanPedro/PovertyLine)^2) * (1/TotalPopulationIndividualSanPedro)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in San-Pedro: %s", percent3((TotalPoorIndividualSanPedro * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro + 1)))/PovertyLine^2/TotalPopulationIndividualSanPedro))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in San-Pedro: %s", percent3((TotalPoorIndividualSanPedro * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro + 1)))/PovertyLine^2/TotalPopulationIndividualSanPedro))

########### Kabadougou ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Kabadougou: %s", percent3(sum((pcgapdeptotndpoorIndividualKabadougou/PovertyLine)^2) * (1/TotalPopulationIndividualKabadougou)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Kabadougou: %s", percent3((TotalPoorIndividualKabadougou * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou + 1)))/PovertyLine^2/TotalPopulationIndividualKabadougou))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Kabadougou: %s", percent3((TotalPoorIndividualKabadougou * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou + 1)))/PovertyLine^2/TotalPopulationIndividualKabadougou))

########### N'Zi ##########

sprintf("Direct - Poverty Severity Index (FGT2) in N'Zi: %s", percent3(sum((pcgapdeptotndpoorIndividualNZi/PovertyLine)^2) * (1/TotalPopulationIndividualNZi)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in N'Zi: %s", percent3((TotalPoorIndividualNZi * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi + 1)))/PovertyLine^2/TotalPopulationIndividualNZi))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in N'Zi: %s", percent3((TotalPoorIndividualNZi * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi + 1)))/PovertyLine^2/TotalPopulationIndividualNZi))

########### Marahoue ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Marahoue: %s", percent3(sum((pcgapdeptotndpoorIndividualMarahoue/PovertyLine)^2) * (1/TotalPopulationIndividualMarahoue)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Marahoue: %s", percent3((TotalPoorIndividualMarahoue * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue + 1)))/PovertyLine^2/TotalPopulationIndividualMarahoue))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Marahoue: %s", percent3((TotalPoorIndividualMarahoue * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue + 1)))/PovertyLine^2/TotalPopulationIndividualMarahoue))

########### Sud-Comoe ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Sud-Comoe: %s", percent3(sum((pcgapdeptotndpoorIndividualSudComoe/PovertyLine)^2) * (1/TotalPopulationIndividualSudComoe)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Sud-Comoe: %s", percent3((TotalPoorIndividualSudComoe * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe + 1)))/PovertyLine^2/TotalPopulationIndividualSudComoe))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Sud-Comoe: %s", percent3((TotalPoorIndividualSudComoe * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe + 1)))/PovertyLine^2/TotalPopulationIndividualSudComoe))

########### Worodougou ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Worodougou: %s", percent3(sum((pcgapdeptotndpoorIndividualWorodougou/PovertyLine)^2) * (1/TotalPopulationIndividualWorodougou)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Worodougou: %s", percent3((TotalPoorIndividualWorodougou * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou + 1)))/PovertyLine^2/TotalPopulationIndividualWorodougou))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Worodougou: %s", percent3((TotalPoorIndividualWorodougou * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou + 1)))/PovertyLine^2/TotalPopulationIndividualWorodougou))

########### Lôh-Djiboua ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Lôh-Djiboua: %s", percent3(sum((pcgapdeptotndpoorIndividualLôhDjiboua/PovertyLine)^2) * (1/TotalPopulationIndividualLôhDjiboua)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Lôh-Djiboua: %s", percent3((TotalPoorIndividualLôhDjiboua * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua + 1)))/PovertyLine^2/TotalPopulationIndividualLôhDjiboua))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Lôh-Djiboua: %s", percent3((TotalPoorIndividualLôhDjiboua * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua + 1)))/PovertyLine^2/TotalPopulationIndividualLôhDjiboua))

########### Agneby-Tiassa ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Agneby-Tiassa: %s", percent3(sum((pcgapdeptotndpoorIndividualAgnebyTiassa/PovertyLine)^2) * (1/TotalPopulationIndividualAgnebyTiassa)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Agneby-Tiassa: %s", percent3((TotalPoorIndividualAgnebyTiassa * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa + 1)))/PovertyLine^2/TotalPopulationIndividualAgnebyTiassa))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Agneby-Tiassa: %s", percent3((TotalPoorIndividualAgnebyTiassa * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa + 1)))/PovertyLine^2/TotalPopulationIndividualAgnebyTiassa))

########### GÔh ##########

sprintf("Direct - Poverty Severity Index (FGT2) in GÔh: %s", percent3(sum((pcgapdeptotndpoorIndividualGÔh/PovertyLine)^2) * (1/TotalPopulationIndividualGÔh)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in GÔh: %s", percent3((TotalPoorIndividualGÔh * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh + 1)))/PovertyLine^2/TotalPopulationIndividualGÔh))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in GÔh: %s", percent3((TotalPoorIndividualGÔh * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh + 1)))/PovertyLine^2/TotalPopulationIndividualGÔh))

########### Cavally ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Cavally: %s", percent3(sum((pcgapdeptotndpoorIndividualCavally/PovertyLine)^2) * (1/TotalPopulationIndividualCavally)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Cavally: %s", percent3((TotalPoorIndividualCavally * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally + 1)))/PovertyLine^2/TotalPopulationIndividualCavally))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Cavally: %s", percent3((TotalPoorIndividualCavally * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally + 1)))/PovertyLine^2/TotalPopulationIndividualCavally))

########### Bafing ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Bafing: %s", percent3(sum((pcgapdeptotndpoorIndividualBafing/PovertyLine)^2) * (1/TotalPopulationIndividualBafing)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Bafing: %s", percent3((TotalPoorIndividualBafing * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing + 1)))/PovertyLine^2/TotalPopulationIndividualBafing))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Bafing: %s", percent3((TotalPoorIndividualBafing * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing + 1)))/PovertyLine^2/TotalPopulationIndividualBafing))

########### Bagoue ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Bagoue: %s", percent3(sum((pcgapdeptotndpoorIndividualBagoue/PovertyLine)^2) * (1/TotalPopulationIndividualBagoue)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Bagoue: %s", percent3((TotalPoorIndividualBagoue * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue + 1)))/PovertyLine^2/TotalPopulationIndividualBagoue))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Bagoue: %s", percent3((TotalPoorIndividualBagoue * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue + 1)))/PovertyLine^2/TotalPopulationIndividualBagoue))

########### Belier ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Belier: %s", percent3(sum((pcgapdeptotndpoorIndividualBelier/PovertyLine)^2) * (1/TotalPopulationIndividualBelier)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Belier: %s", percent3((TotalPoorIndividualBelier * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier + 1)))/PovertyLine^2/TotalPopulationIndividualBelier))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Belier: %s", percent3((TotalPoorIndividualBelier * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier + 1)))/PovertyLine^2/TotalPopulationIndividualBelier))

########### Bere ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Bere: %s", percent3(sum((pcgapdeptotndpoorIndividualBere/PovertyLine)^2) * (1/TotalPopulationIndividualBere)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Bere: %s", percent3((TotalPoorIndividualBere * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere + 1)))/PovertyLine^2/TotalPopulationIndividualBere))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Bere: %s", percent3((TotalPoorIndividualBere * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualBere + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualBere + 1)))/PovertyLine^2/TotalPopulationIndividualBere))

########### Bounkani ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Bounkani: %s", percent3(sum((pcgapdeptotndpoorIndividualBounkani/PovertyLine)^2) * (1/TotalPopulationIndividualBounkani)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Bounkani: %s", percent3((TotalPoorIndividualBounkani * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani + 1)))/PovertyLine^2/TotalPopulationIndividualBounkani))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Bounkani: %s", percent3((TotalPoorIndividualBounkani * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani + 1)))/PovertyLine^2/TotalPopulationIndividualBounkani))

########### Folon ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Folon: %s", percent3(sum((pcgapdeptotndpoorIndividualFolon/PovertyLine)^2) * (1/TotalPopulationIndividualFolon)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Folon: %s", percent3((TotalPoorIndividualFolon * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon + 1)))/PovertyLine^2/TotalPopulationIndividualFolon))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Folon: %s", percent3((TotalPoorIndividualFolon * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon + 1)))/PovertyLine^2/TotalPopulationIndividualFolon))

########### GbÔkle ##########

sprintf("Direct - Poverty Severity Index (FGT2) in GbÔkle: %s", percent3(sum((pcgapdeptotndpoorIndividualGbÔkle/PovertyLine)^2) * (1/TotalPopulationIndividualGbÔkle)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in GbÔkle: %s", percent3((TotalPoorIndividualGbÔkle * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle + 1)))/PovertyLine^2/TotalPopulationIndividualGbÔkle))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in GbÔkle: %s", percent3((TotalPoorIndividualGbÔkle * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle + 1)))/PovertyLine^2/TotalPopulationIndividualGbÔkle))

########### Grands-Ponts ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Grands-Ponts: %s", percent3(sum((pcgapdeptotndpoorIndividualGrandsPonts/PovertyLine)^2) * (1/TotalPopulationIndividualGrandsPonts)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Grands-Ponts: %s", percent3((TotalPoorIndividualGrandsPonts * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts + 1)))/PovertyLine^2/TotalPopulationIndividualGrandsPonts))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Grands-Ponts: %s", percent3((TotalPoorIndividualGrandsPonts * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts + 1)))/PovertyLine^2/TotalPopulationIndividualGrandsPonts))

########### Guemon ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Guemon: %s", percent3(sum((pcgapdeptotndpoorIndividualGuemon/PovertyLine)^2) * (1/TotalPopulationIndividualGuemon)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Guemon: %s", percent3((TotalPoorIndividualGuemon * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon + 1)))/PovertyLine^2/TotalPopulationIndividualGuemon))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Guemon: %s", percent3((TotalPoorIndividualGuemon * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon + 1)))/PovertyLine^2/TotalPopulationIndividualGuemon))

########### Hambol ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Hambol: %s", percent3(sum((pcgapdeptotndpoorIndividualHambol/PovertyLine)^2) * (1/TotalPopulationIndividualHambol)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Hambol: %s", percent3((TotalPoorIndividualHambol * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol + 1)))/PovertyLine^2/TotalPopulationIndividualHambol))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Hambol: %s", percent3((TotalPoorIndividualHambol * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol + 1)))/PovertyLine^2/TotalPopulationIndividualHambol))

########### Iffou ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Iffou: %s", percent3(sum((pcgapdeptotndpoorIndividualIffou/PovertyLine)^2) * (1/TotalPopulationIndividualIffou)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Iffou: %s", percent3((TotalPoorIndividualIffou * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou + 1)))/PovertyLine^2/TotalPopulationIndividualIffou))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Iffou: %s", percent3((TotalPoorIndividualIffou * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou + 1)))/PovertyLine^2/TotalPopulationIndividualIffou))

########### La Me ##########

sprintf("Direct - Poverty Severity Index (FGT2) in La Me: %s", percent3(sum((pcgapdeptotndpoorIndividualLaMe/PovertyLine)^2) * (1/TotalPopulationIndividualLaMe)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in La Me: %s", percent3((TotalPoorIndividualLaMe * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe + 1)))/PovertyLine^2/TotalPopulationIndividualLaMe))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in La Me: %s", percent3((TotalPoorIndividualLaMe * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe + 1)))/PovertyLine^2/TotalPopulationIndividualLaMe))

########### Nawa ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Nawa: %s", percent3(sum((pcgapdeptotndpoorIndividualNawa/PovertyLine)^2) * (1/TotalPopulationIndividualNawa)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Nawa: %s", percent3((TotalPoorIndividualNawa * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa + 1)))/PovertyLine^2/TotalPopulationIndividualNawa))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Nawa: %s", percent3((TotalPoorIndividualNawa * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa + 1)))/PovertyLine^2/TotalPopulationIndividualNawa))

########### Tchologo ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Tchologo: %s", percent3(sum((pcgapdeptotndpoorIndividualTchologo/PovertyLine)^2) * (1/TotalPopulationIndividualTchologo)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Tchologo: %s", percent3((TotalPoorIndividualTchologo * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo + 1)))/PovertyLine^2/TotalPopulationIndividualTchologo))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Tchologo: %s", percent3((TotalPoorIndividualTchologo * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo + 1)))/PovertyLine^2/TotalPopulationIndividualTchologo))

########### Moronou ##########

sprintf("Direct - Poverty Severity Index (FGT2) in Moronou: %s", percent3(sum((pcgapdeptotndpoorIndividualMoronou/PovertyLine)^2) * (1/TotalPopulationIndividualMoronou)))

sprintf("Maximum Likelihood Estimator (MLE) - Poverty Severity Index (FGT2) in Moronou: %s", percent3((TotalPoorIndividualMoronou * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou + 1)))/PovertyLine^2/TotalPopulationIndividualMoronou))

sprintf("Method of Moments Estimator (MME) - Poverty Severity Index (FGT2) in Moronou: %s", percent3((TotalPoorIndividualMoronou * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou + 1)))/PovertyLine^2/TotalPopulationIndividualMoronou))

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

########### Autonome d'Abidjan ##########

# We generate the .tex file
FileNameHistogramAutonomedAbidjan <- paste0('HistogramDefititatTrapping', 'AutonomedAbidjan.tex')
tikz(FileNameHistogramAutonomedAbidjan, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualAutonomedAbidjan, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000012), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualAutonomedAbidjan), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualAutonomedAbidjan), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Haut-Sassandra ##########

# We generate the .tex file
FileNameHistogramHautSassandra <- paste0('HistogramDefititatTrapping', 'HautSassandra.tex')
tikz(FileNameHistogramHautSassandra, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualHautSassandra, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualHautSassandra), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualHautSassandra), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Poro ##########

# We generate the .tex file
FileNameHistogramPoro <- paste0('HistogramDefititatTrapping', 'Poro.tex')
tikz(FileNameHistogramPoro, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualPoro, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualPoro), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualPoro), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Gbeke ##########

# We generate the .tex file
FileNameHistogramGbeke <- paste0('HistogramDefititatTrapping', 'Gbeke.tex')
tikz(FileNameHistogramGbeke, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualGbeke, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000010), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualGbeke), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualGbeke), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Indenie-Djuablin ##########

# We generate the .tex file
FileNameHistogramIndenieDjuablin <- paste0('HistogramDefititatTrapping', 'IndenieDjuablin.tex')
tikz(FileNameHistogramIndenieDjuablin, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualIndenieDjuablin, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualIndenieDjuablin), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualIndenieDjuablin), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Tonkpi ##########

# We generate the .tex file
FileNameHistogramTonkpi <- paste0('HistogramDefititatTrapping', 'Tonkpi.tex')
tikz(FileNameHistogramTonkpi, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualTonkpi, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000006), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualTonkpi), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualTonkpi), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Yamoussoukro ##########

# We generate the .tex file
FileNameHistogramYamoussoukro <- paste0('HistogramDefititatTrapping', 'Yamoussoukro.tex')
tikz(FileNameHistogramYamoussoukro, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualYamoussoukro, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualYamoussoukro), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualYamoussoukro), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Gontougo ##########

# We generate the .tex file
FileNameHistogramGontougo <- paste0('HistogramDefititatTrapping', 'Gontougo.tex')
tikz(FileNameHistogramGontougo, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualGontougo, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000010), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualGontougo), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualGontougo), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### San-Pedro ##########

# We generate the .tex file
FileNameHistogramSanPedro <- paste0('HistogramDefititatTrapping', 'SanPedro.tex')
tikz(FileNameHistogramSanPedro, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualSanPedro, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualSanPedro), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualSanPedro), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Kabadougou ##########

# We generate the .tex file
FileNameHistogramKabadougou <- paste0('HistogramDefititatTrapping', 'Kabadougou.tex')
tikz(FileNameHistogramKabadougou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualKabadougou, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualKabadougou), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualKabadougou), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### N'Zi ##########

# We generate the .tex file
FileNameHistogramNZi <- paste0('HistogramDefititatTrapping', 'NZi.tex')
tikz(FileNameHistogramNZi, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualNZi, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualNZi), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualNZi), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Marahoue ##########

# We generate the .tex file
FileNameHistogramMarahoue <- paste0('HistogramDefititatTrapping', 'Marahoue.tex')
tikz(FileNameHistogramMarahoue, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualMarahoue, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualMarahoue), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualMarahoue), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### SudComoe ##########

# We generate the .tex file
FileNameHistogramSudComoe <- paste0('HistogramDefititatTrapping', 'SudComoe.tex')
tikz(FileNameHistogramSudComoe, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualSudComoe, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000010), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualSudComoe), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualSudComoe), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Worodougou ##########

# We generate the .tex file
FileNameHistogramWorodougou <- paste0('HistogramDefititatTrapping', 'Worodougou.tex')
tikz(FileNameHistogramWorodougou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualWorodougou, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualWorodougou), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualWorodougou), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### LôhDjiboua ##########

# We generate the .tex file
FileNameHistogramLôhDjiboua <- paste0('HistogramDefititatTrapping', 'LôhDjiboua.tex')
tikz(FileNameHistogramLôhDjiboua, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualLôhDjiboua, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualLôhDjiboua), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualLôhDjiboua), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### AgnebyTiassa ##########

# We generate the .tex file
FileNameHistogramAgnebyTiassa <- paste0('HistogramDefititatTrapping', 'AgnebyTiassa.tex')
tikz(FileNameHistogramAgnebyTiassa, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualAgnebyTiassa, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualAgnebyTiassa), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualAgnebyTiassa), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### GÔh ##########

# We generate the .tex file
FileNameHistogramGÔh <- paste0('HistogramDefititatTrapping', 'GÔh.tex')
tikz(FileNameHistogramGÔh, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualGÔh, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualGÔh), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualGÔh), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Cavally ##########

# We generate the .tex file
FileNameHistogramCavally <- paste0('HistogramDefititatTrapping', 'Cavally.tex')
tikz(FileNameHistogramCavally, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualCavally, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCavally), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualCavally), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Bafing ##########

# We generate the .tex file
FileNameHistogramBafing <- paste0('HistogramDefititatTrapping', 'Bafing.tex')
tikz(FileNameHistogramBafing, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualBafing, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualBafing), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualBafing), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Bagoue ##########

# We generate the .tex file
FileNameHistogramBagoue <- paste0('HistogramDefititatTrapping', 'Bagoue.tex')
tikz(FileNameHistogramBagoue, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualBagoue, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000010), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualBagoue), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualBagoue), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Belier ##########

# We generate the .tex file
FileNameHistogramBelier <- paste0('HistogramDefititatTrapping', 'Belier.tex')
tikz(FileNameHistogramBelier, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualBelier, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualBelier), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualBelier), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Bere ##########

# We generate the .tex file
FileNameHistogramBere <- paste0('HistogramDefititatTrapping', 'Bere.tex')
tikz(FileNameHistogramBere, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualBere, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualBere), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualBere, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualBere), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Bounkani ##########

# We generate the .tex file
FileNameHistogramBounkani <- paste0('HistogramDefititatTrapping', 'Bounkani.tex')
tikz(FileNameHistogramBounkani, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualBounkani, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000010), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualBounkani), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualBounkani), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Folon ##########

# We generate the .tex file
FileNameHistogramFolon <- paste0('HistogramDefititatTrapping', 'Folon.tex')
tikz(FileNameHistogramFolon, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualFolon, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000010), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualFolon), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualFolon), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### GbÔkle ##########

# We generate the .tex file
FileNameHistogramGbÔkle <- paste0('HistogramDefititatTrapping', 'GbÔkle.tex')
tikz(FileNameHistogramGbÔkle, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualGbÔkle, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000012), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualGbÔkle), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualGbÔkle), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### GrandsPonts ##########

# We generate the .tex file
FileNameHistogramGrandsPonts <- paste0('HistogramDefititatTrapping', 'GrandsPonts.tex')
tikz(FileNameHistogramGrandsPonts, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualGrandsPonts, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000010), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualGrandsPonts), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualGrandsPonts), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Guemon ##########

# We generate the .tex file
FileNameHistogramGuemon <- paste0('HistogramDefititatTrapping', 'Guemon.tex')
tikz(FileNameHistogramGuemon, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualGuemon, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualGuemon), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualGuemon), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Hambol ##########

# We generate the .tex file
FileNameHistogramHambol <- paste0('HistogramDefititatTrapping', 'Hambol.tex')
tikz(FileNameHistogramHambol, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualHambol, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualHambol), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualHambol), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Iffou ##########

# We generate the .tex file
FileNameHistogramIffou <- paste0('HistogramDefititatTrapping', 'Iffou.tex')
tikz(FileNameHistogramIffou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualIffou, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualIffou), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualIffou), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### LaMe ##########

# We generate the .tex file
FileNameHistogramLaMe <- paste0('HistogramDefititatTrapping', 'LaMe.tex')
tikz(FileNameHistogramLaMe, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualLaMe, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualLaMe), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualLaMe), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Nawa ##########

# We generate the .tex file
FileNameHistogramNawa <- paste0('HistogramDefititatTrapping', 'Nawa.tex')
tikz(FileNameHistogramNawa, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualNawa, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000010), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualNawa), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualNawa), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Tchologo ##########

# We generate the .tex file
FileNameHistogramTchologo <- paste0('HistogramDefititatTrapping', 'Tchologo.tex')
tikz(FileNameHistogramTchologo, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualTchologo, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000008), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualTchologo), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualTchologo), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
legend("topright", inset = 0, legend = my.expressions.hist, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(NA, 1, 2),
       density = c(NA, 0, 0),
       fill = c(MyColors[1], NA, NA),
       border = c("black", NA, NA), x.intersp = c(0, 0, 0), box.col = c("black", NA, NA), cex = 0.8, ncol = 1, lwd = c(NA, 2, 2))
dev.off()

########### Moronou ##########

# We generate the .tex file
FileNameHistogramMoronou <- paste0('HistogramDefititatTrapping', 'Moronou.tex')
tikz(FileNameHistogramMoronou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
hist(pcgapdeptotndpoorIndividualMoronou, prob = TRUE, col = MyColors[1], main = "", xlim = c(0, 350000), ylim = c(0, 0.000010), xlab = "$y$", ylab = "$f(y|x,\\tau_{x}<\\infty)$", cex.lab = 1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, cex.axis = 0.6)
curve(ddbeta(x, MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualMoronou), add = TRUE, col = MyColors[2], type = "l", lwd = 2)
curve(ddbeta(x, MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine), from = 0, to = PovertyLine, n = length(pcgapdeptotndpoorIndividualMoronou), add = TRUE, col = MyColors[3], type = "l", lty = 2, lwd = 2)
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

########### Autonome d'Abidjan ##########

# We generate a vector with the sample quantiles.
samplequantilesAutonomedAbidjan = quantile(pcgapdeptotndpoorIndividualAutonomedAbidjan, probs = seq(0, 1, length = 1000))
unname(samplequantilesAutonomedAbidjan)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorAutonomedAbidjan <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorAutonomedAbidjan

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorAutonomedAbidjan <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorAutonomedAbidjan

# We generate the .tex file
FileNameQQPlotAutonomedAbidjan <- paste0('QQPlotDefititatTrapping', 'AutonomedAbidjan.tex')
tikz(FileNameQQPlotAutonomedAbidjan, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorAutonomedAbidjan, samplequantilesAutonomedAbidjan, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorAutonomedAbidjan, samplequantilesAutonomedAbidjan, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Haut-Sassandra ##########

# We generate a vector with the sample quantiles.
samplequantilesHautSassandra = quantile(pcgapdeptotndpoorIndividualHautSassandra, probs = seq(0, 1, length = 1000))
unname(samplequantilesHautSassandra)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorHautSassandra <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorHautSassandra

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorHautSassandra <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorHautSassandra

# We generate the .tex file
FileNameQQPlotHautSassandra <- paste0('QQPlotDefititatTrapping', 'HautSassandra.tex')
tikz(FileNameQQPlotHautSassandra, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorHautSassandra, samplequantilesHautSassandra, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorHautSassandra, samplequantilesHautSassandra, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Poro ##########

# We generate a vector with the sample quantiles.
samplequantilesPoro = quantile(pcgapdeptotndpoorIndividualPoro, probs = seq(0, 1, length = 1000))
unname(samplequantilesPoro)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorPoro <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorPoro

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorPoro <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorPoro

# We generate the .tex file
FileNameQQPlotPoro <- paste0('QQPlotDefititatTrapping', 'Poro.tex')
tikz(FileNameQQPlotPoro, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorPoro, samplequantilesPoro, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorPoro, samplequantilesPoro, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Gbeke ##########

# We generate a vector with the sample quantiles.
samplequantilesGbeke = quantile(pcgapdeptotndpoorIndividualGbeke, probs = seq(0, 1, length = 1000))
unname(samplequantilesGbeke)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorGbeke <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorGbeke

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorGbeke <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorGbeke

# We generate the .tex file
FileNameQQPlotGbeke <- paste0('QQPlotDefititatTrapping', 'Gbeke.tex')
tikz(FileNameQQPlotGbeke, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorGbeke, samplequantilesGbeke, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorGbeke, samplequantilesGbeke, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Indenie-Djuablin ##########

# We generate a vector with the sample quantiles.
samplequantilesIndenieDjuablin = quantile(pcgapdeptotndpoorIndividualIndenieDjuablin, probs = seq(0, 1, length = 1000))
unname(samplequantilesIndenieDjuablin)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorIndenieDjuablin <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorIndenieDjuablin

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorIndenieDjuablin <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorIndenieDjuablin

# We generate the .tex file
FileNameQQPlotIndenieDjuablin <- paste0('QQPlotDefititatTrapping', 'IndenieDjuablin.tex')
tikz(FileNameQQPlotIndenieDjuablin, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorIndenieDjuablin, samplequantilesIndenieDjuablin, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorIndenieDjuablin, samplequantilesIndenieDjuablin, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Tonkpi ##########

# We generate a vector with the sample quantiles.
samplequantilesTonkpi = quantile(pcgapdeptotndpoorIndividualTonkpi, probs = seq(0, 1, length = 1000))
unname(samplequantilesTonkpi)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorTonkpi <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorTonkpi

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorTonkpi <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorTonkpi

# We generate the .tex file
FileNameQQPlotTonkpi <- paste0('QQPlotDefititatTrapping', 'Tonkpi.tex')
tikz(FileNameQQPlotTonkpi, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorTonkpi, samplequantilesTonkpi, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorTonkpi, samplequantilesTonkpi, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Yamoussoukro ##########

# We generate a vector with the sample quantiles.
samplequantilesYamoussoukro = quantile(pcgapdeptotndpoorIndividualYamoussoukro, probs = seq(0, 1, length = 1000))
unname(samplequantilesYamoussoukro)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorYamoussoukro <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorYamoussoukro

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorYamoussoukro <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorYamoussoukro

# We generate the .tex file
FileNameQQPlotYamoussoukro <- paste0('QQPlotDefititatTrapping', 'Yamoussoukro.tex')
tikz(FileNameQQPlotYamoussoukro, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorYamoussoukro, samplequantilesYamoussoukro, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorYamoussoukro, samplequantilesYamoussoukro, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Gontougo ##########

# We generate a vector with the sample quantiles.
samplequantilesGontougo = quantile(pcgapdeptotndpoorIndividualGontougo, probs = seq(0, 1, length = 1000))
unname(samplequantilesGontougo)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorGontougo <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorGontougo

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorGontougo <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorGontougo

# We generate the .tex file
FileNameQQPlotGontougo <- paste0('QQPlotDefititatTrapping', 'Gontougo.tex')
tikz(FileNameQQPlotGontougo, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorGontougo, samplequantilesGontougo, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorGontougo, samplequantilesGontougo, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### San-Pedro ##########

# We generate a vector with the sample quantiles.
samplequantilesSanPedro = quantile(pcgapdeptotndpoorIndividualSanPedro, probs = seq(0, 1, length = 1000))
unname(samplequantilesSanPedro)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorSanPedro <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorSanPedro

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorSanPedro <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorSanPedro

# We generate the .tex file
FileNameQQPlotSanPedro <- paste0('QQPlotDefititatTrapping', 'SanPedro.tex')
tikz(FileNameQQPlotSanPedro, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorSanPedro, samplequantilesSanPedro, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorSanPedro, samplequantilesSanPedro, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Kabadougou ##########

# We generate a vector with the sample quantiles.
samplequantilesKabadougou = quantile(pcgapdeptotndpoorIndividualKabadougou, probs = seq(0, 1, length = 1000))
unname(samplequantilesKabadougou)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorKabadougou <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorKabadougou

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorKabadougou <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorKabadougou

# We generate the .tex file
FileNameQQPlotKabadougou <- paste0('QQPlotDefititatTrapping', 'Kabadougou.tex')
tikz(FileNameQQPlotKabadougou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorKabadougou, samplequantilesKabadougou, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorKabadougou, samplequantilesKabadougou, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### N'Zi ##########

# We generate a vector with the sample quantiles.
samplequantilesNZi = quantile(pcgapdeptotndpoorIndividualNZi, probs = seq(0, 1, length = 1000))
unname(samplequantilesNZi)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorNZi <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorNZi

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorNZi <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorNZi

# We generate the .tex file
FileNameQQPlotNZi <- paste0('QQPlotDefititatTrapping', 'NZi.tex')
tikz(FileNameQQPlotNZi, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorNZi, samplequantilesNZi, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorNZi, samplequantilesNZi, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Marahoue ##########

# We generate a vector with the sample quantiles.
samplequantilesMarahoue = quantile(pcgapdeptotndpoorIndividualMarahoue, probs = seq(0, 1, length = 1000))
unname(samplequantilesMarahoue)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorMarahoue <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorMarahoue

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorMarahoue <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorMarahoue

# We generate the .tex file
FileNameQQPlotMarahoue <- paste0('QQPlotDefititatTrapping', 'Marahoue.tex')
tikz(FileNameQQPlotMarahoue, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorMarahoue, samplequantilesMarahoue, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorMarahoue, samplequantilesMarahoue, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Sud-Comoe ##########

# We generate a vector with the sample quantiles.
samplequantilesSudComoe = quantile(pcgapdeptotndpoorIndividualSudComoe, probs = seq(0, 1, length = 1000))
unname(samplequantilesSudComoe)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorSudComoe <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorSudComoe

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorSudComoe <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorSudComoe

# We generate the .tex file
FileNameQQPlotSudComoe <- paste0('QQPlotDefititatTrapping', 'SudComoe.tex')
tikz(FileNameQQPlotSudComoe, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorSudComoe, samplequantilesSudComoe, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorSudComoe, samplequantilesSudComoe, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Worodougou ##########

# We generate a vector with the sample quantiles.
samplequantilesWorodougou = quantile(pcgapdeptotndpoorIndividualWorodougou, probs = seq(0, 1, length = 1000))
unname(samplequantilesWorodougou)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorWorodougou <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorWorodougou

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorWorodougou <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorWorodougou

# We generate the .tex file
FileNameQQPlotWorodougou <- paste0('QQPlotDefititatTrapping', 'Worodougou.tex')
tikz(FileNameQQPlotWorodougou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorWorodougou, samplequantilesWorodougou, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorWorodougou, samplequantilesWorodougou, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Lôh-Djiboua ##########

# We generate a vector with the sample quantiles.
samplequantilesLôhDjiboua = quantile(pcgapdeptotndpoorIndividualLôhDjiboua, probs = seq(0, 1, length = 1000))
unname(samplequantilesLôhDjiboua)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorLôhDjiboua <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorLôhDjiboua

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorLôhDjiboua <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorLôhDjiboua

# We generate the .tex file
FileNameQQPlotLôhDjiboua <- paste0('QQPlotDefititatTrapping', 'LôhDjiboua.tex')
tikz(FileNameQQPlotLôhDjiboua, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorLôhDjiboua, samplequantilesLôhDjiboua, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorLôhDjiboua, samplequantilesLôhDjiboua, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Agneby-Tiassa ##########

# We generate a vector with the sample quantiles.
samplequantilesAgnebyTiassa = quantile(pcgapdeptotndpoorIndividualAgnebyTiassa, probs = seq(0, 1, length = 1000))
unname(samplequantilesAgnebyTiassa)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorAgnebyTiassa <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorAgnebyTiassa

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorAgnebyTiassa <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorAgnebyTiassa

# We generate the .tex file
FileNameQQPlotAgnebyTiassa <- paste0('QQPlotDefititatTrapping', 'AgnebyTiassa.tex')
tikz(FileNameQQPlotAgnebyTiassa, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorAgnebyTiassa, samplequantilesAgnebyTiassa, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorAgnebyTiassa, samplequantilesAgnebyTiassa, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### GÔh ##########

# We generate a vector with the sample quantiles.
samplequantilesGÔh = quantile(pcgapdeptotndpoorIndividualGÔh, probs = seq(0, 1, length = 1000))
unname(samplequantilesGÔh)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorGÔh <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorGÔh

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorGÔh <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorGÔh

# We generate the .tex file
FileNameQQPlotGÔh <- paste0('QQPlotDefititatTrapping', 'GÔh.tex')
tikz(FileNameQQPlotGÔh, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorGÔh, samplequantilesGÔh, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorGÔh, samplequantilesGÔh, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Cavally ##########

# We generate a vector with the sample quantiles.
samplequantilesCavally = quantile(pcgapdeptotndpoorIndividualCavally, probs = seq(0, 1, length = 1000))
unname(samplequantilesCavally)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorCavally <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorCavally

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorCavally <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorCavally

# We generate the .tex file
FileNameQQPlotCavally <- paste0('QQPlotDefititatTrapping', 'Cavally.tex')
tikz(FileNameQQPlotCavally, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorCavally, samplequantilesCavally, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorCavally, samplequantilesCavally, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Bafing ##########

# We generate a vector with the sample quantiles.
samplequantilesBafing = quantile(pcgapdeptotndpoorIndividualBafing, probs = seq(0, 1, length = 1000))
unname(samplequantilesBafing)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorBafing <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorBafing

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorBafing <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorBafing

# We generate the .tex file
FileNameQQPlotBafing <- paste0('QQPlotDefititatTrapping', 'Bafing.tex')
tikz(FileNameQQPlotBafing, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorBafing, samplequantilesBafing, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorBafing, samplequantilesBafing, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Bagoue ##########

# We generate a vector with the sample quantiles.
samplequantilesBagoue = quantile(pcgapdeptotndpoorIndividualBagoue, probs = seq(0, 1, length = 1000))
unname(samplequantilesBagoue)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorBagoue <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorBagoue

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorBagoue <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorBagoue

# We generate the .tex file
FileNameQQPlotBagoue <- paste0('QQPlotDefititatTrapping', 'Bagoue.tex')
tikz(FileNameQQPlotBagoue, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorBagoue, samplequantilesBagoue, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorBagoue, samplequantilesBagoue, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Belier ##########

# We generate a vector with the sample quantiles.
samplequantilesBelier = quantile(pcgapdeptotndpoorIndividualBelier, probs = seq(0, 1, length = 1000))
unname(samplequantilesBelier)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorBelier <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorBelier

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorBelier <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorBelier

# We generate the .tex file
FileNameQQPlotBelier <- paste0('QQPlotDefititatTrapping', 'Belier.tex')
tikz(FileNameQQPlotBelier, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorBelier, samplequantilesBelier, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorBelier, samplequantilesBelier, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Bere ##########

# We generate a vector with the sample quantiles.
samplequantilesBere = quantile(pcgapdeptotndpoorIndividualBere, probs = seq(0, 1, length = 1000))
unname(samplequantilesBere)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorBere <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorBere

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorBere <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBere, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorBere

# We generate the .tex file
FileNameQQPlotBere <- paste0('QQPlotDefititatTrapping', 'Bere.tex')
tikz(FileNameQQPlotBere, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorBere, samplequantilesBere, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorBere, samplequantilesBere, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Bounkani ##########

# We generate a vector with the sample quantiles.
samplequantilesBounkani = quantile(pcgapdeptotndpoorIndividualBounkani, probs = seq(0, 1, length = 1000))
unname(samplequantilesBounkani)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorBounkani <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorBounkani

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorBounkani <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorBounkani

# We generate the .tex file
FileNameQQPlotBounkani <- paste0('QQPlotDefititatTrapping', 'Bounkani.tex')
tikz(FileNameQQPlotBounkani, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorBounkani, samplequantilesBounkani, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorBounkani, samplequantilesBounkani, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Folon ##########

# We generate a vector with the sample quantiles.
samplequantilesFolon = quantile(pcgapdeptotndpoorIndividualFolon, probs = seq(0, 1, length = 1000))
unname(samplequantilesFolon)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorFolon <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorFolon

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorFolon <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorFolon

# We generate the .tex file
FileNameQQPlotFolon <- paste0('QQPlotDefititatTrapping', 'Folon.tex')
tikz(FileNameQQPlotFolon, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorFolon, samplequantilesFolon, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorFolon, samplequantilesFolon, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### GbÔkle ##########

# We generate a vector with the sample quantiles.
samplequantilesGbÔkle = quantile(pcgapdeptotndpoorIndividualGbÔkle, probs = seq(0, 1, length = 1000))
unname(samplequantilesGbÔkle)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorGbÔkle <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorGbÔkle

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorGbÔkle <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorGbÔkle

# We generate the .tex file
FileNameQQPlotGbÔkle <- paste0('QQPlotDefititatTrapping', 'GbÔkle.tex')
tikz(FileNameQQPlotGbÔkle, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorGbÔkle, samplequantilesGbÔkle, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorGbÔkle, samplequantilesGbÔkle, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Grands-Ponts ##########

# We generate a vector with the sample quantiles.
samplequantilesGrandsPonts = quantile(pcgapdeptotndpoorIndividualGrandsPonts, probs = seq(0, 1, length = 1000))
unname(samplequantilesGrandsPonts)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorGrandsPonts <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorGrandsPonts

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorGrandsPonts <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorGrandsPonts

# We generate the .tex file
FileNameQQPlotGrandsPonts <- paste0('QQPlotDefititatTrapping', 'GrandsPonts.tex')
tikz(FileNameQQPlotGrandsPonts, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorGrandsPonts, samplequantilesGrandsPonts, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorGrandsPonts, samplequantilesGrandsPonts, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Guemon ##########

# We generate a vector with the sample quantiles.
samplequantilesGuemon = quantile(pcgapdeptotndpoorIndividualGuemon, probs = seq(0, 1, length = 1000))
unname(samplequantilesGuemon)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorGuemon <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorGuemon

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorGuemon <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorGuemon

# We generate the .tex file
FileNameQQPlotGuemon <- paste0('QQPlotDefititatTrapping', 'Guemon.tex')
tikz(FileNameQQPlotGuemon, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorGuemon, samplequantilesGuemon, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorGuemon, samplequantilesGuemon, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Hambol ##########

# We generate a vector with the sample quantiles.
samplequantilesHambol = quantile(pcgapdeptotndpoorIndividualHambol, probs = seq(0, 1, length = 1000))
unname(samplequantilesHambol)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorHambol <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorHambol

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorHambol <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorHambol

# We generate the .tex file
FileNameQQPlotHambol <- paste0('QQPlotDefititatTrapping', 'Hambol.tex')
tikz(FileNameQQPlotHambol, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorHambol, samplequantilesHambol, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorHambol, samplequantilesHambol, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Iffou ##########

# We generate a vector with the sample quantiles.
samplequantilesIffou = quantile(pcgapdeptotndpoorIndividualIffou, probs = seq(0, 1, length = 1000))
unname(samplequantilesIffou)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorIffou <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorIffou

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorIffou <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorIffou

# We generate the .tex file
FileNameQQPlotIffou <- paste0('QQPlotDefititatTrapping', 'Iffou.tex')
tikz(FileNameQQPlotIffou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorIffou, samplequantilesIffou, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorIffou, samplequantilesIffou, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### La Me ##########

# We generate a vector with the sample quantiles.
samplequantilesLaMe = quantile(pcgapdeptotndpoorIndividualLaMe, probs = seq(0, 1, length = 1000))
unname(samplequantilesLaMe)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorLaMe <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorLaMe

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorLaMe <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorLaMe

# We generate the .tex file
FileNameQQPlotLaMe <- paste0('QQPlotDefititatTrapping', 'LaMe.tex')
tikz(FileNameQQPlotLaMe, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorLaMe, samplequantilesLaMe, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorLaMe, samplequantilesLaMe, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Nawa ##########

# We generate a vector with the sample quantiles.
samplequantilesNawa = quantile(pcgapdeptotndpoorIndividualNawa, probs = seq(0, 1, length = 1000))
unname(samplequantilesNawa)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorNawa <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorNawa

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorNawa <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorNawa

# We generate the .tex file
FileNameQQPlotNawa <- paste0('QQPlotDefititatTrapping', 'Nawa.tex')
tikz(FileNameQQPlotNawa, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorNawa, samplequantilesNawa, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorNawa, samplequantilesNawa, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Tchologo ##########

# We generate a vector with the sample quantiles.
samplequantilesTchologo = quantile(pcgapdeptotndpoorIndividualTchologo, probs = seq(0, 1, length = 1000))
unname(samplequantilesTchologo)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorTchologo <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorTchologo

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorTchologo <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorTchologo

# We generate the .tex file
FileNameQQPlotTchologo <- paste0('QQPlotDefititatTrapping', 'Tchologo.tex')
tikz(FileNameQQPlotTchologo, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorTchologo, samplequantilesTchologo, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorTchologo, samplequantilesTchologo, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
legend("topleft", inset = 0.05, legend = my.expressions.qq, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Moronou ##########

# We generate a vector with the sample quantiles.
samplequantilesMoronou = quantile(pcgapdeptotndpoorIndividualMoronou, probs = seq(0, 1, length = 1000))
unname(samplequantilesMoronou)

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Maximum Likelihood Estimator (MLE) for α.
theoreticalquantilesMaximumLikelihoodEstimatorMoronou <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMaximumLikelihoodEstimatorMoronou

# We generate a vector with the theoretical quantiles for a generalised beta distribution considering the Method of Moments Estimator (MME) for α.
theoreticalquantilesMethodofMomentsEstimatorMoronou <- qgenbeta(seq(from = 0, to = 1, length = 1000), shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou, shape3 = 1, scale = PovertyLine)
theoreticalquantilesMethodofMomentsEstimatorMoronou

# We generate the .tex file
FileNameQQPlotMoronou <- paste0('QQPlotDefititatTrapping', 'Moronou.tex')
tikz(FileNameQQPlotMoronou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(theoreticalquantilesMaximumLikelihoodEstimatorMoronou, samplequantilesMoronou, xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", xlim = c(0, 350000), ylim = c(0, 350000), col = MyColors[2], cex.lab = 1, cex = 0.5, yaxt = "n", xaxt = "n", pch = 1)
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
lines(seq(0, 350000, length = 1000), seq(0, 350000, length = 1000), col = "red")
points(theoreticalquantilesMethodofMomentsEstimatorMoronou, samplequantilesMoronou, col = MyColors[3], type = "p", cex = 0.5, pch = 2)
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

########### Autonome d'Abidjan ##########

# We generate the .tex file
FileNameCDFPlotAutonomedAbidjan <- paste0('CDFPlotDefititatTrapping', 'TotalPoorAutonomedAbidjan.tex')
tikz(FileNameCDFPlotAutonomedAbidjan, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualAutonomedAbidjan), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Haut-Sassandra ##########

# We generate the .tex file
FileNameCDFPlotHautSassandra <- paste0('CDFPlotDefititatTrapping', 'TotalPoorHautSassandra.tex')
tikz(FileNameCDFPlotHautSassandra, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualHautSassandra), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Poro ##########

# We generate the .tex file
FileNameCDFPlotPoro <- paste0('CDFPlotDefititatTrapping', 'TotalPoorPoro.tex')
tikz(FileNameCDFPlotPoro, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualPoro), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Gbeke ##########

# We generate the .tex file
FileNameCDFPlotGbeke <- paste0('CDFPlotDefititatTrapping', 'TotalPoorGbeke.tex')
tikz(FileNameCDFPlotGbeke, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualGbeke), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Indenie-Djuablin ##########

# We generate the .tex file
FileNameCDFPlotIndenieDjuablin <- paste0('CDFPlotDefititatTrapping', 'TotalPoorIndenieDjuablin.tex')
tikz(FileNameCDFPlotIndenieDjuablin, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualIndenieDjuablin), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Tonkpi ##########

# We generate the .tex file
FileNameCDFPlotTonkpi <- paste0('CDFPlotDefititatTrapping', 'TotalPoorTonkpi.tex')
tikz(FileNameCDFPlotTonkpi, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualTonkpi), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Yamoussoukro ##########

# We generate the .tex file
FileNameCDFPlotYamoussoukro <- paste0('CDFPlotDefititatTrapping', 'TotalPoorYamoussoukro.tex')
tikz(FileNameCDFPlotYamoussoukro, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualYamoussoukro), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Gontougo ##########

# We generate the .tex file
FileNameCDFPlotGontougo <- paste0('CDFPlotDefititatTrapping', 'TotalPoorGontougo.tex')
tikz(FileNameCDFPlotGontougo, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualGontougo), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### San-Pedro ##########

# We generate the .tex file
FileNameCDFPlotSanPedro <- paste0('CDFPlotDefititatTrapping', 'TotalPoorSanPedro.tex')
tikz(FileNameCDFPlotSanPedro, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualSanPedro), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Kabadougou ##########

# We generate the .tex file
FileNameCDFPlotKabadougou <- paste0('CDFPlotDefititatTrapping', 'TotalPoorKabadougou.tex')
tikz(FileNameCDFPlotKabadougou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualKabadougou), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### N'Zi ##########

# We generate the .tex file
FileNameCDFPlotNZi <- paste0('CDFPlotDefititatTrapping', 'TotalPoorNZi.tex')
tikz(FileNameCDFPlotNZi, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualNZi), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Marahoue ##########

# We generate the .tex file
FileNameCDFPlotMarahoue <- paste0('CDFPlotDefititatTrapping', 'TotalPoorMarahoue.tex')
tikz(FileNameCDFPlotMarahoue, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualMarahoue), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Sud-Comoe ##########

# We generate the .tex file
FileNameCDFPlotSudComoe <- paste0('CDFPlotDefititatTrapping', 'TotalPoorSudComoe.tex')
tikz(FileNameCDFPlotSudComoe, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualSudComoe), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Worodougou ##########

# We generate the .tex file
FileNameCDFPlotWorodougou <- paste0('CDFPlotDefititatTrapping', 'TotalPoorWorodougou.tex')
tikz(FileNameCDFPlotWorodougou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualWorodougou), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Lôh-Djiboua ##########

# We generate the .tex file
FileNameCDFPlotLôhDjiboua <- paste0('CDFPlotDefititatTrapping', 'TotalPoorLôhDjiboua.tex')
tikz(FileNameCDFPlotLôhDjiboua, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualLôhDjiboua), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Agneby-Tiassa ##########

# We generate the .tex file
FileNameCDFPlotAgnebyTiassa <- paste0('CDFPlotDefititatTrapping', 'TotalPoorAgnebyTiassa.tex')
tikz(FileNameCDFPlotAgnebyTiassa, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualAgnebyTiassa), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### GÔh ##########

# We generate the .tex file
FileNameCDFPlotGÔh <- paste0('CDFPlotDefititatTrapping', 'TotalPoorGÔh.tex')
tikz(FileNameCDFPlotGÔh, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualGÔh), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Cavally ##########

# We generate the .tex file
FileNameCDFPlotCavally <- paste0('CDFPlotDefititatTrapping', 'TotalPoorCavally.tex')
tikz(FileNameCDFPlotCavally, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualCavally), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Bafing ##########

# We generate the .tex file
FileNameCDFPlotBafing <- paste0('CDFPlotDefititatTrapping', 'TotalPoorBafing.tex')
tikz(FileNameCDFPlotBafing, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualBafing), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Bagoue ##########

# We generate the .tex file
FileNameCDFPlotBagoue <- paste0('CDFPlotDefititatTrapping', 'TotalPoorBagoue.tex')
tikz(FileNameCDFPlotBagoue, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualBagoue), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Belier ##########

# We generate the .tex file
FileNameCDFPlotBelier <- paste0('CDFPlotDefititatTrapping', 'TotalPoorBelier.tex')
tikz(FileNameCDFPlotBelier, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualBelier), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Bere ##########

# We generate the .tex file
FileNameCDFPlotBere <- paste0('CDFPlotDefititatTrapping', 'TotalPoorBere.tex')
tikz(FileNameCDFPlotBere, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualBere), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Bounkani ##########

# We generate the .tex file
FileNameCDFPlotBounkani <- paste0('CDFPlotDefititatTrapping', 'TotalPoorBounkani.tex')
tikz(FileNameCDFPlotBounkani, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualBounkani), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Folon ##########

# We generate the .tex file
FileNameCDFPlotFolon <- paste0('CDFPlotDefititatTrapping', 'TotalPoorFolon.tex')
tikz(FileNameCDFPlotFolon, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualFolon), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### GbÔkle ##########

# We generate the .tex file
FileNameCDFPlotGbÔkle <- paste0('CDFPlotDefititatTrapping', 'TotalPoorGbÔkle.tex')
tikz(FileNameCDFPlotGbÔkle, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualGbÔkle), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Grands-Ponts ##########

# We generate the .tex file
FileNameCDFPlotGrandsPonts <- paste0('CDFPlotDefititatTrapping', 'TotalPoorGrandsPonts.tex')
tikz(FileNameCDFPlotGrandsPonts, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualGrandsPonts), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Guemon ##########

# We generate the .tex file
FileNameCDFPlotGuemon <- paste0('CDFPlotDefititatTrapping', 'TotalPoorGuemon.tex')
tikz(FileNameCDFPlotGuemon, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualGuemon), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Hambol ##########

# We generate the .tex file
FileNameCDFPlotHambol <- paste0('CDFPlotDefititatTrapping', 'TotalPoorHambol.tex')
tikz(FileNameCDFPlotHambol, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualHambol), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Iffou ##########

# We generate the .tex file
FileNameCDFPlotIffou <- paste0('CDFPlotDefititatTrapping', 'TotalPoorIffou.tex')
tikz(FileNameCDFPlotIffou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualIffou), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### La Me ##########

# We generate the .tex file
FileNameCDFPlotLaMe <- paste0('CDFPlotDefititatTrapping', 'TotalPoorLaMe.tex')
tikz(FileNameCDFPlotLaMe, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualLaMe), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Nawa ##########

# We generate the .tex file
FileNameCDFPlotNawa <- paste0('CDFPlotDefititatTrapping', 'TotalPoorNawa.tex')
tikz(FileNameCDFPlotNawa, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualNawa), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Tchologo ##########

# We generate the .tex file
FileNameCDFPlotTchologo <- paste0('CDFPlotDefititatTrapping', 'TotalPoorTchologo.tex')
tikz(FileNameCDFPlotTchologo, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualTchologo), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
legend("bottomright", inset = 0.05, legend = my.expressions.cdf, col = c(MyColors[1], MyColors[2], MyColors[3]),
       lty = c(1, 1, 2),
       cex = 0.8, 
       ncol = 1, 
       lwd = c(1, 2, 2), 
       pch = c(16, NA, NA))
dev.off()

########### Moronou ##########

# We generate the .tex file
FileNameCDFPlotMoronou <- paste0('CDFPlotDefititatTrapping', 'TotalPoorMoronou.tex')
tikz(FileNameCDFPlotMoronou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(ecdf(pcgapdeptotndpoorIndividualMoronou), main = "", xlab = "$y$", ylab = "$F(y|x,\\tau_{x}<\\infty)$", xlim = c(0, 350000), col = MyColors[1], cex.lab = 1, lty = 1, lwd = 1, cex = 0.1, xaxt = "n", yaxt = "n")
axis(side = 1, at = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), labels = formatC(c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), format = "d", big.mark = ','), cex.axis = 0.6)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[2], type = "l", lwd = 2, cex.axis = 0.6)
curve(ppbeta(x, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine), add = TRUE,  col = MyColors[3], type = "l", lty = 2, lwd = 2, cex.axis = 0.6)
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

########### Autonome d'Abidjan ##########

# We generate the .tex file
FileNamePPPlotAutonomedAbidjan <- paste0('PPPlotDefititatTrapping', 'TotalPoorAutonomedAbidjan.tex')
tikz(FileNamePPPlotAutonomedAbidjan, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorAutonomedAbidjan <- ppbeta(pcgapdeptotndpoorIndividualAutonomedAbidjan, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorAutonomedAbidjan <- ppbeta(pcgapdeptotndpoorIndividualAutonomedAbidjan, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualAutonomedAbidjan)), sort(probDistMaximumLikelihoodEstimatorAutonomedAbidjan), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualAutonomedAbidjan)), sort(probDistMethodofMomentsEstimatorAutonomedAbidjan), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Haut-Sassandra ##########

# We generate the .tex file
FileNamePPPlotHautSassandra <- paste0('PPPlotDefititatTrapping', 'TotalPoorHautSassandra.tex')
tikz(FileNamePPPlotHautSassandra, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorHautSassandra <- ppbeta(pcgapdeptotndpoorIndividualHautSassandra, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorHautSassandra <- ppbeta(pcgapdeptotndpoorIndividualHautSassandra, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHautSassandra, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualHautSassandra)), sort(probDistMaximumLikelihoodEstimatorHautSassandra), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualHautSassandra)), sort(probDistMethodofMomentsEstimatorHautSassandra), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Poro ##########

# We generate the .tex file
FileNamePPPlotPoro <- paste0('PPPlotDefititatTrapping', 'TotalPoorPoro.tex')
tikz(FileNamePPPlotPoro, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorPoro <- ppbeta(pcgapdeptotndpoorIndividualPoro, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorPoro <- ppbeta(pcgapdeptotndpoorIndividualPoro, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualPoro, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualPoro)), sort(probDistMaximumLikelihoodEstimatorPoro), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualPoro)), sort(probDistMethodofMomentsEstimatorPoro), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Gbeke ##########

# We generate the .tex file
FileNamePPPlotGbeke <- paste0('PPPlotDefititatTrapping', 'TotalPoorGbeke.tex')
tikz(FileNamePPPlotGbeke, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorGbeke <- ppbeta(pcgapdeptotndpoorIndividualGbeke, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorGbeke <- ppbeta(pcgapdeptotndpoorIndividualGbeke, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbeke, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualGbeke)), sort(probDistMaximumLikelihoodEstimatorGbeke), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualGbeke)), sort(probDistMethodofMomentsEstimatorGbeke), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Indenie-Djuablin ##########

# We generate the .tex file
FileNamePPPlotIndenieDjuablin <- paste0('PPPlotDefititatTrapping', 'TotalPoorIndenieDjuablin.tex')
tikz(FileNamePPPlotIndenieDjuablin, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorIndenieDjuablin <- ppbeta(pcgapdeptotndpoorIndividualIndenieDjuablin, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorIndenieDjuablin <- ppbeta(pcgapdeptotndpoorIndividualIndenieDjuablin, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIndenieDjuablin, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualIndenieDjuablin)), sort(probDistMaximumLikelihoodEstimatorIndenieDjuablin), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualIndenieDjuablin)), sort(probDistMethodofMomentsEstimatorIndenieDjuablin), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Tonkpi ##########

# We generate the .tex file
FileNamePPPlotTonkpi <- paste0('PPPlotDefititatTrapping', 'TotalPoorTonkpi.tex')
tikz(FileNamePPPlotTonkpi, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorTonkpi <- ppbeta(pcgapdeptotndpoorIndividualTonkpi, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorTonkpi <- ppbeta(pcgapdeptotndpoorIndividualTonkpi, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTonkpi, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualTonkpi)), sort(probDistMaximumLikelihoodEstimatorTonkpi), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualTonkpi)), sort(probDistMethodofMomentsEstimatorTonkpi), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Yamoussoukro ##########

# We generate the .tex file
FileNamePPPlotYamoussoukro <- paste0('PPPlotDefititatTrapping', 'TotalPoorYamoussoukro.tex')
tikz(FileNamePPPlotYamoussoukro, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorYamoussoukro <- ppbeta(pcgapdeptotndpoorIndividualYamoussoukro, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorYamoussoukro <- ppbeta(pcgapdeptotndpoorIndividualYamoussoukro, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualYamoussoukro, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualYamoussoukro)), sort(probDistMaximumLikelihoodEstimatorYamoussoukro), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualYamoussoukro)), sort(probDistMethodofMomentsEstimatorYamoussoukro), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Gontougo ##########

# We generate the .tex file
FileNamePPPlotGontougo <- paste0('PPPlotDefititatTrapping', 'TotalPoorGontougo.tex')
tikz(FileNamePPPlotGontougo, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorGontougo <- ppbeta(pcgapdeptotndpoorIndividualGontougo, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorGontougo <- ppbeta(pcgapdeptotndpoorIndividualGontougo, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualGontougo)), sort(probDistMaximumLikelihoodEstimatorGontougo), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualGontougo)), sort(probDistMethodofMomentsEstimatorGontougo), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### San-Pedro ##########

# We generate the .tex file
FileNamePPPlotSanPedro <- paste0('PPPlotDefititatTrapping', 'TotalPoorSanPedro.tex')
tikz(FileNamePPPlotSanPedro, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorSanPedro <- ppbeta(pcgapdeptotndpoorIndividualSanPedro, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorSanPedro <- ppbeta(pcgapdeptotndpoorIndividualSanPedro, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSanPedro, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualSanPedro)), sort(probDistMaximumLikelihoodEstimatorSanPedro), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualSanPedro)), sort(probDistMethodofMomentsEstimatorSanPedro), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Kabadougou ##########

# We generate the .tex file
FileNamePPPlotKabadougou <- paste0('PPPlotDefititatTrapping', 'TotalPoorKabadougou.tex')
tikz(FileNamePPPlotKabadougou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorKabadougou <- ppbeta(pcgapdeptotndpoorIndividualKabadougou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorKabadougou <- ppbeta(pcgapdeptotndpoorIndividualKabadougou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualKabadougou, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualKabadougou)), sort(probDistMaximumLikelihoodEstimatorKabadougou), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualKabadougou)), sort(probDistMethodofMomentsEstimatorKabadougou), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### N'Zi ##########

# We generate the .tex file
FileNamePPPlotNZi <- paste0('PPPlotDefititatTrapping', 'TotalPoorNZi.tex')
tikz(FileNamePPPlotNZi, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorNZi <- ppbeta(pcgapdeptotndpoorIndividualNZi, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorNZi <- ppbeta(pcgapdeptotndpoorIndividualNZi, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNZi, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualNZi)), sort(probDistMaximumLikelihoodEstimatorNZi), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualNZi)), sort(probDistMethodofMomentsEstimatorNZi), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Marahoue ##########

# We generate the .tex file
FileNamePPPlotMarahoue <- paste0('PPPlotDefititatTrapping', 'TotalPoorMarahoue.tex')
tikz(FileNamePPPlotMarahoue, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorMarahoue <- ppbeta(pcgapdeptotndpoorIndividualMarahoue, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorMarahoue <- ppbeta(pcgapdeptotndpoorIndividualMarahoue, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMarahoue, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualMarahoue)), sort(probDistMaximumLikelihoodEstimatorMarahoue), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualMarahoue)), sort(probDistMethodofMomentsEstimatorMarahoue), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Sud-Comoe ##########

# We generate the .tex file
FileNamePPPlotSudComoe <- paste0('PPPlotDefititatTrapping', 'TotalPoorSudComoe.tex')
tikz(FileNamePPPlotSudComoe, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorSudComoe <- ppbeta(pcgapdeptotndpoorIndividualSudComoe, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorSudComoe <- ppbeta(pcgapdeptotndpoorIndividualSudComoe, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualSudComoe, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualSudComoe)), sort(probDistMaximumLikelihoodEstimatorSudComoe), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualSudComoe)), sort(probDistMethodofMomentsEstimatorSudComoe), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Worodougou ##########

# We generate the .tex file
FileNamePPPlotWorodougou <- paste0('PPPlotDefititatTrapping', 'TotalPoorWorodougou.tex')
tikz(FileNamePPPlotWorodougou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorWorodougou <- ppbeta(pcgapdeptotndpoorIndividualWorodougou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorWorodougou <- ppbeta(pcgapdeptotndpoorIndividualWorodougou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualWorodougou, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualWorodougou)), sort(probDistMaximumLikelihoodEstimatorWorodougou), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualWorodougou)), sort(probDistMethodofMomentsEstimatorWorodougou), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Lôh-Djiboua ##########

# We generate the .tex file
FileNamePPPlotLôhDjiboua <- paste0('PPPlotDefititatTrapping', 'TotalPoorLôhDjiboua.tex')
tikz(FileNamePPPlotLôhDjiboua, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorLôhDjiboua <- ppbeta(pcgapdeptotndpoorIndividualLôhDjiboua, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorLôhDjiboua <- ppbeta(pcgapdeptotndpoorIndividualLôhDjiboua, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLôhDjiboua, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualLôhDjiboua)), sort(probDistMaximumLikelihoodEstimatorLôhDjiboua), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualLôhDjiboua)), sort(probDistMethodofMomentsEstimatorLôhDjiboua), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Agneby-Tiassa ##########

# We generate the .tex file
FileNamePPPlotAgnebyTiassa <- paste0('PPPlotDefititatTrapping', 'TotalPoorAgnebyTiassa.tex')
tikz(FileNamePPPlotAgnebyTiassa, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorAgnebyTiassa <- ppbeta(pcgapdeptotndpoorIndividualAgnebyTiassa, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorAgnebyTiassa <- ppbeta(pcgapdeptotndpoorIndividualAgnebyTiassa, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualAgnebyTiassa, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualAgnebyTiassa)), sort(probDistMaximumLikelihoodEstimatorAgnebyTiassa), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualAgnebyTiassa)), sort(probDistMethodofMomentsEstimatorAgnebyTiassa), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### GÔh ##########

# We generate the .tex file
FileNamePPPlotGÔh <- paste0('PPPlotDefititatTrapping', 'TotalPoorGÔh.tex')
tikz(FileNamePPPlotGÔh, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorGÔh <- ppbeta(pcgapdeptotndpoorIndividualGÔh, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorGÔh <- ppbeta(pcgapdeptotndpoorIndividualGÔh, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGÔh, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualGÔh)), sort(probDistMaximumLikelihoodEstimatorGÔh), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualGÔh)), sort(probDistMethodofMomentsEstimatorGÔh), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Cavally ##########

# We generate the .tex file
FileNamePPPlotCavally <- paste0('PPPlotDefititatTrapping', 'TotalPoorCavally.tex')
tikz(FileNamePPPlotCavally, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorCavally <- ppbeta(pcgapdeptotndpoorIndividualCavally, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorCavally <- ppbeta(pcgapdeptotndpoorIndividualCavally, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualCavally, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualCavally)), sort(probDistMaximumLikelihoodEstimatorCavally), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualCavally)), sort(probDistMethodofMomentsEstimatorCavally), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Bafing ##########

# We generate the .tex file
FileNamePPPlotBafing <- paste0('PPPlotDefititatTrapping', 'TotalPoorBafing.tex')
tikz(FileNamePPPlotBafing, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorBafing <- ppbeta(pcgapdeptotndpoorIndividualBafing, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorBafing <- ppbeta(pcgapdeptotndpoorIndividualBafing, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBafing, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualBafing)), sort(probDistMaximumLikelihoodEstimatorBafing), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualBafing)), sort(probDistMethodofMomentsEstimatorBafing), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Bagoue ##########

# We generate the .tex file
FileNamePPPlotBagoue <- paste0('PPPlotDefititatTrapping', 'TotalPoorBagoue.tex')
tikz(FileNamePPPlotBagoue, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorBagoue <- ppbeta(pcgapdeptotndpoorIndividualBagoue, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorBagoue <- ppbeta(pcgapdeptotndpoorIndividualBagoue, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBagoue, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualBagoue)), sort(probDistMaximumLikelihoodEstimatorBagoue), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualBagoue)), sort(probDistMethodofMomentsEstimatorBagoue), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Belier ##########

# We generate the .tex file
FileNamePPPlotBelier <- paste0('PPPlotDefititatTrapping', 'TotalPoorBelier.tex')
tikz(FileNamePPPlotBelier, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorBelier <- ppbeta(pcgapdeptotndpoorIndividualBelier, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorBelier <- ppbeta(pcgapdeptotndpoorIndividualBelier, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBelier, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualBelier)), sort(probDistMaximumLikelihoodEstimatorBelier), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualBelier)), sort(probDistMethodofMomentsEstimatorBelier), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Bere ##########

# We generate the .tex file
FileNamePPPlotBere <- paste0('PPPlotDefititatTrapping', 'TotalPoorBere.tex')
tikz(FileNamePPPlotBere, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorBere <- ppbeta(pcgapdeptotndpoorIndividualBere, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorBere <- ppbeta(pcgapdeptotndpoorIndividualBere, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBere, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualBere)), sort(probDistMaximumLikelihoodEstimatorBere), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualBere)), sort(probDistMethodofMomentsEstimatorBere), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Bounkani ##########

# We generate the .tex file
FileNamePPPlotBounkani <- paste0('PPPlotDefititatTrapping', 'TotalPoorBounkani.tex')
tikz(FileNamePPPlotBounkani, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorBounkani <- ppbeta(pcgapdeptotndpoorIndividualBounkani, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorBounkani <- ppbeta(pcgapdeptotndpoorIndividualBounkani, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualBounkani)), sort(probDistMaximumLikelihoodEstimatorBounkani), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualBounkani)), sort(probDistMethodofMomentsEstimatorBounkani), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Folon ##########

# We generate the .tex file
FileNamePPPlotFolon <- paste0('PPPlotDefititatTrapping', 'TotalPoorFolon.tex')
tikz(FileNamePPPlotFolon, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorFolon <- ppbeta(pcgapdeptotndpoorIndividualFolon, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorFolon <- ppbeta(pcgapdeptotndpoorIndividualFolon, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualFolon, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualFolon)), sort(probDistMaximumLikelihoodEstimatorFolon), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualFolon)), sort(probDistMethodofMomentsEstimatorFolon), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### GbÔkle ##########

# We generate the .tex file
FileNamePPPlotGbÔkle <- paste0('PPPlotDefititatTrapping', 'TotalPoorGbÔkle.tex')
tikz(FileNamePPPlotGbÔkle, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorGbÔkle <- ppbeta(pcgapdeptotndpoorIndividualGbÔkle, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorGbÔkle <- ppbeta(pcgapdeptotndpoorIndividualGbÔkle, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGbÔkle, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualGbÔkle)), sort(probDistMaximumLikelihoodEstimatorGbÔkle), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualGbÔkle)), sort(probDistMethodofMomentsEstimatorGbÔkle), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Grands-Ponts ##########

# We generate the .tex file
FileNamePPPlotGrandsPonts <- paste0('PPPlotDefititatTrapping', 'TotalPoorGrandsPonts.tex')
tikz(FileNamePPPlotGrandsPonts, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorGrandsPonts <- ppbeta(pcgapdeptotndpoorIndividualGrandsPonts, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorGrandsPonts <- ppbeta(pcgapdeptotndpoorIndividualGrandsPonts, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualGrandsPonts)), sort(probDistMaximumLikelihoodEstimatorGrandsPonts), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualGrandsPonts)), sort(probDistMethodofMomentsEstimatorGrandsPonts), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Guemon ##########

# We generate the .tex file
FileNamePPPlotGuemon <- paste0('PPPlotDefititatTrapping', 'TotalPoorGuemon.tex')
tikz(FileNamePPPlotGuemon, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorGuemon <- ppbeta(pcgapdeptotndpoorIndividualGuemon, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorGuemon <- ppbeta(pcgapdeptotndpoorIndividualGuemon, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualGuemon, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualGuemon)), sort(probDistMaximumLikelihoodEstimatorGuemon), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualGuemon)), sort(probDistMethodofMomentsEstimatorGuemon), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Hambol ##########

# We generate the .tex file
FileNamePPPlotHambol <- paste0('PPPlotDefititatTrapping', 'TotalPoorHambol.tex')
tikz(FileNamePPPlotHambol, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorHambol <- ppbeta(pcgapdeptotndpoorIndividualHambol, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorHambol <- ppbeta(pcgapdeptotndpoorIndividualHambol, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualHambol, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualHambol)), sort(probDistMaximumLikelihoodEstimatorHambol), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualHambol)), sort(probDistMethodofMomentsEstimatorHambol), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Iffou ##########

# We generate the .tex file
FileNamePPPlotIffou <- paste0('PPPlotDefititatTrapping', 'TotalPoorIffou.tex')
tikz(FileNamePPPlotIffou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorIffou <- ppbeta(pcgapdeptotndpoorIndividualIffou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorIffou <- ppbeta(pcgapdeptotndpoorIndividualIffou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualIffou, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualIffou)), sort(probDistMaximumLikelihoodEstimatorIffou), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualIffou)), sort(probDistMethodofMomentsEstimatorIffou), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### La Me ##########

# We generate the .tex file
FileNamePPPlotLaMe <- paste0('PPPlotDefititatTrapping', 'TotalPoorLaMe.tex')
tikz(FileNamePPPlotLaMe, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorLaMe <- ppbeta(pcgapdeptotndpoorIndividualLaMe, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorLaMe <- ppbeta(pcgapdeptotndpoorIndividualLaMe, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualLaMe, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualLaMe)), sort(probDistMaximumLikelihoodEstimatorLaMe), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualLaMe)), sort(probDistMethodofMomentsEstimatorLaMe), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Nawa ##########

# We generate the .tex file
FileNamePPPlotNawa <- paste0('PPPlotDefititatTrapping', 'TotalPoorNawa.tex')
tikz(FileNamePPPlotNawa, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorNawa <- ppbeta(pcgapdeptotndpoorIndividualNawa, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorNawa <- ppbeta(pcgapdeptotndpoorIndividualNawa, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualNawa, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualNawa)), sort(probDistMaximumLikelihoodEstimatorNawa), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualNawa)), sort(probDistMethodofMomentsEstimatorNawa), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Tchologo ##########

# We generate the .tex file
FileNamePPPlotTchologo <- paste0('PPPlotDefititatTrapping', 'TotalPoorTchologo.tex')
tikz(FileNamePPPlotTchologo, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorTchologo <- ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorTchologo <- ppbeta(pcgapdeptotndpoorIndividualTchologo, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualTchologo, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualTchologo)), sort(probDistMaximumLikelihoodEstimatorTchologo), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualTchologo)), sort(probDistMethodofMomentsEstimatorTchologo), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
abline(0, 1, col = "red")
legend("topleft", inset = 0.05, legend = my.expressions.pp, col = c(MyColors[2], MyColors[3]),
       lty = c(NA, NA),
       cex = 0.8, 
       ncol = 1, 
       lwd = 1,
       x.intersp = c(0, 0),
       pch = c(1, 2))
dev.off()

########### Moronou ##########

# We generate the .tex file
FileNamePPPlotMoronou <- paste0('PPPlotDefititatTrapping', 'TotalPoorMoronou.tex')
tikz(FileNamePPPlotMoronou, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}", "\\usepackage{scalerel}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
probDistMaximumLikelihoodEstimatorMoronou <- ppbeta(pcgapdeptotndpoorIndividualMoronou, shape1 = 1, shape2 = MaximumLikelihoodEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine)
probDistMethodofMomentsEstimatorMoronou <- ppbeta(pcgapdeptotndpoorIndividualMoronou, shape1 = 1, shape2 = MethodofMomentsEstimatorAlphaTotalPoorIndividualMoronou, PovertyLine = PovertyLine)
plot(ppoints(length(pcgapdeptotndpoorIndividualMoronou)), sort(probDistMaximumLikelihoodEstimatorMoronou), xlab = "Theoretical Probabilities", ylab = "Empirical Probabilities", ylim = c(0, 1), col = MyColors[2], cex.lab = 1, type = "p", cex = 0.5, lwd = 0.25)
points(ppoints(length(pcgapdeptotndpoorIndividualMoronou)), sort(probDistMethodofMomentsEstimatorMoronou), col = MyColors[3], cex.lab = 1, type = "p", pch = 2, cex = 0.5, lwd = 0.25)
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
GraphPathSensitivityPlots <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Ivory Coast/Graphs/Regions/Individual Level/Sensitivity Plots"

# We set the GraphPathPPPlots's location as working directory and save it in the WorkingDirectory variable. Again, this is 
# the location were the graphs are going to be saved.
WorkingDirectory <- setwd(GraphPathSensitivityPlots)

# Set up a vector with different types of lines for each region.

MyLines <-seq(from = 1, to = 6, by = 1)

# Set up a vector with the expressions for the plot's legend.

my.expressions.sens <- c("Autonome d’Abidjan", "Béré", "Bounkani", "Gontougo", "Grands-Ponts")

# We generate the .tex file
FileNameSensitivityPovertyGapIndex <- paste0('SensitivityPovertyGapIndexAlpha', '.tex')
tikz(FileNameSensitivityPovertyGapIndex, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
alpha <- seq(0.001, 6, length = 1000)
plot(alpha, ((1/(1 + alpha)) * TotalPoorIndividualAutonomedAbidjan)/TotalPopulationIndividualAutonomedAbidjan, xaxs = "i", yaxs = "i", xlim= c(0, max(alpha)), ylim= c(0, 0.6), xlab = "$\\alpha$", ylab = "Poverty Gap Index ($FGT_{1}$)", cex.lab = 1, lty = MyLines[1], lwd = 2, col = MyColors[1], cex.lab = 1, type = "l", cex = 0.5, xaxt = "n", yaxt = "n")
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, ((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan)) * TotalPoorIndividualAutonomedAbidjan)/TotalPopulationIndividualAutonomedAbidjan, col = MyColors[1], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, ((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan)) * TotalPoorIndividualAutonomedAbidjan)/TotalPopulationIndividualAutonomedAbidjan, col = MyColors[1], type = "p", pch = 2)
lines(alpha, ((1/(1 + alpha)) * TotalPoorIndividualBere)/TotalPopulationIndividualBere, lty = MyLines[2], lwd = 2, col = MyColors[2])
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere, ((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere)) * TotalPoorIndividualBere)/TotalPopulationIndividualBere, col = MyColors[2], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualBere, ((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualBere)) * TotalPoorIndividualBere)/TotalPopulationIndividualBere, col = MyColors[2], type = "p", pch = 2)
lines(alpha, ((1/(1 + alpha)) * TotalPoorIndividualBounkani)/TotalPopulationIndividualBounkani, lty = MyLines[3], lwd = 2, col = MyColors[3])
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani, ((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani)) * TotalPoorIndividualBounkani)/TotalPopulationIndividualBounkani, col = MyColors[3], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani, ((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani)) * TotalPoorIndividualBounkani)/TotalPopulationIndividualBounkani, col = MyColors[3], type = "p", pch = 2)
lines(alpha, ((1/(1 + alpha)) * TotalPoorIndividualGontougo)/TotalPopulationIndividualGontougo, lty = MyLines[4], lwd = 2, col = MyColors[4])
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo, ((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo)) * TotalPoorIndividualGontougo)/TotalPopulationIndividualGontougo, col = MyColors[4], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo, ((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo)) * TotalPoorIndividualGontougo)/TotalPopulationIndividualGontougo, col = MyColors[4], type = "p", pch = 2)
lines(alpha, ((1/(1 + alpha)) * TotalPoorIndividualGrandsPonts)/TotalPopulationIndividualGrandsPonts, lty = MyLines[5], lwd = 2, col = MyColors[5])
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts, ((1/(1 + MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts)) * TotalPoorIndividualGrandsPonts)/TotalPopulationIndividualGrandsPonts, col = MyColors[5], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts, ((1/(1 + MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts)) * TotalPoorIndividualGrandsPonts)/TotalPopulationIndividualGrandsPonts, col = MyColors[5], type = "p", pch = 2)
axis(side = 1, at = c(0, 1, 2, 3, 4, 5, 6), labels = c(0, 1, 2, 3, 4, 5, 6), cex.axis = 0.7)
axis(side = 2, at = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60), labels = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60), cex.axis = 0.7)
legend("topright", inset = 0.02, legend = my.expressions.sens, lty = MyLines, lwd = 3, col = MyColors, cex = 0.8)
dev.off()

# We generate the .tex file
FileNameSensitivityPovertySeverityIndex <- paste0('SensitivityPovertySeverityIndexAlpha', '.tex')
tikz(FileNameSensitivityPovertySeverityIndex, standAlone = TRUE, width = 4, height = 4, packages = c("\\usepackage{tikz}", "\\usepackage[active,tightpage,psfixbb]{preview}", "\\PreviewEnvironment{pgfpicture}", "\\setlength\\PreviewBorder{0pt}", "\\usepackage{amssymb}"))
par(mgp = c(2.4, 1, 0), mar = c(3.5, 3.5, 2, 1) + 0.1)
plot(alpha, (TotalPoorIndividualAutonomedAbidjan * (2 * PovertyLine^2)/((alpha + 2) * (alpha + 1)))/PovertyLine^2/TotalPopulationIndividualAutonomedAbidjan, xaxs = "i", yaxs = "i", xlim= c(0, max(alpha)), ylim= c(0, 0.6), xlab = "$\\alpha$", ylab = "Poverty Severity Index ($FGT_{2}$)", cex.lab = 1, lty = MyLines[1], lwd = 2, col = MyColors[1], cex.lab = 1, type = "l", cex = 0.5, xaxt = "n", yaxt = "n")
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, (TotalPoorIndividualAutonomedAbidjan * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualAutonomedAbidjan + 1)))/PovertyLine^2/TotalPopulationIndividualAutonomedAbidjan, col = MyColors[1], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan, (TotalPoorIndividualAutonomedAbidjan * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualAutonomedAbidjan + 1)))/PovertyLine^2/TotalPopulationIndividualAutonomedAbidjan, col = MyColors[1], type = "p", pch = 2)
lines(alpha, (TotalPoorIndividualBere * (2 * PovertyLine^2)/((alpha + 2) * (alpha + 1)))/PovertyLine^2/TotalPopulationIndividualBere, xaxs = "i", yaxs = "i", xlim= c(0, max(alpha)), ylim= c(0, 0.5), xlab = "$\\alpha$", ylab = "Poverty Severity Index ($FGT_{2}$)", cex.lab = 1, lty = MyLines[2], lwd = 2, col = MyColors[2], cex.lab = 1, type = "l", cex = 0.5, xaxt = "n", yaxt = "n")
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere, (TotalPoorIndividualBere * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBere + 1)))/PovertyLine^2/TotalPopulationIndividualBere, col = MyColors[2], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualBere, (TotalPoorIndividualBere * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualBere + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualBere + 1)))/PovertyLine^2/TotalPopulationIndividualBere, col = MyColors[2], type = "p", pch = 2)
lines(alpha, (TotalPoorIndividualBounkani * (2 * PovertyLine^2)/((alpha + 2) * (alpha + 1)))/PovertyLine^2/TotalPopulationIndividualBounkani, xaxs = "i", yaxs = "i", xlim= c(0, max(alpha)), ylim= c(0, 0.5), xlab = "$\\alpha$", ylab = "Poverty Severity Index ($FGT_{2}$)", cex.lab = 1, lty = MyLines[3], lwd = 2, col = MyColors[3], cex.lab = 1, type = "l", cex = 0.5, xaxt = "n", yaxt = "n")
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani, (TotalPoorIndividualBounkani* (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualBounkani + 1)))/PovertyLine^2/TotalPopulationIndividualBounkani, col = MyColors[3], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani, (TotalPoorIndividualBounkani * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualBounkani + 1)))/PovertyLine^2/TotalPopulationIndividualBounkani, col = MyColors[3], type = "p", pch = 2)
lines(alpha, (TotalPoorIndividualGontougo * (2 * PovertyLine^2)/((alpha + 2) * (alpha + 1)))/PovertyLine^2/TotalPopulationIndividualGontougo, xaxs = "i", yaxs = "i", xlim= c(0, max(alpha)), ylim= c(0, 0.5), xlab = "$\\alpha$", ylab = "Poverty Severity Index ($FGT_{2}$)", cex.lab = 1, lty = MyLines[4], lwd = 2, col = MyColors[4], cex.lab = 1, type = "l", cex = 0.5, xaxt = "n", yaxt = "n")
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo, (TotalPoorIndividualGontougo * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGontougo + 1)))/PovertyLine^2/TotalPopulationIndividualGontougo, col = MyColors[4], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo, (TotalPoorIndividualGontougo * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualGontougo + 1)))/PovertyLine^2/TotalPopulationIndividualGontougo, col = MyColors[4], type = "p", pch = 2)
lines(alpha, (TotalPoorIndividualGrandsPonts * (2 * PovertyLine^2)/((alpha + 2) * (alpha + 1)))/PovertyLine^2/TotalPopulationIndividualGrandsPonts, xaxs = "i", yaxs = "i", xlim= c(0, max(alpha)), ylim= c(0, 0.5), xlab = "$\\alpha$", ylab = "Poverty Severity Index ($FGT_{2}$)", cex.lab = 1, lty = MyLines[5], lwd = 2, col = MyColors[5], cex.lab = 1, type = "l", cex = 0.5, xaxt = "n", yaxt = "n")
points(MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts, (TotalPoorIndividualGrandsPonts * (2 * PovertyLine^2)/((MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts + 2) * (MaximumLikelihoodEstimatorAlphaTotalPoorIndividualGrandsPonts + 1)))/PovertyLine^2/TotalPopulationIndividualGrandsPonts, col = MyColors[5], type = "p", pch = 1)
points(MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts, (TotalPoorIndividualGrandsPonts * (2 * PovertyLine^2)/((MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts + 2) * (MethodofMomentsEstimatorAlphaTotalPoorIndividualGrandsPonts + 1)))/PovertyLine^2/TotalPopulationIndividualGrandsPonts, col = MyColors[5], type = "p", pch = 2)
axis(side = 1, at = c(0, 1, 2, 3, 4, 5, 6), labels = c(0, 1, 2, 3, 4, 5, 6), cex.axis = 0.7)
axis(side = 2, at = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60), labels = c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60), cex.axis = 0.7)
legend("topright", inset = 0.02, legend = my.expressions.sens, lty = MyLines, lwd = 3, col = MyColors, cex = 0.8)
dev.off()

