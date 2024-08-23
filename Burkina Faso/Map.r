################################################################################################################################################
###################### Article: The Gerber-Shiu Expected Discounted Penalty Function: An Application to Poverty Trapping #######################
######################################## Author: Flores-Contró, José M. ########################################################################
################################################################################################################################################

#########################
### Loading Packages ####
#########################

# We load the packages.
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)
library(tikzDevice)
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}", "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
library(viridis)

##########################
###### Data Import #######
##########################

# First, we set the working directory to the folder in which the we would like to save our file.

Path <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Burkina Faso"

# We set the Path's location as working directory and save it in the WorkingDirectory variable.

WorkingDirectory<- setwd(Path)

# We set up the mapping. 

# 1 Boucle du Mouhoun
# 2 Cascades
# 3 Centre
# 4 Centre-Est
# 5 Centre-Nord
# 6 Centre-Ouest
# 7 Centre-Sud
# 8 Est
# 9 Hauts-Bassins
# 10 Nord
# 11 Plateau Central
# 12 Sahel
# 13 Sud-Ouest

# We create a data frame that contains the maximum likelihood estimators for α.
# This data frame is called: DataFrameAlphaMLE.

DataFrameAlphaMLE = read.table(text = "
id;alpha;fill
1;3.08;'#E7A967'
2;4.25;'#D97A34'
3;3.76;'#D05B12'
4;2.79;'#F5D79A'
5;3.64;'#E39956'
6;3.61;'#DE8A45'
7;3.57;'#F1C889'
8;3.49;'#ECB878'
9;3.75;'#CC4C02'
10;2.95;'#FFF7BC'
11;3.45;'#FAE7AB'
12;4.23;'#D56B23'
13;3.51;'#ECB878'
", sep = ";", header = T)

# We create a data frame that contains the method of moments estimators for α.
# This data frame is called: DataFrameAlphaMME.

DataFrameAlphaMME = read.table(text = "
id;alpha;fill
1;2.89;'#E7A967'
2;4.11;'#D97A34'
3;3.61;'#D05B12'
4;2.58;'#F5D79A'
5;3.43;'#E39956'
6;3.40;'#DE8A45'
7;3.35;'#F1C889'
8;3.26;'#ECB878'
9;3.62;'#CC4C02'
10;2.69;'#FFF7BC'
11;3.22;'#FAE7AB'
12;4.07;'#D56B23'
13;3.33;'#ECB878'
", sep = ";", header = T)

# Get data for Burkina Faso´s map.

BurkinaFaso <- geodata::gadm(country = "BFA", level = 1, path = getwd()) |> sf::st_as_sf()
BurkinaFaso <- BurkinaFaso[order(BurkinaFaso$NAME_1),]
BurkinaFaso$id <- seq.int(nrow(BurkinaFaso))


map_df_alpha_mle <- merge(BurkinaFaso, DataFrameAlphaMLE, by = "id")
map_df_alpha_mme <- merge(BurkinaFaso, DataFrameAlphaMME, by = "id")


# We generate the plot of the map for the maximum likelihood estimators for α.
BurkinaFasoAlphaMLE <- ggplot() + labs(x="",y="")+ theme_void() +
  geom_sf(data = map_df_alpha_mle, aes(fill = alpha), color = NA) +
  scale_fill_viridis_c(option = "magma", guide = "none") +
  scale_fill_gradientn(colours = rev(magma(6)),
                       name = expression(alpha[MLE]),
                       na.value = "grey100", 
                       trans = "log",
                       breaks = c(2.5, 3, 3.5, 4, 4.5), labels = formatC(c(2.5, 3.0, 3.5, 4.0, 4.5), digits = 1, format = "f"), limits = c(2.5, 4.5)) +
  geom_text(data = map_df_alpha_mle, aes(label = NAME_1, x = st_coordinates(st_centroid(map_df_alpha_mle$geometry))[,1], y = st_coordinates(st_centroid(map_df_alpha_mle$geometry))[,2]), color = "azure3", size = 2)
BurkinaFasoAlphaMLE

# We generate the plot of the map for the method of moments estimators for α.
BurkinaFasoAlphaMME <- ggplot() + labs(x="",y="")+ theme_void() +
  geom_sf(data = map_df_alpha_mme, aes(fill = alpha), color = NA) +
  scale_fill_viridis_c(option = "magma", guide = "none") +
  scale_fill_gradientn(colours = rev(magma(6)),
                       name = expression(alpha[MME]),
                       na.value = "grey100", 
                       trans = "log",
                       breaks = c(2.5, 3, 3.5, 4, 4.5), labels = formatC(c(2.5, 3.0, 3.5, 4.0, 4.5), digits = 1, format = "f"), limits = c(2.5, 4.5)) +
  geom_text(data = map_df_alpha_mme, aes(label = NAME_1, x = st_coordinates(st_centroid(map_df_alpha_mme$geometry))[,1], y = st_coordinates(st_centroid(map_df_alpha_mme$geometry))[,2]), color = "azure3", size = 2)
BurkinaFasoAlphaMME
