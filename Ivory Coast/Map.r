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

Path <- "/Users/josemiguelflorescontro/Documents/UNIL-HEC LAUSANNE/UNIL PhD/Ruin Theory/Project with Jorge/Data/Ivory Coast"

# We set the Path's location as working directory and save it in the WorkingDirectory variable.

WorkingDirectory<- setwd(Path)

# We set up the mapping. 

# 1 Autonome d’Abidjan
# 2 Agnéby-Tiassa
# 3 Bafing
# 4 Bagoué
# 5 Bélier
# 6 Béré
# 7 Bounkani
# 8 Cavally
# 9 Folon
# 10 Gbeke
# 11 Gbôkle
# 12 Gôh
# 13 Gontougo
# 14 Grands Ponts
# 15 Guémon
# 16 Hambol
# 17 Haut-Sassandra
# 18 Iffou
# 19 Indénié-Djuablin
# 20 Kabadougou
# 21 La Mé (Massan)
# 22 Lôh-Djiboua
# 23 Marahoué
# 24 Moronou
# 25 Nawa
# 26 N’Zi
# 27 Poro
# 28 San-Pédro
# 29 Sud-Comoé
# 30 Tchologo
# 31 Tonkpi
# 32 Worodougou
# 33 Yamoussoukro

# We create a data frame that contains the maximum likelihood estimators for α.
# This data frame is called: DataFrameAlphaMLE.

DataFrameAlphaMLE = read.table(text = "
id;alpha;fill
1;4.08;'#E7A967'
2;2.43;'#D97A34'
3;2.12;'#D05B12'
4;2.78;'#F5D79A'
5;2.28;'#E39956'
6;2.63;'#DE8A45'
7;2.84;'#F1C889'
8;2.18;'#ECB878'
9;3.17;'#CC4C02'
10;2.78;'#FFF7BC'
11;3.81;'#FAE7AB'
12;2.55;'#D56B23'
13;3.12;'#ECB878'
14;2.66;'#DE6621'
15;2.64;'#AD470C'
16;2.72;'#D96F32'
17;2.29;'#A33F05'
18;2.73;'#E3834B'
19;2.43;'#803911'
20;2.21;'#CC5D1F'
21;2.46;'#632909'
22;2.66;'#C45F27'
23;2.27;'#C94C06'
24;3.15;'#913E10'
25;2.47;'#8A3201'
26;3.14;'#E87738'
27;2.47;'#FA803C'
28;2.64;'#E35100'
29;3.07;'#B54100'
30;2.15;'#ED712B'
31;1.84;'#DB5509'
32;2.47;'#B54404'
33;2.41;'#F26C1F'
", sep = ";", header = T)

# We create a data frame that contains the method of moments estimators for α.
# This data frame is called: DataFrameAlphaMME.

DataFrameAlphaMME = read.table(text = "
id;alpha;fill
1;3.99;'#E7A967'
2;2.18;'#D97A34'
3;1.89;'#D05B12'
4;2.56;'#F5D79A'
5;2.06;'#E39956'
6;2.58;'#DE8A45'
7;2.67;'#F1C889'
8;2.01;'#ECB878'
9;3.05;'#CC4C02'
10;2.62;'#FFF7BC'
11;3.64;'#FAE7AB'
12;2.40;'#D56B23'
13;3.09;'#ECB878'
14;2.50;'#DE6621'
15;2.40;'#AD470C'
16;2.51;'#D96F32'
17;2.07;'#A33F05'
18;2.50;'#E3834B'
19;2.18;'#803911'
20;2.00;'#CC5D1F'
21;2.24;'#632909'
22;2.42;'#C45F27'
23;2.06;'#C94C06'
24;2.90;'#913E10'
25;2.24;'#8A3201'
26;2.95;'#E87738'
27;2.26;'#FA803C'
28;2.45;'#E35100'
29;2.87;'#B54100'
30;1.90;'#ED712B'
31;1.65;'#DB5509'
32;2.22;'#B54404'
33;2.21;'#F26C1F'
", sep = ";", header = T)

# Get data for Ivory Coast´s map.

IvoryCoast <- geodata::gadm(country = "CIV", level = 2, path = getwd()) |> sf::st_as_sf()
IvoryCoast <- IvoryCoast[order(IvoryCoast$NAME_2),]
IvoryCoast$id <- seq.int(nrow(IvoryCoast))


map_df_alpha_mle <- merge(IvoryCoast, DataFrameAlphaMLE, by = "id")
map_df_alpha_mme <- merge(IvoryCoast, DataFrameAlphaMME, by = "id")


# We generate the plot of the map for the maximum likelihood estimators for α.
IvoryCoastAlphaMLE <- ggplot() + labs(x="",y="")+ theme_void() +
  geom_sf(data = map_df_alpha_mle, aes(fill = alpha), color = NA) +
  scale_fill_viridis_c(option = "magma", guide = "none") +
  scale_fill_gradientn(colours = rev(magma(6)),
                     name = expression(alpha[MLE]),
                     na.value = "grey100", 
                     trans = "log",
                     breaks = c(1.5, 2.0, 2.5, 3, 3.5, 4, 4.5), labels = formatC(c(1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5), digits = 1, format = "f"), limits = c(1.5, 4.5)) +
  geom_text(data = map_df_alpha_mle, aes(label = NAME_2, x = st_coordinates(st_centroid(map_df_alpha_mle$geometry))[,1], y = st_coordinates(st_centroid(map_df_alpha_mle$geometry))[,2]), color = "azure3", size = 1.75)
IvoryCoastAlphaMLE

# We generate the plot of the map for the method of moments estimators for α.
IvoryCoastAlphaMME <- ggplot() + labs(x="",y="")+ theme_void() +
  geom_sf(data = map_df_alpha_mme, aes(fill = alpha), color = NA) +
  scale_fill_viridis_c(option = "magma", guide = "none") +
  scale_fill_gradientn(colours = rev(magma(6)),
                       name = expression(alpha[MME]),
                       na.value = "grey100", 
                       trans = "log",
                       breaks = c(1.5, 2.0, 2.5, 3, 3.5, 4, 4.5), labels = formatC(c(1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5), digits = 1, format = "f"), limits = c(1.5, 4.5)) +
  geom_text(data = map_df_alpha_mme, aes(label = NAME_2, x = st_coordinates(st_centroid(map_df_alpha_mme$geometry))[,1], y = st_coordinates(st_centroid(map_df_alpha_mme$geometry))[,2]), color = "azure3", size = 1.75)
IvoryCoastAlphaMME
