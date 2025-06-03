##Atividade 3- acessar e analisar a ocorrencia da especie Carcharhinus longimanus

library(rgbif)
library(tidyverse)

# baixar ocorrencias
tub_gbif <- occ_data(scientificName = "Carcharhinus longimanus", 
                     hasCoordinate = TRUE,
                     hasGeospatialIssue=FALSE)

# dimensoes
dim(tub_gbif)

dim(tub_gbif$data)

# checar campos
tub_gbif$data %>% names

tub_gbif$data$municipality
tub_gbif$data$locationID

gbif_issues()
?gbif_issues

# checar problemas reportados
tub_issues_gbif <- tub_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% tub_issues_gbif)

#variáveis que serão úteis para a validação dos dados e futuras análises
library(dplyr)
tub_gbif1 <- tub_gbif$data %>%
  dplyr::select(scientificName,acceptedScientificName,decimalLatitude,decimalLongitude, waterBody, depth, issues,basisOfRecord, occurrenceStatus, rightsHolder,datasetName, recordedBy,locality, habitat)

#foram encontrados 500 ocorrencias
tub_gbif1 <- tub_gbif1 %>% 
  distinct() 
#agora foi para 435

# checar niveis dos fatores
lapply(tub_gbif1, unique)

library(bdc)
library(CoordinateCleaner)

# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = tub_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")

# checar coordenadas válidas e próximas a capitais (muitas vezes as coordenadas são erroneamente associadas a capitais dos países)

cl <- tub_gbif1 %>%
  CoordinateCleaner::clean_coordinates(species = "acceptedScientificName",
                                       lat = "decimalLatitude",
                                       lon = "decimalLongitude",
                                       tests = c("capitals", 
                                                 "centroids","equal", 
                                                 "gbif", "institutions", 
                                                 "outliers", "seas", 
                                                 "zeros"))  

# verificar coordenadas com flags

# capitais (padrão é um raio de 10km)
ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = cl, aes(x = decimalLongitude, y = decimalLatitude, color = `.cap`)) +
  coord_quickmap() +
  theme_classic()
?borders

# pontos no mar
ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = cl, aes(x = decimalLongitude, y = decimalLatitude, color = `.sea`)) +
  coord_quickmap() +
  theme_classic()

# investigar niveis suspeitos
tub_gbif1 %>% 
  distinct(waterBody) %>% 
  pull()
# varios

# waterBody
tub_gbif1 %>%
  group_by(waterBody) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=waterBody)) +
  geom_bar(stat = 'identity') 

tub_gbif1 %>% 
  filter(waterBody %in% c("Global","Pacific Ocean","Mar Caribe")) %>% 
  distinct(datasetName)

tub_gbif1 <- tub_gbif1 %>% 
  filter(!waterBody %in% c("Global","Pacific Ocean","Mar Caribe"))

tub_gbif1 %>% 
  filter(datasetName %in% c("Observatoire Pelagis aerial surveys 2002-2021","Diveboard - Scuba diving citizen science ")) %>% 
  data.frame()

# filtrar todas do dataset suspeito
tub_gbif_noDiveboard <- tub_gbif1 %>% 
  filter(!datasetName %in% c("Observatoire Pelagis aerial surveys 2002-2021","Diveboard - Scuba diving citizen science "))
tub_gbif_ok<-tub_gbif_noDiveboard

library(ggmap)
library(maps)
library(mapdata)

world <- map_data("world")
?map_data
# checar pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = tub_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Carcharhinus longimanus")))
# lims(x = c(-100, 0), y = c(-50, 25))

# checar profundidade
tub_gbif_ok %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 


## OBIS
tub_obis <- robis::occurrence("Carcharhinus longimanus")
# checar dados
names(tub_obis)

tub_obis1 <- tub_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()

# check problemas reportados (flags)
tub_obis1 %>% 
  distinct(flags)

# check NA em datasetName
tub_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         is.na(datasetName)) %>% 
  distinct(waterBody)

# depth ok
tub_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Mar Caribe", "Pacific")) %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

tub_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Mar Caribe", "Pacific")) %>% 
  lapply(., unique)


# aplicar filtros
tub_obis_ok <- tub_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Mar Caribe", "Pacific"))

# plot
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = tub_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Carcharhinus longimanus")))

tub_obis_final <- tub_obis_ok %>% 
  filter(decimalLongitude > 0 | decimalLongitude < -100)

# plot
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = tub_obis_final, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Carcharhinus longimanus")))

setdiff(names(tub_gbif_ok), names(tub_obis_ok))
setdiff(names(tub_obis_final), names(tub_obis_final))


all_data <- bind_rows(tub_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      tub_obis_final %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Carcharhinus longimanus") %>% 
  dplyr::select(-rn)


# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Carcharhinus longimanus")))


write.csv(all_data, "occ_GBIF-OBIS_tub_long.csv", row.names = FALSE)
