library(tidyverse) #conjunto de pacotes, manipulação de dados, de vetores. Operações sequenciais- PAIP (%>%) - faz tudo que vem depois
library(vegan)
library (robis)
library (rgbif)
library (bdc)
library (CoordinateCleaner)
library(taxadb)

dados_iris<-read.csv2("arquivo_unico_LUCIANA.csv", h=T)

lapply(dados_iris, unique)

dados_iris %>% 
  mutate(Sepal.Lenght=as.numeric(Sepal.Lenght),
         Sepal.Width=as.numeric(Sepal.Width),
         Petal.Lenght=as.numeric(Petal.Lenght),
         Petal.Width=as.numeric(Petal.Width))%>%
  select(Species, Sepal.Lenght:Petal.Width) %>% #o paip precisa vir a cada termino de comando. o + é so dentro do ggplot
  pivot_longer(cols = -Species, names_to = "variavel", values_to = "valores")  %>% 
  ggplot(aes(x = valores, fill = Species)) +
  geom_histogram()  +
  facet_wrap(~ variavel, scales = 'free_x') +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "tamanho (mm)") +
  scale_fill_discrete(
    expression(bold("Species:")),
    labels = c(expression(italic("Iris setosa")), 
               expression(italic("Iris versicolor")), 
               expression(italic("Iris virginica"))))

library(validate)
rules <- validator(in_range(lat, min = -90, max = 90),
                   in_range(lat, min = -180, max = 180),
                   is.character(site),
                   is.numeric(date),
                   all_complete(iris))

out   <- confront(dados_iris, rules)
summary(out)
plot(out)

species <- dados_iris %>% 
  mutate(Species=gsub("_", " ", Species))%>%
  distinct(Species) %>% 
  pull() %>% 
  #c("Iris_setosa", .) %>% # a sp que voce quer verificar
  filter_name(., provider = "itis") %>% 
  data.frame() %>% 
  bind_cols(Species = dados_iris %>% 
              distinct(Species) %>% 
              pull())  

taxadb::filter_name() #assume que a função esta dentro (::) do pacote citado anteriormente


library(dplyr)

iris_1 <- dados_iris %>% 
  dplyr::mutate(eventID = paste(site, date, sep = "_"), # create indexing fields 
                occurrenceID = paste(site, date, amostra, sep = "_")) %>% 
  left_join(species %>% 
              select(Species, acceptedNameUsageID, scientificName)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = lon, # rename fields according to DwC 
                decimalLatitude = lat,
                eventDate = date) %>% 
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")


## create eventCore
eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct() 

## create occurrence
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificName, acceptedNameUsageID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 


## create measurementsOrFacts
eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, Sepal.Lenght:Petal.Width) %>%    pivot_longer(cols = Sepal.Lenght:Petal.Width,
                                                                                          names_to = "measurementType",
                                                                                          values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))


# check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)
setdiff(eventCore$eventID, eMOF$eventID)
setdiff(occurrences$eventID, eMOF$eventID)

eMOF %>%
  filter(is.na(eventID))

occurrences %>%
  filter(is.na(eventID))



rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}










##############################INFORMAÇOES###################################
#Para publicar arquivo no github primeiro da o commit e depois o push. 
