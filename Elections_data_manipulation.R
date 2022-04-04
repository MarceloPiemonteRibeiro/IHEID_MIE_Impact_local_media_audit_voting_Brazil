# Precinct votes data

rm(list = ls()) # clear environment
# devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
# install.packages("ggspatial")
library(geobr)
library(sf)
library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(tidyverse)
library(Hmisc)
# install.packages("lessR")
library(lessR)
library("dplyr")
library(tidyr)
# install.packages("here")
library(here) 
library(stringr) 
library(purrr) 
library("readxl")
library("writexl")

# Upload Brazilian elections 2008 data (source data: https://dadosabertos.tse.jus.br/dataset/resultados-2008)

# Votes per section (electoral precinct): ####
setwd("C:/Users/Ribeiro/OneDrive/Documents/OneDrive/IHEID/Dissertation/Data Sources/Voting data/votes_section_year_state")
votes_sections_files = list.files(here("./votes_section_year_state"),
                      all.files = T,  
                      pattern = ".txt",
                      full.names = F,
                      recursive = TRUE) # combining all txt state datasets in an object - source: https://aosmith.rbind.io/2017/12/31/many-datasets/#list-all-files-to-read-in
read_votes_section = function(path) {
  ( votes_per_section = read.delim (path, header = FALSE, sep = ";",
                       col.names = c("DATA_GERACAO", "HORA_GERACAO",
                                     "YEAR_ELECTION", "NUM_ROUND", "DESCRIPT_ELECTION",
                                     "ACRONYM_STATE", "ACRONYM_ELECT_ZONE", "CODE_MUNICIPALITY",
                                     "NAME_MUNICIPALITY", "NUM_ELECT_ZONE", "NUM_ELECT_SECTION",
                                     "CODE_POST", "DESCRIPT_POST", "ID_CANDIDATE",
                                     "NUM_VOTES_RECEIVED") ) ) # rename columns variables - refer to "LEIAME" pdf file
  votes_per_section<-filter(votes_per_section, DESCRIPT_POST == "PREFEITO") # keep data related only to mayors elections
  votes_per_section <-votes_per_section %>% 
    select(ID_CANDIDATE, YEAR_ELECTION, NUM_ROUND, ACRONYM_STATE, NAME_MUNICIPALITY, NUM_ELECT_SECTION, NUM_VOTES_RECEIVED) %>%
    group_by(ID_CANDIDATE, YEAR_ELECTION, NUM_ROUND, ACRONYM_STATE, NAME_MUNICIPALITY,NUM_ELECT_SECTION) %>%
    summarise(Total = sum(NUM_VOTES_RECEIVED))
} 
a<- read_votes_section(votes_sections_files[1]) # choose a specific n° to check state datasets - [] ranges from 1 to 26
( votes_per_section = map_dfr(votes_sections_files, read_votes_section) ) # combine all datasets
# Notes:
# Summary information is correct and reflected on the 2008 Electoral Superior Tribunal statistics portal : https://sig.tse.jus.br/ords/dwtse/f?p=150:3:::NO:RP:: and https://www.tse.jus.br/hotsites/estatistica2008/indexResult.htm 
# In "ID_CANDIDATE", blank ballot = 95, vote void = 96 and anulled vote or registered separetely = 97

# Votes per municipality + candidates names: ####
setwd("../votes_candidates_munzona_year_state")
votes_municipalities_names = list.files(here("./votes_candidates_munzona_year_state"),
                      all.files = T,  
                      pattern = ".txt",
                      full.names = F,
                      recursive = TRUE) 
read_votes_municip_candidat_names = function(path) {
  ( votes_municipalities_candidates = read.delim (path, header = FALSE, sep = ";",
                                                  col.names = c("DATA_GERACAO", "HORA_GERACAO" ,
                                                                "YEAR_ELECTION", "NUM_ROUND", "DESCRIPT_ELECTION",
                                                                "ACRONYM_STATE", "CODE_ELECT_UNIT1", "CODE_ELECT_UNIT2",
                                                                "NAME_MUNICIPALITY", "NUM_ELECT_ZONE",
                                                                "CODE_POST", "NUM_BALLOT_CANDIDATE", "TSE_INTERNAL_CANDIDATE_CODE",
                                                                "NAME_CANDIDATE", "NAME_BALLOT_CANDIDATE",
                                                                "DESCRIPT_POST", "COD_SIT_CAND_SUPERIOR", "DESC_SIT_CAND_SUPERIOR",
                                                                "STATUS_ELECT_CANDIDATE_CODE", "STATUS_ELECT_CANDIDATE",
                                                                "CODE_SIT_CAND_TOT", "RESULT", "PARTY_NUMBER", "PARTY_ACRONYM",
                                                                "PARTY_NAME", "SEQUENCIAL_LEGENDA", "NAME_COALITION", 
                                                                "COMPOSITION_COALITION", "NUM_VOTES_RECEIVED"))) # rename columns variables - refer to "LEIAME" pdf file
  votes_municipalities_candidates <- filter(votes_municipalities_candidates, DESCRIPT_POST == "PREFEITO") #filter only mayors, who can be affected by Federal audit's results
  votes_municipalities_candidates <-votes_municipalities_candidates %>% 
    select(NUM_BALLOT_CANDIDATE,YEAR_ELECTION, NUM_ROUND, ACRONYM_STATE, NAME_CANDIDATE, CODE_SIT_CAND_TOT, RESULT, PARTY_NUMBER, PARTY_ACRONYM, NAME_MUNICIPALITY, NUM_VOTES_RECEIVED) %>%
    group_by(NUM_BALLOT_CANDIDATE, YEAR_ELECTION, NUM_ROUND, ACRONYM_STATE, NAME_CANDIDATE, CODE_SIT_CAND_TOT, RESULT, PARTY_NUMBER, PARTY_ACRONYM, NAME_MUNICIPALITY) %>%
    summarise(Total = sum(NUM_VOTES_RECEIVED))
} 
b<- read_votes_municip_candidat_names(votes_municipalities_names[1])  # choose a specific state dataset - [] ranges from 1 to 26
( votes_per_candidate = map_dfr(votes_municipalities_names, read_votes_municip_candidat_names) ) # combine all datasets

# Electors characteristics per section: ####
setwd("../details_votes_section_year_state")
details_votes_sections = list.files(here("./details_votes_section_year_state"),
                                        all.files = T,  
                                        pattern = ".txt",
                                        full.names = F,
                                        recursive = TRUE) 
read_details_votes_sections = function(path) {
  ( detailed_votes = read.delim (path, header = FALSE, sep = ";",
                                                  col.names = c("DATA_GERACAO", "HORA_GERACAO",
                                        "YEAR_ELECTION", "NUM_ROUND", "DESCRIPT_ELECTION",
                                        "ACRONYM_STATE", "CODE_ELECT_UNIT1", "CODE_ELECT_UNIT2",
                                        "NAME_MUNICIPALITY", "NUM_ELECT_ZONE", "NUM_ELECT_SECTION",
                                        "CODE_POST", "DESCRIPT_POST", "NUM_PEOP_ABLE_VOTE",
                                        "ATTENDANCE", "ABSENTS", "NOMINAL_VOTES", "BLANKS_VOTES",
                                        "VOIDS_VOTES", "VOTES_COALITION", "CANCELLED_VOTES"))) # rename columns variables - refer to "LEIAME" pdf file
  detailed_votes <- filter(detailed_votes, DESCRIPT_POST == "PREFEITO")
}
c<- read_details_votes_sections(details_votes_sections[1])  # choose a specific state dataset - [] ranges from 1 to 26
( detailed_votes_per_section = map_dfr(details_votes_sections, read_details_votes_sections) ) # combine all datasets
detailed_votes_per_section$ATTENDANCE_PERCENTAGE <- detailed_votes_per_section[,15]/detailed_votes_per_section[,14] # calculate voting turnout - the % of people voting in the section
detailed_votes_per_section = subset(detailed_votes_per_section, select = -c(DATA_GERACAO,HORA_GERACAO,DESCRIPT_ELECTION,
                                                                            CODE_ELECT_UNIT1, CODE_ELECT_UNIT2,NUM_ELECT_ZONE,
                                                                            CODE_POST)) # Subset dataset
# Profile voters characteristics by sections: ####
setwd("../profile_voters_section_year_state")
voters_profiles_sections = list.files(here("./profile_voters_section_year_state"),
                                    all.files = T,  
                                    pattern = ".csv",
                                    full.names = F,
                                    recursive = TRUE) 
read_voters_profiles_sections = function(path) {
  ( profile_voters_sections = read.csv (path, header = T, sep = ";")) # rename columns variables - refer to "LEIAME" pdf file
    profile_voters_sections <-subset(profile_voters_sections, select = -c(DT_GERACAO,HH_GERACAO,CD_MUNICIPIO,
                                                                           CD_MUN_SIT_BIOMETRICA, DS_MUN_SIT_BIOMETRICA,
                                                                           NR_ZONA,QT_ELEITORES_BIOMETRIA, QT_ELEITORES_DEFICIENCIA,
                                                                           QT_ELEITORES_INC_NM_SOCIAL)) # subset dataset
}
d<- read_voters_profiles_sections(voters_profiles_sections[1]) # example: sections characteristics of a given state, the state "[1]" of AC

# Sections locations dataset ####
# 2008 data is not available, the closest is from 2010 
setwd("../voters_location_voting_year")
location_sections <-read.csv("eleitorado_local_votacao_2010.csv",header=T, sep=";")
location_sections<-subset(location_sections, select = -c(DT_GERACAO,HH_GERACAO,CD_MUNICIPIO,NR_ZONA,
                                                         CD_TIPO_SECAO_AGREGADA,NR_TELEFONE_LOCAL))
location_sections <- filter(location_sections, DT_ELEICAO == "03/10/2010") # interested in the 1st round only as audited information would be used before the 1st round and not possible to have audited between rounds
location_sections<- location_sections %>% filter(!SG_UF %in% c("DF","ZZ")) # exclude DF which has no local elections and ZZ vote locations outside the country


# combine datasets ####
votes_per_candidate_name_per_section<- merge(votes_per_section, votes_per_candidate, by.x=c("ID_CANDIDATE", "NAME_MUNICIPALITY"), by.y=c("NUM_BALLOT_CANDIDATE", "NAME_MUNICIPALITY"), all.x=TRUE, all.y=FALSE)
votes_per_candidate_name_per_section = subset(votes_per_candidate_name_per_section, select= -c(YEAR_ELECTION.y, NUM_ROUND.y,
                                                                                               ACRONYM_STATE.y,Total.y)) # dataset containing candidate ID, name, party, year of the election, election round, state, municipality and votes per section (voting precincts)
votes_per_candidate_name_per_section<-merge(votes_per_candidate_name_per_section,location_sections,  by.x=c("NAME_MUNICIPALITY", "NUM_ELECT_SECTION"), by.y=c("NM_MUNICIPIO", "NR_SECAO"), all.x=TRUE, all.y=FALSE)
# problem with big cities where a section includes several neighbourhoods

# filter data according to municipalities audited ####
drawn_municipalities<-read_excel("Sorteios_UFs.xlsx") # upload dataset containning municipalities drawn in the lotteries 22-38 (2007-2014)
drawn_municipalities<-as.data.frame(unique(drawn_municipalities$NAME_MUNICIPALITY)) # collect unique municipalities as some were audited more than once
votes_per_candidate_name_per_section<- votes_per_candidate_name_per_section %>% 
  filter(NAME_MUNICIPALITY %in% drawn_municipalities$`unique(drawn_municipalities$NAME_MUNICIPALITY)`) # excluding all municipalities which were not audited - verify if drawn municipalities = unique(votes_per_candidate_name_per_section$NAME_MUNICIPALITY) are equal

# several sections has no lat and long information available (eg., -1,-1 lat and lon) or no information about the voting section is available
votes_per_candidate_name_per_section$info_available = ifelse(is.na(votes_per_candidate_name_per_section$NR_LATITUDE),"No","Yes") 

# poor result using tidygeocoder, all the coordinates of missing voting sections (without coordinates) were retrieved using "Geocode by Awesome Table" 
# extracted the below dataset
# write_xlsx(votes_per_candidate_name_per_section,"C:/Users/Ribeiro/OneDrive/Documents/OneDrive/IHEID/Dissertation/Data Sources/Voting data/voters_location_voting_year/votes_per_candidate_name_per_section.xlsx")
# completed the geo coordinates : 
votes_per_candidate_name_per_section<-read_excel("votes_per_candidate_name_per_section_geo_located.xlsx") 
# some sections contain more than one address, this is why the same candidate is displayed twice or more in the same section,
# remain only unique candidates per unique sections:
votes_per_candidate_name_per_section$duplicate_id<-paste(votes_per_candidate_name_per_section$NAME_MUNICIPALITY,
                                                            votes_per_candidate_name_per_section$NUM_ELECT_SECTION,
                                                            votes_per_candidate_name_per_section$ACRONYM_STATE.x,
                                                            votes_per_candidate_name_per_section$Total.x,
                                                            votes_per_candidate_name_per_section$NAME_CANDIDATE) # create an unique id to remove duplicates
votes_per_candidate_name_per_section<-votes_per_candidate_name_per_section %>% distinct(duplicate_id,.keep_all = T) # remove duplicates
votes_per_candidate_name_per_section = subset(votes_per_candidate_name_per_section, select= -c(NR_LATITUDE, NR_LONGITUDE, latitude, longitude, duplicate_id)) # remove unused columns

# map of municipalities BR - https://github.com/ipeaGIT/geobr ####
# utils::remove.packages('geobr')
# Read all municipalities in the country at a given year
mun <- read_municipality(code_muni="all", year=2010)
# plot municipalities together with voting sections
ggplot() + 
  geom_sf(data=mun, fill = NA) + scale_fill_gradientn(colours= brewer.pal(9, "RdYlGn"))+
  geom_point(data = votes_per_candidate_name_per_section, mapping = aes(x = lon, y = lat, colour = factor(ACRONYM_STATE.x)), size = 1) + 
  coord_sf()+
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank())+
  labs(col="States")+
  ggtitle("Brazil's voting precincts locations in 2010")

#ggplot() + geom_sf(data=mun, fill = NA) + ggspatial::annotation_scale() +
#  tema_mapa + geom_sf(size = 0.01)
# ggsave('mapa.pdf', width = 15, height = 15, dpi = 100)


# Elected candidates in 2004 who could participate in the subsequent election ####
setwd("../votes_candidates_munzona_year_state_2004")
votes_municipalities_sections_2004 = list.files(here("./votes_candidates_munzona_year_state_2004"),
                                        all.files = T,  
                                        pattern = ".txt",
                                        full.names = F,
                                        recursive = TRUE) 
read_votes_municipalities_sections_2004 = function(path) {
  ( votes_municipalities_sections_2004 = read.delim (path, header = FALSE, sep = ";",
                                                  col.names = c("DATA_GERACAO", "HORA_GERACAO" ,
                                                                "YEAR_ELECTION", "NUM_ROUND", "DESCRIPT_ELECTION",
                                                                "ACRONYM_STATE", "CODE_ELECT_UNIT1", "CODE_ELECT_UNIT2",
                                                                "NAME_MUNICIPALITY", "NUM_ELECT_ZONE",
                                                                "CODE_POST", "NUM_BALLOT_CANDIDATE", "TSE_INTERNAL_CANDIDATE_CODE",
                                                                "NAME_CANDIDATE", "NAME_BALLOT_CANDIDATE",
                                                                "DESCRIPT_POST", "COD_SIT_CAND_SUPERIOR", "DESC_SIT_CAND_SUPERIOR",
                                                                "STATUS_ELECT_CANDIDATE_CODE", "STATUS_ELECT_CANDIDATE",
                                                                "CODE_SIT_CAND_TOT", "RESULT", "PARTY_NUMBER", "PARTY_ACRONYM",
                                                                "PARTY_NAME", "SEQUENCIAL_LEGENDA", "NAME_COALITION", 
                                                                "COMPOSITION_COALITION", "NUM_VOTES_RECEIVED"))) # rename columns variables - refer to "LEIAME" pdf file
  votes_municipalities_sections_2004 <- filter(votes_municipalities_sections_2004, DESCRIPT_POST == "PREFEITO") #filter only mayors, who can be affected by Federal audit's results
  votes_municipalities_sections_2004 <-votes_municipalities_sections_2004 %>% 
    select(NUM_BALLOT_CANDIDATE,YEAR_ELECTION, NUM_ROUND, ACRONYM_STATE, NAME_CANDIDATE, CODE_SIT_CAND_TOT, RESULT, PARTY_NUMBER, PARTY_ACRONYM, NAME_MUNICIPALITY, NUM_VOTES_RECEIVED) %>%
    group_by(NUM_BALLOT_CANDIDATE, YEAR_ELECTION, NUM_ROUND, ACRONYM_STATE, NAME_CANDIDATE, CODE_SIT_CAND_TOT, RESULT, PARTY_NUMBER, PARTY_ACRONYM, NAME_MUNICIPALITY) %>%
    summarise(Total = sum(NUM_VOTES_RECEIVED))
} 
f<- read_votes_municipalities_sections_2004(votes_municipalities_sections_2004[1])  # choose a specific state dataset - [] ranges from 1 to 26
( elected_mayors_2004 = map_dfr(votes_municipalities_sections_2004, read_votes_municipalities_sections_2004) ) # combine all datasets
# keep only the elected mayors
elected_mayors_2004<-elected_mayors_2004[elected_mayors_2004$RESULT == "ELEITO",] 
# In case these elected mayors are candidates in the 2008 elections, they are running for their 2nd turn and will be the only mayors of interest

# merge the elected mayors in 2004 and the 2008 candidates
candidates_2008<-left_join(votes_per_candidate_name_per_section, elected_mayors_2004, by=c("NAME_CANDIDATE","NAME_MUNICIPALITY"))
candidates_2008<- merge(votes_per_candidate_name_per_section, elected_mayors_2004, by.x=c("NAME_CANDIDATE"), 
                        by.y=c("NAME_CANDIDATE"), all.x=TRUE, all.y=FALSE)




########################################################################################################
#######################################################################################################
###########################################################################################ignore below:
library(electionsBR)
citation('electionsBR')
df <- vote_mun_zone_local(2016)

  tema_mapa <-
    theme_bw() + # Escolhe o tema. Eu gosto do theme_bw() por ser bem simples/limpo
    theme(
      axis.text.y = element_text(
        angle = 90,
        hjust = 0.5,
        size = 8
      ),
      axis.text.x = element_text(size = 8),
      axis.title.y = element_text(size = rel(0.8)),
      axis.title.x = element_text(size = rel(0.8)),
      panel.grid.major = element_line(
        color = gray(0.9),
        linetype = "dashed",
        size = 0.1
      ),
      panel.background = element_rect(fill = "white") +
        annotation_scale(location = "br", width_hint = 0.30)
    ) # reference https://beatrizmilz.com/blog/2020-07-27-criando-mapas-com-os-pacotes-tidyverse-e-geobr/
  
  

# create list of addresses
library(tidygeocoder)
# options(tidygeocoder.progress_bar = FALSE)

# missing_sections %>%  geocode(street = street, city = NAME_MUNICIPALITY, state = ACRONYM_STATE.x, postalcode = NR_CEP, method = 'census', full_results = TRUE)
missing_sections %>% geocode(street, method = 'osm', limit = 2,
                             return_input = FALSE, full_results = F)

# # install packages
# install.packages("ggmap")
# install.packages("tmaptools")
# install.packages("RCurl")
# install.packages("jsonlite")
# install.packages("leaflet")
# load packages
library(ggmap)
library(tmaptools)
library(RCurl)
library(jsonlite)
library(leaflet)
pubs <- c("AVENIDA JOAQUIM CAETANO DA SILVA")
pubs_m <- pubs
pubs_m[pubs_m=="AVENIDA JOAQUIM CAETANO DA SILVA"] <- "AVENIDA JOAQUIM CAETANO DA SILVA"
pubs_m_df <- data.frame(Pubs = pubs_m, stringsAsFactors = FALSE)

# geocoding the London pubs
# "bar" is special phrase added to limit the search
pubs_tmaptools <- geocode_OSM(paste(pubs_m, "bar", sep = " "),
                              details = TRUE, as.data.frame = TRUE)

# extracting from the result only coordinates and address
pubs_tmaptools <- pubs_tmaptools[, c("lat", "lon", "display_name")]
pubs_tmaptools <- cbind(Pubs = pubs_m_df[-10, ], pubs_tmaptools)

# print the results
PUBS_LIST<-pubs_tmaptools


install.packages('tidygeocoder')
devtools::install_github("jessecambon/tidygeocoder")
library(dplyr, warn.conflicts = FALSE)
library(tidygeocoder)
some_addresses <- tibble::tribble(
  ~name,                  ~addr,
  "OIAPOQUE",          "AVENIDA JOAQUIM CAETANO DA SILVA, OIAPOQUE, AP",
  "ABADIA DOS DOURADOS", "AV. SANTOS  , 281, ABADIA DOS DOURADOS, MG "
)
lat_longs <- some_addresses %>%
  geocode(addr, method = 'osm', lat = latitude , long = longitude)

# options(tidygeocoder.progress_bar = FALSE)
library(dplyr, warn.conflicts = FALSE)
geo(street = "AV. SANTOS  , 281", city = "ABADIA DOS DOURADOS",
    state = "MG", country= "Brazil", method = "osm")

sample_addresses %>% slice(1:2) %>%
  geocode(addr, method = 'arcgis')
sample_addresses %>% slice(8:9) %>%
  geocode(addr, method = 'osm', limit = 2,
          return_input = FALSE, full_results = F)
