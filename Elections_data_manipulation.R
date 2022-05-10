# PIEMONTE RIBEIRO, Marcelo
# Data replication - coding


# ---------------------------------------------------------------------------------------------------------------------------#
################################################### DATA MANIPULATION ########################################################
# ---------------------------------------------------------------------------------------------------------------------------#

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
# install.packages("tidyverse")
library(dplyr)
library(tidyr)
# install.packages("here")
library(here) 
library(stringr) 
library(purrr) 
library("readxl")
library("writexl")
library(electionsBR)
library(haven)
library(lubridate)
library("reshape2")

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
# a<- read_votes_section(votes_sections_files[1]) # choose a specific n° to check state datasets - [] ranges from 1 to 26
# b<- read_votes_section(votes_sections_files[27]) # choose a specific n° to check state datasets - [] ranges from 1 to 26
( votes_per_section = map_dfr(votes_sections_files, read_votes_section) ) # combine all datasets
votes_per_section_2008<-filter(votes_per_section, YEAR_ELECTION != 2012)
votes_per_section_2012<-filter(votes_per_section, YEAR_ELECTION != 2008)
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
# b<- read_votes_municip_candidat_names(votes_municipalities_names[1])  # choose a specific state dataset - [] ranges from 1 to 26
( votes_per_candidate = map_dfr(votes_municipalities_names, read_votes_municip_candidat_names) ) # combine all datasets
votes_per_candidate_2008<-filter(votes_per_candidate, YEAR_ELECTION != 2012)
votes_per_candidate_2012<-filter(votes_per_candidate, YEAR_ELECTION != 2008)

# # Electors characteristics per section: ####
# setwd("../details_votes_section_year_state")
# details_votes_sections = list.files(here("./details_votes_section_year_state"),
#                                         all.files = T,  
#                                         pattern = ".txt",
#                                         full.names = F,
#                                         recursive = TRUE) 
# read_details_votes_sections = function(path) {
#   ( detailed_votes = read.delim (path, header = FALSE, sep = ";",
#                                                   col.names = c("DATA_GERACAO", "HORA_GERACAO",
#                                         "YEAR_ELECTION", "NUM_ROUND", "DESCRIPT_ELECTION",
#                                         "ACRONYM_STATE", "CODE_ELECT_UNIT1", "CODE_ELECT_UNIT2",
#                                         "NAME_MUNICIPALITY", "NUM_ELECT_ZONE", "NUM_ELECT_SECTION",
#                                         "CODE_POST", "DESCRIPT_POST", "NUM_PEOP_ABLE_VOTE",
#                                         "ATTENDANCE", "ABSENTS", "NOMINAL_VOTES", "BLANKS_VOTES",
#                                         "VOIDS_VOTES", "VOTES_COALITION", "CANCELLED_VOTES"))) # rename columns variables - refer to "LEIAME" pdf file
#   detailed_votes <- filter(detailed_votes, DESCRIPT_POST == "PREFEITO")
# }
# # c<- read_details_votes_sections(details_votes_sections[1])  # choose a specific state dataset - [] ranges from 1 to 26
# ( detailed_votes_per_section = map_dfr(details_votes_sections, read_details_votes_sections) ) # combine all datasets
# detailed_votes_per_section$ATTENDANCE_PERCENTAGE <- detailed_votes_per_section[,15]/detailed_votes_per_section[,14] # calculate voting turnout - the % of people voting in the section
# detailed_votes_per_section = subset(detailed_votes_per_section, select = -c(DATA_GERACAO,HORA_GERACAO,DESCRIPT_ELECTION,
#                                                                             CODE_ELECT_UNIT1, CODE_ELECT_UNIT2,NUM_ELECT_ZONE,
#                                                                             CODE_POST)) # Subset dataset

# Sections locations dataset ####
# 2008 data is not available, the closest is from 2010 
setwd("../voters_location_voting_year")
location_sections_10 <-read.csv("eleitorado_local_votacao_2010.csv",header=T, sep=";")
location_sections_10<-subset(location_sections_10, select = -c(DT_GERACAO,HH_GERACAO,CD_MUNICIPIO,NR_ZONA,
                                                         CD_TIPO_SECAO_AGREGADA,NR_TELEFONE_LOCAL))
location_sections_10 <- filter(location_sections_10, DT_ELEICAO == "03/10/2010") # interested in the 1st round only as audited information would be used before the 1st round and not possible to have audited between rounds
location_sections_10<- location_sections_10 %>% filter(!SG_UF %in% c("DF","ZZ")) # exclude DF which has no local elections and ZZ vote locations outside the country
location_sections_12 <-read.csv("eleitorado_local_votacao_2012.csv",header=T, sep=";")
location_sections_12<-subset(location_sections_12, select = -c(DT_GERACAO,HH_GERACAO,CD_MUNICIPIO,NR_ZONA,
                                                               CD_TIPO_SECAO_AGREGADA,NR_TELEFONE_LOCAL))
location_sections_12 <- filter(location_sections_12, DT_ELEICAO == "07/10/2012") # interested in the 1st round only as audited information would be used before the 1st round and not possible to have audited between rounds
location_sections_12<- location_sections_12 %>% filter(!SG_UF %in% c("DF","ZZ")) # exclude DF which has no local elections and ZZ vote locations outside the country

# combine datasets ####
votes_per_candidate_name_per_section_08<- merge(votes_per_section_2008, votes_per_candidate_2008, by.x=c("ID_CANDIDATE", "NAME_MUNICIPALITY", "ACRONYM_STATE"), by.y=c("NUM_BALLOT_CANDIDATE", "NAME_MUNICIPALITY", "ACRONYM_STATE"), all.x=TRUE, all.y=FALSE)
votes_per_candidate_name_per_section_08 = subset(votes_per_candidate_name_per_section_08, select= -c(YEAR_ELECTION.y, NUM_ROUND.y,Total.y)) # dataset containing candidate ID, name, party, year of the election, election round, state, municipality and votes per section (voting precincts)
votes_per_candidate_name_per_section_08<-merge(votes_per_candidate_name_per_section_08,location_sections_10,  by.x=c("NAME_MUNICIPALITY", "NUM_ELECT_SECTION", "ACRONYM_STATE"), by.y=c("NM_MUNICIPIO", "NR_SECAO","SG_UF"), all.x=TRUE, all.y=FALSE)
# problem with big cities where a section includes several neighbourhoods
votes_per_candidate_name_per_section_12<- merge(votes_per_section_2012, votes_per_candidate_2012, by.x=c("ID_CANDIDATE", "NAME_MUNICIPALITY", "ACRONYM_STATE"), by.y=c("NUM_BALLOT_CANDIDATE", "NAME_MUNICIPALITY", "ACRONYM_STATE"), all.x=TRUE, all.y=FALSE)
votes_per_candidate_name_per_section_12 = subset(votes_per_candidate_name_per_section_12, select= -c(YEAR_ELECTION.y, NUM_ROUND.y,Total.y)) # dataset containing candidate ID, name, party, year of the election, election round, state, municipality and votes per section (voting precincts)
votes_per_candidate_name_per_section_12<-merge(votes_per_candidate_name_per_section_12,location_sections_12,  by.x=c("NAME_MUNICIPALITY", "NUM_ELECT_SECTION", "ACRONYM_STATE"), by.y=c("NM_MUNICIPIO", "NR_SECAO","SG_UF"), all.x=TRUE, all.y=FALSE)
# problem with big cities where a section includes several neighbourhoods

# filter data according to municipalities audited ####
setwd("C:/Users/Ribeiro/OneDrive/Documents/OneDrive/IHEID/Dissertation/Data Sources/CGU/Avis Ferraz 2018/Stata")
drew_municipalities<-read_dta("drew_municipalities_corruption_data.dta") # upload dataset containning municipalities drawn in the lotteries 22-38 (2007-2014), lotteries 22-26 happened before  October 2008, while lotteries 27-36 happened before October 2012
drew_municipalities_08<-drew_municipalities[drew_municipalities$sorteio<27,]
drew_municipalities_08<-as.data.frame(unique(drew_municipalities_08$NAME_MUNICIPALITY)) # collect unique municipalities as some were audited more than once
votes_per_candidate_name_per_section_08<- votes_per_candidate_name_per_section_08 %>% 
  filter(NAME_MUNICIPALITY %in% drew_municipalities_08$`unique(drew_municipalities_08$NAME_MUNICIPALITY)`) # excluding all municipalities which were not audited - verify if drawn municipalities = unique(votes_per_candidate_name_per_section$NAME_MUNICIPALITY) are equal
drew_municipalities_12<-drew_municipalities[drew_municipalities$sorteio>26 & drew_municipalities$sorteio<37,]
drew_municipalities_12<-as.data.frame(unique(drew_municipalities_12$NAME_MUNICIPALITY)) # collect unique municipalities as some were audited more than once
votes_per_candidate_name_per_section_12<- votes_per_candidate_name_per_section_12 %>% 
  filter(NAME_MUNICIPALITY %in% drew_municipalities_12$`unique(drew_municipalities_12$NAME_MUNICIPALITY)`) # excluding all municipalities which were not audited - verify if drawn municipalities = unique(votes_per_candidate_name_per_section$NAME_MUNICIPALITY) are equal
# several sections has no lat and long information available (eg., -1,-1 lat and lon) or no information about the voting section is available
votes_per_candidate_name_per_section_08$info_available = ifelse(is.na(votes_per_candidate_name_per_section_08$NR_LATITUDE),"No","Yes") 
votes_per_candidate_name_per_section_12$info_available = ifelse(is.na(votes_per_candidate_name_per_section_12$NR_LATITUDE),"No","Yes") 
# poor results using tidygeocoder, all the coordinates of missing voting sections (without coordinates) were retrieved using "Geocode by Awesome Table" 
# extracted the below dataset
# write_xlsx(votes_per_candidate_name_per_section,"C:/Users/Ribeiro/OneDrive/Documents/OneDrive/IHEID/Dissertation/Data Sources/Voting data/voters_location_voting_year/votes_per_candidate_name_per_section.xlsx")
# completed the geo coordinates : 

setwd("C:/Users/Ribeiro/OneDrive/Documents/OneDrive/IHEID/Dissertation/Data Sources/Voting data/voters_location_voting_year")
votes_per_candidate_name_per_section_geo_08<-read_excel("votes_per_candidate_name_per_section_geo_reduced_08.xlsx") 
votes_per_candidate_name_per_section_geo_12<-read_excel("votes_per_candidate_name_per_section_geo_reduced_12.xlsx") 
# remove data regarding the elections 2nd turn as "NUM_ROUND.x"=1 indicates already if the candidate was or not elected
votes_per_candidate_name_per_section_geo_08 <- filter(votes_per_candidate_name_per_section_geo_08, NUM_ROUND.x != 2)
votes_per_candidate_name_per_section_geo_08 <- filter(votes_per_candidate_name_per_section_geo_08, RESULT != "2º TURNO")
votes_per_candidate_name_per_section_geo_12 <- filter(votes_per_candidate_name_per_section_geo_12, NUM_ROUND.x != 2)
votes_per_candidate_name_per_section_geo_12 <- filter(votes_per_candidate_name_per_section_geo_12, RESULT != "2º TURNO")
# remain only unique candidates per unique sections:
votes_per_candidate_name_per_section_geo_08$duplicate_id<-paste(votes_per_candidate_name_per_section_geo_08$NAME_MUNICIPALITY,
                                                            votes_per_candidate_name_per_section_geo_08$NUM_ELECT_SECTION,
                                                            votes_per_candidate_name_per_section_geo_08$ACRONYM_STATE,
                                                            votes_per_candidate_name_per_section_geo_08$Total.x,
                                                            votes_per_candidate_name_per_section_geo_08$NAME_CANDIDATE,
                                                            votes_per_candidate_name_per_section_geo_08$RESULT) # create an unique id to remove duplicates
votes_per_candidate_name_per_section_geo_08<-votes_per_candidate_name_per_section_geo_08 %>% distinct(duplicate_id,.keep_all = T) # remove duplicates
votes_per_candidate_name_per_section_geo_08 = subset(votes_per_candidate_name_per_section_geo_08, select= -c(AA_ELEICAO, DT_ELEICAO, DS_ELEICAO, DS_TIPO_SECAO_AGREGADA,
                                                                                                       NR_LATITUDE, NR_LONGITUDE, id, duplicate_id,QT_ELEITOR)) # remove unused columns
votes_per_candidate_name_per_section_geo_12$duplicate_id<-paste(votes_per_candidate_name_per_section_geo_12$NAME_MUNICIPALITY,
                                                                votes_per_candidate_name_per_section_geo_12$NUM_ELECT_SECTION,
                                                                votes_per_candidate_name_per_section_geo_12$ACRONYM_STATE,
                                                                votes_per_candidate_name_per_section_geo_12$Total.x,
                                                                votes_per_candidate_name_per_section_geo_12$NAME_CANDIDATE,
                                                                votes_per_candidate_name_per_section_geo_12$RESULT) # create an unique id to remove duplicates
votes_per_candidate_name_per_section_geo_12<-votes_per_candidate_name_per_section_geo_12 %>% distinct(duplicate_id,.keep_all = T) # remove duplicates
votes_per_candidate_name_per_section_geo_12 = subset(votes_per_candidate_name_per_section_geo_12, select= -c(AA_ELEICAO, DT_ELEICAO, DS_ELEICAO, DS_TIPO_SECAO_AGREGADA,
                                                                                                             NR_LATITUDE, NR_LONGITUDE, id, duplicate_id,QT_ELEITOR)) # remove unused columns

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
# f<- read_votes_municipalities_sections_2004(votes_municipalities_sections_2004[1])  # choose a specific state dataset - [] ranges from 1 to 26
( elected_mayors_2004 = map_dfr(votes_municipalities_sections_2004, read_votes_municipalities_sections_2004) ) # combine all datasets
elected_mayors_2004<-elected_mayors_2004[elected_mayors_2004$RESULT == "ELEITO",] # 5559 rows - in 2004, 5560 municipalities including the capital state DF ()
# In case these elected mayors are candidates in the 2008 elections, they are running for their 2nd turn and will be the only mayors of interest
elected_mayors_2008<-votes_per_candidate_2008[votes_per_candidate_2008$RESULT== "ELEITO",] #keep only elected mayors in 2008 who could run again in 2012

# some mayors have the same name, create variable with their name and municipality name
elected_mayors_2004$unique_name_candidate<-paste(elected_mayors_2004$NAME_MUNICIPALITY,elected_mayors_2004$NAME_CANDIDATE)
votes_per_candidate_name_per_section_geo_08$unique_name_candidate<-paste(votes_per_candidate_name_per_section_geo_08$NAME_MUNICIPALITY,votes_per_candidate_name_per_section_geo_08$NAME_CANDIDATE)
# same for 2008 elections
elected_mayors_2008$unique_name_candidate<-paste(elected_mayors_2008$NAME_MUNICIPALITY,elected_mayors_2008$NAME_CANDIDATE)
votes_per_candidate_name_per_section_geo_12$unique_name_candidate<-paste(votes_per_candidate_name_per_section_geo_12$NAME_MUNICIPALITY,votes_per_candidate_name_per_section_geo_12$NAME_CANDIDATE)

# merge the elected mayors in 2004 and the 2008 candidates
candidates_2008<-merge(x=votes_per_candidate_name_per_section_geo_08,y=elected_mayors_2004,by="unique_name_candidate", all.x = TRUE)
# identify candidates that were running for re-elections
candidates_2008$reelection_candidature = ifelse(is.na(candidates_2008$NUM_BALLOT_CANDIDATE),"No","Yes") 
# limitation: no available data in 2004 at section disaggregation level.
# create candidate's voting shares and dummy indicating victory or not by voting section
candidates_2008<-candidates_2008 %>% 
  group_by(ACRONYM_STATE.x, NAME_MUNICIPALITY.x, NUM_ELECT_SECTION) %>% 
  mutate(
    Share = Total.x / sum(Total.x),
    Elect_2008 = +(Total.x == max(Total.x))
  ) %>% 
  ungroup()
# filter out the non used columns
candidates_2008 = subset(candidates_2008, select= -c(NUM_BALLOT_CANDIDATE, Total,YEAR_ELECTION, NUM_ROUND,
                                                     ACRONYM_STATE.y, NAME_CANDIDATE.y, CODE_SIT_CAND_TOT.y,
                                                     RESULT.y, PARTY_NUMBER.y,PARTY_ACRONYM.y,NAME_MUNICIPALITY.y)) # remove unused columns
# keep only mayors that were trying to be re-elected as they were the only ones that could be audited, so punished by electors covered (treated) or not (control) by radio coverage
candidates_2008<-candidates_2008[candidates_2008$reelection_candidature == "Yes",] 
# merge the elected mayors in 2008 and the 2012 candidates
candidates_2012<-merge(x=votes_per_candidate_name_per_section_geo_12,y=elected_mayors_2008,by="unique_name_candidate", all.x = TRUE)
# identify candidates that were running for re-elections
candidates_2012$reelection_candidature = ifelse(is.na(candidates_2012$NUM_BALLOT_CANDIDATE),"No","Yes") 
# create candidate's voting shares and dummy indicating victory or not by voting section
candidates_2012<-candidates_2012 %>% 
  group_by(ACRONYM_STATE.x, NAME_MUNICIPALITY.x, NUM_ELECT_SECTION) %>% 
  mutate(
    Share = Total.x / sum(Total.x),
    Elect_2012 = +(Total.x == max(Total.x))
  ) %>% 
  ungroup()
# filter out the non used columns
candidates_2012 = subset(candidates_2012, select= -c(NUM_BALLOT_CANDIDATE, Total,YEAR_ELECTION, NUM_ROUND,
                                                     ACRONYM_STATE.y, NAME_CANDIDATE.y, CODE_SIT_CAND_TOT.y,
                                                     RESULT.y, PARTY_NUMBER.y,PARTY_ACRONYM.y,NAME_MUNICIPALITY.y)) # remove unused columns
# keep only mayors that were trying to be re-elected as they were the only ones that could be audited, so punished by electors covered (treated) or not (control) by radio coverage
candidates_2012<-candidates_2012[candidates_2012$reelection_candidature == "Yes",] 


# Profile voters characteristics by sections: to be used as covariates ####
# voters_profile_by_municipality <-voter_profile(2008) - voter's profiles by municipality
# Profile voters characteristics by sections: to be used as covariates ####
rm(votes_per_candidate, votes_per_section, location_sections_10, location_sections_12, votes_per_candidate_name_per_section_08, votes_per_candidate_name_per_section_12) # liberate memory
setwd("../profile_voters_section_year_state_08")
voters_profiles_sections = list.files(here("./profile_voters_section_year_state_08"),
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
# d<- read_voters_profiles_sections(voters_profiles_sections[1]) # example: sections characteristics of a given state, the state "[1]" of AC
( profile_voters_sections_08 = map_dfr(voters_profiles_sections, read_voters_profiles_sections) )
# exclude the non related municipalities:
profile_voters_sections_08$unique_name_municipality<-paste(profile_voters_sections_08$SG_UF,profile_voters_sections_08$NM_MUNICIPIO) # create unique id for municipalities and states as municipalities with the same name are present in different states
candidates_2008$unique_name_municipality<-paste(candidates_2008$ACRONYM_STATE.x,candidates_2008$NAME_MUNICIPALITY.x) # create group of treatment and control municipalities
treat_control_sample_2008<-as.data.frame(unique(candidates_2008$unique_name_municipality)) # export the previous group list to be used as filter in the profile_voters_sections
profile_voters_sections_08<-subset(profile_voters_sections_08, unique_name_municipality %in% treat_control_sample_2008$`unique(candidates_2008$unique_name_municipality)`) # restricting profile_voters_sections to the 253 treat_control_sample - treat_control_sample_2008 == unique(profile_voters_sections_2008$unique_name_municipality), so 253 municipalities

# profile voters 2012
setwd("../profile_voters_section_year_state_12")
voters_profiles_sections_12 = list.files(here("./profile_voters_section_year_state_12"),
                                      all.files = T,  
                                      pattern = ".csv",
                                      full.names = F,
                                      recursive = TRUE) 
read_voters_profiles_sections_12 = function(path) {
  ( profile_voters_sections_12 = read.csv (path, header = T, sep = ";")) # rename columns variables - refer to "LEIAME" pdf file
  profile_voters_sections_12 <-subset(profile_voters_sections_12, select = -c(DT_GERACAO,HH_GERACAO,CD_MUNICIPIO,
                                                                        CD_MUN_SIT_BIOMETRICA, DS_MUN_SIT_BIOMETRICA,
                                                                        NR_ZONA,QT_ELEITORES_BIOMETRIA, QT_ELEITORES_DEFICIENCIA,
                                                                        QT_ELEITORES_INC_NM_SOCIAL)) # subset dataset
}
# d<- read_voters_profiles_sections(voters_profiles_sections[1]) # example: sections characteristics of a given state, the state "[1]" of AC
( profile_voters_sections_12 = map_dfr(voters_profiles_sections_12, read_voters_profiles_sections_12) )
# 2012
profile_voters_sections_12$unique_name_municipality<-paste(profile_voters_sections_12$SG_UF,profile_voters_sections_12$NM_MUNICIPIO) # create unique id for municipalities and states as municipalities with the same name are present in different states
candidates_2012$unique_name_municipality<-paste(candidates_2012$ACRONYM_STATE.x,candidates_2012$NAME_MUNICIPALITY.x) # create group of treatment and control municipalities
treat_control_sample_2012<-as.data.frame(unique(candidates_2012$unique_name_municipality)) # export the previous group list to be used as filter in the profile_voters_sections

profile_voters_sections_12<-subset(profile_voters_sections_12, unique_name_municipality %in% treat_control_sample_2012$`unique(candidates_2012$unique_name_municipality)`) # restricting profile_voters_sections to the 253 treat_control_sample - treat_control_sample_2012 == unique(profile_voters_sections_2012$unique_name_municipality), so 253 municipalities

# re-organize the dataframe by rows
# install.packages("tidyverse")
install.packages("dplyr")
library(dplyr)
# 2008
profile_voters_sections_08<-profile_voters_sections_08 %>%
  group_by(NM_MUNICIPIO, SG_UF, NR_SECAO) %>%
  summarize(tot_voters = sum(QT_ELEITORES_PERFIL),
            share_male = sum(QT_ELEITORES_PERFIL[DS_GENERO == "MASCULINO"])/tot_voters,
            share_semi_illit = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE == "LÊ E ESCREVE"] / tot_voters),
            share_illit = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE == "ANALFABETO"] / tot_voters),
            share_incomp_elementary = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE == "ENSINO FUNDAMENTAL INCOMPLETO"] / tot_voters),
            share_elementary = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE == "ENSINO FUNDAMENTAL COMPLETO"] / tot_voters),
            share_incomp_hschool = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE ==  "ENSINO MÉDIO INCOMPLETO"] / tot_voters),
            share_hschool = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE ==  "ENSINO MÉDIO COMPLETO"] / tot_voters),
            share_incomp_college = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE ==  "SUPERIOR INCOMPLETO"] / tot_voters),
            share_college = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE ==  "ENSINO MÉDIO COMPLETO"] / tot_voters),
            share_20minus = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA == c(2000,1900,1800,1700,1600)] / tot_voters),
            share_2030 = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA == c(2124,2529)] / tot_voters),
            share_3045 = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA == c(3034,3539,4044)] / tot_voters),
            share_4560 = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA == c(4549,5054,5559)] / tot_voters),
            share_60plus = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA == c(6064,6569,7074,7579,
                                                                        8084,8589,9094,9599,9999)] / tot_voters))
# 2012
profile_voters_sections_12<-profile_voters_sections_12 %>%
  group_by(NM_MUNICIPIO, SG_UF, NR_SECAO) %>%
  summarize(tot_voters = sum(QT_ELEITORES_PERFIL),
            share_male = sum(QT_ELEITORES_PERFIL[DS_GENERO == "MASCULINO"])/tot_voters,
            share_semi_illit = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE == "LÊ E ESCREVE"] / tot_voters),
            share_illit = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE == "ANALFABETO"] / tot_voters),
            share_incomp_elementary = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE == "ENSINO FUNDAMENTAL INCOMPLETO"] / tot_voters),
            share_elementary = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE == "ENSINO FUNDAMENTAL COMPLETO"] / tot_voters),
            share_incomp_hschool = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE ==  "ENSINO MÉDIO INCOMPLETO"] / tot_voters),
            share_hschool = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE ==  "ENSINO MÉDIO COMPLETO"] / tot_voters),
            share_incomp_college = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE ==  "SUPERIOR INCOMPLETO"] / tot_voters),
            share_college = sum(QT_ELEITORES_PERFIL[DS_GRAU_ESCOLARIDADE ==  "ENSINO MÉDIO COMPLETO"] / tot_voters),
            share_20minus = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA == c(2000,1900,1800,1700,1600)] / tot_voters),
            share_2030 = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA == c(2124,2529)] / tot_voters),
            share_3045 = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA == c(3034,3539,4044)] / tot_voters),
            share_4560 = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA == c(4549,5054,5559)] / tot_voters),
            share_60plus = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA == c(6064,6569,7074,7579,
                                                                        8084,8589,9094,9599,9999)] / tot_voters))
# merge with candidates 2008 data
profile_voters_sections_08$unique_name_municipality<-paste(profile_voters_sections_08$SG_UF,profile_voters_sections_08$NM_MUNICIPIO) # create unique id for municipalities and states as municipalities with the same name are present in different states
profile_voters_sections_08<-merge(x=candidates_2008,y=profile_voters_sections_08, by.x=c("unique_name_municipality", "NUM_ELECT_SECTION"), by.y=c("unique_name_municipality", "NR_SECAO")) #merge with candidates 2008
profile_voters_sections_08 <-subset(profile_voters_sections_08, select = -c(tot_voters,SG_UF,NM_MUNICIPIO, QT_ELEITOR_ELEICAO,unique_name_candidate,unique_name_municipality)) # subset dataset
profile_voters_sections_12$unique_name_municipality<-paste(profile_voters_sections_12$SG_UF,profile_voters_sections_12$NM_MUNICIPIO) # create unique id for municipalities and states as municipalities with the same name are present in different states
profile_voters_sections_12<-merge(x=candidates_2012,y=profile_voters_sections_12, by.x=c("unique_name_municipality", "NUM_ELECT_SECTION"), by.y=c("unique_name_municipality", "NR_SECAO")) #merge with candidates 2012
profile_voters_sections_12 <-subset(profile_voters_sections_12, select = -c(tot_voters,SG_UF,NM_MUNICIPIO, QT_ELEITOR_ELEICAO,unique_name_candidate,unique_name_municipality)) # subset dataset
# bind profile voters sections
profile_voters_sections_08<-profile_voters_sections_08 %>% rename(Elected = Elect_2008) # making the same name of columns to rbind
profile_voters_sections_12<-profile_voters_sections_12 %>% rename(Elected = Elect_2012) # making the same name of columns to rbind
profile_voters_sections<-rbind(profile_voters_sections_08,profile_voters_sections_12) %>% rename(NAME_MUNICIPALITY = NAME_MUNICIPALITY.x, UF = ACRONYM_STATE.x)

# Merge with corruption data from Avis, E., Ferraz, C., & Finan, F. (2018) dataset ####
votes_per_precinct = merge(x=profile_voters_sections,y=drew_municipalities,by=c("NAME_MUNICIPALITY","UF"),all.x=TRUE) # the dataset contains the share of votes of the reelection candidatures per precinct as well as the corruption levels
votes_per_precinct<-na.omit(votes_per_precinct) # Avis, E., Ferraz, C., & Finan, F. (2018) does not contain corruption in their dataset - remove
# remove unused columns
votes_per_precinct = subset(votes_per_precinct, select = -c(NUM_ROUND.x, CD_TIPO_LOCAL, DS_TIPO_LOCAL, CD_SITU_LOCAL_VOTACAO,DS_SITU_LOCAL_VOTACAO,
                                CD_SITU_ZONA, DS_SITU_ZONA, CD_SITU_SECAO, DS_SITU_SECAO, CD_SITU_LOCALIDADE,
                                DS_SITU_LOCALIDADE, T_SUPER25M, n_nbr,n_nbraudited, party_id, puf_treatment) )
# export
write_xlsx(votes_per_precinct,"C:/Users/Ribeiro/OneDrive/Documents/OneDrive/IHEID/Dissertation/Data Sources/Voting data/votes_per_precinct.xlsx")


# ---------------------------------------------------------------------------------------------------------------------------#
################################################### ANALYSIS #################################################################
# ---------------------------------------------------------------------------------------------------------------------------#

# Summary statistics ####
sum(table(drew_municipalities$UF)) # between 2006-2013, 1020 municipalities were audited
unique(drew_municipalities$NAME_MUNICIPALITY) # with 962 unique municipalities (then 328 municipalities were audited twice)
first_term_drew_mun<-as.data.frame(unique(votes_per_precinct$NAME_MUNICIPALITY)) # 293 had mayors who ran for re-election.

# Figure 1 ####
# add dates of the lotteries according to https://www.gov.br/cgu/pt-br/assuntos/auditoria-e-fiscalizacao/programa-de-fiscalizacao-em-entes-federativos/edicoes-anteriores/municipios?b_start:int=0
drew_municipalities<-drew_municipalities %>% mutate(lottery_date =
                     case_when(sorteio == 22 ~ as.Date("2006/07/19"), 
                               sorteio == 23 ~ as.Date("2007/05/09"),
                               sorteio == 24 ~ as.Date("2007/07/24"),
                               sorteio == 25 ~ as.Date("2007/10/09"),
                               sorteio == 26 ~ as.Date("2008/04/30"),
                               sorteio == 27 ~ as.Date("2008/10/29"),
                               sorteio == 28 ~ as.Date("2009/05/12"),
                               sorteio == 29 ~ as.Date("2009/08/17"),
                               sorteio == 30 ~ as.Date("2009/10/05"),
                               sorteio == 31 ~ as.Date("2010/03/01"),
                               sorteio == 32 ~ as.Date("2010/05/10"),
                               sorteio == 33 ~ as.Date("2010/07/26"),
                               sorteio == 34 ~ as.Date("2011/08/15"),
                               sorteio == 35 ~ as.Date("2011/10/03"),
                               sorteio == 36 ~ as.Date("2012/07/23"),
                               sorteio == 37 ~ as.Date("2012/10/08"),
                               sorteio == 38 ~ as.Date("2013/03/04")))  %>%
                               mutate(year = year(lottery_date))                                      
plot(factor(drew_municipalities$year),ylim = c(0, 200), yaxp=c(0, 180,6), 
     xlab = "Year", ylab = "N°municipalities audited", border = "black", col="lightgrey") # number of audited municipalities per year
abline(h=c(60,120,180),  lty = 3)

# Figure 2 ####
drew_municipalities_first_terms<- drew_municipalities %>% 
  filter(NAME_MUNICIPALITY %in% first_term_drew_mun$`unique(votes_per_precinct$NAME_MUNICIPALITY)`) # keeping municipalities audited twice 
drew_municipalities_first_terms<- drew_municipalities_first_terms %>%
  distinct(NAME_MUNICIPALITY, drew_municipalities_first_terms$year != 2013, .keep_all = T) %>% select(1:26) # but only those in 2013
# The 2008 elections happened on the 5th October (1st round) and 26th October (2nd round)
# The 2012 elections happened on the 7th October (1st round) and 28th October (2nd round)

drew_municipalities_first_terms %>%
#  mutate(treat_condition = if_else(radio_am==1, 
#                                   "Radio municipality (treatment)", 
#                                   "No radio (control)")) %>% 
  ggplot(aes(lottery_date, exp(lfalha_total))) + #color = treat_condition
  stat_summary(fun = "mean", geom = "line") +
  labs(x = "Month/Year", y = "Acts of corruption, (mean)") +
  geom_vline(xintercept = as.Date(c("2008-10-05","2012-10-05")),linetype="dashed")+ 
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%y")+
  geom_smooth(span = 0.5)+
  annotate(geom = "text", x = as.Date("2008-11-30"), y = 35, label = "2008 elections", hjust = "left")+
  annotate(geom = "text", x = as.Date("2012-08-30"), y = 85, label = "2012 elections", hjust = "right")+
  theme_minimal()
#ggplot(drew_municipalities_first_terms, aes(x = as.factor(lottery_date), y = lfalha_total)) + geom_boxplot()+ theme_minimal()  

# Figure 3 ####
drew_municipalities_first_terms %>%
   mutate(treat_condition = if_else(radio_am==1,
                                    "Radio in the municipality",
                                    "No local radio present")) %>%
  ggplot(aes(lottery_date, exp(lfalha_total),color = treat_condition)) + 
  stat_summary(fun = "mean") +
  labs(x = "Month/Year", y = "Acts of corruption, (mean)") +
  geom_vline(xintercept = as.Date(c("2008-10-05","2012-10-05")),linetype="dashed")+ 
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%y")+
  scale_color_discrete(name = "Treatment condition")+
  geom_smooth(span = 0.6, aes(fill = treat_condition),se=FALSE) +
  guides(fill="none")+
  annotate(geom = "text", x = as.Date("2008-11-30"), y = 35, label = "2008 elections", hjust = "left")+
  annotate(geom = "text", x = as.Date("2012-08-30"), y = 85, label = "2012 elections", hjust = "right")+
  theme_bw()+
  theme(legend.position = c(0.14, 0.88))

# Figure 4 ####
share_votes_by_corruption_levels<-subset(votes_per_precinct,sorteio<32)
library(dplyr)
share_votes_by_corruption_levels<-share_votes_by_corruption_levels %>%
  group_by(NAME_MUNICIPALITY, UF) %>%
  summarize(relection_rate = ((sum(Elected)/nrow(share_votes_by_corruption_levels))*nrow(share_votes_by_corruption_levels))/100, # counts the n°of sections won by the candidate divided by all the sections and it multiples the latter calculation by the number of section (this penalizes municipalities with few sections)
            mean_lfalha_total= mean(lfalha_total),
            mean_lmismanagement= mean(lmismanagement),
            mean_illiterate= mean(share_illit),
            mean_semi_illit= mean(share_semi_illit),
            draw=mean(sorteio),
            share_vote_mean = mean(Share)) 
share_votes_by_corruption_levels$relection_rate<-
  (share_votes_by_corruption_levels$relection_rate-min(share_votes_by_corruption_levels$relection_rate))/ # normalize 0-1 re-election rates
   (max(share_votes_by_corruption_levels$relection_rate)-min(share_votes_by_corruption_levels$relection_rate)) 
share_votes_by_corruption_levels$treat<-ifelse(share_votes_by_corruption_levels$draw<27,"pre-elections","post-elections")
# Plot:
plot_share_votes_by_corruption_levels = share_votes_by_corruption_levels %>% 
  mutate(ds = as.factor(treat)) %>% # Temporary changes the type of ds from integer to factor so ggplot understands
  ggplot(aes(x = mean_lfalha_total, y = relection_rate, group = ds, col = ds)) +
  # stat_summary(fun = "mean") +
  # stat_summary(geom = "line") + # Displays the mean as a line
  # stat_summary(geom = "point") + # Displays the mean as a point
  labs(col = "Audited:") + # Changes the name from "ds" to "Group" in the legend
  theme_classic() + # Changes the theme
  geom_smooth(span = 0.4, aes(fill = treat),se=F) +
  labs(x = "Mean acts of corruption (logarithm scale)", y = "Re-election rates, (mean)") +
  guides(fill="none")+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = c(0.24, 0.88)) # Change aspect ratio of the plot. Not necessary, but some prefer it.
plot_share_votes_by_corruption_levels


# "treatment" originally contained in the dataset from Avis, E., Ferraz, C., & Finan, F. (2018) and here refers to municipalities audited twice or once
# create a new "treatment" to consider municipalities audited before and after the respective elections of 2008 and 2012


# map of municipalities BR - https://github.com/ipeaGIT/geobr ####
library(geobr)
library(ggplot2)
library(RColorBrewer)
library(sf)
# utils::remove.packages('geobr')
# Read all municipalities in the country at a given year
mun <- read_municipality(code_muni="all", year=2010)

# plot only the sections 
ggplot() + 
  geom_sf(data=mun, fill = NA) + scale_fill_gradientn(colours= brewer.pal(2, "RdYlGn"))+
  geom_point(data = votes_per_precinct, mapping = aes(x = longitude, y = latitude, colour = factor(Elected)), size = 1) + 
  coord_sf()+
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank())+
  labs(col="Reelection")+
  ggtitle("Brazil's 2008 and 2012 voting precincts where mayors were re-elected or not")

# plot municipalities together with voting sections
ggplot() + 
  geom_sf(data=mun, fill = NA) + scale_fill_gradientn(colours= brewer.pal(9, "RdYlGn"))+
  geom_point(data = votes_per_precinct, mapping = aes(x = longitude, y = latitude, colour = factor(UF)), size = 1) + 
  coord_sf()+
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank())+
  labs(col="States")+
  ggtitle("Brazil's voting precincts locations in 2010")





# IGNORE ####
# consider only mun audited once
# drew_unique_municipalities_first_terms<- drew_municipalities %>% 
#   filter(NAME_MUNICIPALITY %in% first_term_drew_mun$`unique(votes_per_precinct$NAME_MUNICIPALITY)`) %>% # select municipalities ran by incumbents trying their re-election
#   distinct(NAME_MUNICIPALITY, .keep_all= TRUE) # keeping only municipalities that were audited once. For those audited twice, only the observation regarding the first audited was kept
# drew_unique_municipalities_first_terms %>%
#   ggplot(aes(lottery_date, lfalha_total)) +
#   stat_summary(fun = "mean", geom = "line") +
#   labs(x = "Date", y = "Log falha total, Average") +
#   geom_vline(xintercept = as.Date(c("2008-10-26","2012-10-26")))+ 
#   theme_minimal()
