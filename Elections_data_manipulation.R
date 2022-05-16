# PIEMONTE RIBEIRO, Marcelo
# Data replication - coding


# ---------------------------------------------------------------------------------------------------------------------------#
################################################### DATA MANIPULATION ########################################################
# ---------------------------------------------------------------------------------------------------------------------------#

# Libraries and functions ####
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
# devtools::install_github("tbrugz/ribge")
library(ribge)
# install.packages("splitstackshape")
library(splitstackshape)
# install.packages("sidrar")
library(sidrar) # only PNAD from 2012 onwards has relevant municipal level
# devtools::install_github("lucasmation/microdadosBrasil", force = T)
# library('microdadosBrasil') # https://www.rdocumentation.org/packages/microdadosBrasil/versions/0.0.0.9000
# install.packages("vtable")                                                 
library(vtable)
library(stargazer)
library(car)
library(hrbrthemes)
library(plyr)
library(estimatr)
library(margins)
library(mfx)
library(skedastic)
library(jtools)
library(AER)
library(pander)
library(sandwich)
library(texreg)
library(AICcmodavg)
#install.packages("leaps")
library(leaps)
# functions
source("VIF.R")
source("ProcStep.R")
source("GlobalCrit.R")

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

# 2008
profile_voters_sections_08<-profile_voters_sections_08 %>%
  dplyr::group_by(NM_MUNICIPIO, SG_UF, NR_SECAO) %>%
  dplyr::summarize(tot_voters = sum(QT_ELEITORES_PERFIL),
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
# if error re-install dplyr
# install.packages("dplyr")
# library(dplyr)

# 2012
profile_voters_sections_12<-profile_voters_sections_12 %>%
  dplyr::group_by(NM_MUNICIPIO, SG_UF, NR_SECAO) %>%
  dplyr::summarize(tot_voters = sum(QT_ELEITORES_PERFIL),
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
# write_xlsx(votes_per_precinct,"C:/Users/Ribeiro/OneDrive/Documents/OneDrive/IHEID/Dissertation/Data Sources/Voting data/votes_per_precinct.xlsx")

# Controls ####
# education levels ####
education_levels_municipalities<-get_sidra(api = "/t/3540/n6/all/v/1000140/p/all/c1568/9493,9494,9495,99713/c1/0/c2/0/c86/0/c58/0/d/v1000140%202")
education_levels_municipalities<-reshape(education_levels_municipalities, idvar = "Município", timevar = "Nível de instrução", direction = "wide", v.names="Valor") %>% 
  select("Unidade de Medida",Município, "Município (Código)", "Unidade de Medida", Ano,
         "Valor.Superior completo","Valor.Médio completo e superior incompleto", "Valor.Fundamental completo e médio incompleto",
         "Valor.Sem instrução e fundamental incompleto") 
education_levels_municipalities$state<-str_sub(education_levels_municipalities$Município,start=-2,end=-1) 
education_levels_municipalities$Município <-substr(education_levels_municipalities$Município,1,nchar(education_levels_municipalities$Município)-4)

# employment ####
employment_levels_municipalities<-get_sidra(api="/t/2031/n6/all/v/1000696/p/last%201/c11913/0,96166,96167,96169,96170,96171,96173/d/v1000696%202")
employment_levels_municipalities<-reshape(employment_levels_municipalities, idvar = "Município", 
                                          timevar = "Posição na ocupação e categoria do emprego no trabalho principal", direction = "wide", v.names="Valor") %>% 
  select("Unidade de Medida",Município, "Município (Código)", Ano,
         "Valor.Conta própria","Valor.Empregado", "Valor.Empregado - com carteira de trabalho assinada",
         "Valor.Empregado - outro sem carteira de trabalho assinada","Valor.Empregador", 
         "Valor.Trabalhador na produção para o próprio consumo") 
employment_levels_municipalities$state<-str_sub(employment_levels_municipalities$Município,start=-2,end=-1) 
employment_levels_municipalities$Município <-substr(employment_levels_municipalities$Município,1,nchar(employment_levels_municipalities$Município)-4)

# urban-rural ####
urban_rural_shares_municipalities<-get_sidra(api="/t/1309/n6/all/v/1000093/p/last%201/c2/0/c11277/0,90749,90752,90754/d/v1000093%202")
urban_rural_shares_municipalities<-reshape(urban_rural_shares_municipalities, idvar = "Município", 
                                          timevar = "Situação e localização da área", direction = "wide", v.names="Valor") %>% 
  select("Unidade de Medida",Município, "Município (Código)", Ano,
         "Valor.Rural - aglomerado - povoado", "Valor.Rural - área rural (exceto aglomerado)",
         "Valor.Urbana - cidade ou vila - área urbanizada") 
urban_rural_shares_municipalities$state<-str_sub(urban_rural_shares_municipalities$Município,start=-2,end=-1) 
urban_rural_shares_municipalities$Município <-substr(urban_rural_shares_municipalities$Município,1,nchar(urban_rural_shares_municipalities$Município)-4)

# illiteracy rates ####
setwd("./controls")
illiteracy_rates_municipalities = read.csv("illiteracy_rates_2010_municipalities_tabnet_datasus.csv", skip = 3, header = T, sep=";") %>%
     slice(-c(5569, 5568, 5567, 5566))
illiteracy_rates_municipalities['Município (Código)']<-str_sub(illiteracy_rates_municipalities$Município,start=1,end=6)
illiteracy_rates_municipalities$Município<-substring(illiteracy_rates_municipalities$Município,8)

# gdp per capita ####
gdp_capita_municipalities = read.csv("gdp_per_capita_2008_municipalities_tabnet_datasus.csv", skip = 3, header = T, sep=";") %>%
  slice(-c(5577, 5576, 5575, 5574, 5573, 5572, 5571, 5570, 5569, 5568, 5567, 5566, 5565))
gdp_capita_municipalities['Município (Código)']<-str_sub(gdp_capita_municipalities$Município,start=1,end=6)
gdp_capita_municipalities$Município<-substring(gdp_capita_municipalities$Município,8)

# unemployment rates ####
unemployment_rates_municipalities = read.csv("unemployment_rates_2010_municipalities_tabnet_datasus.csv", skip = 3, header = T, sep=";") %>%
  slice(-c(5557,5556, 5555, 5554, 5553, 5552, 5551, 5550))
unemployment_rates_municipalities['Município (Código)']<-str_sub(unemployment_rates_municipalities$Município,start=1,end=6)
unemployment_rates_municipalities$Município<-substring(unemployment_rates_municipalities$Município,8)

# income levels <1/2 min wage ####
perc_pop_income_less_half_minw_municipalities = read.csv("perctg_population_income_less_half_min_wage_2010_municipalities_tabnet_datasus.csv", skip = 3, header = T, sep=";") %>%
  slice(-c(5575, 5574, 5573, 5572, 5571, 5570, 5569, 5568, 5567, 5566))
perc_pop_income_less_half_minw_municipalities['Município (Código)']<-str_sub(perc_pop_income_less_half_minw_municipalities$Município,start=1,end=6)
perc_pop_income_less_half_minw_municipalities$Município<-substring(perc_pop_income_less_half_minw_municipalities$Município,8)

# income levels <1/4 min wage ####
perc_pop_income_less_quarter_minw_municipalities = read.csv("perctg_population_income_less_quarter_of_min_wage_2010_municipalities_tabnet_datasus.csv", skip = 3, header = T, sep=";") %>%
  slice(-c(5575, 5574, 5573, 5572, 5571, 5570, 5569, 5568, 5567, 5566))
perc_pop_income_less_quarter_minw_municipalities['Município (Código)'] <-str_sub(perc_pop_income_less_quarter_minw_municipalities$Município,start=1,end=6)
perc_pop_income_less_quarter_minw_municipalities$Município<-substring(perc_pop_income_less_quarter_minw_municipalities$Município,8) 

# assemble control variables together
controls_sidra<-Reduce(function(x, y) merge(x, y, all=TRUE), list(education_levels_municipalities, employment_levels_municipalities, urban_rural_shares_municipalities))
controls_sidra$`Município (Código)`<- substr(controls_sidra$`Município (Código)`, 1, 6)
controls_datasus<-Reduce(function(x, y) merge(x, y, all=TRUE), list(gdp_capita_municipalities, illiteracy_rates_municipalities, perc_pop_income_less_half_minw_municipalities,
                                                                    perc_pop_income_less_quarter_minw_municipalities, unemployment_rates_municipalities)) 
controls<-merge(controls_sidra,controls_datasus, by="Município (Código)")
controls$NAME_MUNICIPALITY<-toupper(controls$Município.x) # upper case municipality names
controls$NAME_MUNICIPALITY <-substr(controls$NAME_MUNICIPALITY,1,nchar(controls$NAME_MUNICIPALITY)-1) # remove extra blank space 

# Municipal level dataset ####
# read the 2004 elections results
setwd("./votes_candidates_munzona_year_state_2004")
mayors_2004 = map_dfr(votes_municipalities_sections_2004, read_votes_municipalities_sections_2004)  # combine datasets informing candidates of 2004
# create vote share, margin of victory elections 2004
mayors_2004<-mayors_2004 %>% 
  group_by(ACRONYM_STATE, NAME_MUNICIPALITY) %>% 
  mutate(
    share_vote_04 = Total / sum(Total),
    margin_victory_04 = ifelse(Total == max(Total), max(Total) / min(Total) - 1, 1 - max(Total) / min(Total)),
  ) %>% 
  ungroup()
mayors_2004$elected_04<-ifelse(mayors_2004$RESULT=="ELEITO",1,0) # dummy if elected
elected_mayors_2004_mun<-mayors_2004[mayors_2004$RESULT == "ELEITO",] # 5559 rows - in 2004, 5560 municipalities including the capital state DF ()
# create vote shares, margin victory elections 2008
pol_performance_candidates_2008<-votes_per_candidate_2008 %>% 
  group_by(ACRONYM_STATE, NAME_MUNICIPALITY) %>% 
  mutate(
    share_vote_08 = Total / sum(Total),
    margin_victory_08 = ifelse(Total == max(Total), max(Total) / min(Total) - 1, 1 - max(Total) / min(Total)),
  ) %>% 
  ungroup()
pol_performance_candidates_2008$elected_08<-ifelse(pol_performance_candidates_2008$RESULT=="ELEITO",1,0) # dummy if elected
pol_performance_candidates_2008$unique_id<-paste(pol_performance_candidates_2008$NAME_MUNICIPALITY, pol_performance_candidates_2008$ACRONYM_STATE)

# merge with Avis & Ferraz 2018 dataset 
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
drew_municipalities$unique_id<-paste(drew_municipalities$NAME_MUNICIPALITY, drew_municipalities$UF) # municipalities audited unique id
# filter the municipalities audited
pol_performance_candidates_2008<- pol_performance_candidates_2008 %>% 
  filter(NAME_MUNICIPALITY %in% unique(drew_municipalities$NAME_MUNICIPALITY))
# verify names candidates
name_mayors_2008<-as.data.frame(table(pol_performance_candidates_2008$NAME_CANDIDATE)) # few candidates with the same name, from the same state but different municipalities
name_mayors_2004<-as.data.frame(table(elected_mayors_2004_mun$NAME_CANDIDATE)) # few candidates with the same name, but they were running in different states
# merge candidates 2008 and incumbents (mayors trying the re-election, then elected in 2004)
candidates_2008_mun<-merge(x=pol_performance_candidates_2008,y=elected_mayors_2004_mun,by.x=c("NAME_CANDIDATE", "ACRONYM_STATE"),
                           by.y =c("NAME_CANDIDATE", "ACRONYM_STATE") , all = F)
candidates_2008_mun<-candidates_2008_mun[-c(64, 423, 323), ] # malhador SE, rio branco AC, montes claros MG had double entries (2nd round)
# !!! correct these 3 municipalities in pol_performance

# repeat the procedure for 2012 vs 2008
mayors_2008<-subset(pol_performance_candidates_2008, elected_08==1) # keep only elected mayors in the 2008 elections
votes_per_eligible_candidate_2012<-subset(votes_per_candidate_2012, Total !=0) #drop ineligible candidates
# calculate margins and share votes
pol_performance_candidates_2012<-votes_per_eligible_candidate_2012 %>% 
  group_by(ACRONYM_STATE, NAME_MUNICIPALITY) %>% 
  mutate(
    share_vote_12 = Total / sum(Total),
    margin_victory_12 = ifelse(Total == max(Total), max(Total) / min(Total) - 1, 1 - max(Total) / min(Total)),
  ) %>% 
  ungroup()
pol_performance_candidates_2012$elected_12<-ifelse(pol_performance_candidates_2012$RESULT=="ELEITO",1,0) # dummy if elected
pol_performance_candidates_2012$unique_id<-paste(pol_performance_candidates_2012$NAME_MUNICIPALITY, pol_performance_candidates_2012$ACRONYM_STATE)
pol_performance_candidates_2012<- pol_performance_candidates_2012 %>% 
  filter(NAME_MUNICIPALITY %in% unique(drew_municipalities$NAME_MUNICIPALITY)) # filter the audited municipalities
# merge canidates 2012 with incumbents (candidates trying re-election)
candidates_2012_mun<-merge(x=pol_performance_candidates_2012,y=mayors_2008,by.x=c("NAME_CANDIDATE", "ACRONYM_STATE"),
                           by.y =c("NAME_CANDIDATE", "ACRONYM_STATE") , all = F)
candidates_2008_mun<-candidates_2008_mun[-c(6, 103, 288), ] # santo andré SP, tucunduva RS
# remove agua preta PE because of https://www.tse.jus.br/imprensa/noticias-tse/2013/Maio/agua-preta-pe-tera-novas-eleicoes
# !!! correct these 3 municipalities in pol_performance

# Merge 2008 and 2012 incumbents trying re-election
candidates_2008_mun = subset(candidates_2008_mun, select = -c(NUM_BALLOT_CANDIDATE.x, NUM_ROUND.x, CODE_SIT_CAND_TOT.x,
                                                              elected_08,NUM_BALLOT_CANDIDATE.y,NUM_ROUND.y, CODE_SIT_CAND_TOT.y,
                                                              NAME_MUNICIPALITY.y, elected_04, unique_id)) # remove unecessary columns
candidates_2008_mun<- candidates_2008_mun %>% dplyr::rename(YEAR_ELECTION_CANDIDATURE = YEAR_ELECTION.x, RESULT_CANDIDATURE= RESULT.x, Total_votes=Total.x, 
                                                            SHARE_VOTES_CANDIDATURE =share_vote_08, MARGIN_VICTORY_CANDIDATURE= margin_victory_08,
                                                            YEAR_PREVIOUS_CANDIDATURE= YEAR_ELECTION.y, RESULT_PREVIOUS_CANDIDATURE= RESULT.y, PREVIOUS_PARTY=PARTY_ACRONYM.y,
                                                            PREVIOUS_PARTY_N=PARTY_NUMBER.y, Total_votes_previous =Total.y, SHARE_VOTES_PREVIOUS_CANDID=share_vote_04,
                                                            MARGIN_VICTORY_PREVIOUS_CANDID=margin_victory_04) # rename
candidates_2012_mun = subset(candidates_2012_mun, select = -c(NUM_BALLOT_CANDIDATE.x, NUM_ROUND.x, CODE_SIT_CAND_TOT.x,
                                                              elected_12,NUM_BALLOT_CANDIDATE.y,NUM_ROUND.y, CODE_SIT_CAND_TOT.y,
                                                              NAME_MUNICIPALITY.y, elected_08, unique_id.y, unique_id.x)) # remove unecessary columns
candidates_2012_mun<-candidates_2012_mun %>% dplyr::rename(YEAR_ELECTION_CANDIDATURE = YEAR_ELECTION.x, RESULT_CANDIDATURE= RESULT.x, Total_votes=Total.x, 
                                                           SHARE_VOTES_CANDIDATURE =share_vote_12, MARGIN_VICTORY_CANDIDATURE= margin_victory_12,
                                                           YEAR_PREVIOUS_CANDIDATURE= YEAR_ELECTION.y, RESULT_PREVIOUS_CANDIDATURE= RESULT.y, PREVIOUS_PARTY=PARTY_ACRONYM.y,
                                                           PREVIOUS_PARTY_N=PARTY_NUMBER.y, Total_votes_previous =Total.y, SHARE_VOTES_PREVIOUS_CANDID=share_vote_08,
                                                           MARGIN_VICTORY_PREVIOUS_CANDID=margin_victory_08) # rename
candidates_2008_2012_mun<-rbind(candidates_2008_mun, candidates_2012_mun) # merge 2012 and 2008 incumbents trying re-election

# merge 2008 & 2012 electoral performance and corruption dataset at municipal level
drew_municipalities_elect_performances = subset(drew_municipalities, select = -c(exppop,tx_analf18m, gini, treatment,
                                                                                 lpop,lrenda_pc, shurb, T_SUPER25M, n_nbr, n_nbraudited,
                                                                                 party_id,puf_treatment)) # remove innocuous variables from Avis & Ferraz.  # "treatment" originally contained in the dataset from Avis, E., Ferraz, C., & Finan, F. (2018) and here refers to municipalities audited twice or once. Moreover, controls variables used were from 2000 Census 
drew_municipalities_elect_performances<-na.omit(drew_municipalities_elect_performances) # some municipalities audited did not have their audit reports coded by Avis and Ferraz, only 982 municipalities did.
drew_municipalities_elect_performances$threshold<-ifelse(drew_municipalities_elect_performances$sorteio<34,2008,2012) # create thresholds to compare RD, DD
# merge corruption info with electoral performance
drew_municipalities_elect_performances<-merge(x=drew_municipalities_elect_performances,y=candidates_2008_2012_mun,by.x=c("NAME_MUNICIPALITY", "UF","threshold"),
                                              by.y =c("NAME_MUNICIPALITY.x", "ACRONYM_STATE","YEAR_ELECTION_CANDIDATURE") , all= F) 
drew_municipalities_elect_performances<-drew_municipalities_elect_performances[!duplicated(drew_municipalities_elect_performances$unique_id),] # remove municipalities audited twice


# ---------------------------------------------------------------------------------------------------------------------------#
################################################### DATA ANALYSIS ############################################################
# ---------------------------------------------------------------------------------------------------------------------------#

# Summary statistics ####
sum(table(drew_municipalities$UF)) # between 2006-2013, 1020 municipalities were audited
unique(drew_municipalities$NAME_MUNICIPALITY) # with 967 unique municipalities (then 53 municipalities were audited twice)
unique(drew_municipalities_elect_performances$unique_id) # 385 had mayors who ran for re-election.

# Figure 1 ####
plot(factor(drew_municipalities$year),ylim = c(0, 200), yaxp=c(0, 180,6), 
     xlab = "Year", ylab = "N°municipalities audited", border = "black", col="lightgrey") # number of audited municipalities per year
abline(h=c(60,120,180),  lty = 3)

# Figure 2 ####
drew_municipalities_elect_performances %>%
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
drew_municipalities_elect_performances %>%
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
drew_municipalities_elect_performances$treat<-ifelse(drew_municipalities_elect_performances$sorteio<27,"pre-elections",
                                          ifelse(drew_municipalities_elect_performances$sorteio>33 & 
                                                   drew_municipalities_elect_performances$sorteio<37,"pre-elections","post-elections")) # create a new "treatment" to consider municipalities audited before and after the respective elections of 2008 and 2012
# Plot:
plot_share_votes_by_corruption_levels<-subset(drew_municipalities_elect_performances, SHARE_VOTES_CANDIDATURE<1 & year<2011) 
# !!! 15 candidates with 100% shares discarded
plot_share_votes_by_corruption_levels %>%
  # Add a new column called 'bin': cut the initial corruption in bins
  mutate( bin=cut_width(exp(lfalha_total), width=30, boundary=0) ) %>%
  ggplot( aes(x=bin, y=SHARE_VOTES_CANDIDATURE, fill=treat)) +
  geom_boxplot() +
  theme_minimal()+
  theme(text = element_text(size=10),axis.text.x = element_text(angle=45, hjust=1), legend.position = "none") +
  xlab("Acts of corruption")+
  ylab("Share of votes 2008 elections")+
  theme(legend.title=element_blank())+
  facet_wrap(~treat)

# Figure 6 ####
# map of municipalities BR - https://github.com/ipeaGIT/geobr 
# Read all municipalities in the country at a given year
mun <- read_municipality(code_muni="all", year=2010)
# mun$name_muni_capital<-toupper(mun$name_muni)
# mun %>% 
#   filter(name_muni_capital %in% unique(votes_per_precinct$NAME_MUNICIPALITY))
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

# Table 2 ####
# compare characteristics of municipalities audited before and after the elections
# balance_test<- votes_per_precinct %>%
#   dplyr::group_by(NAME_MUNICIPALITY, UF, YEAR_ELECTION.x, NAME_CANDIDATE.x, RESULT.x, PARTY_ACRONYM.x) %>%
#   dplyr::summarize(party= mean(PARTY_NUMBER.x)) %>% distinct(NAME_MUNICIPALITY, .keep_all = TRUE)
# balance_test<-balance_test[!duplicated(balance_test$NAME_MUNICIPALITY), ]                   


balance_test<- merge(drew_municipalities_elect_performances, controls, by.x = c("NAME_MUNICIPALITY","UF"), by.y=c("NAME_MUNICIPALITY","state"), all.x = T, all.y = F) 
balance_test<-subset(balance_test, select = -c(Município.y))
balance_test[,-c(1:48)]<-sapply(balance_test[,-c(1:48)], function(x) as.numeric(gsub(",",".",x))) # replace comma separators by dot
balance_test$`Political party`<-factor(balance_test$PARTY_ACRONYM.x)
# labels
balnce_test_labels <- data.frame(var = c('treat','RESULT_CANDIDATURE','radio_am','Valor.Superios completo','Valor.Sem instrução e fundamental incompleto',
                               'Valor.Empregado','Valor.Rural - área rural (exceto aglomerado)','Valor.Urbana - cidade ou vila - área urbanizada',
                               'PIB_per_capita','Taxa_de_analfabetismo','X._população_com_renda_._1.2_SM', 'Taxa_de_desemprego_16a_e.'),
                       labels = c('Municipalities audited','Elected/Not','Radio (AM) municipality','% with college','% without elementary school',
                                  '% employed','% municipality rural', '% urban area', 'GDP capita BRL','Illiteracy rate',
                                  '% people earning less 0.5 of min wage','unemployed rate'))
balance_test %>% select(-c(threshold, UF, sorteio, uf_id, no_os, lfalha_total, lnirregular, lmismanagement, lno_os,
                        lottery_date, year, unique_id, NAME_CANDIDATE, PARTY_NUMBER.x, PARTY_ACRONYM.x,
                        YEAR_PREVIOUS_CANDIDATURE, RESULT_PREVIOUS_CANDIDATURE, PREVIOUS_PARTY_N, PREVIOUS_PARTY,
                        Total_votes_previous, SHARE_VOTES_PREVIOUS_CANDID, MARGIN_VICTORY_PREVIOUS_CANDID, `Município (Código)`, `Unidade de Medida`,Município.x,
                        Ano, X._população_com_renda_._1.4_SM,`Valor.Rural - aglomerado - povoado`, `Valor.Trabalhador na produção para o próprio consumo`,
                        Valor.Empregador,`Valor.Empregado - com carteira de trabalho assinada`, `Valor.Empregado - outro sem carteira de trabalho assinada`,
                        `Valor.Conta própria`, `Valor.Fundamental completo e médio incompleto`, `Valor.Médio completo e superior incompleto`)) %>%
                sumtable(group = 'treat', group.test= T, labels = balnce_test_labels, out = 'latex') # https://cran.r-project.org/web/packages/vtable/vtable.pdf


# Exploratory analysis ####
# rename control variables
balance_test<-rename(balance_test, c('Valor.Superior completo'='with_college', 
                                     'Valor.Sem instrução e fundamental incompleto'='no_elementary',
                                     'Valor.Empregado'='perctg_employed','Valor.Rural - área rural (exceto aglomerado)'='perctg_rural',
                                     'Valor.Urbana - cidade ou vila - área urbanizada'='perctg_urban',
                                     'PIB_per_capita'='gdp_capita','Taxa_de_analfabetismo'='illiteracy_rate',
                                     'X._população_com_renda_._1.2_SM'='perctg_earning_half_mwage', 
                                     'Taxa_de_desemprego_16a_e.'='unemploym_rate',"Valor.Rural - aglomerado - povoado"="perctg_rural",
                                     "X._população_com_renda_._1.4_SM"='perctg_earning_quarter_mwage',
                                     "Valor.Trabalhador na produção para o próprio consumo"="self_employed",
                                     "Valor.Empregador"="employer","Valor.Empregado - outro sem carteira de trabalho assinada"=
                                       "perctg_informal_workers","Valor.Empregado - com carteira de trabalho assinada"="perctg_formal_workers",
                                     "Valor.Conta própria"="independent","Valor.Fundamental completo e médio incompleto"="elementary",
                                     "Valor.Médio completo e superior incompleto"="high_school")) 
# create variables "Change in vote share" and "Change in margins" related to previous elections. 
balance_test$delta_vote_share<-(balance_test$SHARE_VOTES_CANDIDATURE- balance_test$SHARE_VOTES_PREVIOUS_CANDID)
balance_test$delta_margin_victory<-(balance_test$MARGIN_VICTORY_CANDIDATURE- balance_test$MARGIN_VICTORY_PREVIOUS_CANDID)
# Create dummy indicated elected or not candidates 
balance_test$elected<-ifelse(balance_test$RESULT_CANDIDATURE=="ELEITO",1,0) # create dummy
# Restrict sample
balance_test_restricted<-subset(balance_test, SHARE_VOTES_CANDIDATURE != 1) # remove observations with vote share = 100% these are cases where one of the candidates had its candidature/election denied due to irregularities
balance_test_restricted_2008<-subset(balance_test,year<2011) # only consider the 2008 elections

# Summary variables
print(summary(balance_test)) #radio and tc are dummies, share_votes contains 100%, margin victory contain weird numbers (review), with_college (max 15%), employer (21 NAs), pctg_rural (190NAs) 
# veriy outliers
data_long <- melt(balance_test)
ggplot(data_long, aes(x = variable, y = value)) +           
  geom_boxplot()+
  facet_wrap( ~ variable, scales="free") 
# lfalha_total,lnirregular and lmismanagemetn few outliers but well distributed, 
# total votes skewed, share votes has outliers but well distributed, margin victory skewed, 
# with college, high school, independent, emplyer, self_employed, rural, gdp_capita, unemployment rate are skewed,  

# dplyr::mutate_each(balance_test, funs(log),
#                    log_tot_votes = Total_votes,
#                    log_share_votes = SHARE_VOTES_CANDIDATURE,
#                    log_margin_votes = MARGIN_VICTORY_CANDIDATURE,
#                    log_college = with_college,
#                    log_high_school=high_school,
#                    log_indpt=independent,
#                    log_employer=employer,
#                    log_self_emplyed=self_employed,
#                    log_rural=perctg_rural,
#                    log_unemploym=unemploym_rate)
# transform
balance_test$ln_gdp_capita<-log(balance_test$gdp_capita)
balance_test$log_tot_votes<-log(balance_test$Total_votes)
balance_test$log_share_votes<-log(balance_test$SHARE_VOTES_CANDIDATURE)
balance_test$log_margin_votes<-log(balance_test$MARGIN_VICTORY_CANDIDATURE)
balance_test$log_college<-log(balance_test$with_college)
balance_test$log_high_school<-log(balance_test$high_school)
balance_test$log_indpt<-log(balance_test$independent)
balance_test$log_employer<-log(balance_test$employer)
balance_test$log_self_emplyed<-log(balance_test$self_employed)
balance_test$log_rural<-log(balance_test$perctg_rural)
balance_test$log_unemploym<-log(balance_test$unemploym_rate)
balance_test$log_delta_vote_share<-log(balance_test$delta_vote_share)

data_long <- melt(balance_test)
ggplot(data_long, aes(x = variable, y = value)) +           
  geom_boxplot()+
  facet_wrap( ~ variable, scales="free") # share_votes still quite skewed as well as vote_shares
# correlations to keep in mind
require(ellipse)
colnames(balance_test) # verify columns used below
plotcorr(cor(balance_test[, c(8,9,10,12,21,22,23,36,39,43,48,50,55,56,57,58)],use="complete.obs")) # pearson corr
plotcorr(cor(balance_test[, c(8,9,10,12,21,22,23,55,59,60,65,63,36,39,43,48,50,55,56,57,58)], method = "spearman", use="complete.obs"))

# VIFs, Step forward and Global method to choose covariates 
# VIF(balance_test[, c(12,13,55,65,63,59,50,48,39)], balance_test[, 56])# exclude no elementary with log_vote_shares, don't include log_high_school 
# ProcStep(CoursEval[, c(12,13,59,37,38,39,41,62,43,63,48,50,52,51,65,55)], CoursEval[, 1], method="forward")

# Table 3 ####
# Model without control - share votes
share_votes_no_controls<-lm(SHARE_VOTES_CANDIDATURE ~ treat, data = balance_test)
share_votes_no_controls_rob<-lm_robust(SHARE_VOTES_CANDIDATURE ~ treat, data = balance_test, se_type = "HC1")
log_share_votes_no_controls<-lm(log_share_votes ~ treat, data = balance_test) # boxplot recommends the share instead
log_share_votes_no_controls_rob<-lm_robust(log_share_votes ~ treat, data = balance_test, se_type = "HC1")
# However share of votes is truncated between 0 and 1
logit_share_votes_no_controls <- glm(SHARE_VOTES_CANDIDATURE ~ treat,
                                  family = binomial(link = logit), data = balance_test) #Logit model
1 - logit_share_votes_no_controls$deviance/logit_share_votes_no_controls$null.deviance # pseudo-R squared values 0.002645236
summary(logit_share_votes_no_controls) # you can trust in the sign of the coeff but not on its magnitude
summary(margins(logit_share_votes_no_controls)) # -2% in vote shares (avg mg effect)
logit_share_votes_no_controls_margins<-margins(logit_share_votes_no_controls)
# Model without control - log total votes
log_tot_votes_no_controls_rob<-lm_robust(log_tot_votes ~ treat, data = balance_test, se_type = "HC1")
# Model without control - delta total votes
delta_share_votes_no_controls_rob<-lm_robust(delta_vote_share ~ treat, data = balance_test, se_type = "HC1")
# Model without control - dummy elected
logit_prob_elect_no_controls <- glm(elected ~ treat,
                                     family = binomial(link = logit), data = balance_test) #Logit model
1 - logit_share_votes_no_controls$deviance/logit_share_votes_no_controls$null.deviance 
summary(logit_prob_elect_no_controls) # you can trust in the sign of the coeff but not on its magnitude
summary(margins(logit_prob_elect_no_controls)) # -4% in vote shares (avg mg effect)
logit_prob_elect_no_controls_margins<-margins(logit_prob_elect_no_controls)
# see results
export_summs(logit_share_votes_no_controls_margins, #share_votes_no_controls_rob,
             delta_share_votes_no_controls_rob, logit_prob_elect_no_controls,
             to.file="pdf",  file.name ="Table 3", robust = TRUE,
             scale = F, model.names = c("Share votes (SV),logit AME","Change share votes","P(re-election), AME"), digits=4,
             error_format = "[{conf.low}, {conf.high}]") # the intercepts are signficant, see Figure 2 why

# Table 4 controls similar to Ferraz & Finan 2008 ####
# Model with control (similar to Ferraz 2008) - share votes
share_votes_controls_from_ferraz<-lm(SHARE_VOTES_CANDIDATURE ~ treat + perctg_urban+illiteracy_rate+
                           ln_gdp_capita + factor(UF) , data = balance_test)
share_votes_controls_from_ferraz_rob<-lm_robust(SHARE_VOTES_CANDIDATURE ~ treat + perctg_urban+illiteracy_rate+
                                       ln_gdp_capita + factor(UF) , data = balance_test, se_type = "HC1")
logit_share_votes_controls_from_ferraz <- glm(SHARE_VOTES_CANDIDATURE ~ treat + perctg_urban+illiteracy_rate+
                                                ln_gdp_capita , # + factor(UF)
                                                family = binomial(link = logit), data = balance_test) #Logit model
1 - logit_share_votes_controls_from_ferraz$deviance/logit_share_votes_controls_from_ferraz$null.deviance # pseudo-R squared values 0.002645236
summary(logit_share_votes_controls_from_ferraz) # you can trust in the sign of the coeff but not on its magnitude
summary(margins(logit_share_votes_controls_from_ferraz)) # -1.8% in vote shares (avg mg effect)
logit_share_votes_controls_from_ferraz_margins<-margins(logit_share_votes_controls_from_ferraz) # margins
log_share_votes_controls_from_ferraz_rob<-lm_robust(log_share_votes ~ treat + perctg_urban+illiteracy_rate+
                                                  ln_gdp_capita + factor(UF) , data = balance_test, se_type = "HC1")
# Model with control (similar to Ferraz 2008) - log total votes
log_tot_votes_controls_from_ferraz_rob<-lm_robust(log_tot_votes ~ treat + perctg_urban+illiteracy_rate+
                                                  ln_gdp_capita , data = balance_test, se_type = "HC1") # + factor(UF)
# Model with control - delta share votes
delta_share_votes_controls_from_ferraz_rob<-lm_robust(delta_vote_share ~ treat+ perctg_urban+illiteracy_rate+
                                                      ln_gdp_capita , data = balance_test, se_type = "HC1") # + factor(UF)
# Model with control - dummy elected
logit_prob_elect_controls_from_ferraz <- glm(elected ~ treat+ perctg_urban+illiteracy_rate+
                                               ln_gdp_capita , # + factor(UF)
                                               family = binomial(link = logit), data = balance_test) #Logit model
1 - logit_prob_elect_controls_from_ferraz$deviance/logit_prob_elect_controls_from_ferraz$null.deviance 
summary(logit_prob_elect_controls_from_ferraz) # you can trust in the sign of the coeff but not on its magnitude
summary(margins(logit_prob_elect_controls_from_ferraz)) # -4% in chances of re-election (avg mg effect)
logit_prob_elect_controls_from_ferraz_margins<-margins(logit_prob_elect_controls_from_ferraz)
# see results
export_summs(logit_share_votes_controls_from_ferraz, #share_votes_no_controls_rob,
             delta_share_votes_controls_from_ferraz_rob, logit_prob_elect_controls_from_ferraz,
             to.file="pdf",  file.name ="Table 4", robust = TRUE,
             scale = F, model.names = c("Share votes,logit Avg Marg Effect, AME","Change share votes","P(re-election), AME"), digits=4,
             error_format = "[{conf.low}, {conf.high}]") # the intercepts are signficant, see Figure 2 why

# Table 4.1 controls selected via vif ####
# Model with controls VIFs - share votes
eval_share_votes = lm(SHARE_VOTES_CANDIDATURE ~ treat +log_college + high_school+ elementary+
                        no_elementary+ perctg_employed+ log_employer+ perctg_informal_workers+
                        log_self_emplyed+ perctg_urban+ illiteracy_rate +ln_gdp_capita +
                        perctg_earning_quarter_mwage+ log_unemploym, data = balance_test)
GlobalCrit(eval_share_votes) # AIC BIC indicates GH, GHN, GKM; G.log_employer, H:perctg_informal K:illiteracy, M:perctg_earning_q, N:log_unemploy
share_votes_controls_vif_rob<-lm_robust(SHARE_VOTES_CANDIDATURE ~ treat+log_employer+ perctg_informal_workers+ log_unemploym ,
                         data = balance_test, se_type = "HC1") #  + factor(UF)
logit_share_votes_controls_vif<-glm(SHARE_VOTES_CANDIDATURE ~ treat+log_employer+ perctg_informal_workers+ log_unemploym,
                                    family = binomial(link = logit), data = balance_test) # + factor(UF)
1 - logit_share_votes_controls_vif$deviance/logit_share_votes_controls_vif$null.deviance # pseudo-R squared values 0.002645236
summary(logit_share_votes_controls_vif) # you can trust in the sign of the coeff but not on its magnitude
summary(margins(logit_share_votes_controls_vif)) # -1.89% in vote shares (avg mg effect)
logit_share_votes_controls_vif<-margins(logit_share_votes_controls_vif) # margins
# Model with control VIFs - log total votes
log_tot_votes_controls_vif_rob<-lm_robust(log_tot_votes ~ treat + log_employer+ perctg_informal_workers+ log_unemploym
                                          , data = balance_test, se_type = "HC1") # + factor(UF)
# Model with control - delta share votes
delta_share_votes_controls_vif_rob<-lm_robust(delta_vote_share ~ treat+ log_employer+ perctg_informal_workers+ log_unemploym 
                                              , data = balance_test, se_type = "HC1") # + factor(UF)
# Model with control - dummy elected
logit_prob_elect_controls_vif<- glm(elected ~ treat+ log_employer+ perctg_informal_workers+ log_unemploym, # + factor(UF)
                                             family = binomial(link = logit), data = balance_test) #Logit model
1 - logit_prob_elect_controls_vif$deviance/logit_prob_elect_controls_vif$null.deviance 
summary(logit_prob_elect_controls_vif) # you can trust in the sign of the coeff but not on its magnitude
summary(margins(logit_prob_elect_controls_vif)) # -4% in chances of re-election (avg mg effect)
logit_prob_elect_controls_vif_margins<-margins(logit_prob_elect_controls_vif)
# see results
export_summs(logit_share_votes_controls_vif, #share_votes_no_controls_rob,
             delta_share_votes_controls_vif_rob, logit_prob_elect_controls_vif,
             to.file="pdf",  file.name ="Table 4.1", robust = TRUE,
             scale = F, model.names = c("Share votes,logit Avg Marg Effect, AME","Change share votes","P(re-election), AME"), digits=4,
             error_format = "[{conf.low}, {conf.high}]") # the intercepts are signficant, see Figure 2 why

# Table 5 - prob election with interacted audited*corruption ####
# Linear probab model - no controls
audit_corrupt_prob_elect_no_controls_rob<-lm_robust(elected ~ treat*lfalha_total, data = balance_test, se_type = "HC1")
summary(audit_corrupt_prob_elect_no_controls_rob, digits=3)
# Linear probab model - with controls
audit_corrupt_prob_elect_controls_rob<-lm_robust(elected ~ treat*lfalha_total + perctg_urban+illiteracy_rate+
                                                   ln_gdp_capita, data = balance_test, se_type = "HC1")
summary(audit_corrupt_prob_elect_controls_rob, digits=3)

# Logitt models - no controls
# !!! try out robust later - require(robustbase) glmrob
logit_audit_corrupt_prob_elect_no_controls <- glm(elected ~treat*lfalha_total, 
                                                  family = binomial(link = logit), data = balance_test) #Logit model
1 - logit_audit_corrupt_prob_elect_no_controls$deviance/logit_audit_corrupt_prob_elect_no_controls$null.deviance # pseudo R2
summary(logit_audit_corrupt_prob_elect_no_controls)
# AME
logit_audit_corrupt_prob_elect_no_controls_AME<-(logitmfx(elected ~ treat +lfalha_total + treat*lfalha_total, data=balance_test,
                                                          robust = F, # calculate robust SE
                                                          atmean = FALSE)) # average partial effect -"atmean" = "FALSE" calculates AMEs instead of MEMs
# margins
logit_audit_corrupt_prob_elect_no_controls_margins<-summary(margins(logit_audit_corrupt_prob_elect_no_controls))
# margins at levels
summary(margins(logit_audit_corrupt_prob_elect_no_controls, at=list(lfalha_total=c(1:6)),variables = "lfalha_total")) # -4% in chances of re-election (avg mg effect)
# margins at quantiles
logit_audit_corrupt_prob_elect_no_controls_1q <- margins(logit_audit_corrupt_prob_elect_no_controls,
                    at = list(lfalha_total = quantile(balance_test$lfalha_total, 0.25)), # at the first quantile 
                    type = "response")
logit_audit_corrupt_prob_elect_no_controls_2q <- margins(logit_audit_corrupt_prob_elect_no_controls,
                                                         at = list(lfalha_total = quantile(balance_test$lfalha_total, 0.50)), # at the first quantile 
                                                         type = "response")
logit_audit_corrupt_prob_elect_no_controls_3q <- margins(logit_audit_corrupt_prob_elect_no_controls,
                                                         at = list(lfalha_total = quantile(balance_test$lfalha_total, 0.75)), # at the first quantile 
                                                         type = "response")
export_summs(logit_audit_corrupt_prob_elect_no_controls_1q, 
             logit_audit_corrupt_prob_elect_no_controls_2q, 
             logit_audit_corrupt_prob_elect_no_controls_3q,
             to.file="pdf",  file.name ="Table 5.1", robust = TRUE,
             scale = F, digits=6, model.names = c("No controls 1Q", "No controls 2Q", "No controls 3Q"), digits=4,
             error_format = "[{conf.low}, {conf.high}]")
            
# Logit & Probit models - with controls
logit_audit_corrupt_prob_elect_controls <- glm(elected ~ treat*lfalha_total + perctg_urban+illiteracy_rate+
                                                 ln_gdp_capita, family = binomial(link = logit), data = balance_test) #Logit model
1 - logit_audit_corrupt_prob_elect_controls$deviance/logit_audit_corrupt_prob_elect_controls$null.deviance # pseudo R2
summary(logit_audit_corrupt_prob_elect_controls)
# AME
logit_audit_corrupt_prob_elect_controls_margins_AME<-(logitmfx(elected ~treat*lfalha_total + perctg_urban+illiteracy_rate+
                                                                 ln_gdp_capita, data=balance_test,
                                                          robust = F, # calculate robust SE
                                                          atmean = FALSE)) # average partial effect -"atmean" = "FALSE" calculates AMEs instead of MEMs
logit_audit_corrupt_prob_elect_controls_margins_AME
# margins
summary(margins(logit_audit_corrupt_prob_elect_controls)) 
# margins at levels
summary(margins(logit_audit_corrupt_prob_elect_controls, at=list(lfalha_total=c(1:6)),variables = "lfalha_total"))
# margins at quantiles
logit_audit_corrupt_prob_elect_controls_1q <- margins(logit_audit_corrupt_prob_elect_controls,
                                                         at = list(lfalha_total = quantile(balance_test$lfalha_total, 0.25)), # at the first quantile 
                                                         type = "response")
logit_audit_corrupt_prob_elect_controls_2q <- margins(logit_audit_corrupt_prob_elect_controls,
                                                         at = list(lfalha_total = quantile(balance_test$lfalha_total, 0.50)), # at the first quantile 
                                                         type = "response")
logit_audit_corrupt_prob_elect_controls_3q <- margins(logit_audit_corrupt_prob_elect_controls,
                                                         at = list(lfalha_total = quantile(balance_test$lfalha_total, 0.75)), # at the first quantile 
                                                         type = "response")
export_summs(logit_audit_corrupt_prob_elect_controls_1q, 
             logit_audit_corrupt_prob_elect_controls_2q, 
             logit_audit_corrupt_prob_elect_controls_3q,
             to.file="pdf",  file.name ="Table 5.2", robust = TRUE,
             scale = F, digits=6, model.names = c("Controls 1Q", "Controls 2Q", "Controls 3Q"))

# Logit model with control and quadratic term
quadratic_logit_audit_corrupt_prob_elect_controls <- glm(elected ~ treat:lfalha_total + treat:I(lfalha_total^2)+ 
                                                         perctg_urban+illiteracy_rate+ ln_gdp_capita, 
                                                         family = binomial(link = logit), data = balance_test) #Logit model
1 - quadratic_logit_audit_corrupt_prob_elect_controls$deviance/quadratic_logit_audit_corrupt_prob_elect_controls$null.deviance # pseudo R2
summary(quadratic_logit_audit_corrupt_prob_elect_controls)
# AME
quadratic_logit_audit_corrupt_prob_elect_controls_AME<-(logitmfx(elected ~ treat:lfalha_total + treat:I(lfalha_total^2)+ 
                                                                  perctg_urban+illiteracy_rate+ ln_gdp_capita, 
                                                                  data = balance_test,
                                                               robust = F, # calculate robust SE
                                                               atmean = FALSE)) # average partial effect -"atmean" = "FALSE" calculates AMEs instead of MEMs
quadratic_logit_audit_corrupt_prob_elect_controls_AME
# margins
summary(margins(quadratic_logit_audit_corrupt_prob_elect_controls)) 
quadratic_logit_audit_corrupt_prob_elect_controls_margins<-margins(quadratic_logit_audit_corrupt_prob_elect_controls)
# margins at levels
summary(margins(quadratic_logit_audit_corrupt_prob_elect_controls, at=list(lfalha_total=c(1:6)),variables = "lfalha_total")) # -4% in chances of re-election (avg mg effect)
# margins at quantiles
quadratic_logit_audit_corrupt_prob_elect_controls_1q <- margins(quadratic_logit_audit_corrupt_prob_elect_controls,
                                                         at = list(lfalha_total = quantile(balance_test$lfalha_total, 0.25)), # at the first quantile 
                                                         type = "response")
quadratic_logit_audit_corrupt_prob_elect_controls_2q <- margins(quadratic_logit_audit_corrupt_prob_elect_controls,
                                                         at = list(lfalha_total = quantile(balance_test$lfalha_total, 0.50)), # at the first quantile 
                                                         type = "response")
quadratic_logit_audit_corrupt_prob_elect_controls_3q <- margins(quadratic_logit_audit_corrupt_prob_elect_controls,
                                                         at = list(lfalha_total = quantile(balance_test$lfalha_total, 0.75)), # at the first quantile 
                                                         type = "response")
export_summs(quadratic_logit_audit_corrupt_prob_elect_controls_1q, 
             quadratic_logit_audit_corrupt_prob_elect_controls_2q, 
             quadratic_logit_audit_corrupt_prob_elect_controls_3q,
             to.file="pdf",  file.name ="Table 5.3", robust = TRUE,
             scale = F, digits=6, model.names = c("Quadratic 1Q", "Quadratic 2Q", "Quadratic 3Q"))
# RESULTS
# MODELS 
stargazer(logit_audit_corrupt_prob_elect_no_controls, 
          logit_audit_corrupt_prob_elect_controls, 
          quadratic_logit_audit_corrupt_prob_elect_controls,
          title="Coefficients logit - interaction term corruption levels and timing audit (pre and post elections)",
          column.labels = c("No control", "Control","Quadratic"),
          covariate.labels = c("constant", "pre elections", "Corruption (log)",
                               "urban", "illiterate", "Gdp capita (log)",
                               "post*corruption","pre*corruption","post*corruption^2",
                               "pre*corrruption^2"),
          dep.var.labels   = "Prob (re-election)",
          type="latex",
          keep.stat = c("aic","chi2","ll","rsq","ser"),
          ci=F,
          intercept.bottom = FALSE,
          notes = "Dep variable is a dummy indicating if incumbent was re-elected")

# Table 6: prob election with interacted audited*corruption*radio ####
# without controls
logit_radio_audit_corrupt_prob_elect_no_controls <- glm(elected ~treat*lfalha_total*radio_am, 
                                                  family = binomial(link = logit), data = balance_test) #Logit model
1 - logit_radio_audit_corrupt_prob_elect_no_controls$deviance/logit_radio_audit_corrupt_prob_elect_no_controls$null.deviance # pseudo R2
summary(logit_radio_audit_corrupt_prob_elect_no_controls)
# AME
logit_radio_audit_corrupt_prob_elect_no_controls_AME<-(logitmfx(elected ~ treat +lfalha_total + radio_am+ treat*lfalha_total*radio_am, data=balance_test,
                                                          robust = F, # calculate robust SE
                                                          atmean = FALSE)) # average partial effect -"atmean" = "FALSE" calculates AMEs instead of MEMs
# margins
logit_radio_audit_corrupt_prob_elect_no_controls_margins<-summary(margins(logit_radio_audit_corrupt_prob_elect_no_controls))
# margins at levels
logit_radio_audit_corrupt_prob_elect_no_controls_margins_levels<-summary(margins(logit_radio_audit_corrupt_prob_elect_no_controls, at=list(lfalha_total=c(1:6)),variables = c("lfalha_total","radio_am") )) 

persp(logit_radio_audit_corrupt_prob_elect_no_controls,"lfalha_total","radio_am",  what = "effect",
      main = "AME eq 3 without control") # reference: https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html#The_plot()_method_for_%E2%80%9Cmargins%E2%80%9D_objects

# including radio level information
balance_test$treat_01<-ifelse(balance_test$treat=="pre-elections",0,1)
logit_radio_audit_corrupt_prob_elect_controls <- glm(elected ~treat_01*lfalha_total*radio_am, 
                                                     family = binomial(link = logit), data = balance_test)
treat_s <- with(balance_test, seq(min(treat_01), max(treat_01),length=25))
lfalha_total_s <- with(balance_test, seq(min(lfalha_total), max(lfalha_total), length=25))
pred_fun <- function(x,y, radio_am=0){
  tmp <- data.frame(treat_01 = x, lfalha_total = y, radio_am=radio_am)
  predict(logit_radio_audit_corrupt_prob_elect_controls, newdata=tmp, type="response")
}
p0 <- outer(treat_s, lfalha_total_s, pred_fun)
p1 <- outer(treat_s, lfalha_total_s, pred_fun, radio_am=1)
persp(treat_s, lfalha_total_s, p0, theta=-55, col=rgb(.75,.65, .95, .35),phi = 15,ticktype = "detailed",border = T,
      xlab = "audited pre-post polls",
      ylab="corruption level (log)",
      main = "Predicted probability eq 3 without control",sub = "audited pre-post polls = 0 or 1 & purple= no radio, grey=radio in the municipality", 
      zlab="prob(re-election)") 
par(new=TRUE)
persp(treat_s, lfalha_total_s, p1, theta=-55, col=rgb(0.1,0,0,.15), xlab="", ylab="", zlab="",phi = 15,border = T, box=F)

# change axis
radioam_s <- with(balance_test, seq(min(radio_am), max(radio_am),length=25))
lfalha_total_s <- with(balance_test, seq(min(lfalha_total), max(lfalha_total), length=25))
pred_fun <- function(x,y, treat_01=0){
  tmp <- data.frame(radio_am = x, lfalha_total = y, treat_01=treat_01)
  predict(logit_radio_audit_corrupt_prob_elect_controls, newdata=tmp, type="response")
}
p0 <- outer(radioam_s, lfalha_total_s, pred_fun)
p1 <- outer(radioam_s, lfalha_total_s, pred_fun, treat_01=1)
persp(radioam_s, lfalha_total_s, p0, theta=-67, col=rgb(.75,.65, .95, .35),phi = 15,ticktype = "detailed",border = T,
      # xlab = "audited pre-post polls",
      ylab="corruption level (log)",
      main = "Predicted probability eq 3 without control",sub = "radio_am_s = 0 or 1 & purple= audit pre, grey=audit pro", 
      zlab="prob(re-election)") 
par(new=TRUE)
persp(radioam_s, lfalha_total_s, p1, theta=-67, col=rgb(0.1,0,0,.15), xlab="", ylab="", zlab="",phi = 15,border = T, box=F)


# with controls
logit_radio_audit_corrupt_prob_elect_controls <- glm(elected ~treat*lfalha_total*radio_am + perctg_urban+illiteracy_rate+
                                                       ln_gdp_capita, 
                                                        family = binomial(link = logit), data = balance_test)
persp(logit_radio_audit_corrupt_prob_elect_controls,"lfalha_total","radio_am",  what = "effect",
      main = "AME eq 3 with control")







#x1 <- glm(hihp ~ drat * wt * am, data = mtcars, family=binomial)
# drat_s <- with(mtcars, seq(min(drat), max(drat),length=25))
# wt_s <- with(mtcars, seq(min(wt), max(wt), length=25))

# pred_fun <- function(x,y, am=0){
#   tmp <- data.frame(drat = x, wt = y, am=am)
#   predict(x1, newdata=tmp, type="response")
# }
# p0 <- outer(drat_s, wt_s, pred_fun)
# p1 <- outer(drat_s, wt_s, pred_fun, am=1)
# persp(drat_s, wt_s, p0, zlim=c(0,1), theta=-50, col=rgb(.75,.75, .75, .75), 
#       xlab = "Axle Ratio", 
#       ylab="Weight", 
#       zlab="Predicted Probability")
# 
# par(new=TRUE)
# persp(drat_s, wt_s, p1, zlim=c(0,1), theta=-50, col=rgb(1,0,0,.75), xlab="", ylab="", zlab="")





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

# previous figure 2
# drew_municipalities_first_terms<- drew_municipalities %>% 
#   filter(NAME_MUNICIPALITY %in% first_term_drew_mun$`unique(votes_per_precinct$NAME_MUNICIPALITY)`) # keeping municipalities audited twice 
# drew_municipalities_first_terms<- drew_municipalities_first_terms %>%
#   distinct(NAME_MUNICIPALITY, drew_municipalities_first_terms$year != 2013, .keep_all = T) %>% select(1:26) # but only those in 2013
# The 2008 elections happened on the 5th October (1st round) and 26th October (2nd round)
# The 2012 elections happened on the 7th October (1st round) and 28th October (2nd round)

# previous figure 4
# share_votes_by_corruption_levels<-subset(votes_per_precinct,sorteio<34)
# share_votes_by_corruption_levels<-share_votes_by_corruption_levels %>%
#   dplyr::group_by(NAME_MUNICIPALITY, UF) %>%
#   dplyr::summarize(relection_rate = ((sum(Elected)/nrow(share_votes_by_corruption_levels))*nrow(share_votes_by_corruption_levels))/100, # counts the n°of sections won by the candidate divided by all the sections and it multiples the latter calculation by the number of section (this penalizes municipalities with few sections)
#             mean_lfalha_total= mean(lfalha_total),
#             mean_lmismanagement= mean(lmismanagement),
#             mean_illiterate= mean(share_illit),
#             mean_semi_illit= mean(share_semi_illit),
#             draw=mean(sorteio),
#             share_vote_mean = mean(Share)) 
# share_votes_by_corruption_levels$relection_rate<-
#   (share_votes_by_corruption_levels$relection_rate-min(share_votes_by_corruption_levels$relection_rate))/ # normalize 0-1 re-election rates
#    (max(share_votes_by_corruption_levels$relection_rate)-min(share_votes_by_corruption_levels$relection_rate)) 
# share_votes_by_corruption_levels$treat<-ifelse(share_votes_by_corruption_levels$draw<27,"pre-elections","post-elections")

# figure 4 in lines
# plot_share_votes_by_corruption_levels = plot_share_votes_by_corruption_levels %>% 
#   mutate(ds = as.factor(treat)) %>% # Temporary changes the type of ds from integer to factor so ggplot understands
#   ggplot(aes(x = lfalha_total, y = SHARE_VOTES_CANDIDATURE, group = ds, col = ds)) +
#   stat_summary(fun = "mean") +
#   # stat_summary(geom = "line") + # Displays the mean as a line
#   # stat_summary(geom = "point") + # Displays the mean as a point
#   labs(col = "Audited:") + # Changes the name from "ds" to "Group" in the legend
#   theme_classic() + # Changes the theme
#   geom_smooth(span = 0.4, aes(fill = treat),se=F) +
#   labs(x = "Mean acts of corruption", y = "Share votes, (mean)") +
#   guides(fill="none")+
#   scale_y_continuous(labels = scales::percent)+
#   theme(legend.position = c(0.24, 0.88)) # Change aspect ratio of the plot. Not necessary, but some prefer it.
# plot_share_votes_by_corruption_levels

# # previous figure 5
# share_votes_by_corruption_levels_radio<- merge(share_votes_by_corruption_levels, drew_municipalities_first_terms, by.x=c("NAME_MUNICIPALITY", "UF"), by.y=c("NAME_MUNICIPALITY", "UF"), all=F) # identify municipalities with radio
# share_votes_by_corruption_levels_radio<-subset(share_votes_by_corruption_levels_radio,year<2013) # remove 2013 observations previously merged
# share_votes_by_corruption_levels_radio$status<-paste(share_votes_by_corruption_levels_radio$treat, share_votes_by_corruption_levels_radio$radio_am)
# share_votes_by_corruption_levels_radio %>%
#   ggplot(aes(mean_lfalha_total, share_vote_mean, color = status)) + 
#   labs(x = "Mean acts of corruption (logarithm scale)", y = "Share of votes, (mean)") + 
#   scale_color_discrete(name = "Treatment condition", labels=c("Post-elections - No radio", "Post-elections - Radio", 
#                                                               "Pre-elections - No radio", "Pre-elections - Radio"))+
#   geom_smooth(span = 0.6, aes(fill = status),se=FALSE) +
#   guides(fill="none")+
#   scale_y_continuous(labels = scales::percent)+
#   theme_bw()+
#   theme(legend.position = c(0.30, 0.84))

# Figure 5 
# OVERLEAF: With the same sample of the previous figure, \emph{Figure 5} disaggregate the previous one by the presence or absence of local radio. It would be expected that relatively lower shares of votes for incumbents ruling municipalities without local radios. The figure roughly suggest such interpretation at the middle range of the mean acts of corruption. Except from lower levels of corruption, \emph{Figure 5} follows the pattern from (\cite{ferraz_exposing_2008}, p.740). 
# plot_share_votes_by_corruption_levels$status<-paste(plot_share_votes_by_corruption_levels$treat, plot_share_votes_by_corruption_levels$radio_am)
# plot_share_votes_by_corruption_levels %>%
#   ggplot(aes(lfalha_total, SHARE_VOTES_CANDIDATURE, color = status)) + 
#   labs(x = "Mean acts of corruption (logarithm scale)", y = "Share of votes, (mean)") + 
#   scale_color_discrete(name = "Treatment condition", labels=c("Post-elections - No radio", "Post-elections - Radio", 
#                                                               "Pre-elections - No radio", "Pre-elections - Radio"))+
#   geom_smooth(span = 0.6, aes(fill = status),se=FALSE) +
#   guides(fill="none")+
#   scale_y_continuous(labels = scales::percent)+
#   theme_bw()+
#   theme(legend.position = c(0.30, 0.84))
# 
# plot_share_votes_by_corruption_levels %>%
#   # Add a new column called 'bin': cut the initial corruption in bins
#   mutate( bin=cut_width(exp(lfalha_total), width=30, boundary=0) ) %>%
#   ggplot( aes(x=bin, y=SHARE_VOTES_CANDIDATURE, fill=status)) +
#   geom_boxplot() +
#   theme_minimal()+
#   theme(text = element_text(size=10),axis.text.x = element_text(angle=45, hjust=1)) +
#   xlab("Acts of corruption")+
#   ylab("Share of votes 2008 elections")+
#   theme(legend.title=element_blank())

# Table 5
# share of votes with radio
# eval_log_share_votes_radio = lm(log_share_votes ~ treat+ radio_am +tv +log_college + high_school+ elementary+
#                                   no_elementary+ perctg_employed+ log_employer+ perctg_informal_workers+
#                                   log_self_emplyed+ perctg_urban+ illiteracy_rate +ln_gdp_capita +
#                                   perctg_earning_quarter_mwage+ log_unemploym, data = balance_test)
# GlobalCrit(eval_share_votes_radio) # AIC BIC indicates BEIJMP as best model: radio_am, high_school, perctg_informal, illiteracy rate, log_unempl
