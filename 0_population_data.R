############################################################################
# Does Political Turnover Reduces Reduces Government Efficiency?  
# This file creates population data
############################################################################
# Data sources:
# MDS - SAGI data
############################################################################
# Preambule
# R version 3.5.0
############################################################################

rm(list=ls())
options(scipen = 999) # disable sci notation

# Packages 
library(tidyverse)
library(haven)
library(stringr)
library(checkpoint)
#checkpoint("2018-01-28", checkpointLocation = tempdir()) #getting error message #REVIEW

setwd("~")
if(grepl("bueno",getwd()) == TRUE){#Allow for different paths in our computers
  home <- "~/Dropbox/"
}else{
  home <- "~/Dropbox/"#add german's path here
}

files <- list.files(paste0(home, "change_govt/population_data/organized/"))

temp05 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2005"),
                          skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccin", 
                          col_names = c("n", "uf", "municipio", "cod_ibge", "pop05")) %>% 
          slice(c(-1, -5572:-5575))

temp06 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2006"),
                   skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccin", 
                   col_names = c("n", "uf", "municipio", "cod_ibge", "pop06")) %>% 
          slice(c(-1, -5572:-5575)) %>% select(cod_ibge, pop06)

temp07 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2007"),
                   skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccin", 
                   col_names = c("n", "uf", "municipio", "cod_ibge", "pop07")) %>% 
          slice(c(-1, -5572:-5575)) %>% select(cod_ibge, pop07)

temp08 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2008"),
                   skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccin", 
                   col_names = c("n", "uf", "municipio", "cod_ibge", "pop08")) %>% 
  slice(c(-1, -5572:-5575)) %>% select(cod_ibge, pop08)

temp09 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2009"),
                   skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccin", 
                   col_names = c("n", "uf", "municipio", "cod_ibge", "pop09")) %>% 
  slice(c(-1, -5572:-5575)) %>% select(cod_ibge, pop09)

temp11 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2011"),
                   skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccin", 
                   col_names = c("n", "uf", "municipio", "cod_ibge", "pop11")) %>% 
  slice(c(-1, -5572:-5575)) %>% select(cod_ibge, pop11)

temp12 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2012"),
                   skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccin", 
                   col_names = c("n", "uf", "municipio", "cod_ibge", "pop12")) %>% 
  slice(c(-1, -5572:-5575)) %>% select(cod_ibge, pop12)

temp13 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2013"),
                   skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccin", 
                   col_names = c("n", "uf", "municipio", "cod_ibge", "pop13")) %>% 
  slice(c(-1, -5572:-5575)) %>% select(cod_ibge, pop13)

temp14 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2014"),
                   skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccin", 
                   col_names = c("n", "uf", "municipio", "cod_ibge", "pop14")) %>% 
  slice(c(-1, -5572:-5575)) %>% select(cod_ibge, pop14)

temp15 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2015"),
                   skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccin", 
                   col_names = c("n", "uf", "municipio", "cod_ibge", "pop15")) %>% 
  slice(c(-1, -5572:-5575)) %>% select(cod_ibge, pop15)

temp16 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2016"),
                   skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccin", 
                   col_names = c("n", "uf", "municipio", "cod_ibge", "pop16")) %>% 
  slice(c(-1, -5572:-5575)) %>% select(cod_ibge, pop16)

temp17 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2017"),
                   skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccin", 
                   col_names = c("n", "uf", "municipio", "cod_ibge", "pop17")) %>% 
  slice(c(-1, -5572:-5575)) %>% select(cod_ibge, pop17)

#2010 is different
temp10 <- read_tsv(paste0(home, "change_govt/population_data/organized/planilha_11_5_2010"),
                   skip = 5, locale = locale(encoding = "latin1", decimal_mark = ","), col_types = "iccinnn", 
                   col_names = c("n", "uf", "municipio", "cod_ibge", "pop10u", "pop10r", "pop10")) %>% 
  slice(c(-1, -5572:-5575)) %>% select(cod_ibge, pop10)

#Multimerge
pop_all <- temp05 %>% left_join(temp06, by = "cod_ibge") %>% left_join(temp07, by = "cod_ibge") %>%
           left_join(temp08, by = "cod_ibge") %>% left_join(temp09, by = "cod_ibge") %>%
           left_join(temp10, by = "cod_ibge") %>% left_join(temp11, by = "cod_ibge") %>%
           left_join(temp12, by = "cod_ibge") %>% left_join(temp13, by = "cod_ibge") %>%
           left_join(temp14, by = "cod_ibge") %>% left_join(temp15, by = "cod_ibge") %>%
           left_join(temp16, by = "cod_ibge") %>% left_join(temp17, by = "cod_ibge")
  
#smell tests
stopifnot(nrow(pop_all)==5570)  
stopifnot(length(unique(pop_all$cod_ibge))==5570)
glimpse(pop_all)
summary(pop_all$pop05) #Mininum is 0?
summary(pop_all$pop07)
summary(pop_all$pop08)
summary(pop_all$pop09)
summary(pop_all$pop10)
summary(pop_all$pop11)
summary(pop_all$pop12)
summary(pop_all$pop13)
summary(pop_all$pop14)
summary(pop_all$pop15)
summary(pop_all$pop16)
summary(pop_all$pop17)

save(pop_all, file = paste0(home, "change_govt/population_data/pop_05_17.Rda"))
