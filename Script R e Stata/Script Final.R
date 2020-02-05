#################################################
#                                               #
#  PPGE/UFPB, 2019                              #
#  Dissertação                                  #
#  Nome: Pedro Jorge Holanda Alves              #
#  Orientador: Professor Jevuks Matheus Araújo  #
#                                               #
#################################################

# Definir Diretório ----

setwd("D:\\Pedro Jorge\\UFPB\\Mestrado\\Dissertação\\R")
setwd("F:\\Pedro Jorge\\UFPB\\Mestrado\\Dissertação\\R")

#setwd("G:\\Pedro Jorge\\UFPB\\Mestrado\\Dissertação\\R")

# Lendo pacotes ----

library(readxl) ; library(car)    ; library(tseries)   ; library(maptools)  ;
library(stringi); library(rgdal)  ; library(spdep)     ; library(GWmodel)   ;
library(splm)   ; library(ggplot2); library(ggplot2)   ; library(sf)        ;
library(dplyr)  ; library(tidyr)  ; library(devtools)  ; library(tseries)   ;
library(lmtest) ; library(sf)     ; library(maptools)  ; library(GWmodel)   ;
library(readr)  ; library(foreign); library(plyr)      ; library(geobr)     ;
library(sidrar) ; library(rgdal)  ; library(magrittr)  ; library(data.table);
library(rdd)


# Arquivo janela ----
rm(list = ls())
edu <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
fund <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
aten_bas <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
adm_geral <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
saude <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
pessoal <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
rcl <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
dcl <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
despesa <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
receita <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
cotapartefpm <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
transferencias <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
tributario <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
Administração <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
Previdência <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
capital <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
Amortização <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
encargo_divida <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
investimentos <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
iptu <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
iss <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
urbanismo <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
cultura <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
legislativo <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
estado <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
seg_publica <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
agricultura <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
transporte <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
desporto_lazer <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
encargos_esp <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)
uniao <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)

# Finbra 2011 ----

finbra <- read_excel("finbra2011.xlsx", col_names=TRUE) 

resultado <- edu %>% 
  group_by(cod_mun) %>%
  left_join(finbra, by=c("cod_mun"="cod_mun"))

edu$"2011" <- resultado$Educação
saude$"2011" <- resultado$Saúde
pessoal$"2011" <- resultado$Pessoal
rcl$"2011" <- resultado$rcl
tributario$"2011" <- resultado$tribut
Administração$"2011" <- resultado$Administração
cotapartefpm$"2011" <- resultado$fpm

# Finbra 2012 ----

finbra <- read_excel("finbra2012.xlsx", col_names=TRUE) 

resultado <- edu %>% 
  group_by(cod_mun) %>%
  left_join(finbra, by=c("cod_mun"="cod_mun"))
edu$"2012" <- resultado$Educação
saude$"2012" <- resultado$Saúde
pessoal$"2012" <- resultado$Pessoal
rcl$"2012" <- resultado$rcl
tributario$"2012" <- resultado$t
Administração$"2012" <- resultado$Administração
iptu$"2012" <- resultado$iptu
iss$"2012" <- resultado$iss
capital$"2012" <- resultado$capital
aten_bas$"2012" <- resultado$aten_bas
fund$"2012" <- resultado$fund
adm_geral$"2012" <- resultado$adm_geral
estado$"2012" <- resultado$estado
uniao$"2012" <- resultado$uniao...37
cotapartefpm$"2012" <- resultado$fpm
despesa$"2012" <- resultado$despesa
receita$"2012" <- resultado$receita
legislativo$"2012" <- resultado$legislativo
transferencias$"2012" <- resultado$transferencias
seg_publica$"2012" <- resultado$seg_publica
agricultura$"2012" <- resultado$agricultura
transporte$"2012" <- resultado$transporte
desporto_lazer$"2012" <- resultado$desp_lazer
encargos_esp$"2012" <- resultado$encargos_esp

# Finbra 2013 ----

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="06 - Segurança Pública" & Coluna=="Despesas Empenhadas") 

resultado <- seg_publica %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

seg_publica$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="20 - Agricultura" & Coluna=="Despesas Empenhadas") 

resultado <- agricultura %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

agricultura$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="26 - Transporte" & Coluna=="Despesas Empenhadas") 

resultado <- transporte %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

transporte$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="27 - Desporto e Lazer" & Coluna=="Despesas Empenhadas") 

resultado <- desporto_lazer %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

desporto_lazer$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="28 - Encargos Especiais" & Coluna=="Despesas Empenhadas") 

resultado <- encargos_esp %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

encargos_esp$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="12 - Educação" & Coluna=="Despesas Empenhadas") 

resultado <- edu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

edu$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="04.122 - Administração Geral" & Coluna=="Despesas Empenhadas") 

resultado <- adm_geral %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

adm_geral$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="12.361 - Ensino Fundamental" & Coluna=="Despesas Empenhadas") 

resultado <- fund %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

fund$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="10.301 - Atenção Básica" & Coluna=="Despesas Empenhadas") 

resultado <- aten_bas %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

aten_bas$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="10 - Saúde" &
           Coluna=="Despesas Empenhadas")

resultado <- saude %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

saude$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="04 - Administração" &
           Coluna=="Despesas Empenhadas")

resultado <- Administração %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Administração$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="09 - Previdência Social" &
           Coluna=="Despesas Empenhadas")

resultado <- Previdência %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Previdência$"2013" <- resultado$Valor

finbra <- read.csv("pes2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="3.1.00.00.00.00 - Pessoal e Encargos Sociais" &
           Coluna=="Despesas Empenhadas")

resultado <- pessoal %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

pessoal$"2013" <- resultado$Valor

finbra <- read.csv("pes2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.0.00.00.00.00 - Despesas de Capital" &
           Coluna=="Despesas Empenhadas")

resultado <- capital %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

capital$"2013" <- resultado$Valor

finbra <- read.csv("pes2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.4.00.00.00.00 - Investimentos" &
           Coluna=="Despesas Empenhadas")

resultado <- investimentos %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

investimentos$"2013" <- resultado$Valor

finbra <- read.csv("pes2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.6.00.00.00.00 - Amortização da Dívida" &
           Coluna=="Despesas Empenhadas")

resultado <- Amortização %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Amortização$"2013" <- resultado$Valor

finbra <- read.csv("pes2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="3.2.00.00.00.00 - Juros e Encargos da Dívida" &
           Coluna=="Despesas Empenhadas")

resultado <- encargo_divida %>% 
  group_by(Cod.IBGE) %>%
  left_join(encargo_divida, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

encargo_divida$"2013" <- resultado$Valor

finbra <- read.csv("pes2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="Total Despesa" &
           Coluna=="Despesas Empenhadas")

resultado <- despesa %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

despesa$"2013" <- resultado$Valor

finbra <- read.csv("receita2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="Total Receitas" &
           Coluna=="Receitas Realizadas")

resultado <- receita %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

receita$"2013" <- resultado$Valor

finbra <- read.csv("receita2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.0.0.00.00.00 - Receita Tributária" &
           Coluna=="Receitas Realizadas")

resultado <- tributario %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

tributario$"2013" <- resultado$Valor

finbra <- read.csv("receita2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU" &
           Coluna=="Receitas Realizadas")

resultado <- iptu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

iptu$"2013" <- resultado$Valor

finbra <- read.csv("receita2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU" &
           Coluna=="Receitas Realizadas")

resultado <- iptu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

iptu$"2013" <- resultado$Valor

finbra <- read.csv("receita2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.1.00.00.00 - Transferências da União" &
           Coluna=="Receitas Realizadas")

resultado <- uniao %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

uniao$"2013" <- resultado$Valor

finbra <- read.csv("receita2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.2.00.00.00 - Transferências dos Estados" &
           Coluna=="Receitas Realizadas")

resultado <- estado %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

estado$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="01 - Legislativa" &
           Coluna=="Despesas Empenhadas")

resultado <- legislativo %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])


legislativo$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="13 - Cultura" &
           Coluna=="Despesas Empenhadas")

resultado <- cultura %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])


cultura$"2013" <- resultado$Valor

finbra <- read.csv("finbra2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="15 - Urbanismo" &
           Coluna=="Despesas Empenhadas")

resultado <- urbanismo %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

urbanismo$"2013" <- resultado$Valor

finbra <- read.csv("receita2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.0.0.00.00.00 - Transferências Correntes" &
           Coluna=="Receitas Realizadas")

resultado <- transferencias %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

transferencias$"2013" <- resultado$Valor

finbra <- read.csv("receita2013.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.1.01.02.00 - Cota-Parte do Fundo de Participação dos Municípios ¿ FPM" &
           Coluna=="Receitas Realizadas")

resultado <- cotapartefpm %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

cotapartefpm$"2013" <- resultado$Valor

finbra <- read.csv("rcl2013.csv", header = TRUE, skip = 5, sep = ";", dec = ",") %>% 
  subset(Conta=="RECEITA CORRENTE LÍQUIDA (III) = (I - II)" &
           Coluna=="TOTAL (ÚLTIMOS 12 MESES)")

resultado <- rcl %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

rcl$"2013" <- resultado$Valor

finbra <- read.csv("dcl2013.csv", header = TRUE, skip = 5, sep = ";", dec = ",") %>% 
  subset(Conta=="% da DCL sobre a RCL (III/RCL)" &
           Coluna=="Até o 3º Quadrimestre")

resultado <- dcl %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

dcl$"2013" <- resultado$Valor

# Finbra 2014 ----

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="06 - Segurança Pública" & Coluna=="Despesas Empenhadas") 

resultado <- seg_publica %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

seg_publica$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="20 - Agricultura" & Coluna=="Despesas Empenhadas") 

resultado <- agricultura %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

agricultura$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="26 - Transporte" & Coluna=="Despesas Empenhadas") 

resultado <- transporte %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

transporte$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="27 - Desporto e Lazer" & Coluna=="Despesas Empenhadas") 

resultado <- desporto_lazer %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

desporto_lazer$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="28 - Encargos Especiais" & Coluna=="Despesas Empenhadas") 

resultado <- encargos_esp %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

encargos_esp$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="12 - Educação" & Coluna=="Despesas Empenhadas") 

resultado <- edu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

edu$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="04.122 - Administração Geral" & Coluna=="Despesas Empenhadas") 

resultado <- adm_geral %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

adm_geral$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="12.361 - Ensino Fundamental" & Coluna=="Despesas Empenhadas") 

resultado <- fund %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

fund$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="10.301 - Atenção Básica" & Coluna=="Despesas Empenhadas") 

resultado <- aten_bas %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

aten_bas$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="10 - Saúde" &
           Coluna=="Despesas Empenhadas")

resultado <- saude %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

saude$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="04 - Administração" &
           Coluna=="Despesas Empenhadas")

resultado <- Administração %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Administração$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="09 - Previdência Social" &
           Coluna=="Despesas Empenhadas")

resultado <- Previdência %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Previdência$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="01 - Legislativa" &
           Coluna=="Despesas Empenhadas")

resultado <- legislativo %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])


legislativo$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="13 - Cultura" &
           Coluna=="Despesas Empenhadas")

resultado <- cultura %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])


cultura$"2014" <- resultado$Valor

finbra <- read.csv("finbra2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="15 - Urbanismo" &
           Coluna=="Despesas Empenhadas")

resultado <- urbanismo %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

urbanismo$"2014" <- resultado$Valor

finbra <- read.csv("pes2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="3.1.00.00.00.00 - Pessoal e Encargos Sociais" &
           Coluna=="Despesas Empenhadas")

resultado <- pessoal %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

pessoal$"2014" <- resultado$Valor

finbra <- read.csv("pes2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.0.00.00.00.00 - Despesas de Capital" &
           Coluna=="Despesas Empenhadas")

resultado <- capital %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

capital$"2014" <- resultado$Valor

finbra <- read.csv("pes2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.4.00.00.00.00 - Investimentos" &
           Coluna=="Despesas Empenhadas")

resultado <- investimentos %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

investimentos$"2014" <- resultado$Valor

finbra <- read.csv("pes2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.6.00.00.00.00 - Amortização da Dívida" &
           Coluna=="Despesas Empenhadas")

resultado <- Amortização %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Amortização$"2014" <- resultado$Valor

finbra <- read.csv("pes2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="3.2.00.00.00.00 - Juros e Encargos da Dívida" &
           Coluna=="Despesas Empenhadas")

resultado <- encargo_divida %>% 
  group_by(Cod.IBGE) %>%
  left_join(encargo_divida, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

encargo_divida$"2014" <- resultado$Valor

finbra <- read.csv("pes2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="Total Geral da Despesa" &
           Coluna=="Despesas Empenhadas")

resultado <- despesa %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

despesa$"2014" <- resultado$Valor

finbra <- read.csv("receita2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.1.00.00.00 - Transferências da União" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- uniao %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

uniao$"2014" <- resultado$Valor

finbra <- read.csv("receita2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.2.00.00.00 - Transferências dos Estados" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- estado %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

estado$"2014" <- resultado$Valor

finbra <- read.csv("receita2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="Total Receitas" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- receita %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

receita$"2014" <- resultado$Valor

finbra <- read.csv("receita2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.0.0.00.00.00 - Receita Tributária" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- tributario %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

tributario$"2014" <- resultado$Valor

finbra <- read.csv("receita2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- iptu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

iptu$"2014" <- resultado$Valor

finbra <- read.csv("receita2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.1.3.05.00.00 - Imposto sobre Serviços de Qualquer Natureza ¿ ISSQN" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- iss %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

iss$"2014" <- resultado$Valor

finbra <- read.csv("receita2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.0.0.00.00.00 - Transferências Correntes" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- transferencias %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

transferencias$"2014" <- resultado$Valor

finbra <- read.csv("receita2014.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.1.01.02.00 - Cota-Parte do Fundo de Participação dos Municípios ¿ FPM" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- cotapartefpm %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

cotapartefpm$"2014" <- resultado$Valor

finbra <- read.csv("rcl2014.csv", header = TRUE, skip = 5, sep = ";", dec = ",") %>% 
  subset(Conta=="RECEITA CORRENTE LÍQUIDA (III) = (I - II)" &
           Coluna=="TOTAL (ÚLTIMOS 12 MESES)")

resultado <- rcl %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

rcl$"2014" <- resultado$Valor

finbra <- read.csv("dcl2014.csv", header = TRUE, skip = 5, sep = ";", dec = ",") %>% 
  subset(Conta=="% da DCL sobre a RCL (III/RCL)" &
           Coluna=="At¿ o 3¿ Quadrimestre")

resultado <- dcl %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

dcl$"2014" <- resultado$Valor

# Finbra 2015 ----

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="06 - Segurança Pública" & Coluna=="Despesas Empenhadas") 

resultado <- seg_publica %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

seg_publica$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="20 - Agricultura" & Coluna=="Despesas Empenhadas") 

resultado <- agricultura %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

agricultura$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="26 - Transporte" & Coluna=="Despesas Empenhadas") 

resultado <- transporte %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

transporte$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="27 - Desporto e Lazer" & Coluna=="Despesas Empenhadas") 

resultado <- desporto_lazer %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

desporto_lazer$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="28 - Encargos Especiais" & Coluna=="Despesas Empenhadas") 

resultado <- encargos_esp %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

encargos_esp$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="12 - Educação" & Coluna=="Despesas Empenhadas") 

resultado <- edu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

edu$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="10 - Saúde" &
           Coluna=="Despesas Empenhadas")

resultado <- saude %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

saude$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="04.122 - Administração Geral" & Coluna=="Despesas Empenhadas") 

resultado <- adm_geral %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

adm_geral$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="12.361 - Ensino Fundamental" & Coluna=="Despesas Empenhadas") 

resultado <- fund %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

fund$"2015" <- resultado$Valor

finbra <- read.csv("receita2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.1.00.00.00 - Transferências da União" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- uniao %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

uniao$"2015" <- resultado$Valor

finbra <- read.csv("receita2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.2.00.00.00 - Transferências dos Estados" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- estado %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

estado$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="10.301 - Atenção Básica" & Coluna=="Despesas Empenhadas") 

resultado <- aten_bas %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

aten_bas$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="01 - Legislativa" &
           Coluna=="Despesas Empenhadas")

resultado <- legislativo %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])


legislativo$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="13 - Cultura" &
           Coluna=="Despesas Empenhadas")

resultado <- cultura %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])


cultura$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="15 - Urbanismo" &
           Coluna=="Despesas Empenhadas")

resultado <- urbanismo %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

urbanismo$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="04 - Administração" &
           Coluna=="Despesas Empenhadas")

resultado <- Administração %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Administração$"2015" <- resultado$Valor

finbra <- read.csv("finbra2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="09 - Previdência Social" &
           Coluna=="Despesas Empenhadas")

resultado <- Previdência %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Previdência$"2015" <- resultado$Valor

finbra <- read.csv("pes2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="3.1.00.00.00.00 - Pessoal e Encargos Sociais" &
           Coluna=="Despesas Empenhadas")

resultado <- pessoal %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

pessoal$"2015" <- resultado$Valor

finbra <- read.csv("pes2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.0.00.00.00.00 - Despesas de Capital" &
           Coluna=="Despesas Empenhadas")

resultado <- capital %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

capital$"2015" <- resultado$Valor

finbra <- read.csv("pes2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.4.00.00.00.00 - Investimentos" &
           Coluna=="Despesas Empenhadas")

resultado <- investimentos %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

investimentos$"2015" <- resultado$Valor

finbra <- read.csv("pes2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.6.00.00.00.00 - Amortização da Dívida" &
           Coluna=="Despesas Empenhadas")

resultado <- Amortização %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Amortização$"2015" <- resultado$Valor

finbra <- read.csv("pes2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="3.2.00.00.00.00 - Juros e Encargos da Dívida" &
           Coluna=="Despesas Empenhadas")

resultado <- encargo_divida %>% 
  group_by(Cod.IBGE) %>%
  left_join(encargo_divida, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

encargo_divida$"2015" <- resultado$Valor

finbra <- read.csv("pes2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="Total Geral da Despesa" &
           Coluna=="Despesas Empenhadas")

resultado <- despesa %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

despesa$"2015" <- resultado$Valor

finbra <- read.csv("receita2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="Total Receitas" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- receita %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

receita$"2015" <- resultado$Valor

finbra <- read.csv("receita2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.0.0.00.00.00 - Receita Tributária" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- tributario %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

tributario$"2015" <- resultado$Valor

finbra <- read.csv("receita2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- iptu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

iptu$"2015" <- resultado$Valor

finbra <- read.csv("receita2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.1.3.05.00.00 - Imposto sobre Serviços de Qualquer Natureza ¿ ISSQN" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- iss %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

iss$"2015" <- resultado$Valor

finbra <- read.csv("receita2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.0.0.00.00.00 - Transferências Correntes" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- transferencias %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

transferencias$"2015" <- resultado$Valor

finbra <- read.csv("receita2015.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.1.01.02.00 - Cota-Parte do Fundo de Participação dos Municípios ¿ FPM" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- cotapartefpm %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

cotapartefpm$"2015" <- resultado$Valor

finbra <- read.csv("rcl2015.csv", header = TRUE, skip = 5, sep = ";", dec = ",") %>% 
  subset(Conta=="RECEITA CORRENTE LÍQUIDA (III) = (I - II)" &
           Coluna=="TOTAL (ÚLTIMOS 12 MESES)")

resultado <- rcl %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

rcl$"2015" <- resultado$Valor

finbra <- read.csv("dcl2015.csv", header = TRUE, skip = 5, sep = ";", dec = ",") %>% 
  subset(Conta=="% da DCL sobre a RCL (III/RCL)" &
           Coluna=="At¿ o 3¿ Quadrimestre")

resultado <- dcl %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

dcl$"2015" <- resultado$Valor

# Finbra 2016 ----

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="06 - Segurança Pública" & Coluna=="Despesas Empenhadas") 

resultado <- seg_publica %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

seg_publica$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="20 - Agricultura" & Coluna=="Despesas Empenhadas") 

resultado <- agricultura %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

agricultura$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="26 - Transporte" & Coluna=="Despesas Empenhadas") 

resultado <- transporte %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

transporte$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="27 - Desporto e Lazer" & Coluna=="Despesas Empenhadas") 

resultado <- desporto_lazer %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

desporto_lazer$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="28 - Encargos Especiais" & Coluna=="Despesas Empenhadas") 

resultado <- encargos_esp %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

encargos_esp$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="12 - Educação" & Coluna=="Despesas Empenhadas") 

resultado <- edu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

edu$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="10 - Saúde" &
           Coluna=="Despesas Empenhadas")

resultado <- saude %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

saude$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="04.122 - Administração Geral" & Coluna=="Despesas Empenhadas") 

resultado <- adm_geral %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

adm_geral$"2016" <- resultado$Valor

finbra <- read.csv("receita2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.1.00.00.00 - Transferências da União" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- uniao %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

uniao$"2016" <- resultado$Valor

finbra <- read.csv("receita2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.2.00.00.00 - Transferências dos Estados" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- estado %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

estado$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="12.361 - Ensino Fundamental" & Coluna=="Despesas Empenhadas") 

resultado <- fund %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

fund$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="10.301 - Atenção Básica" & Coluna=="Despesas Empenhadas") 

resultado <- aten_bas %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

aten_bas$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="04 - Administração" &
           Coluna=="Despesas Empenhadas")

resultado <- Administração %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Administração$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="09 - Previdência Social" &
           Coluna=="Despesas Empenhadas")

resultado <- Previdência %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Previdência$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="01 - Legislativa" &
           Coluna=="Despesas Empenhadas")

resultado <- legislativo %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])


legislativo$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="13 - Cultura" &
           Coluna=="Despesas Empenhadas")

resultado <- cultura %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])


cultura$"2016" <- resultado$Valor

finbra <- read.csv("finbra2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="15 - Urbanismo" &
           Coluna=="Despesas Empenhadas")

resultado <- urbanismo %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

urbanismo$"2016" <- resultado$Valor

finbra <- read.csv("pes2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="3.1.00.00.00.00 - Pessoal e Encargos Sociais" &
           Coluna=="Despesas Empenhadas")

resultado <- pessoal %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

pessoal$"2016" <- resultado$Valor

finbra <- read.csv("pes2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.0.00.00.00.00 - Despesas de Capital" &
           Coluna=="Despesas Empenhadas")

resultado <- capital %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

capital$"2016" <- resultado$Valor

finbra <- read.csv("pes2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.4.00.00.00.00 - Investimentos" &
           Coluna=="Despesas Empenhadas")

resultado <- investimentos %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

investimentos$"2016" <- resultado$Valor

finbra <- read.csv("pes2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.6.00.00.00.00 - Amortização da Dívida" &
           Coluna=="Despesas Empenhadas")

resultado <- Amortização %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Amortização$"2016" <- resultado$Valor

finbra <- read.csv("pes2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="3.2.00.00.00.00 - Juros e Encargos da Dívida" &
           Coluna=="Despesas Empenhadas")

resultado <- encargo_divida %>% 
  group_by(Cod.IBGE) %>%
  left_join(encargo_divida, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

encargo_divida$"2016" <- resultado$Valor

finbra <- read.csv("pes2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="Total Geral da Despesa" &
           Coluna=="Despesas Empenhadas")

resultado <- despesa %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

despesa$"2016" <- resultado$Valor

finbra <- read.csv("receita2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="Total Receitas" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- receita %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

receita$"2016" <- resultado$Valor

finbra <- read.csv("receita2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.0.0.00.00.00 - Receita Tributária" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- tributario %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

tributario$"2016" <- resultado$Valor

finbra <- read.csv("receita2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- iptu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

iptu$"2016" <- resultado$Valor

finbra <- read.csv("receita2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.1.3.05.00.00 - Imposto sobre Serviços de Qualquer Natureza ¿ ISSQN" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- iss %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

iss$"2016" <- resultado$Valor

finbra <- read.csv("receita2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.0.0.00.00.00 - Transferências Correntes" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- transferencias %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

transferencias$"2016" <- resultado$Valor

finbra <- read.csv("receita2016.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.1.01.02.00 - Cota-Parte do Fundo de Participação dos Municípios ¿ FPM" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- cotapartefpm %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

cotapartefpm$"2016" <- resultado$Valor

finbra <- read.csv("rcl2016.csv", header = TRUE, skip = 5, sep = ";", dec = ",") %>% 
  subset(Conta=="RECEITA CORRENTE LÍQUIDA (III) = (I - II)" &
           Coluna=="TOTAL (ÚLTIMOS 12 MESES)")

resultado <- rcl %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

rcl$"2016" <- resultado$Valor

finbra <- read.csv("dcl2016.csv", header = TRUE, skip = 5, sep = ";", dec = ",") %>% 
  subset(Conta=="% da DCL sobre a RCL (III/RCL)" &
           Coluna=="At¿ o 3¿ Quadrimestre")

resultado <- dcl %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

dcl$"2016" <- resultado$Valor

# Finbra 2017 ----

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="12 - Educação" & Coluna=="Despesas Empenhadas") 

resultado <- edu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

edu$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="10 - Saúde" &
           Coluna=="Despesas Empenhadas")

resultado <- saude %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

saude$"2017" <- resultado$Valor

finbra <- read.csv("receita2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.1.00.00.00 - Transferências da União" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- uniao %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

uniao$"2017" <- resultado$Valor

finbra <- read.csv("receita2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.2.00.00.00 - Transferências dos Estados" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- estado %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

estado$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="04 - Administração" &
           Coluna=="Despesas Empenhadas")

resultado <- Administração %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Administração$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="04.122 - Administração Geral" & Coluna=="Despesas Empenhadas") 

resultado <- adm_geral %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

adm_geral$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="12.361 - Ensino Fundamental" & Coluna=="Despesas Empenhadas") 

resultado <- fund %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

fund$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="10.301 - Atenção Básica" & Coluna=="Despesas Empenhadas") 

resultado <- aten_bas %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

aten_bas$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="09 - Previdência Social" &
           Coluna=="Despesas Empenhadas")

resultado <- Previdência %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Previdência$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="01 - Legislativa" &
           Coluna=="Despesas Empenhadas")

resultado <- legislativo %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])


legislativo$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="13 - Cultura" &
           Coluna=="Despesas Empenhadas")

resultado <- cultura %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])


cultura$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="15 - Urbanismo" &
           Coluna=="Despesas Empenhadas")

resultado <- urbanismo %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

urbanismo$"2017" <- resultado$Valor

finbra <- read.csv("pes2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="3.1.00.00.00.00 - Pessoal e Encargos Sociais" &
           Coluna=="Despesas Empenhadas")

resultado <- pessoal %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

pessoal$"2017" <- resultado$Valor

finbra <- read.csv("pes2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.0.00.00.00.00 - Despesas de Capital" &
           Coluna=="Despesas Empenhadas")

resultado <- capital %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

capital$"2017" <- resultado$Valor

finbra <- read.csv("pes2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.4.00.00.00.00 - Investimentos" &
           Coluna=="Despesas Empenhadas")

resultado <- investimentos %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

investimentos$"2017" <- resultado$Valor

finbra <- read.csv("pes2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.6.00.00.00.00 - Amortização da Dívida" &
           Coluna=="Despesas Empenhadas")

resultado <- Amortização %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Amortização$"2017" <- resultado$Valor

finbra <- read.csv("pes2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="3.2.00.00.00.00 - Juros e Encargos da Dívida" &
           Coluna=="Despesas Empenhadas")

resultado <- encargo_divida %>% 
  group_by(Cod.IBGE) %>%
  left_join(encargo_divida, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

encargo_divida$"2017" <- resultado$Valor

finbra <- read.csv("pes2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="Total Geral da Despesa" &
           Coluna=="Despesas Empenhadas")

resultado <- despesa %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

despesa$"2017" <- resultado$Valor

finbra <- read.csv("receita2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="Total Receitas" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- receita %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

receita$"2017" <- resultado$Valor

finbra <- read.csv("receita2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.0.0.00.00.00 - Receita Tributária" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- tributario %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

tributario$"2017" <- resultado$Valor

finbra <- read.csv("receita2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- iptu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

iptu$"2017" <- resultado$Valor

finbra <- read.csv("receita2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.1.3.05.00.00 - Imposto sobre Serviços de Qualquer Natureza ¿ ISSQN" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- iss %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

iss$"2017" <- resultado$Valor

finbra <- read.csv("receita2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.0.0.00.00.00 - Transferências Correntes" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- transferencias %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

transferencias$"2017" <- resultado$Valor

finbra <- read.csv("receita2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.1.01.02.00 - Cota-Parte do Fundo de Participação dos Municípios ¿ FPM" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- cotapartefpm %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

cotapartefpm$"2017" <- resultado$Valor

finbra <- read.csv("rcl2017.csv", header = TRUE, skip = 5, sep = ";", dec = ",") %>% 
  subset(Conta=="RECEITA CORRENTE LÍQUIDA (III) = (I - II)" &
           Coluna=="TOTAL (ÚLTIMOS 12 MESES)")

resultado <- rcl %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

rcl$"2017" <- resultado$Valor

finbra <- read.csv("dcl2017.csv", header = TRUE, skip = 5, sep = ";", dec = ",") %>% 
  subset(Conta=="% da DCL sobre a RCL (III/RCL)" &
           Coluna=="At¿ o 3¿ Quadrimestre")

resultado <- dcl %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

dcl$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="06 - Segurança Pública" & Coluna=="Despesas Empenhadas") 

resultado <- seg_publica %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

seg_publica$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="20 - Agricultura" & Coluna=="Despesas Empenhadas") 

resultado <- agricultura %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

agricultura$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="26 - Transporte" & Coluna=="Despesas Empenhadas") 

resultado <- transporte %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

transporte$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="27 - Desporto e Lazer" & Coluna=="Despesas Empenhadas") 

resultado <- desporto_lazer %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

desporto_lazer$"2017" <- resultado$Valor

finbra <- read.csv("finbra2017.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="28 - Encargos Especiais" & Coluna=="Despesas Empenhadas") 

resultado <- encargos_esp %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

encargos_esp$"2017" <- resultado$Valor

# Finbra 2018 ----

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="06 - Segurança Pública" & Coluna=="Despesas Empenhadas") 

resultado <- seg_publica %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

seg_publica$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="20 - Agricultura" & Coluna=="Despesas Empenhadas") 

resultado <- agricultura %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

agricultura$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="26 - Transporte" & Coluna=="Despesas Empenhadas") 

resultado <- transporte %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

transporte$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="27 - Desporto e Lazer" & Coluna=="Despesas Empenhadas") 

resultado <- desporto_lazer %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

desporto_lazer$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="28 - Encargos Especiais" & Coluna=="Despesas Empenhadas") 

resultado <- encargos_esp %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

encargos_esp$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="04.122 - Administração Geral" & Coluna=="Despesas Empenhadas") 

resultado <- adm_geral %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

adm_geral$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="12.361 - Ensino Fundamental" & Coluna=="Despesas Empenhadas") 

resultado <- fund %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

fund$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="10.301 - Atenção Básica" & Coluna=="Despesas Empenhadas") 

resultado <- aten_bas %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

aten_bas$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="12 - Educação" & Coluna=="Despesas Empenhadas") 

resultado <- edu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE"))

edu$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="10 - Saúde" &
           Coluna=="Despesas Empenhadas")

resultado <- saude %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

saude$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="04 - Administração" &
           Coluna=="Despesas Empenhadas")

resultado <- Administração %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Administração$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="09 - Previdência Social" &
           Coluna=="Despesas Empenhadas")

resultado <- Previdência %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Previdência$"2018" <- resultado$Valor


finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="01 - Legislativa" &
           Coluna=="Despesas Empenhadas")

resultado <- legislativo %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])


legislativo$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="13 - Cultura" &
           Coluna=="Despesas Empenhadas")

resultado <- cultura %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])


cultura$"2018" <- resultado$Valor

finbra <- read.csv("finbra2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="15 - Urbanismo" &
           Coluna=="Despesas Empenhadas")

resultado <- urbanismo %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

urbanismo$"2018" <- resultado$Valor

finbra <- read.csv("pes2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="3.1.00.00.00 - Pessoal e Encargos Sociais" &
           Coluna=="Despesas Empenhadas")

resultado <- pessoal %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

pessoal$"2018" <- resultado$Valor

finbra <- read.csv("pes2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.0.00.00.00 - Despesas de Capital" &
           Coluna=="Despesas Empenhadas")

resultado <- capital %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

capital$"2018" <- resultado$Valor

finbra <- read.csv("pes2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.4.00.00.00 - Investimentos" &
           Coluna=="Despesas Empenhadas")

resultado <- investimentos %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

investimentos$"2018" <- resultado$Valor

finbra <- read.csv("pes2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="4.6.00.00.00 - Amortização da Dívida" &
           Coluna=="Despesas Empenhadas")

resultado <- Amortização %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

Amortização$"2018" <- resultado$Valor

finbra <- read.csv("pes2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="3.2.00.00.00 - Juros e Encargos da Dívida" &
           Coluna=="Despesas Empenhadas")

resultado <- encargo_divida %>% 
  group_by(Cod.IBGE) %>%
  left_join(encargo_divida, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

encargo_divida$"2018" <- resultado$Valor

finbra <- read.csv("pes2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="Total Geral da Despesa" &
           Coluna=="Despesas Empenhadas")

resultado <- despesa %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

despesa$"2018" <- resultado$Valor

finbra <- read.csv("receita2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="Total Receitas" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- receita %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

receita$"2018" <- resultado$Valor

finbra <- read.csv("receita2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.0.0.00.0.0 - Impostos, Taxas e Contribui¿¿es de Melhoria" &
           Coluna=="Receitas Brutas Realizadas")

finbra <- read.csv("receita2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.1.8.01.1.0 Imposto sobre a Propriedade Predial e Territorial Urbana" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- iptu %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

iptu$"2018" <- resultado$Valor

finbra <- read.csv("receita2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.1.1.8.02.3.0 Imposto sobre Serviços de Qualquer Natureza" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- iss %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

iss$"2018" <- resultado$Valor

resultado <- tributario %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

tributario$"2018" <- resultado$Valor

finbra <- read.csv("receita2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.0.0.00.0.0 - Transferências Correntes" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- transferencias %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

transferencias$"2018" <- resultado$Valor

finbra <- read.csv("receita2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.1.00.00.00 - Transferências da União" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- uniao %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

uniao$"2018" <- resultado$Valor

finbra <- read.csv("receita2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.2.2.00.00.00 - Transferências dos Estados" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- estado %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

estado$"2018" <- resultado$Valor

finbra <- read.csv("receita2018.csv", header = TRUE, skip = 3, sep = ";", dec = ",") %>% 
  subset(Conta=="1.7.1.8.01.2.0 Cota-Parte do Fundo de Participação dos Municípios - Cota Mensal" &
           Coluna=="Receitas Brutas Realizadas")

resultado <- cotapartefpm %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

cotapartefpm$"2018" <- resultado$Valor

finbra <- read.csv("rcl2018.csv", header = TRUE, skip = 5, sep = ";", dec = ",") %>% 
  subset(Conta=="RECEITA CORRENTE LÍQUIDA (III) = (I - II)" &
           Coluna=="TOTAL (ÚLTIMOS 12 MESES)")

resultado <- rcl %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

rcl$"2018" <- resultado$Valor

finbra <- read.csv("dcl2018.csv", header = TRUE, skip = 5, sep = ";", dec = ",") %>% 
  subset(Conta=="% da DCL sobre a RCL (III/RCL)" &
           Coluna=="At¿ o 3¿ Quadrimestre")

resultado <- dcl %>% 
  group_by(Cod.IBGE) %>%
  left_join(finbra, by=c("Cod.IBGE"="Cod.IBGE")) ## (Coloca todos que havia no df + o adicional em cyl [1,2,3,5])

dcl$"2018" <- resultado$Valor

## Criando FPM Teórico

### FPM Estimado ----
# OBS: RENDA PER CAPITA - 2018 = 2016
# OBS: POPULA¿¿O - 2018 = 2017
# E assim em diante...

pop1 <- read_excel("limiares.xlsx", sheet = 2,col_names=TRUE)

pop <- read_excel("tabela.xlsx", sheet = 1,col_names=TRUE) %>% 
  group_by(Cod.IBGE, mun) %>% 
  left_join(pop1, by=c("Cod.IBGE"="Cod.IBGE","mun"="mun")) %>% 
  select (-c("2011.y","2012.y","2013.y","2014.y",
             "2015.y","2016.y","2017.y","2018.y"))  %>% 
  gather(ano, pop, "2011.x":"2018.x") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

pop$id <- as.numeric(pop$id.x)
pop$ano <- as.numeric(pop$ano)

renda <- read_excel("tabela1194.xlsx", sheet = 2,col_names=TRUE) %>% 
  gather(ano, renda, "2011":"2018")
renda$ano <- as.numeric(renda$ano)

# Real Valor do FPM  

fpm_real <- read_excel("Transferências_para_Municípios.xlsx",
                       sheet = 1,col_names=TRUE)

fpm <- left_join(fpm_real,pop, by=c("mun"="mun", "uf"="sigla.x", "ano"="ano"), copy = TRUE)

fpm[is.na(fpm)] <- 0
fpm<- subset(fpm, valor!=0)

# Interior

fpm$pop <- as.numeric(fpm$pop)
fpm$pop <- as.numeric(fpm$pop)

fpm$fator_interior <- ifelse(fpm$pop<=10188,0.6,
                             ifelse(fpm$pop>=10188 & fpm$pop<=13584,0.8,
                                    ifelse(fpm$pop>=13584 & fpm$pop<=16980,1.0,
                                           ifelse(fpm$pop>=16980 & fpm$pop<=23772,1.2,
                                                  ifelse(fpm$pop>=23772 & fpm$pop<=30564,1.4,
                                                         ifelse(fpm$pop>=30564 & fpm$pop<=37356,1.6,
                                                                ifelse(fpm$pop>=37356 & fpm$pop<=44148,1.8,
                                                                       ifelse(fpm$pop>=44148 & fpm$pop<=50940,2.0,
                                                                              ifelse(fpm$pop>=50940 & fpm$pop<=61128,2.2,
                                                                                     ifelse(fpm$pop>=61128 & fpm$pop<=71317,2.4,
                                                                                            ifelse(fpm$pop>=71317 & fpm$pop<=81504,2.6,
                                                                                                   ifelse(fpm$pop>=81504 & fpm$pop<=91692,2.8,
                                                                                                          ifelse(fpm$pop>=91692 & fpm$pop<=101880,3.0,
                                                                                                                 ifelse(fpm$pop>=101880 & fpm$pop<=115464,3.2,
                                                                                                                        ifelse(fpm$pop>=115464 & fpm$pop<129048,3.4,
                                                                                                                               ifelse(fpm$pop>=129048 & fpm$pop<=142632,3.6,
                                                                                                                                      ifelse(fpm$pop>=142632 & fpm$pop<=156215,3.8,
                                                                                                                                             ifelse(fpm$pop>=156215,4,0))))))))))))))))))

# Capital

fpm$d <-  ifelse(fpm$mun=="Rio Branco" & fpm$id== 12| fpm$mun=="Macapá" |
                   fpm$mun=="Manaus" | fpm$mun=="Belém"  & fpm$id== 15|
                   fpm$mun=="Porto Velho" | fpm$mun=="Boa Vista"  & fpm$id== 14|
                   fpm$mun=="Palmas"  & fpm$id== 17| fpm$mun=="Maceió" | fpm$mun=="Salvador" |
                   fpm$mun=="Fortaleza" | fpm$mun=="João Pessoa" | fpm$mun=="Natal" |
                   fpm$mun=="São Luís" | fpm$mun=="Recife" | 
                   fpm$mun=="Teresina" | fpm$mun=="Aracaju" |
                   fpm$mun=="Goiânia" | fpm$mun=="Cuiabá" | 
                   fpm$mun=="Campo Grande" & fpm$id== 50| fpm$mun=="Brasilia"|
                   fpm$mun=="Vitória"| fpm$mun=="Belo Horizonte"| fpm$mun=="São Paulo" | fpm$mun=="Rio de Janeiro" |
                   fpm$mun=="Curitiba" | fpm$mun=="Porto Alegre" | fpm$mun=="Florianópolis", 1 , 0)

library(dplyr)

fpm <- fpm %>% 
  dplyr::group_by(d,ano) %>% 
  mutate(populacao = pop/sum((pop))*d)

fpm$p <- ifelse(fpm$populacao> 0 & fpm$populacao<= 0.02,2.0,
                ifelse(fpm$populacao>=0.02 & fpm$populacao<=0.025,2.5,
                       ifelse(fpm$populacao>=0.025 & fpm$populacao<=0.03,3.0,
                              ifelse(fpm$populacao>=0.03 & fpm$populacao<=0.035,3.5,
                                     ifelse(fpm$populacao>=0.035 & fpm$populacao<=0.04,4.0,
                                            ifelse(fpm$populacao>=0.04 & fpm$populacao<=0.045,4.5,
                                                   ifelse(fpm$populacao>=0.046,5.0,0)))))))

fpm <- fpm %>% 
  group_by(id,ano) %>%
  left_join(renda, by=c("id"="id","ano"="ano")) %>% 
  select(-c(uf.y,sigla.y))


fpm$r <-  
  ifelse(fpm$renda<=0.0045,0.4,
         ifelse(fpm$renda>=0.0045 & fpm$renda<=0.0055,0.5,
                ifelse(fpm$renda>=0.0055 & fpm$renda<=0.0065,0.6,
                       ifelse(fpm$renda>=0.0065 & fpm$renda<=0.0075,0.7,
                              ifelse(fpm$renda>=0.0075 & fpm$renda<=0.0085,0.8,
                                     ifelse(fpm$renda>=0.0085 & fpm$renda<=0.0095,0.9,
                                            ifelse(fpm$renda>=0.0095 & fpm$renda<=0.0110,1,
                                                   ifelse(fpm$renda>=0.0110 & fpm$renda<=0.0130,1.2,
                                                          ifelse(fpm$renda>=0.0130 & fpm$renda<=0.0150,1.4,
                                                                 ifelse(fpm$renda>=0.0150 & fpm$renda<=0.0170,1.6,
                                                                        ifelse(fpm$renda>=0.0170 & fpm$renda<=0.0190,1.8,
                                                                               ifelse(fpm$renda>=0.0190 & fpm$renda<=0.0220,2,
                                                                                      ifelse(fpm$renda>=0.0220,2.5,0)))))))))))))

fpm <- fpm %>% 
  mutate(fator_capital=p*r*d)

fpm <- fpm %>% 
  group_by(d, ano) %>% 
  mutate(capital=fator_capital/sum(fator_capital))

sum(fpm$capital)

# Reserva

fpm$int <-  ifelse(fpm$mun=="Rio Branco" & fpm$id== 12| fpm$mun=="Macapá" |
                     fpm$mun=="Manaus" | fpm$mun=="Belém"  & fpm$id== 15|
                     fpm$mun=="Porto Velho" | fpm$mun=="Boa Vista"  & fpm$id== 14|
                     fpm$mun=="Palmas"  & fpm$id== 17| fpm$mun=="Maceió" | fpm$mun=="Salvador" |
                     fpm$mun=="Fortaleza" | fpm$mun=="João Pessoa" |
                     fpm$mun=="Natal" |
                     fpm$mun=="São Luís" | fpm$mun=="Recife" | 
                     fpm$mun=="Teresina" | fpm$mun=="Aracaju" |
                     fpm$mun=="Goiânia" | fpm$mun=="Cuiabá" | 
                     fpm$mun=="Campo Grande" & fpm$id== 50| fpm$mun=="Brasília"|
                     fpm$mun=="Vitória"| fpm$mun=="Belo Horizonte"| fpm$mun=="São Paulo" | fpm$mun=="Rio de Janeiro" |
                     fpm$mun=="Curitiba" | fpm$mun=="Porto Alegre" | fpm$mun=="Florianópolis", 0 , 1)


fpm$reserv <- ifelse(fpm$fator_interior>=3.8 & fpm$int==1,1,0)

fpm <- fpm %>% 
  group_by(reserv,ano)%>% 
  mutate(populacao_reserva=((pop/sum(pop))))

fpm$pr <- ifelse(fpm$populacao_reserva<=0.02,2,
                 ifelse(fpm$populacao_reserva>=0.02 & fpm$populacao_reserva<=0.025,2.5,
                        ifelse(fpm$populacao_reserva>=0.025 & fpm$populacao_reserva<=0.03,3,
                               ifelse(fpm$populacao_reserva>=0.03 & fpm$populacao_reserva<=0.035,3.5,
                                      ifelse(fpm$populacao_reserva>=0.035 & fpm$populacao_reserva<=0.04,4,
                                             ifelse(fpm$populacao_reserva>=0.04 & fpm$populacao_reserva<=0.045,4.5,
                                                    ifelse(fpm$populacao_reserva>=0.045,5,0)))))))

fpm <- fpm %>%
  group_by(reserv,uf.x,ano) %>% 
  mutate(fator_reserva=pr*r*reserv)

fpm[is.na(fpm)] <- 0

fpm <- fpm %>% 
  group_by(reserv,ano) %>% 
  mutate(reserva=fator_reserva/sum(fator_reserva))

fpm[is.na(fpm)] <- 0


teste <- fpm %>%
  filter(ano==2017)

sum(teste$reserva)

fpm <- fpm %>% 
  group_by(ano, reserv) %>% 
  mutate(reserva=fator_reserva/sum(fator_reserva))

# Interior

fpm <- fpm %>%
  mutate(i=fator_interior*int) %>% 
  group_by(ano,int,sigla) %>% 
  mutate(interior=i/sum(i))

fpm[is.na(fpm)] <- 0

fpm <- fpm %>%
  mutate(interior=interior*int)

#FPM

fpm$ano <- as.numeric(fpm$ano)

repasse <- read_excel("fpm.xlsx",
                      sheet = 1,col_names=TRUE) 

#repasse <-  aggregate(repasse$valor, by = list(repasse$ano),
#FUN = "sum") #%>% #select(ano=Group.1,repasse=x)

fpm <- fpm %>% 
  group_by(ano) %>%
  left_join(repasse, by=c("ano"="ano")) 

fpm[is.na(fpm)] <- 0

# Repasse

fpm <- fpm %>% 
  mutate(valor_real=fpm*0.1*reserva+
           fpm*0.864*interior*dist+
           fpm*0.036*capital)

# Criando limiares do FPM

# Primeira Janela

fpm$IJanela0.02 <- ifelse(fpm$pop>=10188-0.02*10188 & fpm$pop<=10188
                          ,1,ifelse(fpm$pop>10188 & fpm$pop<=10188+0.02*10188,2,0))
fpm$IJanela0.03 <- ifelse(fpm$pop>=10188-0.04*10188 & fpm$pop<=10188
                          ,1,ifelse(fpm$pop>10188 & fpm$pop<=10188+0.04*10188,2,0))
fpm$IJanela0.04 <- ifelse(fpm$pop>=10188-0.04*10188 & fpm$pop<=10188
                         ,1,ifelse(fpm$pop>10188 & fpm$pop<=10188+0.04*10188,2,0))
fpm$IJanela0.05 <- ifelse(fpm$pop>=10188-0.05*10188 & fpm$pop<=10188
                          ,1,ifelse(fpm$pop>10188 & fpm$pop<=10188+0.05*10188,2,0))
fpm$IJanela0.08 <- ifelse(fpm$pop>=10188-0.08*10188 & fpm$pop<=10188
                          ,1,ifelse(fpm$pop>10188 & fpm$pop<=10188+0.08*10188,2,0))
fpm$IJanela0.1 <- ifelse(fpm$pop>=10188-0.1*10188 & fpm$pop<=10188
                         ,1,ifelse(fpm$pop>10188 & fpm$pop<=10188+0.1*10188,2,0))
fpm$IJanela0.15 <- ifelse(fpm$pop>=10188-0.15*10188 & fpm$pop<=10188
                          ,1,ifelse(fpm$pop>10188 & fpm$pop<=10188+0.15*10188,2,0))

# Segunda Janela

fpm$IIJanela0.02 <- ifelse(fpm$pop>=10188-0.02*10188 & fpm$pop<=10188
                          ,1,ifelse(fpm$pop>10188 & fpm$pop<=10188+0.02*10188,2,0))
fpm$IIJanela0.03 <- ifelse(fpm$pop>=13584-0.04*13584 & fpm$pop<=13584
                           ,1,ifelse(fpm$pop>13584 & fpm$pop<=13584+0.04*13584,2,0))
fpm$IIJanela0.04 <- ifelse(fpm$pop>=10188-0.02*10188 & fpm$pop<=10188
                          ,1,ifelse(fpm$pop>10188 & fpm$pop<=10188+0.02*10188,2,0))
fpm$IIJanela0.05 <- ifelse(fpm$pop>=13584-0.05*13584 & fpm$pop<=13584
                           ,1,ifelse(fpm$pop>13584 & fpm$pop<=13584+0.05*13584,2,0))
fpm$IIJanela0.08 <- ifelse(fpm$pop>=13584-0.08*13584 & fpm$pop<=13584
                           ,1,ifelse(fpm$pop>13584 & fpm$pop<=13584+0.08*13584,2,0))
fpm$IIJanela0.1 <- ifelse(fpm$pop>=13584-0.1*13584 & fpm$pop<=13584
                          ,1,ifelse(fpm$pop>13584 & fpm$pop<=13584+0.1*13584,2,0))
fpm$IIJanela0.15 <- ifelse(fpm$pop>=13584-0.15*13584 & fpm$pop<=13584
                           ,1,ifelse(fpm$pop>13584 & fpm$pop<=13584+0.15*13584,2,0))

# Terceira Janela

fpm$IIIJanela0.02 <- ifelse(fpm$pop>=16980-0.02*16980 & fpm$pop<=16980
                            ,1,ifelse(fpm$pop>16980 & fpm$pop<=16980+0.02*16980,2,0))
fpm$IIIJanela0.03 <- ifelse(fpm$pop>=16980-0.03*16980 & fpm$pop<=16980
                            ,1,ifelse(fpm$pop>16980 & fpm$pop<=16980+0.03*16980,2,0))
fpm$IIIJanela0.04 <- ifelse(fpm$pop>=16980-0.04*16980 & fpm$pop<=16980
                            ,1,ifelse(fpm$pop>16980 & fpm$pop<=16980+0.04*16980,2,0))
fpm$IIIJanela0.05 <- ifelse(fpm$pop>=16980-0.05*16980 & fpm$pop<=16980
                            ,1,ifelse(fpm$pop>16980 & fpm$pop<=16980+0.05*16980,2,0))
fpm$IIIJanela0.08 <- ifelse(fpm$pop>=16980-0.08*16980 & fpm$pop<=16980
                            ,1,ifelse(fpm$pop>16980 & fpm$pop<=16980+0.08*16980,2,0))
fpm$IIIJanela0.1 <- ifelse(fpm$pop>=16980-0.1*16980 & fpm$pop<=16980
                           ,1,ifelse(fpm$pop>16980 & fpm$pop<=16980+0.1*16980,2,0))
fpm$IIIJanela0.15 <- ifelse(fpm$pop>=16980-0.15*16980 & fpm$pop<=16980
                            ,1,ifelse(fpm$pop>16980 & fpm$pop<=16980+0.15*16980,2,0))

# Quarta Janela

fpm$IVJanela0.02 <- ifelse(fpm$pop>=23772-0.02*23772 & fpm$pop<=23772
                           ,1,ifelse(fpm$pop>23772 & fpm$pop<=23772+0.02*23772,2,0))
fpm$IVJanela0.03 <- ifelse(fpm$pop>=23772-0.03*23772 & fpm$pop<=23772
                           ,1,ifelse(fpm$pop>23772 & fpm$pop<=23772+0.03*23772,2,0))
fpm$IVJanela0.05 <- ifelse(fpm$pop>=23772-0.05*23772 & fpm$pop<=23772
                           ,1,ifelse(fpm$pop>23772 & fpm$pop<=23772+0.05*23772,2,0))
fpm$IVJanela0.05 <- ifelse(fpm$pop>=23772-0.05*23772 & fpm$pop<=23772
                           ,1,ifelse(fpm$pop>23772 & fpm$pop<=23772+0.05*23772,2,0))
fpm$IVJanela0.08 <- ifelse(fpm$pop>=23772-0.08*23772 & fpm$pop<=23772
                           ,1,ifelse(fpm$pop>23772 & fpm$pop<=23772+0.08*23772,2,0))
fpm$IVJanela0.1 <- ifelse(fpm$pop>=23772-0.1*23772 & fpm$pop<=23772
                          ,1,ifelse(fpm$pop>23772 & fpm$pop<=23772+0.1*23772,2,0))
fpm$IVJanela0.15 <- ifelse(fpm$pop>=23772-0.15*23772 & fpm$pop<=23772
                           ,1,ifelse(fpm$pop>23772 & fpm$pop<=23772+0.15*23772,2,0))

# Quinta Janela

fpm$VJanela0.02 <- ifelse(fpm$pop>=30564-0.02*30564 & fpm$pop<=30564
                          ,1,ifelse(fpm$pop>30564 & fpm$pop<=30564+0.02*30564,2,0))
fpm$VJanela0.03 <- ifelse(fpm$pop>=30564-0.03*30564 & fpm$pop<=30564
                          ,1,ifelse(fpm$pop>30564 & fpm$pop<=30564+0.03*30564,2,0))
fpm$VJanela0.02 <- ifelse(fpm$pop>=30564-0.04*30564 & fpm$pop<=30564
                          ,1,ifelse(fpm$pop>30564 & fpm$pop<=30564+0.04*30564,2,0))
fpm$VJanela0.05 <- ifelse(fpm$pop>=30564-0.05*30564 & fpm$pop<=30564
                          ,1,ifelse(fpm$pop>30564 & fpm$pop<=30564+0.05*30564,2,0))
fpm$VJanela0.08 <- ifelse(fpm$pop>=30564-0.08*30564 & fpm$pop<=30564
                          ,1,ifelse(fpm$pop>30564 & fpm$pop<=30564+0.08*30564,2,0))
fpm$VJanela0.1 <- ifelse(fpm$pop>=30564-0.1*30564 & fpm$pop<=30564
                         ,1,ifelse(fpm$pop>30564 & fpm$pop<=30564+0.1*30564,2,0))
fpm$VJanela0.15 <- ifelse(fpm$pop>=30564-0.15*30564 & fpm$pop<=30564
                          ,1,ifelse(fpm$pop>30564 & fpm$pop<=30564+0.15*30564,2,0))
# Sexta Janela

fpm$VIJanela0.02 <- ifelse(fpm$pop>=37356-0.02*37356 & fpm$pop<37356
                           ,1,ifelse(fpm$pop>=37356 & fpm$pop<=37356+0.02*37356,2,0))
fpm$VIJanela0.03 <- ifelse(fpm$pop>=37356-0.03*37356 & fpm$pop<37356
                           ,1,ifelse(fpm$pop>=37356 & fpm$pop<=37356+0.03*37356,2,0))
fpm$VIJanela0.04 <- ifelse(fpm$pop>=37356-0.04*37356 & fpm$pop<37356
                           ,1,ifelse(fpm$pop>=37356 & fpm$pop<=37356+0.04*37356,2,0))
fpm$VIJanela0.05 <- ifelse(fpm$pop>=37356-0.05*37356 & fpm$pop<37356
                           ,1,ifelse(fpm$pop>=37356 & fpm$pop<=37356+0.05*37356,2,0))
fpm$VIJanela0.08 <- ifelse(fpm$pop>=37356-0.08*37356 & fpm$pop<37356
                           ,1,ifelse(fpm$pop>=37356 & fpm$pop<=37356+0.08*37356,2,0))
fpm$VIJanela0.1 <- ifelse(fpm$pop>=37356-0.1*37356 & fpm$pop<37356
                          ,1,ifelse(fpm$pop>=37356 & fpm$pop<=37356+0.1*37356,2,0))
fpm$VIJanela0.15 <- ifelse(fpm$pop>=37356-0.15*37356 & fpm$pop<37356
                           ,1,ifelse(fpm$pop>=37356 & fpm$pop<=37356+0.15*37356,2,0))


# S¿tima Janela

fpm$VIIJanela0.02 <- ifelse(fpm$pop>=44148-0.02*44148 & fpm$pop<=44148
                            ,1,ifelse(fpm$pop>44148 & fpm$pop<=44148+0.02*44148,2,0))
fpm$VIIJanela0.03 <- ifelse(fpm$pop>=44148-0.03*44148 & fpm$pop<=44148
                            ,1,ifelse(fpm$pop>44148 & fpm$pop<=44148+0.03*44148,2,0))
fpm$VIIJanela0.04 <- ifelse(fpm$pop>=44148-0.04*44148 & fpm$pop<=44148
                            ,1,ifelse(fpm$pop>44148 & fpm$pop<=44148+0.04*44148,2,0))
fpm$VIIJanela0.05 <- ifelse(fpm$pop>=44148-0.05*44148 & fpm$pop<=44148
                            ,1,ifelse(fpm$pop>44148 & fpm$pop<=44148+0.05*44148,2,0))
fpm$VIIJanela0.08 <- ifelse(fpm$pop>=44148-0.08*44148 & fpm$pop<=44148
                            ,1,ifelse(fpm$pop>44148 & fpm$pop<=44148+0.08*44148,2,0))
fpm$VIIJanela0.1 <- ifelse(fpm$pop>=44148-0.1*44148 & fpm$pop<=44148
                           ,1,ifelse(fpm$pop>44148 & fpm$pop<=44148+0.1*44148,2,0))
fpm$VIIJanela0.15 <- ifelse(fpm$pop>=44148-0.15*44148 & fpm$pop<=44148
                            ,1,ifelse(fpm$pop>44148 & fpm$pop<=44148+0.15*44148,2,0))

# Apenas as 4 primeiras janelas 

fpm$IVJanelao0.03 <- ifelse(fpm$pop>=10188-0.03*10188 & fpm$pop<10188 |
                              fpm$pop>=13584-0.03*13584 & fpm$pop<13584 |
                              fpm$pop>=16980-0.03*16980 & fpm$pop<16980 |
                              fpm$pop>=23772-0.03*23772 & fpm$pop<23772,1,
                            ifelse(fpm$pop>=10188 & fpm$pop<=10188+0.03*10188 |
                                     fpm$pop>=13584 & fpm$pop<=13584+0.03*13584  |
                                     fpm$pop>=16980 & fpm$pop<=16980+0.03*16980  |
                                     fpm$pop>=23772 & fpm$pop<=23772+0.03*23772,2,0))


fpm$IVJanelao0.05 <- ifelse(fpm$pop>=10188-0.05*10188 & fpm$pop<10188 |
                            fpm$pop>=13584-0.05*13584 & fpm$pop<13584 |
                            fpm$pop>=16980-0.05*16980 & fpm$pop<16980 |
                            fpm$pop>=23772-0.05*23772 & fpm$pop<23772,1,
                          ifelse(fpm$pop>=10188 & fpm$pop<=10188+0.05*10188 |
                                   fpm$pop>=13584 & fpm$pop<=13584+0.05*13584  |
                                   fpm$pop>=16980 & fpm$pop<=16980+0.05*16980  |
                                   fpm$pop>=23772 & fpm$pop<=23772+0.05*23772,2,0))

fpm$IVJanelao0.08 <- ifelse(fpm$pop>=10188-0.08*10188 & fpm$pop<10188 |
                            fpm$pop>=13584-0.08*13584 & fpm$pop<13584 |
                            fpm$pop>=16980-0.08*16980 & fpm$pop<16980 |
                            fpm$pop>=23772-0.08*23772 & fpm$pop<23772,1,
                          ifelse(fpm$pop>=10188 & fpm$pop<=10188+0.08*10188 |
                                   fpm$pop>=13584 & fpm$pop<=13584+0.08*13584  |
                                   fpm$pop>=16980 & fpm$pop<=16980+0.08*16980  | 
                                   fpm$pop>=23772 & fpm$pop<=23772+0.08*23772,2,0))

fpm$IVJanelao0.1 <- ifelse(fpm$pop>=10188-0.1*10188 & fpm$pop<10188 |
                           fpm$pop>=13584-0.1*13584 & fpm$pop<13584 |
                           fpm$pop>=16980-0.1*16980 & fpm$pop<16980 |
                           fpm$pop>=23772-0.1*23772 & fpm$pop<23772,1,
                         ifelse(fpm$pop>=10188 & fpm$pop<=10188+0.1*10188 |
                                  fpm$pop>=13584 & fpm$pop<=13584+0.1*13584 |
                                  fpm$pop>=16980 & fpm$pop<=16980+0.1*16980 |
                                  fpm$pop>=23772 & fpm$pop<=23772+0.1*23772,2,0))

fpm$IIIJanelao0.15 <- ifelse(fpm$pop>=10188-0.15*10188 & fpm$pop<10188 |
                            fpm$pop>=13584-0.15*13584 & fpm$pop<13584 |
                            fpm$pop>=16980-0.15*16980 & fpm$pop<16980,1,
                          ifelse(fpm$pop>=10188 & fpm$pop<=10188+0.15*10188 |
                                   fpm$pop>=13584 & fpm$pop<=13584+0.15*13584 |
                                   fpm$pop>=16980 & fpm$pop<=16980+0.15*16980,2,0))

# Todas as Janelas

fpm$Janelao0.03 <- ifelse(fpm$pop>=10188-0.03*10188 & fpm$pop<10188 |
                            fpm$pop>=13584-0.03*13584 & fpm$pop<13584 |
                            fpm$pop>=16980-0.03*16980 & fpm$pop<16980 |
                            fpm$pop>=23772-0.03*23772 & fpm$pop<23772 |
                            fpm$pop>=30564-0.03*30564 & fpm$pop<30564 |
                            fpm$pop>=37356-0.03*37356 & fpm$pop<=37356 |
                            fpm$pop>=44148-0.03*44148 & fpm$pop<44148,1,
                          ifelse(fpm$pop>=10188 & fpm$pop<=10188+0.03*10188 |
                                   fpm$pop>=13584 & fpm$pop<=13584+0.03*13584  |
                                   fpm$pop>=16980 & fpm$pop<=16980+0.03*16980  |
                                   fpm$pop>=23772 & fpm$pop<=23772+0.03*23772  |
                                   fpm$pop>=30564 & fpm$pop<=30564+0.03*30564  |
                                   fpm$pop>=37356 & fpm$pop<37356+0.03*37356   |
                                   fpm$pop>=44148 & fpm$pop<=44148+0.03*44148,2,0))

fpm$Janelao0.05 <- ifelse(fpm$pop>=10188-0.05*10188 & fpm$pop<10188 |
                            fpm$pop>=13584-0.05*13584 & fpm$pop<13584 |
                            fpm$pop>=16980-0.05*16980 & fpm$pop<16980 |
                            fpm$pop>=23772-0.05*23772 & fpm$pop<23772 |
                            fpm$pop>=30564-0.05*30564 & fpm$pop<30564 |
                            fpm$pop>=37356-0.05*37356 & fpm$pop<=37356 |
                            fpm$pop>=44148-0.05*44148 & fpm$pop<44148,1,
                          ifelse(fpm$pop>=10188 & fpm$pop<=10188+0.05*10188 |
                                   fpm$pop>=13584 & fpm$pop<=13584+0.05*13584  |
                                   fpm$pop>=16980 & fpm$pop<=16980+0.05*16980  |
                                   fpm$pop>=23772 & fpm$pop<=23772+0.05*23772  |
                                   fpm$pop>=30564 & fpm$pop<=30564+0.05*30564  |
                                   fpm$pop>=37356 & fpm$pop<37356+0.05*37356   |
                                   fpm$pop>=44148 & fpm$pop<=44148+0.05*44148,2,0))

fpm$Janelao0.08 <- ifelse(fpm$pop>=10188-0.08*10188 & fpm$pop<10188 |
                            fpm$pop>=13584-0.08*13584 & fpm$pop<13584 |
                            fpm$pop>=16980-0.08*16980 & fpm$pop<16980 |
                            fpm$pop>=23772-0.08*23772 & fpm$pop<23772 |
                            fpm$pop>=30564-0.08*30564 & fpm$pop<30564 |
                            fpm$pop>=37356-0.08*37356 & fpm$pop<37356 |
                            fpm$pop>=44148-0.08*44148 & fpm$pop<44148,1,
                          ifelse(fpm$pop>=10188 & fpm$pop<=10188+0.08*10188 |
                                   fpm$pop>=13584 & fpm$pop<=13584+0.08*13584  |
                                   fpm$pop>=16980 & fpm$pop<=16980+0.08*16980  | 
                                   fpm$pop>=23772 & fpm$pop<=23772+0.08*23772  |
                                   fpm$pop>=30564 & fpm$pop<=30564+0.08*30564  | 
                                   fpm$pop>=37356 & fpm$pop<=37356+0.08*37356  |
                                   fpm$pop>=44148 & fpm$pop<=44148+0.08*44148 ,2,0))

fpm$Janelao0.1 <- ifelse(fpm$pop>=10188-0.1*10188 & fpm$pop<10188 |
                           fpm$pop>=13584-0.1*13584 & fpm$pop<13584 |
                           fpm$pop>=16980-0.1*16980 & fpm$pop<16980 |
                           fpm$pop>=23772-0.1*23772 & fpm$pop<23772 |
                           fpm$pop>=30564-0.1*30564 & fpm$pop<30564 |
                           fpm$pop>=37356-0.1*37356 & fpm$pop<37356 |
                           fpm$pop>=44148-0.1*44148 & fpm$pop<44148,1,
                         ifelse(fpm$pop>=10188 & fpm$pop<=10188+0.1*10188 |
                                  fpm$pop>=13584 & fpm$pop<=13584+0.1*13584 |
                                  fpm$pop>=16980 & fpm$pop<=16980+0.1*16980 |
                                  fpm$pop>=23772 & fpm$pop<=23772+0.1*23772 | 
                                  fpm$pop>=30564 & fpm$pop<=30564+0.1*30564 |
                                  fpm$pop>=37356 & fpm$pop<=37356+0.1*37356 |
                                  fpm$pop>=44148 & fpm$pop<=44148+0.1*44148,2,0))

fpm$Janelao0.15 <- ifelse(fpm$pop>=10188-0.15*10188 & fpm$pop<10188 |
                            fpm$pop>=13584-0.15*13584 & fpm$pop<13584 |
                            fpm$pop>=16980-0.15*16980 & fpm$pop<16980 |
                            fpm$pop>=23772-0.15*23772 & fpm$pop<23772 |
                            fpm$pop>=30564-0.15*30564 & fpm$pop<30564 |
                            fpm$pop>=37356-0.15*37356 & fpm$pop<37356 |
                            fpm$pop>=44148-0.15*44148 & fpm$pop<44148,1,
                          ifelse(fpm$pop>=10188 & fpm$pop<=10188+0.15*10188 |
                                   fpm$pop>=13584 & fpm$pop<=13584+0.15*13584 |
                                   fpm$pop>=16980 & fpm$pop<=16980+0.15*16980 |
                                   fpm$pop>=23772 & fpm$pop<=23772+0.15*23772 |
                                   fpm$pop>=30564 & fpm$pop<=30564+0.15*30564 |
                                   fpm$pop>=37356 & fpm$pop<=37356+0.15*37356 |
                                   fpm$pop>=44148 & fpm$pop<=44148+0.15*44148,2,0))

# Faixa

fpm <- fpm %>% 
  mutate(faixa = ifelse(IJanela0.15 == 1 | IJanela0.15 == 2,10188,
                        ifelse(IIJanela0.15 == 1 | IIJanela0.15 == 2,13584,
                        ifelse(IIIJanela0.15 == 1 | IIIJanela0.15 == 2,16980,
                        ifelse(IVJanela0.15 == 1 | IVJanela0.15 == 2,23772,
                        ifelse(VJanela0.15 == 1 | VJanela0.15 == 2,30564,
                        ifelse(VIJanela0.15 == 1 | VIJanela0.15 == 2,37356,
                        ifelse(VIIJanela0.15 == 1 | VIIJanela0.15 == 2,44148,NA))))))),
         distancia = ((pop-faixa)/faixa)*100,
         grande = ifelse(pop>= 44149,1,0),
         pequena = ifelse(pop<= 44149,1,0))

## CRESCIMENTO DO PIB

pib <- get_sidra(api='/t/5938/n6/all/v/37/p/last%207/d/v37%200') %>% 
  dplyr::rename(cod = "Município (Código)",
         mun = "Município", pib = Valor, ano = Ano) %>% 
  dplyr::select(-c("Variável (Código)","Variável","Ano (Código)",
            "Unidade de Medida (Código)","Unidade de Medida"))


pop1.1 <- get_sidra(api='/t/6579/n6/all/v/all/p/2011,2012,2013,2014,2015,2016') %>% 
  dplyr::rename(cod = "Município (Código)",
                mun = "Município", pop = Valor, ano = Ano) %>% 
  dplyr::select(c(cod,mun,ano,pop))

pop1.2 <- get_sidra(api='/t/1378/n6/all/v/allxp/p/all/c1/0/c2/0/c287/0/c455/0') %>% 
  dplyr::rename(cod = "Município (Código)",
                mun = "Município", pop = Valor, ano = Ano) %>% 
  dplyr::select(c(cod,mun,ano,pop))

pop <- rbind(pop1.1,pop1.2)

pib_pc <- pib %>% 
  left_join(pop, by = c("cod"="cod", "ano"="ano"), copy = FALSE) %>% 
  mutate(inf = ifelse(ano==2011,66.72583,
               ifelse(ano==2012,70.62262,
               ifelse(ano==2013,74.79642,
               ifelse(ano==2014,79.59087,
               ifelse(ano==2015,88.08321,
               ifelse(ano==2016,93.62364,
               ifelse(ano==2017,96.38554,
               ifelse(ano==2018,100.00000,NA)))))))),
         pib = pib/inf,
         pib_pop = pib/pop)

pib_pc <- as.data.table(pib_pc)
pib_pc[, crescimento := (pib-lag(pib,1))/lag(pib,1), by = cod]
pib_pc[, crescimento_pop := (pib_pop-lag(pib_pop,1))/lag(pib_pop,1), by = cod]

pib_pc <- pib_pc %>% 
  filter(ano!=2010 | ano!=2011) %>% 
  mutate(cod= as.numeric(substr(cod,1,6)),
         ano = as.numeric(ano))

# Gastos

edu <- edu %>% 
  gather(ano, edu, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

adm_geral <- adm_geral %>% 
  gather(ano, adm_geral, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

estado <- estado %>% 
  gather(ano, estado, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

uniao <- uniao %>% 
  gather(ano, uniao, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

aten_bas <- aten_bas %>% 
  gather(ano, aten_bas, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

fund <- fund %>% 
  gather(ano, fund, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

saude <- saude %>% 
  gather(ano, saude, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

pessoal <- pessoal %>% 
  gather(ano, pessoal, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

rcl <- rcl %>% 
  gather(ano, rcl, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

dcl <- dcl %>% 
  gather(ano, dcl, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

despesa <- despesa %>% 
  gather(ano, despesa, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

receita <- receita %>% 
  gather(ano, receita, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

cotapartefpm <- cotapartefpm %>% 
  gather(ano, cotapartefpm, "2012":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

transferencias <- transferencias %>% 
  gather(ano, transferencias, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

tributario <- tributario %>% 
  gather(ano, tributario, "2012":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

Administração <- Administração %>% 
  gather(ano, Administração, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

Amortização <- Amortização %>% 
  gather(ano, Amortização, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

capital <- capital %>% 
  gather(ano, capital, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

legislativo <- legislativo %>% 
  gather(ano, legislativo, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

cultura <- cultura %>% 
  gather(ano, cultura, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

urbanismo <- urbanismo %>% 
  gather(ano, urbanismo, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

encargo_divida <- encargo_divida %>% 
  gather(ano, encargo_divida, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

Previdência <- Previdência %>% 
  gather(ano, Previdência, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

investimentos <- investimentos %>% 
  gather(ano, investimentos, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

iss <- iss %>% 
  gather(ano, iss, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

iptu <- iptu %>% 
  gather(ano, iptu, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

seg_publica <- seg_publica %>% 
  gather(ano, seg_publica, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

agricultura <- agricultura %>% 
  gather(ano, agricultura, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

iptu <- iptu %>% 
  gather(ano, iptu, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

transporte <- transporte %>% 
  gather(ano, transporte, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

desporto_lazer <- desporto_lazer %>% 
  gather(ano, desporto_lazer, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)

encargos_esp <- encargos_esp %>% 
  gather(ano, encargos_esp, "2011":"2018") %>% 
  separate(ano, c("ano","temp"), sep = ".x") %>% 
  select(-temp)


# Juntar

fund$ano <- as.numeric(fund$ano)
aten_bas$ano <- as.numeric(aten_bas$ano)
adm_geral$ano <- as.numeric(adm_geral$ano)
edu$ano <- as.numeric(edu$ano)
pessoal$ano <- as.numeric(pessoal$ano)
saude$ano <- as.numeric(saude$ano)
rcl$ano <- as.numeric(rcl$ano)
dcl$ano <- as.numeric(dcl$ano)
despesa$ano <- as.numeric(despesa$ano)
receita$ano <- as.numeric(receita$ano)
cotapartefpm$ano <- as.numeric(cotapartefpm$ano)
transferencias$ano <- as.numeric(transferencias$ano)
tributario$ano <- as.numeric(tributario$ano)
Administração$ano <- as.numeric(Administração$ano)
Amortização$ano <- as.numeric(Amortização$ano)
capital$ano <- as.numeric(capital$ano)
encargo_divida$ano <- as.numeric(encargo_divida$ano)
Previdência$ano <- as.numeric(Previdência$ano)
legislativo$ano <- as.numeric(legislativo$ano)
urbanismo$ano <- as.numeric(urbanismo$ano)
cultura$ano <- as.numeric(cultura$ano)
investimentos$ano <- as.numeric(investimentos$ano)
iss$ano <- as.numeric(iss$ano)
iptu$ano <- as.numeric(iptu$ano)
estado$ano <- as.numeric(estado$ano)
uniao$ano <- as.numeric(uniao$ano)
seg_publica$ano <- as.numeric(seg_publica$ano)
agricultura$ano <- as.numeric(agricultura$ano)
transporte$ano <- as.numeric(transporte$ano)
desporto_lazer$ano <- as.numeric(desporto_lazer$ano)
encargos_esp$ano <- as.numeric(encargos_esp$ano)

uniao <- mutate(uniao, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
estado <- mutate(estado, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
adm_geral <- mutate(adm_geral, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
aten_bas <- mutate(aten_bas, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
fund <- mutate(fund, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
edu <- mutate(edu, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
rcl <- mutate(rcl, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
saude <- mutate(saude, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
pessoal <- mutate(pessoal, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
dcl <- mutate(dcl, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
Municípios <- mutate(Municípios, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
despesa <- mutate(despesa, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
receita <- mutate(receita, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
cotapartefpm <- mutate(cotapartefpm, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
transferencias <- mutate(transferencias, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
tributario <- mutate(tributario, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
Municípios91 <- mutate(Municípios91, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
Municípios20 <- mutate(Municípios20, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
Administração <- mutate(Administração, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
Amortização <- mutate(Amortização, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
capital <- mutate(capital, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
encargo_divida <- mutate(encargo_divida, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
Previdência <- mutate(Previdência, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
investimentos <- mutate(investimentos, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
legislativo <- mutate(legislativo, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
cultura <- mutate(cultura, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
urbanismo <- mutate(urbanismo, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
iss <- mutate(iss, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
iptu <- mutate(iptu, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
seg_publica <- mutate(seg_publica, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
agricultura <- mutate(agricultura, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
transporte <- mutate(transporte, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
desporto_lazer <- mutate(desporto_lazer, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))
encargos_esp <- mutate(encargos_esp, Cod.IBGE=(substr(Cod.IBGE, 1, 6)))

estado$Cod.IBGE <- as.numeric(estado$Cod.IBGE)
uniao$Cod.IBGE <- as.numeric(uniao$Cod.IBGE)
Municípios$Cod.IBGE <- as.numeric(Municípios$Cod.IBGE)
Municípios91$Cod.IBGE <- as.numeric(Municípios91$Cod.IBGE)
Municípios20$Cod.IBGE <- as.numeric(Municípios20$Cod.IBGE)
edu$Cod.IBGE <- as.numeric(edu$Cod.IBGE)
pessoal$Cod.IBGE <- as.numeric(pessoal$Cod.IBGE)
rcl$Cod.IBGE <- as.numeric(rcl$Cod.IBGE)
fund$Cod.IBGE <- as.numeric(fund$Cod.IBGE)
adm_geral$Cod.IBGE <- as.numeric(adm_geral$Cod.IBGE)
aten_bas$Cod.IBGE <- as.numeric(aten_bas$Cod.IBGE)
saude$Cod.IBGE <- as.numeric(saude$Cod.IBGE)
dcl$Cod.IBGE <- as.numeric(dcl$Cod.IBGE)
despesa$Cod.IBGE <- as.numeric(despesa$Cod.IBGE)
receita$Cod.IBGE <- as.numeric(receita$Cod.IBGE)
cotapartefpm$Cod.IBGE <- as.numeric(cotapartefpm$Cod.IBGE)
transferencias$Cod.IBGE <- as.numeric(transferencias$Cod.IBGE)
tributario$Cod.IBGE <- as.numeric(tributario$Cod.IBGE)
Administração$Cod.IBGE <- as.numeric(Administração$Cod.IBGE)
Amortização$Cod.IBGE <- as.numeric(Amortização$Cod.IBGE)
capital$Cod.IBGE <- as.numeric(capital$Cod.IBGE)
encargo_divida$Cod.IBGE <- as.numeric(encargo_divida$Cod.IBGE)
Previdência$Cod.IBGE <- as.numeric(Previdência$Cod.IBGE)
investimentos$Cod.IBGE <- as.numeric(investimentos$Cod.IBGE)
iss$Cod.IBGE <- as.numeric(iss$Cod.IBGE)
iptu$Cod.IBGE <- as.numeric(iptu$Cod.IBGE)
legislativo$Cod.IBGE <- as.numeric(legislativo$Cod.IBGE)
urbanismo$Cod.IBGE <- as.numeric(urbanismo$Cod.IBGE)
cultura$Cod.IBGE <- as.numeric(cultura$Cod.IBGE)
seg_publica$Cod.IBGE <- as.numeric(seg_publica$Cod.IBGE)
agricultura$Cod.IBGE <- as.numeric(agricultura$Cod.IBGE)
transporte$Cod.IBGE <- as.numeric(transporte$Cod.IBGE)
desporto_lazer$Cod.IBGE <- as.numeric(desporto_lazer$Cod.IBGE)
encargos_esp$Cod.IBGE <- as.numeric(encargos_esp$Cod.IBGE)


janela  <- edu %>%
  left_join(fpm, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(pessoal, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(rcl, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(saude, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(dcl, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%  
  left_join(despesa, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>% 
  left_join(receita, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(cotapartefpm, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(transferencias, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(tributario, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>% 
  left_join(Administração, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%  
  left_join(Amortização, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>% 
  left_join(capital, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(Previdência, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(investimentos, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>% 
  left_join(iss, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(iptu, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(legislativo, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>% 
  left_join(cultura, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(urbanismo, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(aten_bas, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(adm_geral, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(fund, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(uniao, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(estado, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(seg_publica, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(agricultura, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(transporte, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(desporto_lazer, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  left_join(encargos_esp, by=c("Cod.IBGE"="Cod.IBGE", "ano"="ano"), copy=FALSE) %>%
  select(-c("id.y", "uf.y", "sigla.y", "cd_mun.x", "cod_mun.x",
            "2010.x", "id.x.x", "id.y.y", "nome regiao.y",
            "cod regiao.y", "sigla.x", "cd_mun.y", "uf.y.y",
            "cod_mun.y", "mun.y", "2010.y",
            "cod regiao.x.x", "nome regiao.x.x", "id.x.x.x",
            "sigla.y.y", "uf.x.x", "cd_mun.x.x", "cod_mun.x.x",
            "mun.x.x", "2010.x.x", "id.y.y.y", "sigla.x.x",
            "uf.y.y.y", "cd_mun.y.y", "cod_mun.y.y",
            "mun.y.y", "2010.y.y", "cod regiao.x.x.x",
            "nome regiao.x.x.x", "id.x.x.x.x", "sigla.y.y.y",
            "uf.x.x.x", "cd_mun.x.x.x", "cod_mun.x.x.x", "mun.x.x.x",
            "2010.x.x.x", "cod regiao.y.y.y", "nome regiao.y.y.y",
            "id.y.y.y.y", "sigla.x.x.x", "uf.y.y.y.y", "cd_mun.y.y.y",
            "cod_mun.y.y.y", "mun.y.y.y", "2010.y.y.y", "cod regiao.x.x.x.x",
            "nome regiao.x.x.x.x", "id.x.x.x.x.x", "sigla.y.y.y.y",
            "uf.x.x.x.x", "cd_mun.x.x.x.x", "cod_mun.x.x.x.x",
            "mun.x.x.x.x", "2010.x.x.x.x", "cod regiao.y.y.y.y",
            "nome regiao.y.y.y.y", "id.y.y.y.y.y", "sigla.x.x.x.x",
            "uf.y.y.y.y.y", "cd_mun.y.y.y.y", "cod_mun.y.y.y.y",
            "mun.y.y.y.y", "2010.y.y.y.y", "cod regiao.x.x.x.x.x",
            "nome regiao.x.x.x.x.x", "id.x.x.x.x.x.x", "sigla.y.y.y.y.y",
            "uf.x.x.x.x.x", "cd_mun.x.x.x.x.x", "cod_mun.x.x.x.x.x",
            "mun.x.x.x.x.x", "2010.x.x.x.x.x", "cod regiao.y.y.y.y.y",
            "nome regiao.y.y.y.y.y", "id.y.y.y.y.y.y", "sigla.x.x.x.x.x",
            "uf.y.y.y.y.y.y", "cd_mun.y.y.y.y.y", "cod_mun.y.y.y.y.y",
            "mun.y.y.y.y.y", "2010.y.y.y.y.y", "cod regiao.x.x.x.x.x.x",
            "nome regiao.x.x.x.x.x.x", "id.x.x.x.x.x.x.x",
            "sigla.y.y.y.y.y.y", "uf.x.x.x.x.x.x", "cd_mun.x.x.x.x.x.x",
            "cod_mun.x.x.x.x.x.x", "mun.x.x.x.x.x.x", "2010.x.x.x.x.x.x",
            "cod regiao.y.y.y.y.y.y", "nome regiao.y.y.y.y.y.y",
            "id.y.y.y.y.y.y.y", "sigla.x.x.x.x.x.x", "uf.y.y.y.y.y.y.y",
            "cd_mun.y.y.y.y.y.y", "cod_mun.y.y.y.y.y.y",
            "mun.y.y.y.y.y.y", "2010.y.y.y.y.y.y",
            "cod regiao.x.x.x.x.x.x.x", "nome regiao.x.x.x.x.x.x.x",
            "id.x.x.x.x.x.x.x.x", "sigla.y.y.y.y.y.y.y", "uf.x.x.x.x.x.x.x",
            "cd_mun.x.x.x.x.x.x.x", "cod_mun.x.x.x.x.x.x.x",
            "mun.x.x.x.x.x.x.x", "2010.x.x.x.x.x.x.x",
            "nome regiao.y.y.y.y.y.y.y", "cod regiao.y.y.y.y.y.y.y",
            "id.y.y.y.y.y.y.y.y", "sigla.x.x.x.x.x.x.x", "uf.y.y.y.y.y.y.y.y",
            "cd_mun.y.y.y.y.y.y.y", "cod_mun.y.y.y.y.y.y.y", "mun.y.y.y.y.y.y.y",
            "2010.y.y.y.y.y.y.y", "cod regiao.x.x.x.x.x.x.x.x",
            "id.x.x.x.x.x.x.x.x.x", "nome regiao.x.x.x.x.x.x.x.x",
            "uf.x.x.x.x.x.x.x.x", "sigla.y.y.y.y.y.y.y.y",
            "cd_mun.x.x.x.x.x.x.x.x", "cod_mun.x.x.x.x.x.x.x.x",
            "mun.x.x.x.x.x.x.x.x", "2010.x.x.x.x.x.x.x.x",
            "cod regiao.y.y.y.y.y.y.y.y", "nome regiao.y.y.y.y.y.y.y.y",
            "id.y.y.y.y.y.y.y.y.y", "uf.y.y.y.y.y.y.y.y.y", "cd_mun.y.y.y.y.y.y.y.y",
            "mun.y.y.y.y.y.y.y.y", "cod_mun.y.y.y.y.y.y.y.y" ,"mun.y.y.y.y.y.y.y.y",
            "2010.y.y.y.y.y.y.y.y", "nome regiao.y.y", "cod regiao.y.y",
            "sigla.x.x.x.x.x.x.x.x","cod regiao.x.x.x.x.x.x.x.x.x",
            "nome regiao.x.x.x.x.x.x.x.x.x","id.x.x.x.x.x.x.x.x.x.x",
            "sigla.y.y.y.y.y.y.y.y.y","uf.x.x.x.x.x.x.x.x.x",
            "cd_mun.x.x.x.x.x.x.x.x.x","cod_mun.x.x.x.x.x.x.x.x.x",
            "mun.x.x.x.x.x.x.x.x.x", "2010.x.x.x.x.x.x.x.x.x",
            "cod regiao.y.y.y.y.y.y.y.y.y","nome regiao.y.y.y.y.y.y.y.y.y",
            "id.y.y.y.y.y.y.y.y.y.y","uf.y.y.y.y.y.y.y.y.y.y",
            "cd_mun.y.y.y.y.y.y.y.y.y","cod_mun.y.y.y.y.y.y.y.y.y",
            "mun.y.y.y.y.y.y.y.y.y","2010.y.y.y.y.y.y.y.y.y","sigla.x.x.x.x.x.x.x.x.x",
            "cod regiao.x.x.x.x.x.x.x.x.x.x","nome regiao.x.x.x.x.x.x.x.x.x.x",
            "id.x.x.x.x.x.x.x.x.x.x.x","sigla.y.y.y.y.y.y.y.y.y.y",
            "uf.x.x.x.x.x.x.x.x.x.x","cd_mun.x.x.x.x.x.x.x.x.x.x",
            "cod_mun.x.x.x.x.x.x.x.x.x.x","mun.x.x.x.x.x.x.x.x.x.x",
            "2010.x.x.x.x.x.x.x.x.x.x","uf.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y",
            "cod regiao.y.y.y.y.y.y.y.y.y.y","nome regiao.y.y.y.y.y.y.y.y.y.y",
            "id.y.y.y.y.y.y.y.y.y.y.y","sigla.x.x.x.x.x.x.x.x.x.x",
            "uf.y.y.y.y.y.y.y.y.y.y.y","cd_mun.y.y.y.y.y.y.y.y.y.y","mun.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y",
            "cod_mun.y.y.y.y.y.y.y.y.y.y","mun.y.y.y.y.y.y.y.y.y.y","sigla.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y",
            "2010.y.y.y.y.y.y.y.y.y.y","sigla.y.y.y.y.y.y.y.y.y.y.y","2010.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y",
            "cod regiao.x.x.x.x.x.x.x.x.x.x.x","cd_mun.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y"   
            ,"nome regiao.x.x.x.x.x.x.x.x.x.x.x","id.x.x.x.x.x.x.x.x.x.x.x.x"         
            ,"uf.x.x.x.x.x.x.x.x.x.x.x","cd_mun.x.x.x.x.x.x.x.x.x.x.x","cod_mun.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y"       
            ,"cod_mun.x.x.x.x.x.x.x.x.x.x.x","mun.x.x.x.x.x.x.x.x.x.x.x","2010"          
            ,"2010.x.x.x.x.x.x.x.x.x.x.x","id.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y","uf.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x"                          
            ,"cod regiao.y.y.y.y.y.y.y.y.y.y.y","nome regiao.y.y.y.y.y.y.y.y.y.y.y","nome regiao.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y"  
            ,"id.y.y.y.y.y.y.y.y.y.y.y.y","sigla.x.x.x.x.x.x.x.x.x.x.x","cod regiao.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y"        
            ,"uf.y.y.y.y.y.y.y.y.y.y.y.y","cd_mun.y.y.y.y.y.y.y.y.y.y.y","mun.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x"       
            ,"cod_mun.y.y.y.y.y.y.y.y.y.y.y","mun.y.y.y.y.y.y.y.y.y.y.y","cd_mun.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x"          
            ,"2010.y.y.y.y.y.y.y.y.y.y.y","cod_mun.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x","2010.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x"                               
            ,"cod regiao.x.x.x.x.x.x.x.x.x.x.x.x","nome regiao.x.x.x.x.x.x.x.x.x.x.x.x","uf.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y"
            ,"id.x.x.x.x.x.x.x.x.x.x.x.x.x","sigla.y.y.y.y.y.y.y.y.y.y.y.y","nome regiao.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x"      
            ,"uf.x.x.x.x.x.x.x.x.x.x.x.x","cd_mun.x.x.x.x.x.x.x.x.x.x.x.x","sigla.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x"     
            ,"cod_mun.x.x.x.x.x.x.x.x.x.x.x.x","mun.x.x.x.x.x.x.x.x.x.x.x.x","cod regiao.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x"        
            ,"2010.x.x.x.x.x.x.x.x.x.x.x.x","id.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x","2010.y.y.y.y.y.y.y.y.y.y.y.y.y.y",                            
            "cod regiao.y.y.y.y.y.y.y.y.y.y.y.y","nome regiao.y.y.y.y.y.y.y.y.y.y.y.y","cd_mun.y.y.y.y.y.y.y.y.y.y.y.y.y.y"
            ,"id.y.y.y.y.y.y.y.y.y.y.y.y.y","id.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y","mun.y.y.y.y.y.y.y.y.y.y.y.y.y.y"                            
            ,"cd_mun.y.y.y.y.y.y.y.y.y.y.y.y","uf.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x","cod_mun.y.y.y.y.y.y.y.y.y.y.y.y.y.y"     
            ,"cod_mun.y.y.y.y.y.y.y.y.y.y.y.y","mun.y.y.y.y.y.y.y.y.y.y.y.y","sigla.y.y.y.y.y.y.y.y.y.y.y.y.y.y"      
            ,"2010.y.y.y.y.y.y.y.y.y.y.y.y","sigla.x.x.x.x.x.x.x.x.x.x.x.x", "cd_mun.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
            "uf.x.x.x.x.x.x.x.x.x.x.x.x.x","cod regiao.x.x.x.x.x.x.x.x.x.x.x.x.x","uf.y.y.y.y.y.y.y.y.y.y.y.y.y.y",
            "nome regiao.x.x.x.x.x.x.x.x.x.x.x.x.x","id.x.x.x.x.x.x.x.x.x.x.x.x.x.x","nome regiao.y.y.y.y.y.y.y.y.y.y.y.y.y.y"             
            ,"uf.y.y.y.y.y.y.y.y.y.y.y.y.y","cd_mun.x.x.x.x.x.x.x.x.x.x.x.x.x","cod_mun.x.x.x.x.x.x.x.x.x.x.x.x.x.x"     
            ,"cod_mun.x.x.x.x.x.x.x.x.x.x.x.x.x","mun.x.x.x.x.x.x.x.x.x.x.x.x.x","cod regiao.y.y.y.y.y.y.y.y.y.y.y.y.y.y"        
            ,"2010.x.x.x.x.x.x.x.x.x.x.x.x.x","cod regiao.y.y.y.y.y.y.y.y.y.y.y.y.y",
            "nome regiao.y.y.y.y.y.y.y.y.y.y.y.y.y","sigla.x.x.x.x.x.x.x.x.x.x.x.x.x","2010.x.x.x.x.x.x.x.x.x.x.x.x.x.x" 
            ,"id.y.y.y.y.y.y.y.y.y.y.y.y.y.y","sigla.y.y.y.y.y.y.y.y.y.y.y.y.y","mun.x.x.x.x.x.x.x.x.x.x.x.x.x.x"   
            ,"cd_mun.y.y.y.y.y.y.y.y.y.y.y.y.y","cod_mun.y.y.y.y.y.y.y.y.y.y.y.y.y","mun.y.y.y.y.y.y.y.y.y.y.y.y.y"        
            ,"2010.y.y.y.y.y.y.y.y.y.y.y.y.y"),"uf.x.x.x.x.x.x.x.x.x.x.x.x.x.x","cod regiao.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
            "nome regiao.x.x.x.x.x.x.x.x.x.x.x.x.x.x","id.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
         "sigla.x.x.x.x.x.x.x.x.x.x.x.x.x.x") 

janela <- janela[-c(127:133,119:122,117)]

#  rename(uf = uf.x, mun = mun.x, id = id.x, "cod.regiao" = "cod regiao.x",
#         "nome.regiao" = "nome regiao.x")

# PESOS ESPACIAIS ----

mapa <- read_municipality(code_muni = "all", year = 2017) %>% 
  mutate(code_muni = as.numeric(substr(code_muni,1,6)))

mapa <- read_municipality(code_muni = "all", year = 2017)

mapa_fpm <- pop1 %>%
  left_join(mapa, by = c("Cod.IBGE"="code_muni"), copy = FALSE) %>% 
  select(c("name_muni","Cod.IBGE","geometry"))

mapa_fpm <- st_as_sf(mapa_fpm)

list.rook<-poly2nb(mapa_fpm, queen=FALSE)
W <- nb2listw(list.rook, style="W", zero.policy=TRUE, glist = NULL)

matrix <- W %>% 
  mutate_all(~ifelse(. >0,1,0))

janela[is.na(janela)] <- 0

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$edu))
  year = year + 1
}

edu.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                           lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","edu.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$saude))
  year = year + 1
}

saude.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                     lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","saude.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$pessoal))
  year = year + 1
}

pessoal.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                      lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","pessoal.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$Administração))
  year = year + 1
}

Administração.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                             lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","Administração.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$tributario))
  year = year + 1
}

tributario.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                          lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","tributario.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$capital.y))
  year = year + 1
}

capital.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                      lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","capital.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))


year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$legislativo))
  year = year + 1
}

legislativo.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                   lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","legislativo.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$Previdência))
  year = year + 1
}

Previdencia.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                       lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","Previdencia.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$estado))
  year = year + 1
}

estado.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                       lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","estado.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$cotapartefpm))
  year = year + 1
}

cotapartefpm.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                  lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","cotapartefpm.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$despesa))
  year = year + 1
}

despesa.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                  lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","despesa.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$receita))
  year = year + 1
}

receita.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                  lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","receita.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$seg_publica))
  year = year + 1
}

seg_publica.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                  lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","seg_publica.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$desporto_lazer))
  year = year + 1
}

desporto_lazer.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                  lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","desporto_lazer.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$encargos_esp))
  year = year + 1
}

encargos_esp.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                  lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","encargos_esp.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$agricultura))
  year = year + 1
}

agricultura.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                            lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","agricultura.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$transporte))
  year = year + 1
}

transporte.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                           lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","transporte.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$transporte))
  year = year + 1
}

agricultura.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                      lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","agricultura.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))

year <- 2011

while (year<=2018){
  x <- janela %>% 
    filter(ano==year) 
  assign(paste0("lag_",year),lag.listw(W,x$transporte))
  year = year + 1
}

agricultura.lag <- as.data.frame(cbind(lag_2011,lag_2012,lag_2013,lag_2014,
                                      lag_2015,lag_2016,lag_2017,lag_2018)) %>% 
  gather("ano","agricultura.lag") %>% 
  separate(ano, c("key","ano"),sep = "_") %>% 
  select(-c("key","ano"))


lag <- data.frame(Administração.lag,agricultura.lag,capital.lag,cotapartefpm.lag,
                  despesa.lag,desporto_lazer.lag,edu.lag,encargos_esp.lag,estado.lag,
                  legislativo.lag,receita.lag,saude.lag,tributario.lag,pessoal.lag,
                  Previdencia.lag,seg_publica.lag,transporte.lag) %>% 
  select(-c(ano))

rm(lag_2011,lag_2012,lag_2013,lag_2014,
   lag_2015,lag_2016,lag_2017,lag_2018,lag_2019,
   Administração.lag,agricultura.lag,capital.lag,cotapartefpm.lag,
   despesa.lag,desporto_lazer.lag,edu.lag,encargos_esp.lag,estado.lag,
   legislativo.lag,receita.lag,saude.lag,tributario.lag,pessoal.lag,
   Previdencia.lag,seg_publica.lag,transporte.lag)

janela <- data.frame(janela,lag)

# Dados eleitorais ----

output2008 <- read.table(file = "output2008.txt",
                         sep = ";", dec = ",", encoding="Unicode") %>% 
  select(ANO_ELEICAO=V3, NUM_TURNO=V4, DESCRIÇÃO_ELEICAO=V5, UF=V6, NOME = V14,
         APELIDO = V15, NUMERO_URNA = V12, ZONA = V10,
         SQ_CANDIDATO = V11, NOME_MUNICIPIO = V9, cd_mun_tse = V8,
         CARGO = V16, APTOS = V18, PARTIDO = V24, VALOR_PARTIDO = V23,
         RESULTADO = V22, VALOR_RESULTADO = V21, COMPAERCIMENTO = V10,
         COLIGACAO = V27, VALOR_COLIGACAO = V26, VOTOS = V29) %>% 
  filter(CARGO == "PREFEITO" & RESULTADO == "ELEITO")

output2008 <- output2008[!duplicated(output2008$cd_mun_tse), ]

output2012 <- read.table(file = "output2012.txt",
                         sep = ";", dec = ",", encoding="Unicode") %>% 
  select(ANO_ELEICAO=V3, NUM_TURNO=V4, DESCRIÇÃO_ELEICAO=V5, UF=V6, NOME = V14,
         APELIDO = V15, NUMERO_URNA = V12, ZONA = V10,
         SQ_CANDIDATO = V11, NOME_MUNICIPIO = V9, cd_mun_tse = V8,
         CARGO = V16, APTOS = V18, PARTIDO = V24, VALOR_PARTIDO = V23,
         RESULTADO = V22, VALOR_RESULTADO = V21, COMPAERCIMENTO = V10,
         COLIGACAO = V27, VALOR_COLIGACAO = V26, VOTOS = V29) %>% 
  filter(CARGO == "PREFEITO" & RESULTADO == "ELEITO")

output2012 <- output2012[!duplicated(output2012$cd_mun_tse), ]

output2016 <- read.table(file = "output2016.txt",
                         sep = ";", head = T, dec = ",") %>% 
  filter(DS_SIT_TOT_TURNO=="ELEITO" & DS_CARGO == "Prefeito" 
         & DS_ELEICAO == "ELEIÇÕES MUNICIPAIS 2016")

output2016$cd_mun_tse <- stri_replace_all_regex(output2016$SG_UE, 
                                                "\\b0*(\\d+)\\b",
                                                "$1")
output2016$cd_mun_tse <- as.numeric(output2016$cd_mun_tse)

output2016 <- output2016[!duplicated(output2016$cd_mun_tse), ]

cod <- read_excel("cod.xlsx",col_names=TRUE)

output <- output2008 %>%
  full_join(output2012, by=c("cd_mun_tse"="cd_mun_tse"), copy=FALSE) %>% 
  full_join(output2016, by=c("cd_mun_tse"="cd_mun_tse"), copy=FALSE) %>% 
  full_join(cod, by=c("cd_mun_tse"="cd_mun_tse"), copy=FALSE) %>% 
  select(-c("NUM_TURNO.x", "APTOS.x", "CARGO.x","RESULTADO.x","VALOR_RESULTADO.x","VALOR_RESULTADO.y","UF.y","NOME_MUNICIPIO.y",
            "NUM_TURNO.y", "APTOS.y", "CARGO.y","RESULTADO.y"
            ,"CD_TIPO_ELEICAO", "NM_TIPO_ELEICAO", "NR_TURNO","CD_ELEICAO",
            "DS_ELEICAO","DT_ELEICAO","TP_ABRANGENCIA"))

output$cod_munic <- as.numeric(output$cod_munic)
output$Cod.IBGE <- substr(output$cod_munic,1,6)
output$Cod.IBGE <- as.numeric(output$Cod.IBGE)

janela <- janela %>%
  left_join(output, by=c("Cod.IBGE"="Cod.IBGE"), copy=FALSE) %>% 
  select(-c("NUMERO_URNA.x","COMPAERCIMENTO.x",
            "NOME_MUNICIPIO.x","COLIGACAO.x","COMPAERCIMENTO.y","NUMERO_URNA.y",
            "CD_MUNICIPIO","municipio","nome_uf","NM_MUNICIPIO",
            "NR_ZONA","DS_CARGO","SG_UE","NM_UE","nome_uf","munic","cod_munic",
            "APELIDO.x","APELIDO.y","SG_UF","CD_CARGO"
            ,"NR_CANDIDATO","HH_GERACAO","DT_GERACAO","VOTOS.y","VALOR_COLIGACAO.y",
            "COLIGACAO.y","SQ_CANDIDATO.y","DESCRIÇÃO_ELEICAO.y",
            "VOTOS.x","VALOR_COLIGACAO.x",
            "SQ_CANDIDATO.x","DESCRIÇÃO_ELEICAO.x",
            "NM_SOCIAL_CANDIDATO","SQ_CANDIDATO","CD_CARGO","SG_UF"
            ,"CD_SITUACAO_CANDIDATURA","DS_SITUACAO_CANDIDATURA",
            "CD_DETALHE_SITUACAO_CAND","DS_DETALHE_SITUACAO_CAND","TP_AGREMIACAO",
            "SQ_COLIGACAO","NM_COLIGACAO",
            "DS_COMPOSICAO_COLIGACAO","CD_SIT_TOT_TURNO","DS_SIT_TOT_TURNO",
            "ST_VOTO_EM_TRANSITO","QT_VOTOS_NOMINAIS"))

names(janela)[names(janela) == "uf.x"] <- "UF"

names(janela)

janela$Poder_partido <- ifelse(janela$VALOR_PARTIDO.x==janela$VALOR_PARTIDO.y &
                               janela$VALOR_PARTIDO.y==janela$NR_PARTIDO,1,0)

janela$NOME.x <- as.character(janela$NOME.x)
janela$NOME.y <- as.character(janela$NOME.y)
janela$NM_CANDIDATO <- as.character(janela$NM_CANDIDATO)

janela$Exp_gest <- ifelse(janela$NOME.y==janela$NOME.x,1,0)
janela$Poss_Rei <- ifelse(janela$NOME.y!=janela$NOME.x,1,0)

names(janela)[names(janela) == "PARTIDO.x"] <- "Partido_2008"
names(janela)[names(janela) == "PARTIDO.y"] <- "Partido_2012"
names(janela)[names(janela) == "SG_PARTIDO"] <- "Partido_2016"

names(janela)[names(janela) == "NOME.x"] <- "Eleito_2008"
names(janela)[names(janela) == "NOME.y"] <- "Eleito_2012"
names(janela)[names(janela) == "NM_CANDIDATO"] <- "Eleito_2016"

ja <- janela[-c(3)]

janela[is.na(janela)] <- 0

names(janela)

janela <- janela %>% 
  select(-c("ANO_ELEICAO.x","VALOR_PARTIDO.x","ANO_ELEICAO.y"
            ,"VALOR_PARTIDO.y","ANO_ELEICAO","NM_URNA_CANDIDATO"))


## RAIS ----

janela_estados <- read_state(code_state = "all", year = 2017) %>% as.data.frame() %>% 
  select(-c(geometry))

#setwd("D:\\Base de dados\\MTE\\RAIS")
rais <- NULL

rais_ext <- function(ano = NULL, tipo = NULL){
  
  if(tipo=="vinculos"){
    
    for (estados in janela_estados$abbrev_state){
      
      temp <- fread(input=paste0(estados,ano,".txt"))  %>% 
        data.frame(check.names = TRUE) %>% 
        select(c(Vínculo.Ativo.31.12,Município)) %>%
        dplyr::rename(vin = Vínculo.Ativo.31.12, code_mun = "Município") %>% 
        filter(vin==1) %>% 
        group_by(code_mun) %>% 
        dplyr::summarize(empregados = sum(vin))
      
      rais <- rbind(rais,temp)
      
      rm(temp)
      gc(TRUE)
    }    
    
  }
  
  if(tipo=="estabelecimento"){
    
    temp <- fread(input=paste0("ESTB",ano,".txt"))  %>% 
      data.frame(check.names = TRUE) %>% 
      select(c(Tamanho.Estabelecimento,Município,Qtd.Vínculos.Ativos)) %>%
      dplyr::rename(tam = Tamanho.Estabelecimento, code_mun = "Município",
                    vin = Qtd.Vínculos.Ativos)%>% 
      mutate(emp = 1,
             tam = ifelse(tam==1 | tam ==-1,0,tam)) %>% 
      group_by(code_mun) %>% 
      dplyr::summarize(empregados = sum(vin), emp = sum(emp), tam = mean(tam))
    
    rais <- rbind(rais,temp)
    
    rm(temp)
    gc(TRUE)
    
  }
  
  x <- rais  
  
}

rais2012 <- rais_ext(ano=2012, tipo ="estabelecimento") %>% mutate(ano = 2012)
rais2013 <- rais_ext(ano=2013, tipo ="estabelecimento") %>% mutate(ano = 2013)
rais2014 <- rais_ext(ano=2014, tipo ="estabelecimento") %>% mutate(ano = 2014)
rais2015 <- rais_ext(ano=2015, tipo ="estabelecimento") %>% mutate(ano = 2015)
rais2016 <- rais_ext(ano=2016, tipo ="estabelecimento") %>% mutate(ano = 2016)

rais <- rbind(rais2016,rais2014,rais2015,rais2013,rais2012)

rm(rais2016,rais2014,rais2015,rais2013,rais2012)

setwd("F:\\Pedro Jorge\\UFPB\\Mestrado\\Dissertação\\R")

# Distância da capital ----

Mapa <- read_municipality(code_muni = "all", year = 2017)

Mapa <- pop %>% 
  mutate(ano = as.numeric(ano),
                cod = as.numeric(cod)) %>% 
  filter(ano == 2016) %>% 
  select(-c(pop)) %>% 
    left_join(mapa, by = c("cod"="code_muni"), copy=FALSE)

Mapa <- cbind(Mapa, st_coordinates(st_centroid(Mapa$geometry))) %>% 
  select(-c("geometry")) %>% 
  mutate(lon0 =ifelse(abbrev_state == 'AM',-2.57290,
               ifelse(abbrev_state == 'PA',-1.27265,
               ifelse(abbrev_state == 'MT',-15.41888,
               ifelse(abbrev_state == 'RO',-8.98448,
               ifelse(abbrev_state == 'AC',-9.99527,
               ifelse(abbrev_state == 'RR',3.01737,
               ifelse(abbrev_state == 'AP',0.58849,
               ifelse(abbrev_state == 'TO',-10.18871,
               ifelse(abbrev_state == 'MA',-2.62711,
               ifelse(abbrev_state == 'CE',-3.785656,
               ifelse(abbrev_state == 'RN',-5.803182,
               ifelse(abbrev_state == 'PB',-7.165403,
               ifelse(abbrev_state == 'PE',-8.039225,
               ifelse(abbrev_state == 'PI',-5.102260,
               ifelse(abbrev_state == 'AL',-9.522406,
               ifelse(abbrev_state == 'SE',-10.994156,
               ifelse(abbrev_state == 'BA',-12.873412,
               ifelse(abbrev_state == 'GO',-16.643335,
               ifelse(abbrev_state == 'MS',-20.913169,
               ifelse(abbrev_state == 'DF',-15.780692,
               ifelse(abbrev_state == 'MG',-19.902739,
               ifelse(abbrev_state == 'ES',-20.302209,
               ifelse(abbrev_state == 'RJ',-22.92322,
               ifelse(abbrev_state == 'SP',-23.650081,
               ifelse(abbrev_state == 'PR',-25.477891,
               ifelse(abbrev_state == 'SC',-27.577897,
               ifelse(abbrev_state == 'RS',-30.095389,
               ifelse(abbrev_state == 'ES',-20.302209,
               0)))))))))))))))))))))))))))),
         lat0 =ifelse(abbrev_state == 'AM',-60.37242,
               ifelse(abbrev_state == 'PA',-48.50024,
               ifelse(abbrev_state == 'MT',-56.00402,
               ifelse(abbrev_state == 'RO',-63.99115,
               ifelse(abbrev_state == 'AC',-68.15925,
               ifelse(abbrev_state == 'RR',-60.74384,
               ifelse(abbrev_state == 'AP',-50.96231,
               ifelse(abbrev_state == 'TO',-48.11134,
               ifelse(abbrev_state == 'MA',-44.30617,
               ifelse(abbrev_state == 'CE',-38.52770,
               ifelse(abbrev_state == 'RN',-35.22891,
               ifelse(abbrev_state == 'PB',-34.86978,
               ifelse(abbrev_state == 'PE',-34.93317,
               ifelse(abbrev_state == 'PI',-42.74065,
               ifelse(abbrev_state == 'AL',-35.71134,
               ifelse(abbrev_state == 'SE',-37.09491,
               ifelse(abbrev_state == 'BA',-38.51472,
               ifelse(abbrev_state == 'GO',-49.27419,
               ifelse(abbrev_state == 'MS',-54.24969,
               ifelse(abbrev_state == 'DF',-47.79736,
               ifelse(abbrev_state == 'MG',-43.95988,
               ifelse(abbrev_state == 'ES',-39.13743,
               ifelse(abbrev_state == 'RJ',-43.45093,
               ifelse(abbrev_state == 'SP',-46.64811,
               ifelse(abbrev_state == 'PR',-49.28826,
               ifelse(abbrev_state == 'SC',-48.50817,
               ifelse(abbrev_state == 'RS',-51.16507,
               ifelse(abbrev_state == 'ES',-39.13743,
               0))))))))))))))))))))))))))))) %>% 
  mutate(cod = as.numeric(substr(cod,1,6)))

Mapa$dist <- (sqrt((Mapa$X-Mapa$lat0)^2+(Mapa$Y-Mapa$lon0)^2))*100

## FIRJAN ----

# IFGF

temp <- tempfile()
download.file("https://www.firjan.com.br/data/files/7B/91/01/B7/04E1E610B71B21E6A8A809C2/IFGF-2019_evolucao-serie-historica_2013-a-2018.xlsx",destfile = "./IFGF.xlsx",mode = "wb")

ifgf <- read_excel(path = "IFGF.xlsx", sheet = 1) %>% 
  select(c("Código","UF","Município","IFGF 2013","IFGF 2014",
           "IFGF 2015","IFGF 2016","IFGF 2017","IFGF 2018")) %>% 
  gather("Tipo","Valor",-"Código",-"UF",-"Município")

ifgf_autonomia <- read_excel(path = "IFGF.xlsx", sheet = 2) %>% 
  select(c("Código","UF","Município","IFGF Autonomia 2013",
           "IFGF Autonomia 2014","IFGF Autonomia 2015",
           "IFGF Autonomia 2016","IFGF Autonomia 2017","IFGF Autonomia 2018")) %>% 
  dplyr::rename("IFGF_autonomia 2013" = "IFGF Autonomia 2013",
         "IFGF_autonomia 2014" = "IFGF Autonomia 2014",
         "IFGF_autonomia 2015" = "IFGF Autonomia 2015",
         "IFGF_autonomia 2016" = "IFGF Autonomia 2016",
         "IFGF_autonomia 2017" = "IFGF Autonomia 2017",
         "IFGF_autonomia 2018" = "IFGF Autonomia 2018") %>% 
  gather("Tipo","Valor",-"Código",-"UF",-"Município")

ifgf_pessoal <- read_excel(path = "IFGF.xlsx", sheet = 3) %>% 
  select(c("Código","UF","Município","IFGF Gastos com Pessoal 2013",
           "IFGF Gastos com Pessoal 2014",
           "IFGF Gastos com Pessoal 2015","IFGF Gastos com Pessoal 2016",
           "IFGF Gastos com Pessoal 2017","IFGF Gastos com Pessoal 2018")) %>% 
  dplyr::rename("IFGF_pessoal 2013" = "IFGF Gastos com Pessoal 2013",
         "IFGF_pessoal 2014" = "IFGF Gastos com Pessoal 2014",
         "IFGF_pessoal 2015" = "IFGF Gastos com Pessoal 2015",
         "IFGF_pessoal 2016" = "IFGF Gastos com Pessoal 2016",
         "IFGF_pessoal 2017" = "IFGF Gastos com Pessoal 2017",
         "IFGF_pessoal 2018" = "IFGF Gastos com Pessoal 2018") %>% 
  gather("Tipo","Valor",-"Código",-"UF",-"Município")

ifgf_liquidez <- read_excel(path = "IFGF.xlsx", sheet = 4) %>% 
  select(c("Código","UF","Município","IFGF Liquidez 2013",
           "IFGF Liquidez 2014","IFGF Liquidez 2015","IFGF Liquidez 2016"
           ,"IFGF Liquidez 2015","IFGF Liquidez 2017","IFGF Liquidez 2018")) %>% 
  dplyr::rename("IFGF_liquidez 2013" = "IFGF Liquidez 2013",
         "IFGF_liquidez 2014" = "IFGF Liquidez 2014",
         "IFGF_liquidez 2015" = "IFGF Liquidez 2015",
         "IFGF_liquidez 2016" = "IFGF Liquidez 2016",
         "IFGF_liquidez 2017" = "IFGF Liquidez 2017",
         "IFGF_liquidez 2018" = "IFGF Liquidez 2018") %>% 
  gather("Tipo","Valor",-"Código",-"UF",-"Município")

ifgf_investimento <- read_excel(path = "IFGF.xlsx", sheet = 5) %>% 
  select(c("Código","UF","Município","IFGF Investimentos 2013",
           "IFGF Investimentos 2014","IFGF Investimentos 2015","IFGF Investimentos 2016",
           "IFGF Investimentos 2017","IFGF Investimentos 2018")) %>% 
  dplyr::rename("IFGF_investimento 2013" = "IFGF Investimentos 2013",
         "IFGF_investimento 2014" = "IFGF Investimentos 2014",
         "IFGF_investimento 2015" = "IFGF Investimentos 2015",
         "IFGF_investimento 2016" = "IFGF Investimentos 2016",
         "IFGF_investimento 2017" = "IFGF Investimentos 2017",
         "IFGF_investimento 2018" = "IFGF Investimentos 2018") %>% 
  gather("Tipo","Valor",-"Código",-"UF",-"Município")

ifgf <- rbind(ifgf,ifgf_autonomia,ifgf_investimento,ifgf_liquidez,ifgf_pessoal) %>% separate(Tipo,c("Tipo","Ano")," ") %>%
  mutate(Ano = as.numeric(Ano)) %>% filter(Ano <= 2016) %>% spread(Tipo,Valor)

rm(ifgf_autonomia,ifgf_investimento,ifgf_liquidez,ifgf_pessoal)

# Geral

temp <- tempfile()
download.file("https://www.firjan.com.br/data/files/11/04/E5/97/F1B8461049FF6646A8A809C2/Evolu__o%20do%20IFDM%20Geral%20-%202005%20a%202016.xlsx",destfile = "./IFDM.xlsx",mode = "wb")

ifdm <- read_excel(path = "IFDM.xlsx",skip = 2) %>% 
  dplyr::rename(IFDM_2005 = Nota...5,IFDM_2006 = Nota...7,IFDM_2007 = Nota...9,
         IFDM_2008 = Nota...11,IFDM_2009 = Nota...13,IFDM_2010 = Nota...15,
         IFDM_2011 = Nota...17,IFDM_2012 = Nota...19,IFDM_2013 = Nota...21,
         IFDM_2014 = Nota...23,IFDM_2015 = Nota...25,IFDM_2016 = Nota...27) %>% 
  select(c("Código","Região","UF","Município",IFDM_2005,IFDM_2006,IFDM_2007,
           IFDM_2008,IFDM_2009,IFDM_2010,IFDM_2011,IFDM_2012,IFDM_2013,IFDM_2014,
           IFDM_2015,IFDM_2016)) %>% 
  gather("Tipo","Valor",-"Código",-"Região",-"UF",-"Município") %>% 
  filter(!is.na(Código)) %>% separate(Tipo,c("Tipo","Ano"),"_") %>% 
  dplyr::rename(IFDM = Valor) %>% select(-c("Região","UF","Município","Tipo"))

# Edu

temp <- tempfile()
download.file("https://www.firjan.com.br/data/files/5B/F3/48/87/F1B8461049FF6646A8A809C2/Evolu__o%20do%20IFDM%20Educa__o%20-%202005%20a%202016.xlsx",destfile = "./IFDM_edu.xlsx",mode = "wb")

ifdm_edu <- read_excel(path = "IFDM_edu.xlsx",skip = 2) %>% 
  dplyr::rename(Edu_2005 = Nota...5,Edu_2006 = Nota...7,Edu_2007 = Nota...9,
         Edu_2008 = Nota...11,Edu_2009 = Nota...13,Edu_2010 = Nota...15,
         Edu_2011 = Nota...17,Edu_2012 = Nota...19,Edu_2013 = Nota...21,
         Edu_2014 = Nota...23,Edu_2015 = Nota...25,Edu_2016 = Nota...27) %>% 
  select(c("Código","Região","UF","Município",Edu_2005,Edu_2006,Edu_2007,
           Edu_2008,Edu_2009,Edu_2010,Edu_2011,Edu_2012,Edu_2013,Edu_2014,Edu_2015,Edu_2016)) %>% 
  gather("Tipo","Valor",-"Código",-"Região",-"UF",-"Município") %>% 
  filter(!is.na(Código)) %>% separate(Tipo,c("Tipo","Ano"),"_") %>% 
  dplyr::rename(educa = Valor) %>% select(-c("Região","UF","Município","Tipo"))

# Emprego e Renda

temp <- tempfile()
download.file("https://www.firjan.com.br/data/files/2E/F3/1F/87/F1B8461049FF6646A8A809C2/Evolu__o%20do%20IFDM%20EmpregoRenda%20-%202005%20a%202016.xlsx",destfile = "./IFDM_ER.xlsx",mode = "wb")

ifdm_er <- read_excel(path = "IFDM_ER.xlsx",skip = 2) %>% 
  dplyr::rename(ER_2005 = Nota...5,ER_2006 = Nota...7,ER_2007 = Nota...9,
         ER_2008 = Nota...11,ER_2009 = Nota...13,ER_2010 = Nota...15,
         ER_2011 = Nota...17,ER_2012 = Nota...19,ER_2013 = Nota...21,
         ER_2014 = Nota...23,ER_2015 = Nota...25,ER_2016 = Nota...27) %>% 
  select(c("Código","Região","UF","Município",ER_2005,ER_2006,ER_2007,
           ER_2008,ER_2009,ER_2010,ER_2011,ER_2012,ER_2013,ER_2014,ER_2015,ER_2016)) %>% 
  gather("Tipo","Valor",-"Código",-"Região",-"UF",-"Município") %>% 
  filter(!is.na(Código)) %>% separate(Tipo,c("Tipo","Ano"),"_")  %>% 
  dplyr::rename(ER = Valor) %>% select(-c("Região","UF","Município","Tipo"))

# Saúde

temp <- tempfile()
download.file("https://www.firjan.com.br/data/files/04/04/CC/97/F1B8461049FF6646A8A809C2/Evolu__o%20do%20IFDM%20Sa_de%20-%202005%20a%202016.xlsx",destfile = "./IFDM_Saud.xlsx",mode = "wb")

ifdm_saud <- read_excel(path = "IFDM_Saud.xlsx",skip = 2) %>% 
  dplyr::rename(Saude_2005 = Nota...5,Saude_2006 = Nota...7,Saude_2007 = Nota...9,
         Saude_2008 = Nota...11,Saude_2009 = Nota...13,Saude_2010 = Nota...15,
         Saude_2011 = Nota...17,Saude_2012 = Nota...19,Saude_2013 = Nota...21,
         Saude_2014 = Nota...23,Saude_2015 = Nota...25,Saude_2016 = Nota...27) %>% 
  select(c("Código","Região","UF","Município",Saude_2005,Saude_2006,Saude_2007,
           Saude_2008,Saude_2009,Saude_2010,Saude_2011,Saude_2012,Saude_2013,
           Saude_2014,Saude_2015,Saude_2016)) %>% 
  gather("Tipo","Valor",-"Código",-"Região",-"UF",-"Município") %>% 
  filter(!is.na(Código)) %>% separate(Tipo,c("Tipo","Ano"),"_") %>% 
  dplyr::rename(Saude = Valor) %>% select(-c("Região","UF","Município","Tipo"))

ifdm <- ifdm %>% 
  left_join(ifdm_edu) %>% 
  left_join(ifdm_er) %>% 
  left_join(ifdm_saud) %>% 
  mutate(Ano = as.numeric(Ano))

rm(ifdm_edu,ifdm_er,ifdm_saud)

janela <- janela %>% 
  left_join(rais, by = c("Cod.IBGE"="code_mun","ano"="ano")) %>%
  left_join(pib_pc, by = c("Cod.IBGE"="cod","ano"="ano")) %>% 
  left_join(ifgf, by = c("Cod.IBGE"="Código","ano"="Ano")) %>% 
  left_join(ifdm, by = c("Cod.IBGE"="Código","ano"="Ano")) %>% 
  left_join(Mapa, by = c("Cod.IBGE"="cod")) 
  
# Finalizando ----

janel <- janela %>% 
  dplyr::select(-c(pop.x)) %>%
  dplyr::rename(ano = ano.x, dist = dist.y) %>% 
  dplyr::mutate(inf = ifelse(ano==2011,66.72583,
                      ifelse(ano==2012,70.62262,
                      ifelse(ano==2013,74.79642,
                      ifelse(ano==2014,79.59087,
                      ifelse(ano==2015,88.08321,
                      ifelse(ano==2016,93.62364,
                      ifelse(ano==2017,96.38554,
                      ifelse(ano==2018,100.00000,NA)))))))),
         urbanismo_real = log(urbanismo/pop*inf), urbanismo_real.lag = log(urbanismo.lag/pop*inf),
         cultura_real = log(cultura/pop*inf), cultura_real.lag = log(cultura.lag/pop*inf),       
         edu_real = log(edu/pop*inf), edu_real.lag = log(edu.lag/pop*inf),
         saude_real = log(saude/pop*inf), saude_real.lag = log(saude.lag/pop*inf),
         fund_real = log(fund/pop*inf), pessoal.lag = log(pessoal.lag/pop*inf),
         fpm_real = log(fpm/pop*inf), tributario.lag = log(tributario.lag/pop*inf),
         adm_real = log(Administração/pop*inf),adm_real.lag = log(Administração.lag/pop*inf),
         adm_geral_real = log(adm_geral/pop*inf), Previdencia.lag = log(Previdencia.lag/pop*inf),
         aten_bas_real = log(aten_bas/pop*inf), cotapartefpm.lag = log(cotapartefpm.lag/pop*inf),
         receita_real = log(receita/pop*inf),receita_real.lag = log(receita.lag/pop*inf),
         despesa_real = log(despesa/pop*inf),despesa_real.lag = log(despesa.lag/pop*inf),  Previdencia_real = log(Previdência/pop*inf),
         fpm_real = log(cotapartefpm/pop*inf), seg_publica_real = log(seg_publica/pop*inf),
         pessoal_real = log(pessoal/pop*inf), agricultura_real = log(agricultura/pop*inf),
         tributario_real = log(tributario/pop*inf), transporte_real = log(transporte/pop*inf),
         capital_real = log(capital.y/pop*inf), desporto_lazer_real = log(desporto_lazer/pop*inf),
         desporto_lazer_real.lag = log(desporto_lazer.lag/pop*inf), transporte_real.lag = log(transporte.lag/pop*inf),
         agricultura_real.ag = log(agricultura.lag/pop*inf),
         legislativo_real = log(legislativo/pop*inf), encargos_esp_real = log(encargos_esp/pop*inf),
         transferencias_real = log(transferencias/pop*inf),
         outras_transf_real = log((transferencias-cotapartefpm)/pop*inf), estado_real = log(estado/pop*inf),
         receita_pib = receita/pib,encargos_esp.lag = log(encargos_esp.lag/pop*inf),
         despesa_pib = despesa/pib, capital.lag = log(capital.lag/pop*inf),
         edu_pct = edu/despesa, Previdencia_pct = Previdência/pop,
         saude_pct = saude/despesa, seg_publica_pct = seg_publica/pop,
         pessoal_pct = pessoal/despesa, agricultura_pct = agricultura/pop,
         adm_pct = Administração/despesa, transporte_pct = transporte/pop,
         capital_pct = capital.y/despesa, desporto_lazer_pct = desporto_lazer/pop,
         legislativo_pct = legislativo/despesa, encargos_esp_pct = encargos_esp/pop,
         fpm_pct = cotapartefpm/receita, estado_pct = estado/pop,
         tributario_pct = tributario/receita,
         outras_transf_pct = (cotapartefpm-transferencias)/receita) %>% 
  select(-c("mun.y","pop.y","mun.x.y"))

## AJUSTES FINAIS

janela <- janela %>% 
  mutate(IFGF = as.numeric(IFGF),
         IFGF_autonomia = as.numeric(IFGF_autonomia),
         IFGF_investimento = as.numeric(IFGF_investimento),
         IFGF_liquidez = as.numeric(IFGF_liquidez),
         IFGF_pessoal = as.numeric(IFGF_pessoal),
         IFDM = as.numeric(IFDM),
         educa = as.numeric(educa),
         ER = as.numeric(ER),
         Saude = as.numeric(Saude)) %>% 
  select(-c(ano.y))


# Exportando ----

library(foreign)

saveRDS(janela, file="janela de dados.rds")
write.dta(janela, file="janela.dta")

# Fazendo mapas ----

setwd("F:\\Pedro Jorge\\UFPB\\Mestrado\\Dissertação\\R")

library(rnaturalearth); library(viridis); library(ggspatial); library(sf)

x <- readRDS("janela de dados.rds") %>% filter(ano == 2016)

mapa <- read_municipality(code_muni = 'all', year = 2016) %>%
  mutate(code_muni = as.numeric(substr(code_muni,1,6)))

x <- x %>% left_join(mapa, by = c("Cod.IBGE"="code_muni"))

worldMap <- ne_countries(scale = "medium", returnclass = "sf")

xx <- x

x <- xx %>% st_sf()

ggplot() + geom_sf(data=x, aes(fill=IFGF))

y <- x %>% filter(IJanela0.15 != 0 | IIJanela0.15 != 0 | IIIJanela0.15 != 0) %>% 
  mutate(IJanelao = IJanela0.1 + IIJanela0.1 + IIIJanela0.1)

threshold <- ggplot() +
  geom_sf(data=worldMap, fill="white", color="gray90") +
  geom_sf(data=mapa, fill = "white") +
  geom_sf(data=y, aes(fill= IJanelao), colour = NA) +
  labs(subtitle = paste0("(", length(unique(y$ifgf)), " Municipalities)"), 
       caption="Source: Own elaboration based on FIRJAN using R software"
  ) +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(option = "magma", direction = -1,
                     name = "Ratio of PBF beneficiaries by population",
                     # here we use guide_colourbar because it is still a continuous scale
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       # some shifting around
                       title.hjust = 0.5,
                       label.hjust = 0.5
                     )) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed",
                                        size = 0.5),
        panel.background = element_rect(fill = "lightblue")) +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) +
  coord_sf(xlim = c(st_bbox(y)[[1]],
                    st_bbox(y)[[3]]),
           ylim = c(st_bbox(y)[[2]],
                    st_bbox(y)[[4]]))

ifgf <- ggplot() +
  geom_sf(data=worldMap, fill="white", color="gray48") +
  geom_sf(data=x, aes(fill= IFGF), color = NA) +
  labs(subtitle = paste0("(", length(unique(x$IFGF)), " Municípios)"), 
       caption="Fonte: Elaboração própria usando o R"
  ) +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(option = "magma", direction = -1,
                     name = "Índice FIRJAN",
                     # here we use guide_colourbar because it is still a continuous scale
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       # some shifting around
                       title.hjust = 0.5,
                       label.hjust = 0.5
                     )) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed",
                                        size = 0.5),
        panel.background = element_rect(fill = "lightblue")) +
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) +
  coord_sf(xlim = c(st_bbox(x)[[1]],
                    st_bbox(x)[[3]]),
           ylim = c(st_bbox(x)[[2]],
                    st_bbox(x)[[4]]))

ggsave(ifgf, file ="ifgf.png", dpi = 300,
       height = 17, width = 17, units = "cm")  

ggsave(ifgf, file ="threshold.png", dpi = 300,
       height = 17, width = 17, units = "cm") 


## TESTE ADICIONAL

setwd("F:\\Pedro Jorge\\UFPB\\Mestrado\\Dissertação\\R")

base <- readRDS("Base_de_Dados_Final.rds")

setwd("D:\\Base de dados\\MTE\\RAIS")

rais_ext <- function(ano = NULL, tipo = NULL){
  rais <- NULL
  
  if(ano < 2018){
    
    if(tipo=="vinculos"){
      
      for (estados in base_estados$abbrev_state){
        
        temp <- fread(input=paste0(estados,ano,".txt"), dec = ",")  %>% 
          data.frame(check.names = TRUE) %>% 
          select(c("Vínculo.Ativo.31.12","Município","CBO.Ocupação.2002","CNAE.2.0.Classe",
                   "CNAE.95.Classe","Escolaridade.após.2005",
                   "Qtd.Hora.Contr","Idade","Tempo.Emprego","Natureza.Jurídica",
                   "Vl.Remun.Dezembro.Nom","Vl.Remun.Dezembro..SM.",
                   "Vl.Remun.Média.Nom")) %>% 
          dplyr::rename(vin = Vínculo.Ativo.31.12, code_mun = "Município",
                        nat = "Natureza.Jurídica",
                        horas = "Qtd.Hora.Contr", rem_dez = "Vl.Remun.Dezembro.Nom", rem = "Vl.Remun.Média.Nom") %>% 
          mutate(year=ano,
                 Orgao_pub_pod_exec_mun = ifelse(nat == 1031,1,0),
                 rem_dez_h = rem_dez/horas,
                 Orgao_pub_pod_leg_mun = ifelse(nat == 1066,1,0),
                 Autarquia_mun = ifelse(nat == 1120,1,0),
                 Fund_DP_mun = ifelse(nat == 1155,1,0),
                 Orgao_pub_autonomo = ifelse(nat == 1180,1,0),
                 fun_priv_mun = ifelse(nat == 1279,1,0),
                 mun = ifelse(nat == 1244,1,0),
                 adm_ind_mun = ifelse(nat == 1309,1,0),
                 adm_dir_mun = ifelse(nat == 1333,1,0),
                 rem_h = rem/horas) %>% filter(vin==1) %>% 
          group_by(code_mun,year) %>% 
          dplyr::summarize(Orgao_pub_pod_exec_mun = sum(Orgao_pub_pod_exec_mun),
                           Orgao_pub_pod_leg_mun = sum(Orgao_pub_pod_leg_mun),
                           Autarquia_mun = sum(Autarquia_mun),
                           Orgao_pub_autonomo = sum(Orgao_pub_autonomo),
                           fun_priv_mun = sum(fun_priv_mun),
                           adm_ind_mun = sum(adm_ind_mun),
                           mun = sum(mun),
                           adm_dir_mun = sum(adm_dir_mun), horas = mean(horas),
                           Idade = mean(Idade),
                           Tempo.Emprego = mean(Tempo.Emprego),
                           rem_dez = mean(rem_dez),
                           rem_ano = mean(rem),
                           rem_dez_h = mean(rem_dez_h),
                           rem_ano_h = mean(rem_h)) 
        #  ungroup() %>% spread(tipo_emprego,rem_h)
        
        rais <- rbind(rais,temp)
        
        rm(temp)
        gc(TRUE)
      }    
      
    }
    
    if(tipo=="estabelecimento"){
      
      temp <- fread(input=paste0("ESTB",ano,".txt"))  %>% 
        data.frame(check.names = TRUE) %>% 
        select(c(Tamanho.Estabelecimento,Município,Qtd.Vínculos.Ativos)) %>%
        dplyr::rename(tam = Tamanho.Estabelecimento, code_mun = "Município",
                      vin = Qtd.Vínculos.Ativos)%>% 
        mutate(emp = 1,
               tam = ifelse(tam==1 | tam ==-1,0,tam)) %>% 
        group_by(code_mun) %>% 
        summarize(empregados = sum(vin), emp = sum(emp), tam = mean(tam))
      
      rais <- rbind(rais,temp)
      
      rm(temp)
      gc(TRUE)
      
    }
    
    return(rais)  
    
  } else {
    
    if(tipo=="vinculos"){
      
      tipo2018 <- c("RAIS_VINC_PUB_CENTRO_OESTE","RAIS_VINC_PUB_MG_ES_RJ","RAIS_VINC_PUB_NORDESTE",
                    "RAIS_VINC_PUB_NORTE","RAIS_VINC_PUB_SP","RAIS_VINC_PUB_SUL")
      
      for (estados in tipo2018){
        
        # https://concla.ibge.gov.br/estrutura/natjur-estrutura/natureza-juridica-2018.html
        
        temp <- fread(input=paste0(estados,".txt"), dec = ",")  %>% 
          data.frame(check.names = TRUE) %>% 
          select(c("Vínculo.Ativo.31.12","Município","CBO.Ocupação.2002","CNAE.2.0.Classe",
                   "CNAE.95.Classe","Escolaridade.após.2005",
                   "Qtd.Hora.Contr","Idade","Tempo.Emprego","Natureza.Jurídica",
                   "Vl.Remun.Dezembro.Nom","Vl.Remun.Dezembro..SM.",
                   "Vl.Remun.Média.Nom")) %>% 
          dplyr::rename(vin = Vínculo.Ativo.31.12, code_mun = "Município",
                        nat = "Natureza.Jurídica",
                        horas = "Qtd.Hora.Contr", rem_dez = "Vl.Remun.Dezembro.Nom", rem = "Vl.Remun.Média.Nom") %>% 
          mutate(year=ano,
                 Orgao_pub_pod_exec_mun = ifelse(nat == 1031,1,0),
                 rem_dez_h = rem_dez/horas,
                 Orgao_pub_pod_leg_mun = ifelse(nat == 1066,1,0),
                 Autarquia_mun = ifelse(nat == 1120,1,0),
                 Fund_DP_mun = ifelse(nat == 1155,1,0),
                 Orgao_pub_autonomo = ifelse(nat == 1180,1,0),
                 fun_priv_mun = ifelse(nat == 1279,1,0),
                 mun = ifelse(nat == 1244,1,0),
                 adm_ind_mun = ifelse(nat == 1309,1,0),
                 adm_dir_mun = ifelse(nat == 1333,1,0),
                 rem_h = rem/horas) %>% filter(vin==1) %>% 
          group_by(code_mun,tipo_emprego,year) %>% 
          dplyr::summarize(Orgao_pub_pod_exec_mun = sum(Orgao_pub_pod_exec_mun),
                           Orgao_pub_pod_leg_mun = sum(Orgao_pub_pod_leg_mun),
                           Autarquia_mun = sum(Autarquia_mun),
                           Orgao_pub_autonomo = sum(Orgao_pub_autonomo),
                           fun_priv_mun = sum(fun_priv_mun),
                           adm_ind_mun = sum(adm_ind_mun),
                           mun = sum(mun),
                           adm_dir_mun = sum(adm_dir_mun), horas = mean(horas),
                           Idade = mean(Idade),
                           Tempo.Emprego = mean(Tempo.Emprego),
                           rem_dez = mean(rem_dez),
                           rem_ano = mean(rem),
                           rem_dez_h = mean(rem_dez_h),
                           rem_ano_h = mean(rem_h))  
        rais <- rbind(rais,temp)
        rm(temp)
        gc(TRUE)
      }    
      
    }
    
    if(tipo=="estabelecimento"){
      
      temp <- fread(input="RAIS_ESTAB_PUB.txt")  %>% 
        data.frame(check.names = TRUE) %>% 
        select(c(Tamanho.Estabelecimento,Município,Qtd.Vínculos.Ativos)) %>%
        dplyr::rename(tam = Tamanho.Estabelecimento, code_mun = "Município",
                      vin = Qtd.Vínculos.Ativos)%>% 
        mutate(emp = 1,
               tam = ifelse(tam==1 | tam ==-1,0,tam)) %>% 
        group_by(code_mun) %>% 
        summarize(empregados = sum(vin), emp = sum(emp), tam = mean(tam))
      
      rais <- rbind(rais,temp)
      
      rm(temp)
      gc(TRUE)
      
    }
    
  }
  
}

base_estados <- read_state(code_state = 'all')

rais2013 <- rais_ext(ano=2013,tipo="vinculos")
rais2014 <- rais_ext(ano=2014,tipo="vinculos")
rais2015 <- rais_ext(ano=2015,tipo="vinculos")
rais2016 <- rais_ext(ano=2016,tipo="vinculos")

rais <- rbind(rais2013,rais2014,rais2015,rais2016) %>% dplyr::rename(ano = year)

base2<- base %>% left_join(rais, by = c("Cod.IBGE"="code_mun","ano"="ano"))

write.dta(base2, file = "karys.dta")
