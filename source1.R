# 0 - Library -------------------------------------------------------------
#source("scripts/fonctions.R", encoding = 'UTF-8')
library(openxlsx)
library(tidyverse)
library(plotly)
library(scales)
source("scripts/fonctions.R", encoding = 'UTF-8')

PATH_THEME            <- "data/liste_indicateurs_v7.xlsx"
PATH_THEME2           <- "data/liste_indicateurs_v8.xlsx"
PATH_PS               <- "data/liberal.xlsx"
PATH_PS_5y            <- paste0("data/PS 2017.xlsx")
PATH_PS_y             <- paste0("data/PS 2022.xlsx")
PATH_MEDICO_SOC       <- "data/LISTE ET Medico-Sociaux 31-12-2022.xlsx"
PATH_CAP_MEDICO_SOC   <- "data/Liste ETs Médico Sociaux capacitaire - PA - PH adultes-PH enfants - 31-12-2022.xlsx"
PATH_SOURCE           <- "data/sources.xlsx"

# Sources -------------------------------------------------------------

#modfier sur fichier excl principal le 23/11/23
source_pop                <- read.xlsx(PATH_SOURCE, sheet = 1,  cols = c(1,2,3), sep.names = " ")
source_offre_sanitaire    <- read.xlsx(PATH_SOURCE, sheet = 2,  cols = c(1,2,3), sep.names = " ")
source_determinant_sante  <- read.xlsx(PATH_SOURCE, sheet = 3,  cols = c(1,2,3), sep.names = " ")
source_PRS3               <- read.xlsx(PATH_SOURCE, sheet = 4,  cols = c(1,2), sep.names = " ")

# METHODO -------------------------------------------------------------

# source_evol        <- read.xlsx(PATH_THEME, sheet = 2,  cols = c(1,11,12), sep.names = " ")
# source_mort        <- read.xlsx(PATH_THEME, sheet = 3,  cols = c(1,11,12), sep.names = " ")
# source_handi       <- read.xlsx(PATH_THEME, sheet = 4,  cols = c(1,11,12), sep.names = " ")
# source_age         <- read.xlsx(PATH_THEME, sheet = 5,  cols = c(1,11,12), sep.names = " ")
# source_es          <- read.xlsx(PATH_THEME, sheet = 6,  cols = c(1,11,12), sep.names = " ")
# source_offre_handi <- read.xlsx(PATH_THEME, sheet = 7,  cols = c(1,11,12), sep.names = " ")
# source_focus       <- read.xlsx(PATH_THEME, sheet = 8,  cols = c(1,11,12), sep.names = " ")
# source_det         <- read.xlsx(PATH_THEME, sheet = 9,  cols = c(1,11,12), sep.names = " ")
# source_vacsi       <- read.xlsx(PATH_THEME, sheet = 10, cols = c(1,11,12), sep.names = " ")
# source_patho       <- read.xlsx(PATH_THEME, sheet = 11, cols = c(1,11,12), sep.names = " ")
# source_etat        <- read.xlsx(PATH_THEME, sheet = 12, cols = c(1,11,12), sep.names = " ")
# source_psy         <- read.xlsx(PATH_THEME, sheet = 13, cols = c(1,11,12), sep.names = " ")
# source_psy_enf     <- read.xlsx(PATH_THEME, sheet = 14, cols = c(1,11,12), sep.names = " ")
# source_recours     <- read.xlsx(PATH_THEME, sheet = 15, cols = c(1,11,12), sep.names = " ")
#source_lib          <- read.xlsx(PATH_PS, sheet = 10, sep.names = " ")

# 1 - Thème 1 : Données démo et socio-éco ---------------------------------

# AU FINAL MES DONNEES COMMENCE ICI CAR JE N'ai PAS FAIT LA MEME METHOE QUE SALOME POUR LA RECUP DE DONNEES CAR JAI QUE DES INDICATEURS DONC PAS VMRT BESOIN DE 15 SHEETS

population <- read.xlsx("data/dataset.xlsx", sheet = 1, cols = c(1:49), sep.names = " ")

# population <- formating(population, 
#                         cols_any_digits = c(1,2,15,16,17), 
#                         cols_percent = c(4:11,14), 
#                         within_IDF = 11, 
#                         digits_within = 0)

# 2 - Thème 2 : Offre de soins --------------------------------------------


# 2.1 PS ------------------------------------------------------------------

ps_5y <- read.xlsx(PATH_PS_5y, sheet = 1)

ps_5y <- ps_5y %>% 
  group_by(LIB_DEP, LIB_SPE) %>% 
  summarise("2017" = sum(NB_PS))
ps_y <- read.xlsx(PATH_PS_y, sheet = 1)
ps_y <- ps_y %>% 
  group_by(LIB_DEP, LIB_SPE) %>% 
  summarise("2022" = sum(NB_PS))

ps <- left_join(ps_5y, ps_y)

# région
ps_reg <- ps %>% 
  group_by(LIB_SPE) %>% 
  summarise("2017" = sum(`2017`),
            "2022" = sum(`2022`)) %>% 
  mutate(LIB_DEP = "ILE-DE-FRANCE")

ps <- rbind(ps,ps_reg)
ps$evolution <- (ps$`2022`-ps$`2017`)/ps$`2017`*100
ps <- as.data.frame(ps)
ps <- col_format(ps,
                 cols_any_digits = c(3:4),
                 cols_percent = 5)

colnames(ps) <- c("LIB_DEP", "Spécialité", paste("Effectifs en", year(Sys.Date())-6), paste("Effectifs en", year(Sys.Date())-1), "Evolution")

# on supprime le surplus
rm(ps_y, ps_5y, ps_reg)


es <- read.xlsx(PATH_THEME, sheet = 6, cols = c(1:10), sep.names = " ")
es <- formating(es,
                row_any_digits = c(1:4,6,8),
                row_percent = c(5,7,9))
offre_handi <- read.xlsx(PATH_THEME, sheet = 7, cols = c(1:10), sep.names = " ")
focus <- read.xlsx(PATH_THEME, sheet = 8, cols = c(1:10), sep.names = " ")
focus <- formating(focus,
                   row_percent = c(4,5))


# 2.3 Medicosoc -----------------------------------------------------------

ms <- read.xlsx(PATH_THEME2, sheet = 6, cols = c(1:10), sep.names = " ")

# ajout extract Finess
capacit <- openxlsx::read.xlsx(PATH_CAP_MEDICO_SOC, sheet = 4, startRow = 2, sep.names = " ")
# capacitaire
capacit_pha <- capacit %>%
  select(c(12,32,43)) %>%
  group_by(`ET-Département Code`) %>%
  summarise(sum(`AUT-Capacité autorisée`, na.rm = TRUE)) %>%
  column_to_rownames("ET-Département Code") %>%
  t() %>%
  as.data.frame()
capacit_pha <- capacit_pha %>%
  mutate('IDF' = rowSums(capacit_pha))
# ajout dans ms
ms[1, c(2:length(ms))] <- capacit_pha[1,]

capacit_phe <- capacit %>%
  select(c(12,32,43)) %>%
  group_by(`ET-Département Code`) %>%
  summarise(sum(`AUT-Capacité autorisée`, na.rm = TRUE)) %>%
  column_to_rownames("ET-Département Code") %>%
  t() %>%
  as.data.frame()
capacit_phe <- capacit_phe %>%
  mutate('IDF' = rowSums(capacit_phe))
# ajout dans ms
ms[2, c(2:length(ms))] <- capacit_phe[1,]

rm(capacit_pha, capacit_phe)

# nb établissement PA
etab_pa <- openxlsx::read.xlsx(PATH_MEDICO_SOC, sheet = 2, startRow = 5, sep.names = " ")
etab_pa <- etab_pa %>%
  select(c(7,13,15,17,24))

# nb etab pour PA
nb_etab_pa <- etab_pa %>%
  filter(`ET-Catégorie Niv2 Code` == 4400) %>%
  group_by(`ET-Département Code`) %>%
  summarise(nb = n_distinct(`ET-N°FINESS`)) %>%
  column_to_rownames(var = "ET-Département Code") %>%
  t() %>%
  as.data.frame()
nb_etab_pa <- nb_etab_pa %>%
  mutate('IDF' = rowSums(nb_etab_pa))
# ajout dans ms
ms[3, c(2:length(ms))] <- nb_etab_pa[1,]

# nb etab PA dépendante 500 & résidence autonomie 202
nb_etab <- etab_pa %>%
  filter(`ET-Catégorie Code` %in% c(500, 202)) %>%
  group_by(`ET-Département Code`, `ET-Catégorie Code`) %>%
  summarise(nb = n_distinct(`ET-N°FINESS`)) %>%
  pivot_wider(names_from = `ET-Département Code`, values_from = nb) %>%
  as.data.frame() %>%
  column_to_rownames("ET-Catégorie Code")
nb_etab <- nb_etab %>%
  mutate('IDF' = rowSums(nb_etab))
# ajout dans ms
ms[4, c(2:length(ms))] <- nb_etab[2,]
ms[5, c(2:length(ms))] <- nb_etab[1,]

ms <- row_format(ms,
                 row_any_digits = c(1:5))

rm(nb_etab, nb_etab_pa, etab_pa)


# 3 - Thème 2 : Offre de soins ~ libéraux mixtes --------------------------

PATH_PS <- "data/liberal.xlsx"

offre_sanitaire2    <- read.xlsx("data/dataset.xlsx", sheet = 2, cols = c(1:49), sep.names = " ")

medico2             <- read.xlsx("data/dataset.xlsx", sheet = 3, cols = c(1:49), sep.names = " ")

ville2              <- read.xlsx("data/dataset.xlsx", sheet = 4, cols = c(1:49), sep.names = " ")

ville2tab           <- read.xlsx("data/dataset.xlsx", sheet = 5, cols = c(1:49), sep.names = " ")

# AFFICHAGE DE TABLEAU
detsante1           <- read.xlsx("data/dataset.xlsx", sheet = 6, cols = c(1:49), sep.names = " ")

detsante2           <- read.xlsx("data/dataset.xlsx", sheet = 7, cols = c(1:49), sep.names = " ")






# 4 - Thème 3 : PRS3 --------------------------------------------

#cas des differents tableaux en fonction du select par departement choisi
prs_idf <- read.xlsx("data/PRS3.xlsx", sheet = 9,  cols = c(1:6), sep.names = " ")
prs_75  <- read.xlsx("data/PRS3.xlsx", sheet = 1,  cols = c(1:4), sep.names = " ")
prs_77  <- read.xlsx("data/PRS3.xlsx", sheet = 2,  cols = c(1:4), sep.names = " ")
prs_78  <- read.xlsx("data/PRS3.xlsx", sheet = 3,  cols = c(1:4), sep.names = " ")
prs_91  <- read.xlsx("data/PRS3.xlsx", sheet = 4,  cols = c(1:4), sep.names = " ")
prs_92  <- read.xlsx("data/PRS3.xlsx", sheet = 5,  cols = c(1:4), sep.names = " ")
prs_93  <- read.xlsx("data/PRS3.xlsx", sheet = 6,  cols = c(1:4), sep.names = " ")
prs_94  <- read.xlsx("data/PRS3.xlsx", sheet = 7,  cols = c(1:4), sep.names = " ")
prs_95  <- read.xlsx("data/PRS3.xlsx", sheet = 8,  cols = c(1:4), sep.names = " ")

prs_indicBox <- read.xlsx("data/PRS3.xlsx", sheet = 10,  cols = c(1:109), sep.names = " ")



# 5.1 - Dept 75 -----------------------------------------------------------




lib_eff_75 <- read.xlsx(PATH_PS,  sheet = 1, rows = c(11:18), cols = c(1:4), sep.names = " ")
lib_eff_75 <- ps_format(lib_eff_75,
                        cols_any_digits = c(2,3),
                        cols_percent = 4)
lib_dens_75 <- read.xlsx(PATH_PS, sheet = 1, rows = c(20:27), cols = c(1:4), sep.names = " ")
lib_dens_75 <- ps_format(lib_dens_75,
                         cols_one_digits = c(2,3),
                         cols_percent = 4)
# lib_graph_75 <- read.xlsx(PATH_PS, sheet = 1, rows = c(1:29), cols = c(7:10), sep.names = " ")
lib_graph_75 <- read.xlsx(PATH_PS, sheet = 9, rows = c(1:15), cols = c(7:10), sep.names = " ")

# 5.2 - Dept 77 -----------------------------------------------------------

lib_eff_77 <- read.xlsx(PATH_PS,   sheet = 2, rows = c(11:18), cols = c(1:4), sep.names = " ")
lib_eff_77 <- ps_format(lib_eff_77,
                        cols_any_digits = c(2,3),
                        cols_percent = 4)
lib_dens_77 <- read.xlsx(PATH_PS,  sheet = 2, rows = c(20:27), cols = c(1:4), sep.names = " ")
lib_dens_77 <- ps_format(lib_dens_77,
                         cols_one_digits = c(2,3),
                         cols_percent = 4)
# lib_graph_77 <- read.xlsx(PATH_PS, sheet = 2, rows = c(1:29), cols = c(7:10), sep.names = " ")
lib_graph_77 <- read.xlsx(PATH_PS, sheet = 9, rows = c(1:15), cols = c(7:10), sep.names = " ")

# 5.3 - Dept 78 -----------------------------------------------------------

lib_eff_78 <- read.xlsx(PATH_PS,   sheet = 3, rows = c(11:18), cols = c(1:4), sep.names = " ")
lib_eff_78 <- ps_format(lib_eff_78,
                        cols_any_digits = c(2,3),
                        cols_percent = 4)
lib_dens_78 <- read.xlsx(PATH_PS,  sheet = 3, rows = c(20:27), cols = c(1:4), sep.names = " ")
lib_dens_78 <- ps_format(lib_dens_78,
                         cols_one_digits = c(2,3),
                         cols_percent = 4)
# lib_graph_78 <- read.xlsx(PATH_PS, sheet = 3, rows = c(1:29), cols = c(7:10), sep.names = " ")
lib_graph_78 <- read.xlsx(PATH_PS, sheet = 9, rows = c(1:15), cols = c(7:10), sep.names = " ")

# 5.4 - Dept 91 -----------------------------------------------------------

lib_eff_91 <- read.xlsx(PATH_PS,   sheet = 4, rows = c(11:18), cols = c(1:4), sep.names = " ")
lib_eff_91 <- ps_format(lib_eff_91,
                        cols_any_digits = c(2,3),
                        cols_percent = 4)
lib_dens_91 <- read.xlsx(PATH_PS,  sheet = 4, rows = c(20:27), cols = c(1:4), sep.names = " ")
lib_dens_91 <- ps_format(lib_dens_91,
                         cols_one_digits = c(2,3),
                         cols_percent = 4)
# lib_graph_91 <- read.xlsx(PATH_PS, sheet = 4, rows = c(1:29), cols = c(7:10), sep.names = " ")
lib_graph_91 <- read.xlsx(PATH_PS, sheet = 9, rows = c(1:15), cols = c(7:10), sep.names = " ")

# 5.5 - Dept 92 -----------------------------------------------------------

lib_eff_92 <- read.xlsx(PATH_PS,   sheet = 5, rows = c(11:18), cols = c(1:4), sep.names = " ")
lib_eff_92 <- ps_format(lib_eff_92,
                        cols_any_digits = c(2,3),
                        cols_percent = 4)
lib_dens_92 <- read.xlsx(PATH_PS,  sheet = 5, rows = c(20:27), cols = c(1:4), sep.names = " ")
lib_dens_92 <- ps_format(lib_dens_92,
                         cols_one_digits = c(2,3),
                         cols_percent = 4)
# lib_graph_92 <- read.xlsx(PATH_PS, sheet = 5, rows = c(1:29), cols = c(7:10), sep.names = " ")
lib_graph_92 <- read.xlsx(PATH_PS, sheet = 9, rows = c(1:15), cols = c(7:10), sep.names = " ")

# 5.6 - Dept 93 -----------------------------------------------------------

lib_eff_93 <- read.xlsx(PATH_PS,   sheet = 6, rows = c(11:18), cols = c(1:4), sep.names = " ")
lib_eff_93 <- ps_format(lib_eff_93,
                        cols_any_digits = c(2,3),
                        cols_percent = 4)
lib_dens_93 <- read.xlsx(PATH_PS,  sheet = 6, rows = c(20:27), cols = c(1:4), sep.names = " ")
lib_dens_93 <- ps_format(lib_dens_93,
                         cols_one_digits = c(2,3),
                         cols_percent = 4)
# lib_graph_93 <- read.xlsx(PATH_PS, sheet = 6, rows = c(1:29), cols = c(7:10), sep.names = " ")
lib_graph_93 <- read.xlsx(PATH_PS, sheet = 9, rows = c(1:15), cols = c(7:10), sep.names = " ")

# 5.7 - Dept 94 -----------------------------------------------------------

lib_eff_94 <- read.xlsx(PATH_PS,   sheet = 7, rows = c(11:18), cols = c(1:4), sep.names = " ")
lib_eff_94 <- ps_format(lib_eff_94,
                        cols_any_digits = c(2,3),
                        cols_percent = 4)
lib_dens_94 <- read.xlsx(PATH_PS,  sheet = 7, rows = c(20:27), cols = c(1:4), sep.names = " ")
lib_dens_94 <- ps_format(lib_dens_94,
                         cols_one_digits = c(2,3),
                         cols_percent = 4)
# lib_graph_94 <- read.xlsx(PATH_PS, sheet = 7, rows = c(1:29), cols = c(7:10), sep.names = " ")
lib_graph_94 <- read.xlsx(PATH_PS, sheet = 9, rows = c(1:15), cols = c(7:10), sep.names = " ")

# 5.8 - Dept 95 -----------------------------------------------------------

lib_eff_95 <- read.xlsx(PATH_PS,   sheet = 8, rows = c(11:18), cols = c(1:4), sep.names = " ")
lib_eff_95 <- ps_format(lib_eff_95,
                        cols_any_digits = c(2,3),
                        cols_percent = 4)
lib_dens_95 <- read.xlsx(PATH_PS,  sheet = 8, rows = c(20:27), cols = c(1:4), sep.names = " ")
lib_dens_95 <- ps_format(lib_dens_95,
                         cols_one_digits = c(2,3),
                         cols_percent = 4)
# lib_graph_95 <- read.xlsx(PATH_PS, sheet = 8, rows = c(1:29), cols = c(7:10), sep.names = " ")
lib_graph_95 <- read.xlsx(PATH_PS, sheet = 9, rows = c(1:15), cols = c(7:10), sep.names = " ")

# 5.9 - IDF ---------------------------------------------------------------

lib_eff_idf <- read.xlsx(PATH_PS,   sheet = 9, rows = c(11:18), cols = c(1:4), sep.names = " ")
lib_eff_idf <- ps_format(lib_eff_idf,
                         cols_any_digits = c(2,3),
                         cols_percent = 4)
lib_dens_idf <- read.xlsx(PATH_PS,  sheet = 9, rows = c(20:27), cols = c(1:4), sep.names = " ")
lib_dens_idf <- ps_format(lib_dens_idf,
                          cols_one_digits = c(2,3),
                          cols_percent = 4)
# lib_graph_idf <- read.xlsx(PATH_PS, sheet = 9, rows = c(1:29), cols = c(7:10), sep.names = " ")
lib_graph_idf <- read.xlsx(PATH_PS, sheet = 9, rows = c(1:15), cols = c(7:10), sep.names = " ")








# tab2    <- read.csv2("datasetsimple.csv")
# 
# ev      <- read.csv2("evolution.csv")
# 
# #handi   <- read.csv2("handi.csv")
# 
# mortali <- read.csv2("mortalite.csv")
# 
# nb_tot      = tab2 %>%
#   summarise (n_hab = sum(n_hab, na.rm = T))
# 
# nb_natali   = tab2 %>%
#   summarise (natalite = sum(natalite, na.rm = T))
# 
# nb_pauvrete = tab2 %>%
#   summarise (pauvrete = sum(pauvrete, na.rm = T))
# 
# nb_chom     =  tab2 %>%
#   summarise (chomage = sum(chomage, na.rm = T))
# 
# nb_evo     =  ev %>%
#   summarise (evol1 = sum(evol1, na.rm = T))
# 
# nb_evo2     =  ev %>%
#   summarise (evol2 = sum(evol2, na.rm = T))
# 
# nb_handi     =  handi %>%
#   summarise (aah = sum(aah, na.rm = T))
# 
# nb_handi2    =  handi %>%
#   summarise (aeeh = sum(aeeh, na.rm = T))
# 
# nb_mortalite = mortali %>%
#   summarise (mortalite_infantile = sum(mortalite_infantile, na.rm = T))
# 
# nb_mortalite2 = mortali %>%
#   summarise (mortalite = sum(mortalite, na.rm = T))
# 



# 6 - Graphiques ----------------------------------------------------------
# 6.1 - Libéral et mixtes -------------------------------------------------
ps_graph_75 <- ps_graph(lib_graph_75)
ps_graph_77 <- ps_graph(lib_graph_77)
ps_graph_78 <- ps_graph(lib_graph_78)
ps_graph_91 <- ps_graph(lib_graph_91)
ps_graph_92 <- ps_graph(lib_graph_92)
ps_graph_93 <- ps_graph(lib_graph_93)
ps_graph_94 <- ps_graph(lib_graph_94)
ps_graph_95 <- ps_graph(lib_graph_95)
ps_graph_idf <- ps_graph(lib_graph_idf)

#graph natalite
graph_natali    <- ggplot(population) +
  aes(x = Dpt, y = natali, fill = Dpt) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Département", y = "‰ Natalité", title = "Taux de natalité par département") +
  theme_minimal()

graph_mortalite_infantile   <- ggplot(population) +
  aes(x = Dpt, y = mortalite_infantile, fill = Dpt) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Département", y = "‰ Mortalité infantile", title = "Taux de mortalité infantile par département") +
  theme_minimal()

#graph pauvrete
graph_pauvrete <- ggplot(population) +
  aes(x = Dpt, y = pauvrete, fill = Dpt) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Département", y = "% Pauvreté", title = "Taux de pauvreté par département") +
  theme_minimal()


#Permet d'avoir des graph plus coerent en retirant pour ces deux les chiffres de l'IDF
population_stat <- population[population$Dpt != "IDF", ]

#graph population
graph_population <- ggplot(population_stat) +
  aes(x = Dpt, y = n_hab, fill = Dpt) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Département", y = "Population", title = "Nombre d'habitants par département") +
  coord_flip() +
  theme_minimal()

#graph alloc_adulte
graph_alloc_adulte <- ggplot(population_stat) +
  aes(x = Dpt, y = alloc_adulte, fill = Dpt) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Département", y = "Allocation adulte handicapé", title = "Nombre de bénéficiaires d'AAH par département") +
  theme_minimal()

#graph alloc_enfant
graph_alloc_enfant <- ggplot(population_stat) +
  aes(x = Dpt, y = alloc_enfant, fill = Dpt) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(x = "Département", y = "Allocation enfant handicapé", title = "Nombre de bénéficiaires d'AEEH par département") +
  theme_minimal()
