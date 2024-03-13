
# 0 - Library -------------------------------------------------------------
source("scripts/fonctions.R", encoding = 'UTF-8')
library(openxlsx)
library(tidyverse)
library(plotly)
library(scales)

PATH_THEME <- "data/liste_indicateurs_v7.xlsx"
PATH_SOURCE <- "data/indicateurs.xlsx"
PATH_PS <- "data/liberal.xlsx"


# Sources -------------------------------------------------------------

source_pop         <- read.xlsx(PATH_THEME, sheet = 1, cols = c(1,11,12), sep.names = " ")
source_evol        <- read.xlsx(PATH_THEME, sheet = 2, cols = c(1,11,12), sep.names = " ")
source_mort        <- read.xlsx(PATH_THEME, sheet = 3, cols = c(1,11,12), sep.names = " ")
source_handi       <- read.xlsx(PATH_THEME, sheet = 4, cols = c(1,11,12), sep.names = " ")
source_age         <- read.xlsx(PATH_THEME, sheet = 5, cols = c(1,11,12), sep.names = " ")
source_es          <- read.xlsx(PATH_THEME, sheet = 6, cols = c(1,11,12), sep.names = " ")
source_offre_handi <- read.xlsx(PATH_THEME, sheet = 7, cols = c(1,11,12), sep.names = " ")
source_focus       <- read.xlsx(PATH_THEME, sheet = 8, cols = c(1,11,12), sep.names = " ")
source_det         <- read.xlsx(PATH_THEME, sheet = 9, cols = c(1,11,12), sep.names = " ")
source_vacsi       <- read.xlsx(PATH_THEME, sheet = 10, cols = c(1,11,12), sep.names = " ")
source_patho       <- read.xlsx(PATH_THEME, sheet = 11, cols = c(1,11,12), sep.names = " ")
source_etat        <- read.xlsx(PATH_THEME, sheet = 12, cols = c(1,11,12), sep.names = " ")
source_psy         <- read.xlsx(PATH_THEME, sheet = 13, cols = c(1,11,12), sep.names = " ")
source_psy_enf     <- read.xlsx(PATH_THEME, sheet = 14, cols = c(1,11,12), sep.names = " ")
source_recours     <- read.xlsx(PATH_THEME, sheet = 15, cols = c(1,11,12), sep.names = " ")
source_lib         <- read.xlsx(PATH_PS, sheet = 10, sep.names = " ")

# 1 - Thème 1 : Données démo et socio-éco ---------------------------------

population <- read.xlsx(PATH_THEME, sheet = 1, cols = c(1:10), sep.names = " ")
population <- formating(population, 
                         row_any_digits = c(1,2,14), 
                         row_one_digits = c(11,12), 
                         row_percent = c(3:10,13))
evol <- read.xlsx(PATH_THEME, sheet = 2, cols = c(1:10), sep.names = " ")
evol <- formating(evol,
                   row_percent = c(1:4))
mortalite <- read.xlsx(PATH_THEME, sheet = 3, cols = c(1:10), sep.names = " ")
mortalite <- formating(mortalite,
                       row_percent = c(1:2))
handi <- read.xlsx(PATH_THEME, sheet = 4, cols = c(1:10), sep.names = " ")
age <- read.xlsx(PATH_THEME, sheet = 5, cols = c(1:10), sep.names = " ")
# age <- formating(age, 
#                  row_any_digits = 1)

# 2 - Thème 2 : Offre de soins --------------------------------------------

es <- read.xlsx(PATH_THEME, sheet = 6, cols = c(1:10), sep.names = " ")
es <- formating(es,
                row_any_digits = c(1:4,6,8),
                row_percent = c(5,7,9))
offre_handi <- read.xlsx(PATH_THEME, sheet = 7, cols = c(1:10), sep.names = " ")
focus <- read.xlsx(PATH_THEME, sheet = 8, cols = c(1:10), sep.names = " ")
focus <- formating(focus,
                   row_percent = c(4,5))

# 3 - Thème 3 : Déterminants de Santé -------------------------------------

det <- read.xlsx(PATH_THEME, sheet = 9, cols = c(1:10), sep.names = " ")
det <- formating(det,
                 row_percent = c(1:3))
vacsi <- read.xlsx(PATH_THEME, sheet = 10, cols = c(1:10), sep.names = " ")
vacsi <- formating(vacsi,
                   row_percent = c(1:7))
patho <- read.xlsx(PATH_THEME, sheet = 11, cols = c(1:10), sep.names = " ")
patho <- formating(patho,
                   row_percent = c(1:3))
etat <- read.xlsx(PATH_THEME, sheet = 12, cols = c(1:10), sep.names = " ")
etat <- formating(etat,
                  row_percent = c(1:5))

# 4 - Thème 4 : Focus Psychiatrie -----------------------------------------

psy_ge <- read.xlsx(PATH_THEME, sheet = 13, cols = c(1:10), sep.names = " ")
psy_ge <- formating(psy_ge,
                    row_one_digits = c(3:4),
                    row_percent = c(1:2))
psy_enf <- read.xlsx(PATH_THEME, sheet = 14, cols = c(1:10), sep.names = " ")
psy_enf <- formating(psy_enf,
                     row_any_digits = 3,
                     row_percent = c(1:2))
recours <- read.xlsx(PATH_THEME, sheet = 15, cols = c(1:10), sep.names = " ")
recours <- formating(recours,
                     row_percent = c(1:3))


# 5 - Thème 2 : Offre de soins ~ libéraux mixtes --------------------------

PATH_PS <- "data/liberal.xlsx"

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

# 6.2 - Répartition séjours -----------------------------------------------

graph_MCO <- es_graph("Répartition séjours MCO", col = 2)
graph_SSR <- es_graph("Répartition séjours SSR", col = 3)
graph_PSY <- es_graph("Répartition journées PSY", col = 4)

