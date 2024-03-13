library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(openxlsx)
library(tidyverse)
library(DT)

# UI -------------------------------------------------------------------

# A. DONNEES -------------------------------------------------------------------

source("./source1.R")
source("scripts/fonctions.R", encoding = 'UTF-8')

# B. DEBUT APPLICATION -------------------------------------------------
shinyApp(
  ui = dashboardPage(
    # C. Onglet présentation -------------------------------------------------
    # D. DASHBOARD HEADER -------------------------------------------------
    
    dashboardHeader(
      title = tags$li(class = "dropdown", img(src = "ARS.png", height = "47px")),
     #  title = "ARS IDF",
     # titleWidth = 230,
        
      
      # MENU
      dropdownMenu(type = "messages",
                   messageItem(
                     from = "DIRNVOV",
                     message = "Les indicateurs ne sont pas trop mal :)"
                   ),
                   messageItem(
                     from = "Nouvel utilisateur",
                     message = "Comment s'enregistrer?",
                     icon = icon("question"),
                     time = "13:45"
                   ),
                   messageItem(
                     from = "Support",
                     message = "Le nouveau serveur PYTHON est configuré.",
                     icon = icon("life-ring"),
                     time = "2023-12-01"
                   )
      ),
      
      # NOTIFICATION
      dropdownMenu(type = "notifications",
                   notificationItem(
                     text = "5 nouvelles utilisateurs aujourd'hui",
                     icon("users")
                   ),
                   notificationItem(
                     text = "12 actions délivrés",
                     icon("truck"),
                     status = "success"
                   ),
                   notificationItem(
                     text = "Rendez-vous avec la DOS à 12h sur ReMI",
                     icon = icon("exclamation-triangle"),
                     status = "warning"
                   )
      ),
      # TACHE
      dropdownMenu(type = "tasks", badgeStatus = "success",
                   taskItem(value = 90, color = "green",
                            "Documentation"
                   ),
                   taskItem(value = 17, color = "aqua",
                            "Projet TALE"
                   ),
                   taskItem(value = 75, color = "yellow",
                            "Server en deploiement"
                   ),
                   taskItem(value = 80, color = "red",
                            "Projet ReMI"
                   )
      )
    ),
    
    # E. Menu DEROULANT SIDEBAR  -------------------------------------------------
    
    #Barre de menu 
    dashboardSidebar(
      sidebarMenu(
        menuItem(" Caractéristiques population", tabName = "vue", icon = icon("person")),
        menuItem(" Offre sanitaire", tabName = "vue_2", icon = icon("briefcase-medical")),
        menuItem(" Déterminants de santé", tabName = "determinant", icon = icon("notes-medical")),
        menuItem(" Indicateurs du PRS3 ", tabName = "prs_3", icon = icon("business-time")),
        menuItem(" Informations sur les données", tabName = "introduction", icon = icon("circle-exclamation"))
        #menuItem(" Données", icon = icon("database"), href = "https://www.data.gouv.fr/fr/pages/donnees-sante/")
      )),
    
    # 1 - Dashbord BODY -------------------------------------------------
    #Page du dashboard
    dashboardBody(
      includeCSS("www/styles.css"),
      tabItems(
        
        # 1. - Introduction -------------------------------------------------
        tabItem(
          "introduction",
          fluidRow(
            column(width = 3,
                   helpText(HTML("<span style='color:black'><b>La sélection ci-dessous concerne la partie source</b></span>")),
                   selectInput(inputId = "source", label = "Choix du thème",
                               choices = c("Caractéristiques de la population", 
                                           "Offre sanitaire",
                                           "Déterminant de santé"
                                           
                                           # "Evolution",
                                           # "Mortalité",
                                           # "Personnes en situation de handicap",
                                           # "Médico-social grand âge",
                                           # "Offre sanitaire (en ville)",
                                           # "Etablissements de santé",
                                           # "Médico-sociale personnes en situation de handicap",
                                           # "Offre sanitaire - Focus",
                                           # "Déterminants de santé",
                                           # "Vaccination et dépistage",
                                           # "Pathologies chroniques",
                                           # "Etat de santé",
                                           # "Psychatrie générale",
                                           # "Psychatrie infanto-juvénile",
                                           # "Recours aux soins"
                                           
                                           ))
            ),
            column(width = 9,
                   # 1.2 - Main : tableau/graph/text ----------------------------------------
                   mainPanel(
                     tabsetPanel(
                       tabPanel( "Contexte",
                                 br(),
                                 h4("Ce tableau de bord contient les indicateurs de diagnostic territorial par Délégations Départementales et pour la région."),
                                 br(),
                                 HTML("<br>Ce tableau de bord est multisources. Vous trouverez pour chaque indicateur la source ainsi que l'année de la dernière mise à jour dans l'onglet <i>Sources et années</i>",
                                      "<br><b><span style='color:red'>Attention </span> les données ne sont pas toutes de la même année.</b>"),
                                 br(),
                                 HTML(
                                   "<br>Les indicateurs sont répartis en 3 grands thèmes :",
                                   "<br> - Données démographiques et socio-économiques,",
                                   "<br> - Offre sanitaire,",
                                   "<br> - Déterminants et état de santé de la population,"),
                                 br(),
                                 HTML("<br>Pour chacun des thèmes vous avez la possibilité de télécharger chaque tableau au format <i>xlsx</i>. <b>Une note est prévue à cet effet.</b>"),
                                 HTML("<br>De même lorsque que vous pouvez afficher ou non des tableaux/graphiques ou changer de département."),
                                 HTML("<br><br><br><b><span style='color:red'> Ce tableau de bord est toujours en construction et en vérification de sources.</span></b>")
                       ),
                       tabPanel("Sources et Années",
                                h1("Sources et années par variable"),
                                dataTableOutput(outputId = "sources")),
                       tabPanel("PRS3",
                                h1("Méthodologie"),
                                dataTableOutput(outputId = "sourcesPRS"))
                     )
                   ))
          )),
        
        
        # 2 - Caractéristiques population (première vue) -------------------------------------------------
        tabItem(
          "vue",
          fluidPage(
            # 2.1 - WIDGETS -------------------------------------------------
            fluidRow(
              # colonne 2
              column(
                width = 12,
                h2("Données"),
                # BOX de choix des départements
                box(
                  selectInput ("Dpt", "Niveau géographique",
                               choices = c(unique(population$Dpt))),
                  width = 6
                ),
                # BOX Date et Heure
                box(
                  verbatimTextOutput(outputId = "dateheure"),
                  width = 6,
                  height = "150%"
                ),
                # Infobox Habitants
              div(
                class = "rounded-info-box",
                infoBox(
                  title = "Nombre d'habitants",
                  value = textOutput(outputId = "habitants"),
                  # possibilite de mettre un icone pour illustrer
                  icon = icon ("chart-column"),
                  #subtitle = "Population actuelle",
                  fill = TRUE,
                  color = "light-blue",
                  width = 12
                  )
                ),
                # NATALITE
              div(
                class = "rounded-info-box",
                infoBox(
                  title = "Taux de Natalite",
                  value = textOutput(outputId = "progression"),
                  icon = icon("baby"),
                  fill = TRUE,
                  color = "green",
                  width = 6
                )
              ),
                #PAUVRETE
              div(
                class = "rounded-info-box",
                infoBox(
                  title = "Taux de pauvreté",
                  value = textOutput("pauvrette"),
                  #subtitle = "2019 à 2021",
                  icon = icon("sack-xmark"),
                  fill = TRUE,
                  color = "green",
                  width = 6
                )), 
                #ESPERANCE DE VIE HOMME
              div(
                class = "rounded-info-box",
                infoBox(
                  title = "Espérance de vie Homme",
                  value = textOutput("esp_h"),
                  #subtitle = "2019 à 2021",
                  icon = icon("heart"),
                  fill = TRUE,
                  color = "orange",
                  width = 6
                )),
                #ESPERENCE DE VIE FEMME
              div(
                class = "rounded-info-box",
                infoBox(
                  title = "Espérance de vie Femme",
                  value = textOutput("esp_f"),
                  #subtitle = "2019 à 2021",
                  icon = icon("heart"),
                  fill = TRUE,
                  color = "orange",
                  width = 6
                )),
                #ALLoc adultes handicape
              div(
                class = "rounded-info-box",
                infoBox(
                  title = "Nbre bénéficiares AAH",
                  value = textOutput("alloc"),
                  #subtitle = "2019 à 2021",
                  icon = icon("wheelchair"),
                  fill = TRUE,
                  color = "blue",
                  width = 6
                )),
              div(
                class = "rounded-info-box",
                infoBox(
                  title = "Nbre bénéficiares AEEH",
                  value = textOutput("alloc_e"),
                  #subtitle = "2019 à 2021",
                  icon = icon("wheelchair"),
                  fill = TRUE,
                  color = "blue",
                  width = 6
                )),
              div(
                class = "rounded-info-box",
                infoBox(
                  title = "Taux de chômage",
                  value = textOutput("chomage"),
                  #subtitle = "2019 à 2021",
                  icon = icon("briefcase"),
                  fill = TRUE,
                  color = "fuchsia",
                  width = 6
                )),
              div(
                class = "rounded-info-box",
                infoBox(
                  title = "Revenu salarial net moyen",
                  value = textOutput("salaire"),
                  #subtitle = "2019 à 2021",
                  icon = icon("dollar-sign"),
                  fill = TRUE,
                  color = "fuchsia",
                  width = 6
                )),
              div(
                class = "rounded-info-box",
                infoBox(
                  title = "Taux de mortalité infantile",
                  value = textOutput("mortalite_infantile"),
                  #subtitle = "2019 à 2021",
                  icon = icon("skull-crossbones"),
                  fill = TRUE,
                  color = "blue",
                  width = 6
                )),
              div(
                class = "rounded-info-box",
                infoBox(
                  title = "Taux brut de mortalité",
                  value = textOutput("mortalite_taux_brute"),
                  #subtitle = "2019 à 2021",
                  icon = icon("skull-crossbones"),
                  fill = TRUE,
                  color = "blue",
                  width = 6
                ))
              ),
              
              # trait de séparation entre les deux colonnes
              column(width = 12,tags$hr(style = "border-top: 2px solid black; width: 75%;")
              ),        
              
              # colonne 1
              column(
                width = 12,
                h2("Graphiques"),
                # BOX de choix des graphs
                box(
                  selectInput ("GraphChoice", "Graphique choisi",
                               choices = c("Population", "Natalité", "Pauvreté" , "Mortalité infantile", "AAH", "AEEH")),
                  width = 4
                ),
                box(
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("graphPop"),
                  width = 8
                )
                
              ),
            )
          )
        ),
        
        # 3 - Offre de soins ------------------------------------------------------
        tabItem(
          "vue_2",
          tabPanel("offre",
                   fluidRow(
                     # 3.1 - Widgets -----------------------------------------------------------
                     column(width = 2,
                            helpText(h4("Aide à l'utilisation"),
                                     HTML("<span style='color:black'>La barre de recherche permet de rechercher un indicateur présent dans <i>Offre sanitaire</i>.</span>")),
                            helpText(HTML("<b><span style='color:ForestGreen'>Note :</span><span style='color:black'> Les sélections suivantes",
                                          "concernent uniquement la partie <i>Offre sanitaire </i></b></span>")),
                            
                             selectInput(inputId = "DptS", label = "Niveau géographique",
                                        choices = c('75','77','78','91','92','93','94','95','IDF'))
                            
                           # selectInput("DptS", "Département choisi", choices = c("Ile de France", unique(population$Dpt)))),
                     ),
                     
                     # 3.2 - Main : tableau/graph/text >-----------------------------------------
                     
                     column(width = 10,
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Offre sanitaire (en ville)",
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de Gynécologues Obstétriciens",
                                           value = textOutput(outputId = "gyneco"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("user-doctor"),
                                           fill = TRUE,
                                           color = "green",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Densité de Gynéco Obstétriciens (pour 100 000 hab)",
                                             value = textOutput(outputId = "dens_gyneco"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("user-doctor"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre d'Infirmiers",
                                           value = textOutput("infirmier"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("user-nurse"),
                                           fill = TRUE,
                                           color = "orange",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Densité d'Infirmiers (pour 100 000 hab)",
                                             value = textOutput("dens_infirmier"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("user-nurse"),
                                             fill = TRUE,
                                             color = "orange",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de Laboratoires",
                                           value = textOutput("labo"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("microscope"),
                                           fill = TRUE,
                                           color = "blue",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Densité de Laboratoires (pour 100 000 hab)",
                                             value = textOutput("dens_labo"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("microscope"),
                                             fill = TRUE,
                                             color = "blue",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de Laboratoires polyvalents",
                                           value = textOutput("labo_polyv"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("microscope"),
                                           fill = TRUE,
                                           color = "yellow",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Densité de Laboratoires polyvalents (pour 100 000 hab)",
                                             value = textOutput("dens_labo_polyv"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("microscope"),
                                             fill = TRUE,
                                             color = "yellow",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de Médecins Généralistes",
                                           value = textOutput("med"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("stethoscope"),
                                           fill = TRUE,
                                           color = "aqua",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Densité de Médecins Généralistes (pour 100 000 hab)",
                                             value = textOutput("dens_med"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("stethoscope"),
                                             fill = TRUE,
                                             color = "aqua",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de Pédiatres",
                                           value = textOutput("pediatrie"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("stethoscope"),
                                           fill = TRUE,
                                           color = "purple",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Densité de Pédiatres (pour 100 000 hab)",
                                             value = textOutput("dens_pediatrie"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("stethoscope"),
                                             fill = TRUE,
                                             color = "purple",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de Sages-femmes",
                                           value = textOutput("sage_femme"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("user-nurse"),
                                           fill = TRUE,
                                           color = "fuchsia",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Densité de Sages-femmes (pour 100 000 hab)",
                                             value = textOutput("dens_sage_femme"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("user-nurse"),
                                             fill = TRUE,
                                             color = "fuchsia",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de Psychiatres",
                                           value = textOutput("psyCH"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("brain"),
                                           fill = TRUE,
                                           color = "maroon",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Densité de Psychiatres (pour 100 000 hab)",
                                             value = textOutput("dens_psyCH"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("brain"),
                                             fill = TRUE,
                                             color = "maroon",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de Masseurs - kinésithérapeutes",
                                           value = textOutput("masseur"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("hands-holding-child"),
                                           fill = TRUE,
                                           color = "yellow",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Densité de Masseurs-Kiné (pour 100 000 hab)",
                                             value = textOutput("dens_masseur"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("hands-holding-child"),
                                             fill = TRUE,
                                             color = "yellow",
                                             width = 12
                                           )),

                                           # trait de séparation entre les deux colonnes
                                           tags$hr(style = "border-top: 2px solid black; width: 75%;"),
                                           DTOutput(outputId = "ofsanitaire", width = '150%')
                                ),
                                tabPanel("Offre sanitaire - établissement de santé",
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre d’établissements de santé",
                                           value = textOutput(outputId = "nb_etablissement"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("hospital"),
                                           fill = TRUE,
                                           color = "green",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "DONT Secteur public",
                                           value = textOutput("secteur_public"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("hospital"),
                                           fill = TRUE,
                                           color = "green",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "DONT Secteur privé",
                                           value = textOutput("secteur_prive"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("hospital"),
                                           fill = TRUE,
                                           color = "green",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de séjours en MCO",
                                           value = textOutput("nb_sejours"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("truck-medical"),
                                           fill = TRUE,
                                           color = "orange",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Part régional de séjours MCO",
                                           value = textOutput("repartition_mco"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("chart-column"),
                                           fill = TRUE,
                                           color = "orange",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de journées de SSR",
                                           value = textOutput("nb_ssr"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("truck-medical"),
                                           fill = TRUE,
                                           color = "blue",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Répartition SSR",
                                           value = textOutput("repartition_ssr"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("chart-column"),
                                           fill = TRUE,
                                           color = "blue",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nb de journées de PSY",
                                           value = textOutput("nb_psy"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("truck-medical"),
                                           fill = TRUE,
                                           color = "yellow",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Répartition psy",
                                           value = textOutput("repartition_psy"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("chart-column"),
                                           fill = TRUE,
                                           color = "yellow",
                                           width = 12
                                         )),
                                         # trait de séparation entre les deux colonnes
                                        tags$hr(style = "border-top: 2px solid black; width: 75%;"),  
                                        DTOutput(outputId = "es", width = "150%")
                                    
                                ),
                                tabPanel("Offre médico-sociale",
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de Places PH adultes",
                                           value = textOutput(outputId = "ph_adulte"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("bed"),
                                           fill = TRUE,
                                           color = "green",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de Places PH enfants",
                                           value = textOutput("ph_enfant"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("bed"),
                                           fill = TRUE,
                                           color = "green",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre d'établissements pour personnes âgées",
                                           value = textOutput("nb_etab_age"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("person-cane"),
                                           fill = TRUE,
                                           color = "orange",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre d'établissements personnes âgées dépendantes",
                                           value = textOutput("nb_etab_age_inde"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("person-cane"),
                                           fill = TRUE,
                                           color = "orange",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Nombre de Résidences en autonomie",
                                           value = textOutput("residence_autonomie"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("hospital"),
                                           fill = TRUE,
                                           color = "blue",
                                           width = 12
                                         )),
                                         # trait de séparation entre les deux colonnes
                                         tags$hr(style = "border-top: 2px solid black; width: 75%;"),  
                                         DTOutput(outputId = "ms", width = '150%')
                                )
                                
                                
                              )
                            )),
                     
                     
                     ## FIN COLUM 9 width
                     ))),
        
        
        # 4 - Determinant de sante (quatrieme vue) -------------------------------------------------
        tabItem(
          "determinant",
          tabPanel("det", 
                   fluidRow(
                     # 4.2 - Widgets -----------------------------------------------------------
                     column(width = 2,
                            helpText(h4("Aide à l'utilisation"),
                                     HTML("<span style='color:black'>La barre de recherche permet de rechercher un indicateur présent dans <i>Déterminants de Santé et Etat de santé</i>.</span>")),
                            helpText(HTML("<b><span style='color:ForestGreen'>Note :</span><span style='color:black'> Les sélections suivantes",
                                          "concernent uniquement la partie <i>Déterminant de Santé et Etat de santé</i></b></span>")),
                            selectInput("DptDet", "Niveau géographique",
                                        choices = c(unique(population$Dpt)))
                            ),
                     
                     column(width = 10,
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Pathologies chroniques",
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de Diabète",
                                           value = textOutput(outputId = "diabete"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("hand-holding-medical"),
                                           fill = TRUE,
                                           color = "green",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de Maladie coronaire",
                                           value = textOutput("coronaire"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("hand-holding-medical"),
                                           fill = TRUE,
                                           color = "green",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de Maladies psychiatriques",
                                           value = textOutput("psy"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("brain"),
                                           fill = TRUE,
                                           color = "green",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de Démences",
                                           value = textOutput("demence"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("brain"),
                                           fill = TRUE,
                                           color = "green",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de Cancer du sein de la femme",
                                           value = textOutput("cancer_sein"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("circle-radiation"),
                                           fill = TRUE,
                                           color = "green",
                                           width = 12
                                         )),
                                         # trait de séparation entre les deux colonnes
                                         tags$hr(style = "border-top: 2px solid black; width: 75%;"),  
                                         DTOutput(outputId = "detsante1", width = '150%')
                                ),
                                tabPanel("Vaccination et dépistage",
                                         div(
                                            class = "rounded-info-box",
                                           infoBox(
                                           title = "Taux de participation au dépistage du Colon",
                                           value = textOutput(outputId = "colon"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("bacterium"),
                                           fill = TRUE,
                                           color = "blue",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de participation au dépistage du Sein",
                                           value = textOutput("sein"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("bacterium"),
                                           fill = TRUE,
                                           color = "blue",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de participation au dépistage du Col de l'utérus",
                                           value = textOutput("col_uterus"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("bacterium"),
                                           fill = TRUE,
                                           color = "blue",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de vaccination de l'HPV HOMME",
                                           value = textOutput("hpv_h"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("syringe"),
                                           fill = TRUE,
                                           color = "orange",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de vaccination de l'HPV FEMME",
                                           value = textOutput("hpv_f"),
                                           #subtitle = "2019 à 2021",
                                           icon  = icon("syringe"),
                                           fill  = TRUE,
                                           color = "orange",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de vaccination pour la Méningocoque C",
                                           value = textOutput("meningocoque"),
                                           #subtitle = "2019 à 2021",
                                           icon  = icon("syringe"),
                                           fill  = TRUE,
                                           color = "orange",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de vaccination pour la Coqueluche",
                                           value = textOutput("coqueluche"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("syringe"),
                                           fill = TRUE,
                                           color = "orange",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de vaccination pour le ROR",
                                           value = textOutput("ror"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("syringe"),
                                           fill = TRUE,
                                           color = "orange",
                                           width = 12
                                         )),
                                         div(
                                           class = "rounded-info-box",
                                         infoBox(
                                           title = "Taux de vaccination pour la Covid-19",
                                           value = textOutput("covid"),
                                           #subtitle = "2019 à 2021",
                                           icon = icon("syringe"),
                                           fill = TRUE,
                                           color = "orange",
                                           width = 12
                                         )),
                                         # trait de séparation entre les deux colonnes
                                         tags$hr(style = "border-top: 2px solid black; width: 75%;"),  
                                         DTOutput(outputId = "detsante2", width = '150%')
                                )
                              )
                            )),
            
                     # fin du 4
                   )
          )
        ),
        
        # 5 - Indicateur du PRS3 (cinquième vue) -------------------------------------------------
        tabItem(
          "prs_3",
          tabPanel("prs3", 
                   fluidRow(
                     # 4.2 - Widgets -----------------------------------------------------------
                     column(width = 2,
                            helpText(h4("Aide à l'utilisation")),
                            helpText(HTML("<b><span style='color:ForestGreen'>Note :</span><span style='color:black'> Les sélections suivantes",
                                          "concernent uniquement la partie <i>Indicateurs du PRS3</i></b></span>")),
                            
                            selectInput("DptDet4", "Niveau géographique", choices = c('IDF','75','77','78','91','92','93','94','95'))),
                            
                            #selectInput("DptDet4", "Niveau géographique", choices = c("IDF", unique(prs_indicBox$Dpt)))),
                     
                     column(width = 10,
                            mainPanel(
                              tabsetPanel(
                                tabPanel("PRS3", DTOutput(outputId = "PRSX", width = "150%")),
                                tabPanel("Axe 1",
                                         #regional
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nb de depistages VIH /str de prévention",
                                             value = textOutput(outputId = "indic_1"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "orange",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Taux de prévalence VIH",
                                             value = textOutput(outputId = ""),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "orange",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre partenariats ARS/CD - pour réduction mortalité périnatale",
                                             value = textOutput(outputId = "indic_2"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Part des femmes enceintes ayant effectué les trois échographies de suivi aux dates recommandées",
                                             value = textOutput(outputId = "indic_3"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         #regional
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre d'enfants 3-12 ans avec pgm CPS",
                                             value = textOutput(outputId = "indic_4"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "orange",
                                             width = 12
                                           ))
                                ),
                                tabPanel("Axe 2",
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "File active moyenne des DAC",
                                             value = textOutput(outputId = "indic_5"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre de structures/ES avec organisation pour femmes enceintes vulnérables",
                                             value = textOutput(outputId = "indic_6"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Taux de mortalité infantile",
                                             value = textOutput(outputId = "indic_7"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Taux de participation au depistage cancer du sein",
                                             value = textOutput(outputId = "indic_8.1"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Taux de participation au depistage cancer du col",
                                             value = textOutput(outputId = "indic_8.2"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Taux de participation au depistage cancer du colon",
                                             value = textOutput(outputId = "indic_8.3"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Part hospitalisations > 6 mois PSY",
                                             value = textOutput(outputId = "indic_9"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Taux d'hospitalisation urgences patient Psy sévères",
                                             value = textOutput(outputId = "indic_10"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre de mesures soins sans consentement",
                                             value = textOutput(outputId = "indic_11"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre d’aidants accompagnés par des plateformes de répit",
                                             value = textOutput(outputId = "indic_12"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         #regional
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre d'usagers de crack avec soins et hors rue",
                                             value = textOutput(outputId = "indic_13"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "orange",
                                             width = 12
                                           ))
                                ),
                                tabPanel("Axe 3",
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre de dossiers coordonnés",
                                             value = textOutput(outputId = "indic_14"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre de médecins/SAS",
                                             value = textOutput(outputId = "indic_15"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Taux d'hospitalisation directe des > 75 ans",
                                             value = textOutput(outputId = "indic_16"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre de passages au SAU/an",
                                             value = textOutput(outputId = "indic_17.1"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "yellow",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Dont + de 75 ans",
                                             value = textOutput(outputId = "indic_17.2"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "yellow",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre solutions installées pour PH ESMS",
                                             value = textOutput(outputId = "indic_18"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Taux de générosité",
                                             value = textOutput(outputId = "indic_19"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "orange",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre prélèvement SME",
                                             value = textOutput(outputId = "indic_20"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "orange",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Part des franciliens avec MT",
                                             value = textOutput(outputId = "indic_21.1"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "blue",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Part des franciliens ALD avec MT",
                                             value = textOutput(outputId = "indic_21.2"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "blue",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Part des franciliens < 16 ans avec MT",
                                             value = textOutput(outputId = "indic_21.3"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "blue",
                                             width = 12
                                           ))
                                ),
                                tabPanel("Axe 4",
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nb de CAE et Nb de CESP",
                                             value = textOutput(outputId = "indic_22"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nb de postes patagés ville/hôpital",
                                             value = textOutput(outputId = "indic_23"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "orange",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nb de postes partagés CHU/hors CHU",
                                             value = textOutput(outputId = "indic_23.1"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "orange",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Densité professionnels de santé",
                                             value = textOutput(outputId = "indic_24"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Part installations ZIP/total 1er recours",
                                             value = textOutput(outputId = "indic_25"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nb soignants en logements co-financé ARS",
                                             value = textOutput(outputId = "indic_26"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           ))
                                ),
                                tabPanel("Axe 5",
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre ES ayant faits exercices cybersécurité",
                                             value = textOutput(outputId = "indic_27"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Part ES et ESMS en conformité / Bilan gaz effets de serre",
                                             value = textOutput(outputId = "indic_28"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Part nouveaux CPOM ES et ESMS avec action dev durable",
                                             value = textOutput(outputId = "indic_29"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           ))
                                ),
                                tabPanel("Axe 6",
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Part communes avec CLSM/communes QPV",
                                             value = textOutput("indic_30"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "yellow",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre projets AAP ISS",
                                             value = textOutput("indic_31"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "yellow",
                                             width = 12
                                           ))
                                         
                                ),
                                tabPanel("PRAPS",
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Déployer le dispositif d’équipes mobiles médico-sociales",
                                             value = textOutput("indic_32"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre de médiateurs en santé dans ES",
                                             value = textOutput("indic_33"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           )),
                                         div(
                                           class = "rounded-info-box",
                                           infoBox(
                                             title = "Nombre de médiateurs en santé dans les structures regroupées",
                                             value = textOutput("indic_33.1"),
                                             #subtitle = "2019 à 2021",
                                             icon = icon("line-chart"),
                                             fill = TRUE,
                                             color = "green",
                                             width = 12
                                           ))
                                         
                                )
                              ),
                              
                              
                            )),
                    ))) # fin du 5
        
      )
    ),
    
    
    title = "Dashboard ARS IDF",
    skin = "blue"
  ),
  # Fin UI ------------------------------------------------------------------
  
  # SERVER ------------------------------------------------------------------
  server = function(input, output) {
    
    # 1. Fonction login --------------------------------------------------
    
    login_info <- reactiveValues(logged_in = FALSE, username = "")
    
    observeEvent(input$login, {
      # Check login credentials
      login_attempt <- users[users$username == input$username &
                               users$password == input$password, ]
      if (nrow(login_attempt) > 0) {
        login_info$logged_in <- TRUE
        login_info$username <- input$username
      } else {
        login_info$logged_in <- FALSE
        login_info$username <- ""
        showModal(modalDialog(
          title = "Error",
          "Erreur de login ou de mot de passe."
        ))
      }
    })
    output$welcome <- renderText({
      if (login_info$logged_in) {
        paste0("Bienvenue, ", login_info$username)
      } else {
        "Cliquez sur 'Login' pour continuer."
      }
    })
    
    # 2. Fonction generation de message --------------------------------------------------
    
    output$messageMenu <- renderMenu({
      # Code to generate each of the messageItems here, in a list. This assumes
      # that messageData is a data frame with two columns, 'from' and 'message'.
      msgs <- apply(messageData, 1, function(row) {
        messageItem(from = row[["from"]], message = row[["message"]])
      })
      
      # This is equivalent to calling:
      #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
      dropdownMenu(type = "messages", .list = msgs)
    })
    
    # renderText pour infobox dans UI 
    
    
    # VUE N°1 ------------------
    
    
    # dpt 
    donnees <- reactive ({
        hab = population %>%
          filter (Dpt == input$Dpt) 
      hab
    })
    
    # données progression evo 2012 à 2017 
    output$evo12 <- renderText({
      evol = donnees()
      paste(evol$evo12, '%')
    })
    
    # données progression evo 2017 à 2022 
    output$evo17 <- renderText({
      evol = donnees()
      paste(evol$evo17, '%')
    })
    
    # données progression natalite 
    output$progression <- renderText({
      evol = donnees()
      paste(evol$natali, '‰')
    })
    
        # données mortalite_infantile pour 1000 enfants
    output$mortalite_infantile <- renderText({
      evol = donnees()
      paste(evol$mortalite_infantile, '‰')
    })
    
    # données taux mortalite brute 
    output$mortalite_taux_brute <- renderText({
      evol = donnees()
      paste(evol$mortalite_taux_brute, '‰')
    })
    
    output$evolution <- renderPlot({
      evol = donnees()
      ggplot(evol$natali) + geom_line() + theme_minimal() + 
        labs(x = "" , y= "Nombre de personne")
      
    })
    
    # données pauvrette
    output$pauvrette <- renderText({
      pauvrette = donnees()
      paste(pauvrette$pauvrete, "%")
    })
    
    # données esp vie homme 
    output$esp_h <- renderText({
      esp = donnees()
      paste(esp$esp_H, " ans")
    })
    
    # données esp vie femme 
    output$esp_f <- renderText({
      espF = donnees()
      paste(espF$esp_F, " ans")
    })
    
    # données alloc adulte handicape
    output$alloc <- renderText({
      alloc = donnees()
      alloc_adulte_formatted <- format(alloc$alloc_adulte, big.mark = " ")
      paste(alloc_adulte_formatted, " ")
    })
    
    # données alloc enfant handicape
    output$alloc_e <- renderText({
      alloc_e = donnees()
      alloc_enfant_formatted <- format(alloc_e$alloc_enfant, big.mark = " ")
      paste(alloc_enfant_formatted, " ")
    })
    
    # données progression chomage
    output$chomage <- renderText({
      chomage = donnees()
      paste(chomage$chomage, '%')
    })
    
    # données Salaire
    output$salaire <- renderText({
      salaire = donnees()
      salaire2 <- format(salaire$salaire, big.mark = " ")
      paste(salaire2, "€")
      
    })
    
    # données habitants 
    output$habitants <- renderText({
      hab = donnees()
      habFormatted <- format(hab$n_hab, big.mark = " ")
      paste(habFormatted)
    })
    
    # dptDeterminant 
    donneesDet <- reactive ({
        hab = population %>%
          filter (Dpt == input$DptDet) 
      hab
    })
    
    # données ald tot 
    output$ald_tot <- renderText({
      ald = donneesDet()
      paste(ald$ald_tot, '%')
    })
    
    # données ald cancer 
    output$ald_cancer <- renderText({
      ald = donneesDet()
      paste(ald$ald_cancer, '%')
    })
    
    # données ald Diabete 
    output$ald_diabete <- renderText({
      ald = donneesDet()
      paste(ald$ald_diabete, '%')
    })
    # données colon
    output$colon <- renderText({
      ald = donneesDet()
      paste(ald$colon, '%')
    })
    
    # données sein
    output$sein <- renderText({
      ald = donneesDet()
      paste(ald$sein, '%')
    })
    
    # données col_uterus 
    output$col_uterus <- renderText({
      ald = donneesDet()
      paste(ald$col_uterus, '%')
    })
    
    # données hpv_f
    output$hpv_f <- renderText({
      ald = donneesDet()
      paste(ald$hpv_f, '%')
    })
    
    # données hpv_h
    output$hpv_h <- renderText({
      ald = donneesDet()
      paste(ald$hpv_h, '%')
    })
    
    # données meningocoque
    output$meningocoque <- renderText({
      ald = donneesDet()
      paste(ald$meningocoque, '%')
    })
    
    # données coqueluche
    output$coqueluche <- renderText({
      ald = donneesDet()
      paste(ald$coqueluche, '%')
    })
    
    # données ror 
    output$ror <- renderText({
      ald = donneesDet()
      paste(ald$ror, '%')
    })
    
    # données covid 
    output$covid <- renderText({
      ald = donneesDet()
      paste(ald$covid, '%')
    })
    
    # données diabete 
    output$diabete <- renderText({
      ald = donneesDet()
      paste(ald$diabete, '%')
    })
    
    # données coronaire 
    output$coronaire <- renderText({
      ald = donneesDet()
      paste(ald$coronaire, '%')
    })
    
    # données psy 
    output$psy <- renderText({
      ald = donneesDet()
      paste(ald$psy, '%')
    })
    
    # données cancer_sein 
    output$cancer_sein <- renderText({
      ald = donneesDet()
      paste(ald$cancer_sein, '%')
    })
    
    # données demence 
    output$demence <- renderText({
      ald = donneesDet()
      paste(ald$demence, '%')
    })
    
    # output$dateheure <- renderText({
    #   paste("Date et heure actuelle :", format(Sys.time(), "%d-%m-%Y %H:%M"))
    # })
    
    output$dateheure <- renderText({
    paste("Dernière mise à jour effectuée :", format(as.POSIXct("2024-02-25"), "%d-%m-%Y %H:%M"))
    })
    
    
    
    graphPopulationInput <- reactive({
      switch (input$GraphChoice,
              'Population'             =   graph_population,
              'Natalité'               =   graph_natali,
              'Pauvreté'               =   graph_pauvrete,
              'Mortalité'              =   graph_mortalite,
              'Mortalité infantile'    =   graph_mortalite_infantile,
              'AAH'                    =   graph_alloc_adulte,   
              'AEEH'                   =   graph_alloc_enfant
      )
    })
    
    output$graphPop <- renderPlotly({ graphPopulationInput() })
    
    # données démo socio-éco --------------------------------------------------
    output$population <- renderDataTable({ DT::datatable(population, rownames = FALSE, filter = 'top',
                                                         # container=sketch,
                                                         extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                                                buttons = c('csv', 'excel'),
                                                                                                pageLength = 20
                                                         )) })
    
    # VUE N°2  --------------------------------------------------
    # offre de soins ----------------------------------------------------------
    

    donneesDetOffre <- reactive ({
      if (input$DptS == "Ile de France") {
        hab = offre_sanitaire2 %>%
          filter (Dpt == 'IDF')
      } else {
        hab = offre_sanitaire2 %>%
          filter (Dpt == input$DptS)
      }
      hab
    })
    
    # données nb_etablissement
    output$nb_etablissement <- renderText({
      offre = donneesDetOffre()
      paste(offre$nb_etablissement, ' ')
    })
    
    # données secteur_public
    output$secteur_public <- renderText({
      offre = donneesDetOffre()
      paste(offre$secteur_public, ' ')
    })
    
    # données secteur_prive
    output$secteur_prive <- renderText({
      offre = donneesDetOffre()
      paste(offre$secteur_prive, ' ')
    })
    
    # données nb_sejours
    output$nb_sejours <- renderText({
      offre = donneesDetOffre()
      nb_sejoursFormatted <- format(offre$nb_sejours, big.mark = " ")
      paste(nb_sejoursFormatted)
    })
    
    # données repartition_mco
    output$repartition_mco <- renderText({
      offre = donneesDetOffre()
      paste(offre$repartition_mco, ' %')
    })
    
    # données nb_ssr
    output$nb_ssr <- renderText({
      offre = donneesDetOffre()
      nb_ssrFormatted <- format(offre$nb_ssr, big.mark = " ")
      paste(nb_ssrFormatted)
    })
    
    # données repartition_ssr
    output$repartition_ssr <- renderText({
      offre = donneesDetOffre()
      paste(offre$repartition_ssr, ' %')
    })
    
    # données nb_psy
    output$nb_psy <- renderText({
      offre = donneesDetOffre()
      nb_psyFormatted <- format(offre$nb_psy, big.mark = " ")
      paste(nb_psyFormatted)
    })
    
    # données repartition_psy
    output$repartition_psy <- renderText({
      offre = donneesDetOffre()
      paste(offre$repartition_psy, ' %')
    })
    
    
    # medico social -----
    
    donneesDetMedico <- reactive ({
      if (input$DptS == "Ile de France") {
        hab = medico2 %>%
          filter (Dpt == 'IDF')
      } else {
        hab = medico2 %>%
          filter (Dpt == input$DptS)
      }
    })
    
    donneesDetOffre <- reactive ({
      if (input$DptS == "Ile de France") {
        hab = offre_sanitaire2 %>%
          filter (Dpt == 'IDF')
      } else {
        hab = offre_sanitaire2 %>%
          filter (Dpt == input$DptS)
      }
      hab
    })
    
    # données ph_adulte
    output$ph_adulte <- renderText({
      offre = donneesDetMedico()
      paste(offre$ph_adulte, ' ')
    })
    
    # données ph_enfant
    output$ph_enfant <- renderText({
      offre = donneesDetMedico()
      paste(offre$ph_enfant, ' ')
    })
    
    # données nb_etab_age
    output$nb_etab_age <- renderText({
      offre = donneesDetMedico()
      paste(offre$nb_etab_age, ' ')
    })
    
    # données nb_etab_age_inde
    output$nb_etab_age_inde <- renderText({
      offre = donneesDetMedico()
      paste(offre$nb_etab_age_inde, ' ')
    })
    
    # données residence_autonomie
    output$residence_autonomie <- renderText({
      offre = donneesDetMedico()
      paste(offre$residence_autonomie, ' ')
    })
    
    
    donneesDetVille<- reactive ({
      if (input$DptS == "Ile de France") {
        hab = ville2 %>%
          filter (Dpt == 'IDF')
      } else {
        hab = ville2 %>%
          filter (Dpt == input$DptS)
      }
      hab
    })
    
    # données gyneco
    output$gyneco <- renderText({
      offre = donneesDetVille()
      paste(offre$gyneco, ' ')
    })
    
    output$dens_gyneco <- renderText({
      offre = donneesDetVille()
      paste(offre$dens_gyneco, ' ')
    })
    
    # données infirmier
    output$infirmier <- renderText({
      offre = donneesDetVille()
      paste(offre$infirmier, ' ')
    })
    
    output$dens_infirmier <- renderText({
      offre = donneesDetVille()
      paste(offre$dens_infirmier, ' ')
    })
    
    # données labo
    output$labo <- renderText({
      offre = donneesDetVille()
      paste(offre$labo, ' ')
    })
    
    output$dens_labo <- renderText({
      offre = donneesDetVille()
      paste(offre$dens_labo, ' ')
    })
    
    # données labo_polyv
    output$labo_polyv <- renderText({
      offre = donneesDetVille()
      paste(offre$labo_polyv, ' ')
    })
    
    output$dens_labo_polyv <- renderText({
      offre = donneesDetVille()
      paste(offre$dens_labo_polyv, ' ')
    })
    
    # données masseur
    output$masseur <- renderText({
      offre = donneesDetVille()
      paste(offre$masseur, ' ')
    })
    
    output$dens_masseur <- renderText({
      offre = donneesDetVille()
      paste(offre$dens_masseur, ' ')
    })
    
    # données med
    output$med <- renderText({
      offre = donneesDetVille()
      paste(offre$med, ' ')
    })
    
    output$dens_med <- renderText({
      offre = donneesDetVille()
      paste(offre$dens_med, ' ')
    })
    
    # données psyCH
    output$psyCH <- renderText({
      offre = donneesDetVille()
      paste(offre$psyCH, ' ')
    })
    
    output$dens_psyCH <- renderText({
      offre = donneesDetVille()
      paste(offre$dens_psyCH, ' ')
    })
    
    # données pediatrie
    output$pediatrie <- renderText({
      offre = donneesDetVille()
      paste(offre$pediatrie, ' ')
    })
    
    output$dens_pediatrie <- renderText({
      offre = donneesDetVille()
      paste(offre$dens_pediatrie, ' ')
    })
    
    # données sage_femme
    output$sage_femme <- renderText({
      offre = donneesDetVille()
      paste(offre$sage_femme, ' ')
    })
    
    output$dens_sage_femme <- renderText({
      offre = donneesDetVille()
      paste(offre$dens_sage_femme, ' ')
    })

    
    output$es <- renderDataTable({ DT::datatable(es, rownames = FALSE, filter = 'top',
                                                 # container=sketch,
                                                 extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                                        buttons = c('csv', 'excel'),
                                                                                        scrollX = TRUE,
                                                                                        columnDefs = list(list(width = '175px', targets = '_all'))
                                                 )) })
    
    output$ofsanitaire <- renderDataTable({ DT::datatable(ville2tab, rownames = FALSE, filter = 'top',
                                                 # container=sketch,
                                                 extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                                        buttons = c('csv', 'excel'),
                                                                                        scrollX = TRUE,
                                                                                        columnDefs = list(list(width = '175px', targets = '_all'))
                                                 )) })
    
    output$ms <- renderDataTable({ DT::datatable(ms, rownames = FALSE, filter = 'top', 
                                                 # container=sketch,
                                                 extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                                        buttons = c('csv', 'excel'),
                                                                                        scrollX = TRUE,
                                                                                        columnDefs = list(list(width = '175px', targets = '_all'))
                                                 )) })
    
    output$detsante1 <- renderDataTable({ DT::datatable(detsante1, rownames = FALSE, filter = 'top',
                                                          # container=sketch,
                                                          extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                                                 buttons = c('csv', 'excel'),
                                                                                                 scrollX = TRUE,
                                                                                                 columnDefs = list(list(width = '175px', targets = '_all'))
                                                          )) })
    
    output$detsante2 <- renderDataTable({ DT::datatable(detsante2, rownames = FALSE, filter = 'top',
                                                          # container=sketch,
                                                          extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                                                 buttons = c('csv', 'excel'),
                                                                                                 scrollX = TRUE,
                                                                                                 columnDefs = list(list(width = '175px', targets = '_all'))
                                                          )) })
    
    
    output$offre_handi <- renderDataTable({ DT::datatable(offre_handi, rownames = FALSE, filter = 'top', 
                                                          # container=sketch,
                                                          extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                                                 buttons = c('csv', 'excel')
                                                          )) })
    output$focus <- renderDataTable({ DT::datatable(focus, rownames = FALSE, filter = 'top', 
                                                    # container=sketch,
                                                    extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                                           buttons = c('csv', 'excel')
                                                    )) })
    
    
    prs3Input <- reactive({
      switch(input$DptDet4,
             "IDF" = prs_idf,
             "75"  = prs_75,
             "77"  = prs_77,
             "78"  = prs_78,
             "91"  = prs_91,
             "92"  = prs_92,
             "93"  = prs_93,
             "94"  = prs_94,
             "95"  = prs_95)
    })
    
    output$PRSX <- renderDataTable({ DT::datatable(prs3Input(), rownames = FALSE, filter = 'top',
                                                   # container=sketch,
                                                   extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                                          buttons = c('csv', 'excel'),
                                                                                          scrollX = TRUE,
                                                                                          pageLength = 13,
                                                                                          columnDefs = list(list(width = '175px', targets = '_all'))
                                                   )) })
    
    donneesPRS3 <- reactive ({
      if (input$DptDet4 == "Ile de France") {
        hab = prs_indicBox %>%
          filter (Dpt == 'IDF')
      } else {
        hab = prs_indicBox %>%
          filter (Dpt == input$DptDet4)
      }
      hab
    })
    
    output$indic_1 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_1, ' ')
    })
    
    output$indic_2 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_2, ' ')
    })
    
    output$indic_3 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_3, ' ')
    })
    
    output$indic_4 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_4, ' ')
    })
    
    output$indic_5 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_5, ' ')
    })
    
    output$indic_6 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_6, ' ')
    })
    
    output$indic_7 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_7, ' ')
    })
    
    output$indic_8.1 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_8.1, ' ')
    })
    
    output$indic_8.2 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_8.2, ' ')
    })
    
    output$indic_8.3 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_8.3, ' ')
    })
    
    output$indic_9 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_9, ' ')
    })
    
    output$indic_10 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_10, ' ')
    })
    
    output$indic_11 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_11, ' ')
    })
    
    output$indic_12 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_12, ' ')
    })
    
    output$indic_13 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_13, ' ')
    })
    
    output$indic_14 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_14, ' ')
    })
    
    output$indic_15 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_15, ' ')
    })
    
    output$indic_16 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_16, ' ')
    })
    
    output$indic_17.1 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_17.1, ' ')
    })
    
    output$indic_17.2 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_17.2, ' ')
    })
    
    output$indic_18 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_18, ' ')
    })
    
    output$indic_19 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_19, ' ')
    })
    
    output$indic_20 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_20, ' ')
    })
    
    output$indic_21.1 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_21.1, ' ')
    })
    
    output$indic_21.2 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_21.2, ' ')
    })
    
    output$indic_21.3 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_21.3, ' ')
    })
    
    output$indic_22 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_22, ' ')
    })
    
    output$indic_23 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_23, ' ')
    })
    
    output$indic_23.1 <- renderText({
      indic = donneesPRS3()
      paste(' A realiser ')
    })
    
    output$indic_24 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_24, ' ')
    })
    
    output$indic_25 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_25, ' ')
    })
    
    output$indic_26 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_26, ' ')
    })
    
    output$indic_27 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_27, ' ')
    })
    
    output$indic_28 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_28, ' ')
    })
    
    output$indic_29 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_29, ' ')
    })
    
    output$indic_30 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_30, ' ')
    })
    
    output$indic_31 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_31, ' ')
    })
    
    output$indic_32 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_32, ' ')
    })
    
    output$indic_33 <- renderText({
      indic = donneesPRS3()
      paste(indic$indic_33, ' ')
    })
    
    output$indic_33.1 <- renderText({
      indic = donneesPRS3()
      paste('A realiser ')
    })
    
    
    # cas particulier - liberal -----------------------------------------------
    
    effInput <- reactive({
      switch (input$DptS,
              '75' =    lib_eff_75,
              '77' =    lib_eff_77,
              '78' =    lib_eff_78,
              '91' =    lib_eff_91,
              '92' =    lib_eff_92,
              '93' =    lib_eff_93,
              '94' =    lib_eff_94,
              '95' =    lib_eff_95,
              'IDF' =   lib_eff_idf
      )
    })
    
    output$eff <- DT::renderDataTable({ DT::datatable(effInput(), rownames = FALSE, filter = 'top', 
                                                      # container=sketch,
                                                      extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                                             buttons = c('csv', 'excel')
                                                      )) %>% 
        formatStyle(columns = 4, 
                    backgroundColor = styleInterval(c('0%', '10%', '1000%'), c('red', 'ForestGreen', 'ForestGreen', "ForestGreen"))
        )
    }) 
    
    
    # ps
    
    psInput <- reactive({
      switch (input$Dpt,
              '75' = ps %>% 
                filter(LIB_DEP == "PARIS") %>% 
                select(-LIB_DEP),
              '77' = ps %>% 
                filter(LIB_DEP == "SEINE-ET-MARNE") %>% 
                select(-LIB_DEP),
              '78' = ps %>% 
                filter(LIB_DEP == "YVELINES") %>% 
                select(-LIB_DEP),
              '91' = ps %>% 
                filter(LIB_DEP == "ESSONNE") %>% 
                select(-LIB_DEP),
              '92' = ps %>% 
                filter(LIB_DEP == "HAUTS-DE-SEINE") %>% 
                select(-LIB_DEP),
              '93' = ps %>% 
                filter(LIB_DEP == "SEINE-SAINT-DENIS") %>% 
                select(-LIB_DEP),
              '94' = ps %>% 
                filter(LIB_DEP == "VAL-DE-MARNE") %>% 
                select(-LIB_DEP),
              '95' = ps %>% 
                filter(LIB_DEP == "VAL-D'OISE") %>% 
                select(-LIB_DEP),
              'IDF' = ps %>% 
                filter(LIB_DEP == 'ILE-DE-FRANCE') %>% 
                select(-LIB_DEP)
      )
    })
    
    output$ps <- DT::renderDataTable({ DT::datatable(psInput(), rownames = FALSE, filter = 'none', 
                                                     # container=sketch,
                                                     extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                                            buttons = c('csv', 'excel'),
                                                                                            autoWidth = T,
                                                                                            columnDefs = list(list(width = '175px', targets = '_all'))
                                                     )) %>% 
        formatStyle(columns = 4, 
                    backgroundColor = styleInterval(c('0%', '10%', '1000%'), c('Tomato', 'PaleGreen', 'PaleGreen', "PaleGreen"))
        )
    }) 
    
    
    
    # Sources et années -------------------------------------------------------
    
    sourcesInput <- reactive({
      switch(input$source,
             "Caractéristiques de la population" = source_pop,
             "Offre sanitaire"                   = source_offre_sanitaire,
             "Déterminant de santé"              = source_determinant_sante)
    
             # "Evolution" = source_evol,
             # "Mortalité" = source_mort,
             # "Personnes en situation de handicap" = source_handi,
             # "Médico-social grand âge" = source_age,
             # "Etablissements de santé" = source_es,
             # "Médico-sociale personnes en situation de handicap" = source_offre_handi,
             # "Offre sanitaire - Focus" = source_focus,
             # "Déterminants de santé" = source_det,
             # "Vaccination et dépistage" = source_vacsi,
             # "Pathologies chroniques" = source_patho,
             # "Etat de santé" = source_etat,
             # "Psychatrie générale" = source_psy,
             # "Psychatrie infanto-juvénile" = source_psy_enf,
             # "Recours aux soins" = source_recours)
    })
    
    output$sources <- renderDataTable({ DT::datatable(sourcesInput(), options = list(pageLength = 10)) })
    
    
    output$sourcesPRS <- renderDataTable({
      datatable(source_PRS3, escape = FALSE, rownames = FALSE, filter = 'top',
                extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('csv', 'excel'),
                  scrollX = TRUE,
                  columnDefs = list(list(width = '175px', targets = '_all'))
                )
      )
    })
    
    
    
    
    # FIN DE BOUCLE ------------------------ 
  }
  
)