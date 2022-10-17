source("functions.R", encoding = "UTF-8")
source("libraries.R", encoding = "UTF-8")
source("load_data.R", encoding = "UTF-8")

# source("modules/mod_carte.R", encoding = "UTF-8")
source("modules/mod_ban.R", encoding = "UTF-8")

Sys.setlocale("LC_TIME", "French")

# DATE DE MISE A JOUR ####
LAST_UPDATE_DATE <- "5 Avril 2022"

# PALETTE CEREMA ####
couleurs_cerema <- readRDS("data/couleurs_cerema.rds")
palette_cerema <- readRDS("data/palette_cerema.rds")

# MATOMO ####
matomo <- "matomo.txt"

# COULEURS ####
palette <- fromJSON(file = "www/palette_cerema.json")
orange <- palette$secondaire$orange
gris <- "#e4e4e4"

# LEAFLET ####
baseGroups <- c("Ortho IGN", "Plan IGN", "OpenStreetMap", "OSM Stamen")
overlayGroups <- c("Villes", "Parcelles IGN")

# Icônes Leaflet
icon <- awesomeIcons(
  icon = 'fa-industry',
  iconColor = 'black',
  library = 'fa',
  markerColor = "blue"
)

# BBOX ####
bb_ini <- readRDS("data/contours/bb_fr.rds")
coords_fr <- c(as.numeric(bb_ini[1]),
               as.numeric(bb_ini[2]),
               as.numeric(bb_ini[3]),
               as.numeric(bb_ini[4]))

# # LOAD_DATA ####
# load_data()

# ZOOM LEVELS ####
# Niveaux de zoom conditionnant l'affichage de certaines entités (cercles statistiques, marqueurs ou surfaces)
ZOOM_LEVELS <- c("Région"      = 6, 
                 "Département" = 8, 
                 "Commune"     = 9,
                 "Marqueur"    = 12) 

# OBSERVATOIRES ####
Observatoires <- c(
  "Agence d'Urbanisme du Grand Amiénois (ADUGA)" = "ADUGA", 
  "Commune de Fougères"                          = "Fougeres",
  "DDT de l'Ain"                                 = "Ain",
  "DDT des Ardennes"                             = "Ardennes",
  "DDT de la Marne"                              = "Marne",
  "Établissement Public Foncier de Grand Est"    = "GrandEst",
  "Grand Angoulême"                              = "Grand Angouleme",
  "Région Occitanie"                             = "Occitanie"
) %>% get_slc(label = "Sélectionnez un observatoire")

Secteurs <- c("Métropole", 
  "Guadeloupe",
  "Guyane",
  "La Réunion",
  "Martinique",
  "Mayotte") %>% get_slc(label = "Sélectionnez un territoire")

SEARCH_DISTANCE <- 2000

# COULEURS DES FRICHES SELON LE TYPE ----
# Dans la légende
couleur_friche <- list()
couleur_friche$mte               <- "#5ab1ce"
couleur_friche$ademe              <- "#5ab1ce"
couleur_friche$observatoire       <- "#ff90e9"
couleur_friche$aap                <- "#39a855"
# couleur_friche$mte_pv             <- "#39a855"
couleur_friche$mte_pv             <- "#f69730"
couleur_friche$user               <- "#d152b8"
couleur_friche$mte_non_expertise  <- "#c5c5c5"

# COULEUR DES ICONES SUR LA CARTE ----
# Le couleur des choix est plus limité pour les icônes de la carte
# Liste des couleurs dispos : https://github.com/lennardv2/Leaflet.awesome-markers/blob/2.0/develop/dist/leaflet.awesome-markers.css
couleur_icone <- list()
couleur_icone$mte               <- "blue"
couleur_icone$ademe              <- "blue"
couleur_icone$observatoire       <- "pink"
couleur_icone$aap                <- "green"
# couleur_icone$mte_pv             <- "green"
couleur_icone$mte_pv             <- "orange"
couleur_icone$user               <- "purple"
couleur_icone$mte_non_expertise <- "lightgray"

# ICONE DES FRICHES SELON LE TYPE ----
icone_friche <- list()
icone_friche$mte                <- "fa-industry"
icone_friche$ademe              <- "fa-industry"
icone_friche$observatoire       <- "fa-tag"
icone_friche$aap                <- "fa-building"
icone_friche$mte_pv             <- "fa-sun"
icone_friche$user               <- "fa-user"
icone_friche$mte_non_expertise  <- "fa-industry"

# FILTRES ----
Filtres <- c("Données nationales" = "mte",
             "Données locales" = "observatoire", 
             "Appels à Projets" = "aap", 
             "Potentiel solaire au sol" = "mte_pv", 
             "Retours utilisateurs" = "user")

# LOGOS DES OBSERVATOIRES (IMAGE, TAILLE ET MESSAGE D'ACCUEIL) ----
Logos <- list(
  MTE = list(img = "logos/logo-brgm-mtes.png", 
               height = 60, 
               message = "Friches vérifiées MTE"),
  "MTE PV" = list(img = "logos/logo-mte.png", 
              height = 80, 
              message = "Friches qualifiées MTE"),
  ADEME = list(img = "logos/logo-brgm-mtes.png", 
              height = 60, 
              message = "Friches vérifiées Ademe"),
  AAP = list(img = "logos/logo-app.png", 
               height = 70, 
               message = "Friches qualifiées AAP"),
  ADUGA = list(img = "logos/logo-org-aduga.png", 
               height = 60, 
               message = "Bienvenue sur le Grand Amiénois"),
  Ain = list(img = "logos/logo-ddt-ain.png", 
             height = 70, 
             message = "Bienvenue dans l'Ain"),
  Ardennes = list(img = "logos/logo-org-ardennes.png", 
                  height = 80, 
                  message = "Bienvenue sur le territoire des Ardennes"),
  "Grand Angouleme" = list(img = "logos/grand-angouleme.jpg", 
                  height = 80, 
                  message = "Bienvenue sur le territoire du Grand Angoulême"),
  Marne = list(img = "logos/logo-org-marne.png", 
               height = 110, 
               message = "Bienvenue sur le territoire de la Marne"),
  Fougeres = list(img = "logos/logo-fougeres.png", 
                  height = 50, 
                  message = "Bienvenue sur la commune de Fougères"),
  Occitanie = list(img = "logos/logo-occitanie.png", 
                   height = 70, 
                   message = "Bienvenue en Région Occitanie"),
  Lorraine = list(img = "logos/Logo_EPFGE.png", 
                  height = 70, 
                  message = "Bienvenue en territoire Grand Est")
)

# UI ----
Txt_search <- "Adresse, commune, département, région ou parcelle"

br_code <- "<br style='content: \"\"; margin: -5px;display: block;'>"