source("functions.R", encoding = "UTF-8")
source("libraries.R", encoding = "UTF-8")
source("load_data.R", encoding = "UTF-8")

source("modules/mod_ban.R", encoding = "UTF-8")

Sys.setlocale("LC_TIME", "French")

# DATE DE MISE A JOUR ####
LAST_UPDATE_DATE <- "6 Décembre 2023"

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

# COULEURS DES FRICHES DANS LA LEGENDE ----
couleur_friche <- list()
couleur_friche$potentielles <- "#a2a2a2"
couleur_friche$sans_projet  <- "#ffc98f"
couleur_friche$avec_projet  <- "#eb912e"
couleur_friche$reconverties <- "lightblue"

# COULEUR DES ICONES SUR LA CARTE ----
# Le couleur des choix est plus limité pour les icônes de la carte
# Liste des couleurs dispos : https://github.com/lennardv2/Leaflet.awesome-markers/blob/2.0/develop/dist/leaflet.awesome-markers.css
couleur_icone <- list()
couleur_icone$potentielles               <- "lightgray" # #a2a2a2
couleur_icone$sans_projet              <- "beige" # #ffc98f
couleur_icone$avec_projet         <- "orange" # #eb912e
couleur_icone$reconverties                <- "lightblue"

# ICONE DES FRICHES SELON LE TYPE ----
icone_friche <- list()
icone_friche$potentielles                <- "fa-industry"
icone_friche$sans_projet              <- "fa-industry"
icone_friche$avec_projet       <- "fa-industry"
icone_friche$reconverties                <- "fa-building"

# FILTRES ----
Filtres <- c("Friches sans projet" = "friche sans projet",
             "Friches avec projet" = "friche avec projet", 
             "Friches reconverties" = "friche reconvertie")

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