source("functions.R", encoding = "UTF-8")
source("libraries.R", encoding = "UTF-8")
source("load_data.R", encoding = "UTF-8")

source("modules/mod_ban.R", encoding = "UTF-8")

Sys.setlocale("LC_TIME", "French")

# DATE DE MISE A JOUR ####
LAST_UPDATE_DATE <- "11 Mars 2024"

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
baseGroups <- c("OpenStreetMap", "Ortho IGN", "Plan IGN")
overlayGroups <- c("Parcelles IGN")
# baseGroups <- c("Ortho IGN", "Plan IGN", "OpenStreetMap", "OSM Stamen")
# overlayGroups <- c("Villes", "Parcelles IGN")

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
  "Région Occitanie"                             = "Occitanie",
  "Établissement Public Foncier de Grand Est"    = "Grand Est",
  "Établissement Public Foncier de Normandie"    = "Normandie",  
  "DDT de l'Ain"                                 = "Ain",
  "DDT des Ardennes"                             = "Ardennes",
  "DDT du Cantal"                                = "Cantal",
  "DDT de la Loire"                              = "Loire",
  "DDT de la Marne"                              = "Marne",
  "DDTM de la Somme"                             = "Somme",
  "Grand Angoulême"                              = "Grand Angouleme",
  "Agence d'Urbanisme du Grand Amiénois (ADUGA)" = "ADUGA", 
  "Commune de Fougères"                          = "Fougeres"
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
couleur_friche$potentielles <- "#a2a2a2"
couleur_friche$sans_projet  <- "orange"
couleur_friche$avec_projet  <- "#70AD25"
couleur_friche$reconverties <- "#BBF870"

# COULEUR DES ICONES SUR LA CARTE ----
# Le couleur des choix est plus limité pour les icônes de la carte
# Liste des couleurs dispos : https://github.com/lennardv2/Leaflet.awesome-markers/blob/2.0/develop/dist/leaflet.awesome-markers.css
couleur_icone <- list()
couleur_icone$potentielles <- "lightgray" # #a2a2a2
couleur_icone$sans_projet  <- "orange" # #ffc98f
couleur_icone$avec_projet  <- "green" # #eb912e
couleur_icone$reconverties <- "lightgreen" # #6eaa25

# ICONE DES FRICHES SELON LE TYPE ----
icone_friche <- list()
icone_friche$potentielles <- "fa-question-circle"
icone_friche$sans_projet  <- "fa-industry"
icone_friche$avec_projet  <- "fa-industry"
icone_friche$reconverties <- "fa-check-circle" #building

# FILTRES ----
Filtres <- c("Friches sans projet"  = "friche sans projet",
             "Friches avec projet"  = "friche avec projet", 
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
  "Grand Est" = list(img = "logos/Logo_EPFGE.png", 
                  height = 70, 
                  message = "Bienvenue en territoire Grand Est"),
  
  Normandie = list(img = "logos/Logo_EPFNormandie.jpg", 
                   height = 70, 
                   message = "Bienvenue en territoire Normandie"),
  
  Somme = list(img = "logos/logo-DDTM-Somme.jpg", 
                   height = 70, 
                   message = "Bienvenue en territoire de la Somme"),
  
  Cantal = list(img = "logos/logo-DDT-Cantal.jpg", 
               height = 70, 
               message = "Bienvenue en territoire du Cantal"),
  
  Loire = list(img = "logos/logo-DDT-Loire.png", 
                height = 70, 
                message = "Bienvenue en territoire de la Loire")
  
  
)

# UI ----
Txt_search <- "Adresse, commune, département, région ou parcelle"

br_code <- "<br style='content: \"\"; margin: -5px;display: block;'>"