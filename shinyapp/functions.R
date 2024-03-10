source("modules/mod_ban.R", encoding = "UTF-8")

format_surface <- function(x) { paste0(format(round(x,0), big.mark = " ", decimal.mark = ",", trim = TRUE, scientific = FALSE)," m²") }
format_numericFR <- function(x) { paste0(format(round(x,0), big.mark = " ", decimal.mark = ",", trim = TRUE, scientific = FALSE)) }


# > CARTE ----

# Ajout des tiles les plus utiles
add_tiles <- function(m) {
  
  m %>% 
    
    # OSM
    addTiles(group = "OpenStreetMap") %>% 
    
    # Ortho
    addTiles("http://wxs.ign.fr/choisirgeoportail/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/jpeg&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
             options = c(WMSTileOptions(tileSize = 256), 
                         providerTileOptions(minZoom = 1, maxZoom = 22)),
             attribution='<a target="_blank" href="https://www.geoportail.gouv.fr/">Geoportail France</a>',
             group = "Ortho IGN"
    ) %>%
    
    # Plan
    addTiles("http://wxs.ign.fr/choisirgeoportail/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
             options = WMSTileOptions(tileSize = 256, minZoom = 1, maxZoom = 20),
             group = "Plan IGN"
    ) %>%
    
    # Parcelles
    addTiles("http://wxs.ign.fr/choisirgeoportail/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&style=PCI%20vecteur&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=CADASTRALPARCELS.PARCELLAIRE_EXPRESS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
             options = WMSTileOptions(tileSize = 256),
             attribution='<a target="_blank" href="https://www.geoportail.gouv.fr/">Geoportail France</a>',
             group = "Parcelles IGN"
    )
    
}

# Ajoute les cercles statistiques à la carte
# Lors du lancement de Cartofriches, ce sont les stats qui sont affichées
add_circles <- function(proxy, 
                        f, 
                        group = "group", 
                        chk_all = FALSE) {
  
  message(">> add_circles")
  
  scale_linear <- function(value) {
    scaled <- (value - min(value)) / diff(range(value))
    return(scaled)
  }
  
  ##=##=##=##=##=##=##=##=##
  #  Filtre des données  ##
  ##=##=##=##=##=##=##=##=##
  # On calcule le nb de friches total
  # ce dernier sera affiché dans le cercle et conditionnera sa taille
  if(chk_all) {
    f$n_friches <- f$n_friches_avec_projet + 
      f$n_friches_sans_projet + 
      f$n_friches_reconverties + 
      f$n_friches_potentielles
  } else {
    f$n_friches <- f$n_friches_avec_projet + 
      f$n_friches_sans_projet + 
      f$n_friches_reconverties
  }
  
  if(group %in% c("stat_comm", "stat_iris")) f <- f %>% filter(n_friches > 0)
  
  ##=##=##=##=##=##=##=##=##
  #  Tailles de cercles  ##
  ##=##=##=##=##=##=##=##=##
  values <- f$n_friches
  if(group %in% c("stat_comm", "stat_iris")) {
    values <- values^2
  }
  
  get_sizes <- function(values, min_size=5, max_size=30){
    if(length(unique(values)) == 1) {
      circle_sizes = max_size + min_size
    } else {
      circle_sizes <- (scale_linear(values) * max_size) + min_size
    }
    circle_sizes
  }
  
  circle_sizes <- get_sizes(values, min_size=8, max_size=30)
  
  ##=##=##=##=##=##
  #  Couleurs   ##
  ##=##=##=##=##=##
  get_fillColor <- function(n_friches) {
    if(n_friches == 0) {
      "#808285"
    } else {
      "#F49D54"
    }
  }
  
  get_strokeColor <- function(n_friches) {
    if(n_friches == 0) {
      "#BCBDC0"
    } else {
      "#EF7757"
    }
  }
  
  fillColors <- sapply(f$n_friches, get_fillColor) %>% as.character
  strokeColors <- sapply(f$n_friches, get_strokeColor) %>% as.character
  
  ##=##=##=##=##
  #  Popup   ##
  ##=##=##=##=##
  
  ##=##=##=##=##
  # Titre
  ##=##=##=##=##
  popup_titres <- lapply(1:nrow(f), function(i) {
    tags$p(tags$b(f$libelle[i]))})
  
  ##=##=##=##=##=#=#
  # Stats friches
  ##=##=##=##=##=#=#
  # n_friches_observatoires <- f$n_friches_industrielles_observatoires
  # n_friches_qualifiees    <- f$n_friches_industrielles_mte_qualifiees
  
  popup_stats <- lapply(1:nrow(f), function(x) {
    # print(x)
    f_sel <- f[x, ]
    
    stats <- list()
    
    stats$avec_projet       <- f_sel$n_friches_avec_projet
    stats$sans_projet                <- f_sel$n_friches_sans_projet
    stats$reconverties             <- f_sel$n_friches_reconverties
    stats$potentielles               <- f_sel$n_friches_potentielles
    
    div(
      get_ui_legende(stats, 
                     chk_all = chk_all, 
                     popup = TRUE)
    )
  })
  
  # Popups
  popups <- lapply(1:length(f$n_friches), function(i) {
    
    tags$div(class='map_popup',
             popup_titres[[i]],
             popup_stats[[i]]) %>% as.character
    
  })
  
  popups <- popups %>% as.list %>% lapply(HTML)
  
  ##=##=##=##=##
  #  Carte   ##
  ##=##=##=##=##
  circle_sizes <- as.numeric(circle_sizes)
  proxy %>%
    clearMarkers() %>%
    addMapPane(group, zIndex = 500) %>% 
    addCircleMarkers(
      lng = f$Long, lat = f$Lat,
      layerId = f$layerId,
      weight = 3,
      fillColor = fillColors,
      color = strokeColors,
      opacity = 1,
      fillOpacity = 0.5,
      radius = circle_sizes,
      label = popups,
      group = group
      ,
      options = pathOptions(pane = group)
    ) %>% 
    showGroup(group)
  
  ##=##=##=##=##
  # Map Labels
  ##=##=##=##=##
  group <- paste(group, "label", sep="__")
  map_labels <- ifelse(f$n_friches == 0, "", as.character(f$n_friches))
  text_sizes <- paste0(round(get_sizes(f$n_friches, min_size=10, max_size=20)), "px")
  layerIds <- paste(f$layerId, "label", sep="__")
  for(i in 1:nrow(f)) {
    proxy %>% 
      addMapPane(group, zIndex = 500) %>% 
      addLabelOnlyMarkers(lng = f$Long[i], 
                          lat = f$Lat[i], 
                          layerId = layerIds[i],
                          label= map_labels[i],
                          labelOptions = labelOptions(noHide = T, 
                                                      direction = "center", 
                                                      textOnly = T, 
                                                      className="map_label",
                                                      textsize=text_sizes[i]),
                          group = group,
                          options = pathOptions(pane = group)
      )
  }
}

add_circles_AVANTSTANDARD <- function(proxy, f, group = "group") {
  
  f <- Data$stats$regs
  
  scale_linear <- function(value) {
    scaled <- (value - min(value)) / diff(range(value))
    return(scaled)
  }
  
  ##=##=##=##=##=##=##=##=##
  #  Filtre des données  ##
  ##=##=##=##=##=##=##=##=##
  # On calcule le nb de friches total
  # ce dernier sera affiché dans le cercle et conditionnera sa taille
  f$n_friches <- f$n_friches_observatoires + 
    f$n_friches_mte_qualifiees + 
    f$n_friches_user + 
    f$n_friches_aap  + 
    f$n_friches_ademe +
    f$n_friches_mte_pv
  if(group %in% c("stat_comm", "stat_iris")) f <- f %>% filter(n_friches > 0)
  
  
  ##=##=##=##=##=##=##=##=##
  #  Tailles de cercles   ##
  ##=##=##=##=##=##=##=##=##
  values <- f$n_friches
  if(group %in% c("stat_comm", "stat_iris")) {
    values <- values^2
  }
  
  get_sizes <- function(values, min_size=5, max_size=30){
    if(length(unique(values)) == 1) {
      circle_sizes = max_size + min_size
    } else {
      circle_sizes <- (scale_linear(values) * max_size) + min_size
    }
    circle_sizes
  }
  circle_sizes <- get_sizes(values, min_size=8, max_size=30)
  
  ##=##=##=##=##=##=##=##=##
  #  Couleurs des cerles  ##
  ##=##=##=##=##=##=##=##=##
  get_fillColor <- function(n_friches) {
    if(n_friches == 0) {
      "#808285"
    } else { 
      "#F58220" # Nicolas
    }
  }
  fillColors <- sapply(f$n_friches, get_fillColor) %>% as.character
  
  get_strokeColor <- function(n_friches) {
    if(n_friches == 0) {
      "#BCBDC0"
    } else {
      "#E54719" # Nicolas
    }
  }
  strokeColors <- sapply(f$n_friches, get_strokeColor) %>% as.character
  
  ##=##=##=##=##
  #  Popup   ##
  ##=##=##=##=##
  
  ##=##=##=##=##
  # Titre
  ##=##=##=##=##
  popup_titres <- lapply(1:nrow(f), function(i) {
    tags$p(tags$b(f$libelle[i]))})
  
  ##=##=##=##=##=#=#
  # Stats friches ##
  ##=##=##=##=##=#=#
  popup_stats <- lapply(1:nrow(f), function(x) {
    f <- f[x, ]
    
    stats <- list()
    
    stats$observatoire       <- f$n_friches_observatoires
    stats$aap                <- f$n_friches_aap
    stats$mte_pv             <- f$n_friches_mte_pv
    stats$user               <- f$n_friches_user
    
    stats$ademe              <- f$n_friches_ademe
    stats$mte_qualifiees     <- (f$n_friches_mte_qualifiees + stats$ademe) # Nb de friches qualifiées selon le MTE + Ademe
    
    stats$n_qualifiees       <- stats$mte_qualifiees + 
      stats$observatoire + 
      stats$aap + 
      stats$user + 
      stats$mte_pv
    div(
      get_ui_legende(stats, 
                     chk_all = FALSE, 
                     popup = TRUE)
    )
  })
  
  # Popups
  popups <- lapply(1:length(f$n_friches), function(i) {
    
    tags$div(class='map_popup',
             popup_titres[[i]],
             popup_stats[[i]]) %>% as.character
    
  })
  
  popups <- popups %>% as.list %>% lapply(HTML)
  
  ##=##=##=##=##
  #  Carte   ##
  ##=##=##=##=##
  circle_sizes <- as.numeric(circle_sizes)
  proxy %>%
    clearMarkers() %>%
    addMapPane(group, zIndex = 500) %>% 
    addCircleMarkers(
      lng = f$Long, lat = f$Lat,
      layerId = f$layerId,
      weight = 3,
      fillColor = fillColors,
      color = strokeColors,
      opacity = 1,
      fillOpacity = 0.9,
      radius = circle_sizes,
      label = popups,
      group = group
      ,
      options = pathOptions(pane = group)
    ) %>% 
    showGroup(group)
  
  ##=##=##=##=##
  # Map Labels
  ##=##=##=##=##
  group <- paste(group, "label", sep="__")
  map_labels <- ifelse(f$n_friches == 0, "", as.character(f$n_friches))
  text_sizes <- paste0(round(get_sizes(f$n_friches, min_size=10, max_size=20)), "px")
  layerIds <- paste(f$layerId, "label", sep="__")
  for(i in 1:nrow(f)) {
    proxy %>% 
      addMapPane(group, zIndex = 500) %>% 
      addLabelOnlyMarkers(lng = f$Long[i], 
                          lat = f$Lat[i], 
                          layerId = layerIds[i],
                          label= map_labels[i],
                          labelOptions = labelOptions(noHide = T, 
                                                      direction = "center", 
                                                      textOnly = T, 
                                                      className="map_label",
                                                      textsize=text_sizes[i]),
                          group = group,
                          options = pathOptions(pane = group)
      )
  }
}

# Zoome vers des coordonnées
zoom_to_coords <- function(m, coords, type) {
  message(">> Zoom vers ", paste(coords, collapse=", "))
  if(length(coords) == 2) {
    message(">> setView")
    zoom <- ifelse(type %in% c("street", "housenumber", "adresse"), 19, 12)
    m <- m %>% setView(lng = coords[1], lat = coords[2], zoom = zoom)
  } else {
    message(">> fitBounds")
    m <- m %>% fitBounds(coords[1], coords[2], coords[3], coords[4])
  }
}

# Zoome vers des coordonnées
zoom_to <- function(m, coords, type, value, label = NULL) {
  
  # Zoom
  m <- m %>% zoom_to_coords(coords, type)
  
  # Ajout du point ou du contour
  message(">> type ", type)
  if(!is.null(type)) {
    # Adresse
    if (type %in% c("street", "housenumber", "adresse")) {
      m <- m %>% addCircleMarkers(lng = coords[1],
                                  lat = coords[2],
                                  color = '#EF7757',
                                  label = label,
                                  fillColor = '#EF7757',
                                  radius = 12,
                                  layerId = 'address',
                                  group = 'address',
                                  opacity = 1,
                                  fillOpacity = 0.5,
                                  weight = 3)
      # Contour de commune-surface, de département ou de région
    } else if(type != "commune-point") {
      message(">> Ajout du contour ", value)
      res <- get_polygons_from_value(type, value)
      m   <- m %>% add_contour(res$pol, 
                               label = res$value, 
                               group = "Contours")        
    } 
  }
  
  m
}

# Ajoute les marqueurs à la carte (pins centroïdes des sites)
# Les marqueurs sont affichésà un niveau de zoom moyen
add_points <- function(proxy, f, replaceMarkers = TRUE) {
  
  message(">> add_points")
  
  # # ICONS
  # icons <- get_icone_friche(f)
  
  # POPUP
  # nom_site <- sapply(1:nrow(f), function(x) get_nom_site(f[x, ]))
  # nom_site <- toupper(f$site_nom)
  nom_site <- sapply(1:nrow(f), function(x) get_nom_site(f[x, ]))
  
  width <- 40
  
  # SITE_ID
  sitesNumeros <- f$site_id
  
  # BR
  br_code <- "<br style='content: \"\"; margin: -5px;display: block;'>"
  
  # ACTIVITES
  activites <- ifelse(is.na(f$activite_libelle), "Non renseignée", f$activite_libelle)
  activites <- sapply(activites, function(x) paste(strwrap(x, width), collapse=br_code))
  
  # SOURCES  
  sources <- sapply(f$nom_prodcartofriches, function(x) paste(strwrap(x, width), 
                                                           collapse=br_code))
  
  # LOGOS
  the_logos <- sapply(f$source_r, get_img_logo)
  
  # Souci sur Grand Angoulème donc édition manuelle du logo
  # à voir si cela est dû au ê, ou à l'espace
  w <- which(f$source_r == "Grand Angoulême")
  logo_img <- Logos[["Grand Angouleme"]]$img
  logo_height <- Logos[["Grand Angouleme"]]$height
  the_logos[w] <- glue("<br><img src={logo_img} height={logo_height}>")
  # logos/grand-angouleme.jpg"
  
  # On cache certains logos
  # en particulier ceux de l'Ademe et du MTE
  # Ademe et MTE sont de la même classe (MTE)
  # Pas de logo non plus pour les retours utilisateurs
  the_logos[which(f$source_r %in% c("MTE", "Ademe"))] <- ""
  the_logos[which(f$is_user)] <- ""
  
  ##=##=##=##=##=##=##
  # Popup > Labels
  ##=##=##=##=##=##=##
  
  nom_site <- sapply(nom_site, function(x) strwrap(x, width = 30) %>% paste(collapse="<br>"))
  labels <- glue("<div class='map_popup'>
                 <div><b>{nom_site}</b>
                 <br>
                 <span style='
                 margin-left:0px;
                 color:white;
                 background-color:#c5c5c5;
                 font-size:0.8em;
                 padding-top:3px;
                 padding-right:3px;
                 padding-left:3px;
                 padding-bottom:3px;
                 border-radius:3px;
                 '>{sitesNumeros}</span>
                 </div>
                 <small>Source : {sources}</small>
                 {the_logos}") %>% 
    as.list %>% 
    lapply(HTML)
  
  ##=##=##=##=##=##=##
  # Carte & Popups
  ##=##=##=##=##=##=#
  
  # Supprime les marqueurs
  if(replaceMarkers) {
    proxy %>% removeMarker(f$site_id)
  }
  
  icons <- get_icone_friche(f)
  
  message(">> Affichage de ", nrow(f), " friches")
  proxy %>%
    addAwesomeMarkers(
      lng = f$long,
      lat = f$lat,
      layerId = f$site_id,
      label = labels,
      group = "Basias et Basol",
      icon = icons
    )
}

# Ajoute les surfaces à la carte (unités foncières)
# Les UFs sont affichées au niveau de zoom le plus fort
add_polygons <- function(proxy, f, group = "Locaux vacants") {
  
  message(">> add_polygons")
  
  labels <- glue("<div class='map_popup'>Unit&eacute fonci&egravere de <b>{round(f$site_surface) %>% format_number}m<sup>2</sup></b><br><i><small>Regroupement de parcelles contig&uuml;es<br>appartenant au m&ecirc;me propri&eacutetaire</small></i></div>") %>% 
    as.list(labels) %>% 
    lapply(HTML)  
  
  # Add Polygons
  proxy %>% addPolygons(data = f,
                        label = labels,
                        layerId = f$site_id,
                        color = "#e632ef",
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 1.0,
                        fillOpacity = 0.3,
                        fillColor = "#ad24ad",
                        highlightOptions = highlightOptions(color = "#ad24ad",
                                                            fillOpacity = 0,
                                                            weight = 2,
                                                            bringToFront = FALSE),
                        group = group
  )
}

add_polygons_AVANTSTANDARD <- function(proxy, f, group = "Locaux vacants") {
  
  labels <- glue("<div class='map_popup'>Unit&eacute fonci&egravere de <b>{round(f$site_surface) %>% format_number}m<sup>2</sup></b><br><i><small>Regroupement de parcelles contig&uuml;es<br>appartenant au m&ecirc;me propri&eacutetaire</small></i></div>") %>% 
    as.list(labels) %>% 
    lapply(HTML)  
  
  # Add Polygons
  proxy %>% addPolygons(data = f,
                        label = labels,
                        layerId=~layerId,
                        color = "#e632ef", 
                        weight = 1, 
                        smoothFactor = 0.5,
                        opacity = 1.0, 
                        fillOpacity = 0.3,
                        fillColor = "#ad24ad",
                        highlightOptions = highlightOptions(color = "#ad24ad", 
                                                            fillOpacity = 0,
                                                            weight = 2,
                                                            bringToFront = FALSE),
                        group = group)
}

# CARTE
# On récupère l'objet (emprise) correspondant à la recherche
# On récupère l'objet cliqué quand on a des cercles
get_polygons_from_value <- function(type, value) {
  
  # On prend le fichier de stats qui convient en fonction du type de cercle cliqué
  if(type == "reg") {
    pol <- regs %>% filter(NOM_REG == value)
  } else if(type == "dep") {
    pol <- deps %>% filter(NOM_DEP == value)
  } else if(type == "comm" | type == "municipality" | type == "commune-surface") {
    res <- getCommune(value)
    pol <- res$geometry
    value <- res$nom
  } else if(type == "refCad") {
    pol <- get_parcelle(value)$geometry
  }
  
  return(list(pol = pol, 
              type = type, 
              value = value))
}

# CARTE
# On récupère l'objet cliqué quand on a des cercles
get_polygons_from_id <- function(id) {
  
  the_type <- gsub("stat_(.*)_(.*)", "\\1", id)
  the_code <- gsub("stat_(.*)_(.*)", "\\2", id)
  
  # On prend le fichier de stats qui convient en fonction du type de cercle cliqué
  if(the_type == "reg") {
    f <- regs %>% filter(INSEE_REG == the_code)
  } else if(the_type == "dep") {
    f <- deps %>% filter(INSEE_DEP == the_code)
  }
  
  return(list(f = f, type = the_type, code = the_code))
}

# CARTE
# Retourne la couleur de l'icône sur la carte
# Selon si le site est d'un observatoire ou pas
get_icone_friche <- function(f) {
  
  # get_icone_friche(f)
  
  icon$markerColor <- get_color(f, map = TRUE)
  # icon$iconColor <- get_color(f, map = TRUE)
  
  # Icon
  icon$icon <- case_when(
    f$site_statut == "friche avec projet" ~ icone_friche$avec_projet,
    f$site_statut == "friche sans projet" ~ icone_friche$sans_projet,
    f$site_statut == "friche potentielle" ~ icone_friche$potentielles,
    f$site_statut == "friche reconvertie" ~ icone_friche$reconverties)
  
  return(icon)
}

# Retourne les objets compris dans une certaine bounding box
get_objects_bounds <- function(f, map_bb) {
  
  map_bb_xmin <- map_bb$west
  map_bb_ymin <- map_bb$south
  map_bb_xmax <- map_bb$east
  map_bb_ymax <- map_bb$north
  
  # map_bb_xmin <- -9.84375
  # map_bb_ymin <- 30.63791
  # map_bb_xmax <- 14.98535
  # map_bb_ymax <- 58.47072
  
  # Filter and count
  # f <- f %>% filter(long >= map_bb_xmin & long <= map_bb_xmax & lat >= map_bb_ymin & lat <= map_bb_ymax)  #AVANTSTANDARD
  f <- f %>% filter(Long >= map_bb_xmin & Long <= map_bb_xmax & Lat >= map_bb_ymin & Lat <= map_bb_ymax)
  
  return(f)
}

# FUNC
# Récupère un site friche depuis son identifiant
get_friche_from_id <- function(id) {
  message(">> get_friche_from_id ", id)
  # num_site <- gsub("^industrielle_[a-z]*_(.*)$", "\\1", id)
  num_site <- id
  f <- Data$points %>% filter(site_id == num_site)
  
  return(f)
}

# CARTE - Zoome vers un observatoire
zoom_observatoire <- function(proxy, observatoire) {
  
  message(">> Zoom sur l'observatoire")

  elt <- observatoire %>% remove_accents()
  
  # Zoom
  f_emprise <- emprises[[elt]]
  
  # print(f_emprise)
  
  bb <- f_emprise %>% 
    st_bbox() %>% 
    as.numeric
  
  proxy %>% flyToBounds(bb[1], bb[2], bb[3], bb[4])
  
  # On affiche un message de notification
  img_logo <- get_img_logo(elt) %>% HTML
  
  bloc <- tagList(
    Logos[[elt]]$message,
    tags$br(),
    img_logo)
  
  showNotification(
    bloc,
    type = "message")
  
  # Affichage du contour
  message(">> Ajout du contour")
  proxy %>% add_contour(f_emprise, 
                        label = observatoire, 
                        group = "Contours")
}

# CARTE - Zoom vers la Métropole ou des DOM TOMs
zoom_secteur <- function(proxy, secteur) {
  if(secteur == "Métropole") {
    proxy %>% flyToBounds(as.numeric(bb_ini[1]), as.numeric(bb_ini[2]), as.numeric(bb_ini[3]),as.numeric(bb_ini[4]))
  } else {
    # Pour la Guyane, on forme le zoom vers les friches
    if(secteur == "Guyane") {
      guyane <- regs %>% filter(NOM_REG == "Guyane")
      w <- sapply(st_intersects(Data$points, guyane), function(x) length(x) > 0) %>% which
      bb <- st_bbox(Data$points %>% slice(w)) %>% as.numeric
    } else {
      bb <- st_bbox(regs %>% filter(NOM_REG == secteur)) %>% as.numeric
    }
    proxy %>% flyToBounds(as.numeric(bb[1]), as.numeric(bb[2]), as.numeric(bb[3]),as.numeric(bb[4]))
  }
}

# Trouve la friche la plus proche
find_closest_friche <- function(coords, f) {
  
  # Make point
  pt <- st_point(coords) %>%
    st_sfc %>%
    st_set_crs(4326)
  
  distances <- st_distance(pt, f) %>% as.numeric # distances en mètres
  minDistance <- min(distances)
  message("Distance minimale : ", minDistance)
  # Si la distance est inférieure au km, alors on affiche la distance en mètres, sinon kilom
  distance_txt <- ifelse(ceiling(minDistance) < 1000, 
                         glue("{round(minDistance)} m"), 
                         glue("{round((minDistance/1000), 1)} km"))
  f_sel <- f[which.min(distances), ]
  coords <- f_sel %>% st_coordinates
  sie_numero <- f_sel$site_id
  
  return(list(site_id  = f_sel$site_id,
              coords       = f_sel %>% st_coordinates, 
              distance     = minDistance, 
              distance_txt = distance_txt))
}


# Ajoute le contour d'une région, d'un département ou de l'emprise de l'observatoire sur la carte
add_contour <- function(proxy, f, label, group) {
  
  message(">> Ajout du groupe ", group)
  
  proxy %>%
    # clearGroup(group) %>%
    addPolygons(data = f, 
                group = group, 
                layerId = paste(group, f$type, f$code, sep="_"),
                fillColor = "white", 
                opacity = 0.5, 
                color = "white",
                label = label,
                weight = 1)
  
  # Si un contour associé à un marqueur est survolé,
  # On cache les contours de la zone que l'on avait précédemment atteinte
  if(group == "ContoursMouseOver") {
    proxy %>% hideGroup("Contours")
  }
  
  # Si on a sélectionné une zone, type commune, département ou région
  else if(group == "Contours") {
    proxy %>%  
      addMapPane("contours", zIndex = 430) %>% 
      addLayersControl(baseGroups    = baseGroups,
                       overlayGroups = c(overlayGroups, "Contours"),
                       options       = layersControlOptions(collapsed = FALSE)) %>% 
      showGroup("Contours")
    # hideGroup("Parcelles IGN")
  }
}

# > FORMULAIRE ----

# Permet de router vers la bonne fonction :
# - soit show_info_friche : affiche la popup du site
# - soit goto : zoome vers une commune, un département, ou une région
show_info <- function(proxy = NULL, id) {
  
  message(">> show_info")
  
  if (grepl("[0-9]+_[0-9]+", id)) {
    message(">> On affiche la popup pour la friche ", id)
    # on affiche la popup des friches
    show_info_friche(id) 
  } else if(grepl("stat_reg", id)) {
    message(">> On zoome sur la région ", id)
    proxy %>% goto(id)
  } else if(grepl("stat_dep", id)) {
    message(">> On zoome sur le département ", id)
    proxy %>% goto(id)
  } else if(grepl("contour_reg", id)) {
    message(">> On zoome sur la région ", id)
    proxy %>% goto(id)
  } else if(grepl("contour_dep", id)) {
    message(">> On zoome sur le département ", id)
    proxy %>% goto(id)
  }
}


# FORMULAIRE D'UNE FRICHE
# Affiche la popup d'une friche
show_info_friche <- function(id) {
  
  message(">> show_info_friche_industrielle ", id)
  
  mymap_modalDialog <- leaflet(width = 50, height = 50, 
                               options = leafletOptions(minZoom = 0, maxZoom = 20)) %>%
    add_tiles() %>%
    addLayersControl(baseGroups = baseGroups,
                     overlayGroups = c("Parcelles IGN"),
                     options = layersControlOptions(collapsed = TRUE)) %>% 
    showGroup("Ortho IGN") %>% 
    hideGroup("Parcelles IGN")
  
  ##=##=##=##
  # Filtre ##
  ##=##=##=##
  
  sf_points <- get_friche_from_id(id)
  num_site <- sf_points$site_id
  if(nrow(sf_points) == 0) {
    sf_points <- Data$polygons %>% filter(site_id == num_site) %>% st_centroid
  }
  sf_polygons <- Data$polygons %>% filter(site_id == num_site)
  
  ##=##=##=##=##
  # Content  ##
  ##=##=##=##=##
  
  bloc_content <- get_popup_content(sf_points)
  
  ##=##=##=##
  # Popup ##
  ##=##=##=##
  
  # BLOC FICHE
  # Les fiches ne sont pas affichées pour les AAP
  if(!is.na(sf_points$site_numero_basias) & sf_points$source_r != "AAP") {
    site_numero_basias <- sf_points$site_numero_basias
    url <- glue("https://fiches-risques.brgm.fr/georisques/casias/{site_numero_basias}")
    bloc_fiche <- tags$a(href = url, 
                         tagList(icon("info-circle"), "fiche"), 
                         target="_blank", style="font-size: 1em; margin-left: 10px;")
  } else {
    bloc_fiche <- NULL
  }
  
  # BLOC SITE
  bloc_site <- ifelse(is.na(sf_points$site_nom), 
                      glue("Friche {toupper(sf_points$source_r)}"), 
                      toupper(sf_points$site_nom))
  
  tag_color <- get_color(sf_points)
  
  ##=##=##=##=##
  # The map  ##
  ##=##=##=##=##
  
  mymap_popup <- mymap_modalDialog # on prend la carte définie pour les popups
  
  # Set View (étendue de la carte)
  if(!is.null(sf_polygons)) {
    bb <- sf_polygons %>% 
      st_bbox %>% 
      as.numeric
    
    mymap_modalDialog <- mymap_modalDialog %>% 
      clearBounds %>% 
      fitBounds(bb[1], bb[2], bb[3], bb[4])
  } else {
    coords <- sf_points %>% st_coordinates
    mymap_modalDialog <- mymap_modalDialog %>% 
      setView(coords[1], 
              coords[2], 
              zoom = 18)
  }
  
  # 1. Ajout de Marqueur
  mymap_modalDialog <- mymap_modalDialog %>% 
    add_points(sf_points, 
               replaceMarkers = TRUE)
  
  # 2. Ajout de Surface
  if(!is.null(sf_polygons)) {
    # print(sf_polygons$site_id)
    # sf_polygons <- sf_points %>% st_buffer(1)
    mymap_modalDialog <- mymap_modalDialog %>% 
      add_polygons(sf_polygons,
                   group = "Unités foncières")
  }
  
  ##=##=##=##=##=##=##
  # Modal Dialog   ##
  ##=##=##=##=##=##=##
  
  # On définit le contenu de la popup : titre, carte, etc...
  bloc_close <- column(2, actionLink("lnk_close", "X", style="color: #cbcbcb;
               text-decoration: none;
               font-size: 1.3em;
               font-weight: 100;"), 
                       style="margin-top: -20px;
               text-align: right;")
  
  message(">> bloc_title")
  if(sf_points$source_r == "MTE") {
    
    bloc_title <- fluidRow(
      fluidRow(
        column(10, 
               bloc_site, 
               bloc_fiche),
        bloc_close))
    
    # OFF
    # if(sf_points$checked) {
    #   bloc_title <- fluidRow(
    #     fluidRow(
    #       column(10, 
    #              bloc_site, 
    #              bloc_fiche),
    #       bloc_close))
    # } else {
    #   # On n'affiche pas de logo pour les Sources MTE
    #   bloc_title <- fluidRow(
    #     fluidRow(
    #       column(8, 
    #              bloc_site, 
    #              bloc_fiche),
    #       column(2,
    #              tags$span("Non qualifiée", 
    #                        style = glue("color:{tag_color};"))),
    #       bloc_close))
    # }
    
  } else {
    
    # Logo de l'observatoire
    source     <- sf_points$source_r
    img_logo   <- Logos[[remove_accents(source)]]$img
    img_height <- Logos[[remove_accents(source)]]$height
    
    bloc_logo <- tags$img(src = img_logo, 
                          height = img_height, 
                          align = "left")
    
    # Titre complet avec le logo
    bloc_title <- fluidRow(
      fluidRow(
        column(10,
               bloc_site, #NICO
               #NICO toupper(sf_points$site_nom), 
               bloc_fiche),
        bloc_close),
      fluidRow(column(12, 
                      bloc_logo,
                      style = "margin-top: 10px;")))
  }
  
  ##=##=##=##=##=##=##=##=##=##
  # Affiche la boîte modale ##
  ##=##=##=##=##=##=##=##=##=##
  message(">> showModal")
  showModal(
    modalDialog(
      
      # Titre
      title = bloc_title,
      
      # Contenu
      tagList(
        
        # Carte Leaflet
        renderLeaflet(mymap_modalDialog),
        
        # Voir sur Street View
        div(icon("street-view"), 
            HTML(get_street_view(sf_points$long, sf_points$lat)), "|",
            HTML(get_mapillary(sf_points$long, sf_points$lat)), 
            style="font-size: 0.9em;
                 text-align: right;
                 padding-top: 5px;"),
        
        tags$hr(style="margin-top:10px;margin-bottom:10px;"),
        
        # Partage du site
        div(tagList(tags$a(href = get_url_site(sf_points$site_id), 
                           tagList(icon("share"), 
                                   get_url_site(sf_points$site_id)),
                           target="_blank"),
        ), 
        style="
                          text-align: center;
                          # background-color: #e6e7e8;
                          font-size: 1em;
                          padding-top: 2px;
                          padding-bottom: 2px;"),
        
        # Ce site n'est pas une friche ?
        div(tags$a(href = get_mailto(sf_points$site_id), 
                   tagList("Ce site n'est pas une friche ?",
                           tags$br(), 
                           "Contactez-nous à l'adresse cartofriches@cerema.fr ! ", 
                           icon("comments")), target="_blank"), 
            style="
                background-color: #E6E7E8;
                padding: 10px;
                padding-left: 20px;
                border-radius: 10px;
                padding-right: 20px;
                color: white;
                margin-top: 10px;
                text-align: center;
                /* border: 1px solid #F58220; */
                margin-left: 20%;
                margin-right: 20%;
                margin-bottom: 20px;
                font-size: 1em;
              "),
        
        # Informations associées au site
        bloc_content
      ),
      
      size = "l",
      easyClose = TRUE,
      fade = TRUE,
      class="popup_body",
      footer = modalButton("Fermer la fenêtre")
    ))
}

# Retourne les documents de dépollution relatifs à la valeur f$comment_depollution
# Utilisé dans get_popup_content
get_docs_depollution <- function(value){
  if(is.na(value) | value=="[]") return("Non renseignées")
  value %>% 
    fromJSON %>% 
    lapply(function(x) tagList(tags$a(href=x, 
                                      tagList(icon("file"), x), 
                                      target="_blank"), " ")) %>% tagList
}

# Retourne le contenu de la popup en HTML relative à un site
# Le code HTML concerne les caractéristiques du site
# Soit une mise en forme des attributs du site en HTML
# Fait appel à d'autres fonctions présentes dans helpers.R comme get_docs_depollution, etc...
get_popup_content <- function(f) {
  
  # > get_popup_content(f.xy[1, ])
  
  message(">> get_popup_content")
  
  coords <- f %>% st_coordinates
  
  # Bloc du nom du projet
  print("1")
  if(!is.na(f$site_nom)) {
    bloc_nom_site <- tagList(
      tagList(tags$b("Nom du site : "), f$site_nom, tags$br())
    )
  } else {
    bloc_nom_site <- ""
  }
  
  # PROJET
  print("2")
  if(!is.na(f$site_statut) & !is.na(f$site_projet)) {
    bloc_projet <- tagList(
      tags$b("Site web du projet : "), coalesce(f$site_projet, "Non renseignée"), tags$br()
    )
  } else {
    bloc_projet <- ""
  }
  
  #bloc_occupation
  print("3")
  if(!is.na(f$site_occupation) & !f$site_occupation == "inconnu") {
    bloc_occupation <- tagList(
      tags$b("Occupation du site : "), coalesce(f$site_occupation, "Non renseignée"), tags$br()
    )
  } else {
    bloc_occupation <- ""
  }
  
  #bloc_typefriche
  print("4")
  if(f$site_statut == "friche avec projet") {
    bloc_typefriche <- tagList(#tags$b("Occupation du site : "), coalesce(f$site_occupation, "Non renseignée"), tags$br(),
                                              tags$b("Type de friche (avant projet de reconversion) : "), ifelse(is.na(f$site_type),"Non renseigné",
                                                                                  f$site_type %>% sapply(function(x) paste(x, collapse=","))))
  }  else if(f$site_statut == "friche reconvertie") {
    bloc_typefriche <- tagList(tags$b("Type de friche (avant reconversion) : "), ifelse(is.na(f$site_type),"Non renseigné",
                                                                                  f$site_type %>% sapply(function(x) paste(x, collapse=","))))
  } else {
    bloc_typefriche <- tagList(#tags$b("Occupation du site : "), coalesce(f$site_occupation, "Non renseignée"), tags$br(),
                                              tags$b("Type de friche : "), ifelse(is.na(f$site_type),"Non renseigné",
                                                                                  f$site_type %>% sapply(function(x) paste(x, collapse=","))))
  }
   
  # bloc_reconversion
  print("5")
  if(f$site_statut == "site avec projet" & !is.na(f$site_reconv_type)) {
    bloc_reconversion <- tagList(
      tags$b("Projet en cours : "), coalesce(f$site_reconv_type, "Non renseignée"), tags$br()
    )
  }  else if(f$site_statut == "friche reconvertie" & !is.na(f$site_reconv_type)) {
    bloc_reconversion <- tagList(
      tags$b("Reconversion de la friche : "), coalesce(f$site_reconv_type, "Non renseignée"), tags$br()
    )
  } else {
    bloc_reconversion <- ""
  }

  # Actualisation de la date
  print("6")
  if(is.na(f$site_actu_date)) {
    bloc_actualisationdate <- ""
  } else {
    bloc_actualisationdate <- tagList(
      # tags$b("Date d'actualisation de la friche : "), coalesce(format(as.Date(f$site_actu_date, "%d%m%y"), format="%d/%m/%Y"), "Pas d'actualisation"), tags$br(),
      tags$b("Date d'actualisation de la friche : "), coalesce(format(f$site_actu_date, "%d/%m/%Y"), "Pas d'actualisation"), tags$br(),
      )
  }  

  # Libellé de la zone du PLU
  print("7")
  if (is.na(f$urba_doc_type)) {
      bloc_zonePLU <- ""
    } else if(f$urba_doc_type == "CC") {
    bloc_zonePLU <- tagList(tags$b("Type de document d'urbanisme : "), "Carte communale", tags$br())
  }  else {
    bloc_zonePLU <- tagList(

      tags$b("Type de document d'urbanisme : "), f$urba_doc_type, tags$br(),
      tags$b("Libellé de la zone : "), f$urba_zone_lib, tags$br()
    ) }
  
  

  # }

  # POLLUTION
  print("8")
  if(all(is.na(c(f$sol_pollution_annee,
                 f$sol_pollution_commentaire,
                 f$sol_depollution_fiche))) & f$sol_pollution_existe == "inconnu" & f$sol_pollution_origine == "inconnu") {
    bloc_pollution <- tags$i("Informations sur la pollution des sols non renseignées")
  } else if (f$sol_pollution_existe == "pollution inexistante") {
    bloc_pollution <- tags$i("Pollution inexistante")
    } else {
    sol_pollution_annee <- f$sol_pollution_annee %>% get_texte
    sol_pollution_existe <- f$sol_pollution_existe %>% get_texte
    sol_pollution_origine <- f$site_en_securite %>% get_texte
    sol_pollution_commentaire <- f$sol_pollution_commentaire %>% get_texte
    sol_depollution_fiche <- f$sol_depollution_fiche %>% get_texte

    bloc_pollution <- tagList(tags$b("Année de constatation de la pollution : "), sol_pollution_annee %>% get_texte, tags$br(),
                                tags$b("Existence de pollution : "), sol_pollution_existe, tags$br(),
                                tags$b("Origine de la pollution : "), sol_pollution_origine, tags$br(),
                                tags$b("Commantaires : "), sol_pollution_commentaire, tags$br(),
                                tags$b("Fiches de dépollution : "), sol_depollution_fiche
    )

  }

  # ## Source des données
  print("9")
  if(!is.na(f$source_url)) {
    bloc_source_data <- tagList(
      h4("Source des données"),
      tagList(icon("database"),
              tags$a(href=f$source_url, target="_blank", "Source des données")),tags$br(),
      tagList(tags$b("Adresse mail de contact : "), coalesce(f$source_contact, "Non renseignée"))
      )
  } else {
    bloc_source_data <- ""
  }
  
  # print(f)
  
  # ## Surface
  # if(!is.na(f$bati_surface) | f$bati_surface == "NA") {
  #   bloc_surface <- tagList(
  #     tags$b("Surface de plancher totale des bâtiments : "), format_number(f$bati_surface), tags$br(),
  #   )
  # } else {
  #   bloc_surface <- "Non renseigné"
  # }
  
  
  #--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
  
  # f <- Data$points %>% filter(pk == 12787)
  
  # CONTENT
  content <- tagList(
    
    ##=##=##=##=##=##=##
    # Infos générales =##
    ##=##=##=##=##=##=##
    h4("Informations générales")
    ,
    tags$b("Identifiant du site : "), f$site_id, tags$br(),
    # tags$b("Nom du site : "), f$site_nom, tags$br(),
    bloc_nom_site,
    
    tags$b("Statut du site (projet en cours) : "), coalesce(f$site_statut, "Non renseignée"), tags$br(),
    bloc_projet,
    bloc_reconversion,
    
    bloc_occupation,
    bloc_typefriche, tags$br(),
    
    tags$b("Surface de la friche : "), coalesce(format_surface(f$site_surface), "Non calculée"), tags$br(),
    tags$b("Surface (de l'unité(s) de propriété) : "), coalesce(format_surface(f$unite_fonciere_surface), "Non calculée"), tags$br(),
    tags$b("Date d'identification de la friche : "), coalesce(format(f$site_identif_date, "%d/%m/%Y"), "Non renseignée"), tags$br(),
    
    bloc_actualisationdate,



    # ##=##=##=##=##=##
    # # Localisation =##
    # ##=##=##=##=##=##
    h4("Localisation"),
    tags$b("Commune : "), coalesce(f$comm_nom, "Non renseignée"), tags$br(),
    tags$b("Adresse : "), coalesce(f$site_adresse, "Non renseignée"), tags$br(),


    # ##=##=##=##=##=##=##=##=##
    # # Existence d'un projet =##
    # ##=##=##=##=##=##=##=##=##
    # h4("Existence d'un projet / friche réhabilitée"),
    # tags$b("Statut du site (projet en cours) : "), coalesce(f$site_statut, "Non renseignée"), tags$br(),
    # bloc_projet,
    # bloc_reconversion,

    ##=##=##=##=##=##=##=##
    # Ancienne activités =##
    ##=##=##=##=##=##=##=##
    h4("Ancienne activité"),
    tags$b("Ancienne(s) activité(s) : "), coalesce(f$activite_libelle, "Non renseignée"), tags$br(),
    tags$b("Codes NAF de(s) ancienne(s) activité(s) : "), coalesce(f$activite_code, "Non renseignée"), tags$br(),
    tags$b("Année de fin d'activité : "), coalesce(format(as.Date(f$activite_fin_annee, "%y"), format="%Y"), "Non renseignée"), tags$br(),

    ##=##=##=##
    # Bâti =##
    ##=##=##=##
    h4("Bâti"),
    tags$b("Nombre de bâtiments : "), coalesce(f$bati_nombre %>% as.character, "Non renseigné"), tags$br(),
    tags$b("Type de bâtiments : "), coalesce(f$bati_type, "Non renseignée"), tags$br(),
    # bloc_surface,
    tags$b("Surface de plancher totale des bâtiments : "), ifelse(is.na(f$bati_surface),
                                                                  "Non renseignée",
                                                                  coalesce(f$bati_surface,
                                                                           "Non renseignée")), tags$br(),
    tags$b("Vacance de bâtis : "), coalesce(f$bati_vacance %>% as.character, "Non renseigné"), tags$br(),
    tags$b("Présence de bâtiment de valeur patrimoniale : "), coalesce(f$bati_patrimoine %>% as.character, "Non renseigné"), tags$br(),
    tags$b("Etat de dégration des bâtiments : "), coalesce(f$bati_etat %>% as.character, "Non renseigné"), tags$br(),
    tags$b("Année de construction du local le plus ancien : "), coalesce(format(f$local_ancienne_annee, "%m/Y"), "Non renseignée"), tags$br(),
    tags$b("Année de construction du local le plus récent : "), coalesce(format(f$local_recent_annee, "%m/Y"), "Non renseignée"), tags$br(),
 
    ##=##=##=##=##
    # Propriété =##
    ##=##=##=##=##
    h4("Propriété"),
    
    tags$b("Personne physique ou morale : "), ifelse(is.na(f$proprio_personne),
                                                     "Non renseigné",
                                                     coalesce(str_to_title(f$proprio_personne),
                                                              "Nom du propriétaire non renseigné")),
    tags$br(),
    
    tags$b("Type de propriétaire : "), ifelse(is.na(f$l_catpro3txt),
                                             "Non renseigné",
                                             coalesce(f$l_catpro3txt %>% get_texte,
                                                      "Nom du propriétaire non renseigné")),
    tags$br(),
    
    tags$b("Nom du propriétaire : "), ifelse(is.na(f$proprio_nom),
                                             "Non renseigné",
                                             coalesce(f$proprio_nom %>% get_texte,
                                                      "Nom du propriétaire non renseigné")),
    tags$br(),
    
    tags$b("Date de la dernière mutation : "), ifelse(is.na(f$date_mutation),
                                             "Non renseigné",
                                             coalesce(f$date_mutation,
                                                      "Non renseigné")),
    tags$br(),

    tagList(tags$b("Références des unités foncières :"),
            ifelse(is.null(f$l_idtup),
                   "Non renseignées",
                   get_texte(f$l_idtup)),
            tags$br()),
    tagList(tags$b("Références cadastrales :"),
            ifelse(is.null(f$l_idpar),
                   "Non renseignées",
                   get_texte(f$l_idpar)),
            tags$br()),

    # ##=##=##=##=##
    # # Urbanisme =##
    # ##=##=##=##=##
    h4("Urbanisme"),
    bloc_zonePLU,

    tagList(

      # tags$b("Type de document d'urbanisme : "), ifelse(f$urba_doc_type == "CC",
      #                                                   "Carte communale",
      #                                                   coalesce(f$urba_doc_type, "Non renseignée")), tags$br(),
      #
      # tags$b("Libellé de la zone : "), coalesce(f$urba_zone_lib, "Non renseignée"), tags$br(),
      
      tags$b("Date d'approbation du document d'urbanisme : "), ifelse(is.na(f$urba_datappro),
                                                                  "Non renseigné",
                                                                  f$urba_datappro),
      tags$br(),

      tags$b("Forme dominante de la zone d'urbanisme : "), ifelse(is.na(f$urba_zone_formdomi_txt),
                                                 "Non renseigné",
                                                 paste0(f$urba_zone_formdomi," - ",f$urba_zone_formdomi_txt)),
      ),
      # tags$b("Document approuvé le : "), ifelse(is.na(f$datappro),
      #                                           "Date non renseignée",
      #                                           format(as.Date(f$datappro, "%Y%m%d"), format="%d/%m/%Y"))),

    ##=##=##=##=##
    # Pollution =##
    ##=##=##=##=##
    h4("Caractéristiques du sol"),
    bloc_pollution,

    
    ##=##=##=##=##
    # Desserte en transport =##
    ##=##=##=##=##
    h4("Desserte en transport"),
    tagList(
      
      tags$b("Distance d'accès au réseau ferroviaire : "), ifelse(is.na(f$desserte_distance_ferroviaire),
                                                                      "Non renseigné",
                                                                      glue(round(f$desserte_distance_ferroviaire/1000,1), " km")),
    ),

    # ##=##=##=##=##=##=##=##=##
    # # Sources des données =##
    # ##=##=##=##=##=##=##=##=##
    bloc_source_data
  )
  
  print("// content")
  
  return(content)
}
get_popup_content_AVANTSTANDARD <- function(f) {
  
  coords <- f %>% st_coordinates
  
  # URBANISME
  if(all(is.na(c(f$libzone, f$zone, f$destdomi, f$datappro)))) {
    bloc_urba <- "Non renseigné"
  } else {
    destdomi_lib <- switch(f$destdomi,
                           "00" = "sans objet ou non encore définie dans le règlement",
                           "01" = "habitat",
                           "02" = "activité",
                           "03" = "destination mixte habitat / activité",
                           "04" = "Loisirs et tourisme",
                           "05" = "Équipement public",
                           "07" = "activité agricole",
                           "01" = "espace naturel",
                           "09" = "espace remarquable (dispositions littoral / montagne)",
                           "10" = "secteur de carrière",
                           "99" = "autre")
    
    bloc_urba <- tagList(
      
      tags$b("Type de document d'urbanisme : "), f$docurba, tags$br(),
      
      tags$b("Zone : "), coalesce(glue("{f$libelle} - "), ""), 
      unique(f$libzone), 
      tags$br(), # f$zone enlevé
      
      tags$b("Destination dominante : "), ifelse(is.na(f$destdomi), 
                                                 "Non renseigné", 
                                                 paste0(destdomi_lib, "(", f$destdomi, ")")), 
      tags$br(),
      
      tags$b("Document approuvé le : "), ifelse(is.na(f$datappro), 
                                                "Date non renseignée", 
                                                format(as.Date(f$datappro, "%Y%m%d"), format="%d/%m/%Y")))
  }
  
  # POLLUTION
  if(all(is.na(c(f$annee_pollution, 
                 f$risque_pollution, 
                 f$pollution, 
                 f$site_en_securite, 
                 f$comment_depollution)))) {
    bloc_pollution_1 <- tags$i("Informations sur la pollution des sols non renseignées")
  } else {
    
    annee_pollution <- f$annee_pollution %>% get_texte
    origine_pollution <- f$origine_pollution %>% get_texte(json = TRUE)
    pollution <- f$pollution %>% get_texte(json = grepl("\"", f$pollution)) # si on trouve des guillemets, alors on a du JSON, donc on met json = TRUE
    
    if(class(f$risque_pollution) == "character") {
      risque_pollution <- f$risque_pollution %>% get_texte(json = FALSE)
    } else {
      risque_pollution <- f$risque_pollution %>% get_texte(json = TRUE)
    }
    
    site_en_securite <- f$site_en_securite %>% get_texte(json = TRUE)
    docs_depollution <- f$comment_depollution %>% get_docs_depollution
    
    bloc_pollution_1 <- tagList(tags$b("Année de constatation de la pollution : "), annee_pollution %>% get_texte, tags$br(),
                                tags$b("Origine de la pollution : "), origine_pollution, tags$br(),
                                tags$b("Pollution : "), pollution,tags$br(),
                                tags$b("Risques de pollution : "), risque_pollution, tags$br(),
                                tags$b("Site en sécurité : "), site_en_securite, tags$br(),
                                tags$b("Fiches de dépollution : "), docs_depollution
    )
    
  }
  bloc_pollution <- tagList(tags$b("Type de sol : "), coalesce(f$type_sol, "Non renseigné"), tags$br(), 
                            bloc_pollution_1)
  
  # SOURCES
  source <- f$source_r
  
  ## Source des données
  if(!is.na(f$url_source) & f$url_source != "") {
    bloc_source_data <- tagList(
      h4("Source des données"),
      tagList(icon("database"),
              tags$a(href=f$url_source, target="_blank", "Source des données")))
  } else {
    bloc_source_data <- ""
    
  }
  
  #--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
  
  # CONTENT
  content <- tagList(
    
    ##=##=##=##=##=##
    # Infos générales
    ##=##=##=##=##=##
    h4("Informations générales"),
    tags$b("Code du site : "), f$site_id, tags$br(),
    tags$b("Surface (de l'unité de propriété) : "), coalesce(format_number(f$site_surface), "Non calculée"), " m²", tags$br(),
    tags$b("Activité : "), ifelse(is.na(f$activite_libelle),
                                  "Non renseigné",
                                  f$activite_libelle %>% sapply(function(x) paste(x, collapse=","))), tags$br(),
    
    ##=##=##=##=##=##
    # Localisation
    ##=##=##=##=##=##
    h4("Localisation"),
    tags$b("Adresse : "), coalesce(f$loc_adresse, "Non renseignée"), tags$br(),
    tags$b("Commune : "), coalesce(f$nom_commune, "Non renseignée"), tags$br(),
    
    ##=##=##=##=##=##
    # Bâti
    ##=##=##=##=##=##
    h4("Bâti"),
    tags$b("Date de l'acte de mutation : "), coalesce(format(as.Date(f$jdatat, "%d%m%y"), format="%d/%m/%Y"), "Non renseignée"), tags$br(),
    tags$b("Nombre de bâtiments : "), coalesce(f$bati_nb %>% as.character, "Non renseigné"), tags$br(),
    tags$b("Année du local le plus ancien : "), coalesce(ifelse(f$jannatmin %in% c(-1, 0), "Non renseigné", f$jannatmin) %>% as.character, "Non renseignée"), tags$br(),
    tags$b("Année du local le plus récent : "), coalesce(ifelse(f$jannatmax %in% c(-1, 0), "Non renseigné", f$jannatmax) %>% as.character, "Non renseignée"), tags$br(),
    
    ##=##=##=##=##=##
    # Existence d'un projet
    ##=##=##=##=##=##
    h4("Existence d'un projet"),
    ifelse(is.na(f$projet), "Non renseigné", ifelse(f$projet, "Oui", "Non")),
    
    ##=##=##=##=##=##
    # Propriété
    ##=##=##=##=##=##
    h4("Propriété"),
    tags$b("Ancien propriétaire : "), ifelse(all(is.na(c(f$proprietaire_nom))),
                                             "Non renseigné",
                                             coalesce(f$proprietaire_nom, "Nom du propriétaire non renseigné")
    ), 
    tags$br(),
    tags$b("Propriétaire actuel : "), ifelse(is.na(f$ddenom),
                                             "Non renseigné",
                                             coalesce(f$ddenom, 
                                                      "Nom du propriétaire non renseigné")), 
    tags$br(),
    tagList(tags$b("Références cadastrales :"), 
            ifelse(is.null(f$ref_cad),
                   "Non renseignées",
                   get_texte(f$ref_cad)),
            tags$br()),
    
    ##=##=##=##=##=##
    # Urbanisme
    ##=##=##=##=##=##
    h4("Urbanisme"),
    bloc_urba,
    
    ##=##=##=##=##=##
    # Pollution des sols
    ##=##=##=##=##=##
    h4("Caractéristiques du sol"),
    bloc_pollution,
    
    # Source des données
    bloc_source_data
  )
  
  return(content)
}

# FORM
# Retourne le lien Mapillary associé aux coordonnées long lat du site
get_mapillary <- function(lng, lat) {
  link_mapillary <- glue("https://www.mapillary.com/app/?lat={lat}&lng={lng}&z=19.836305450776447&focus=photo")
  link_mapillary <- glue("<a href={link_mapillary} target=_blank> Voir depuis la rue sur Mapillary</a>")
  link_mapillary
}

# FORM
# Retourne le lien Street View associé aux coordonnées long lat du site
get_street_view <- function(lng, lat){
  link_street_view <- glue("https://www.google.com/maps?layer=c&cbll={lat},{lng}")
  link_street_view <- glue("<a href={link_street_view} target=_blank>Voir depuis la rue sur Street View")
  link_street_view
}

# FORM
# Retourne le texte associé à une valeur
# Retourne "Non renseigné" si pas de valeur
# Si JSON, renvoie la chaîne de caractères associée
# Utilisé dans get_popup_content
get_texte <- function(value, json = FALSE) {
  if(is.null(value)) {
    "Non renseigné"
  } else if (is.na(value)) {
    "Non renseigné"
  } else {
    if(json) {
      paste(fromJSON(value), collapse=", ")
    } else if(grepl("\\{.*\\}", value)) {
      value <- gsub("\\{(.*)\\}", "\\1", value)
      paste(strsplit(value, ",")[[1]], collapse = ", ")
    } else {
      value
    }
  }
}


# > HELPERS ----

# Reformate et simplifie la chaîne de caractères
transform_string <- function(s) {
  iconv(gsub("-", " ", tolower(s)), from="UTF-8", to="ASCII//TRANSLIT")
}

# Matomo (suivi consultation du site)
get_matomo <- function() {
  # if (file.exists(matomo)) {
  #   s <- readLines(matomo) %>% paste(collapse = "\n")
  #   res <- HTML(s)
  #   return(res)
  # } else {
  #   return()
  # }
  HTML(
    "<!-- Matomo -->
        <script type='text/javascript'>
          var _paq = window._paq = window._paq || [];
          /* tracker methods like 'setCustomDimension' should be called before 'trackPageView' */
          _paq.push(['trackPageView']);
          _paq.push(['enableLinkTracking']);
          (function() {
          var u='//stats.cerema.fr/';
          _paq.push(['setTrackerUrl', u+'matomo.php']);
          _paq.push(['setSiteId', '28']);
          var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
          g.async=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
          })();
        </script>
        <!-- End Matomo Code -->"
  )
}

# Transforme la première lettre en majuscule
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Enlève les accents
remove_accents <- function(s) {
  s <- iconv(s, to="ASCII//TRANSLIT")
  s <- gsub("ê", "e", s)
  return(s)
}

# Formate les nombres avec séparateurs de milliers
format_number <- function(number) {
  format(number, big.mark=" ")
}

# Récupère le logo associé à un observatoire
get_img_logo <- function(source_r) {
  source_r2 <- remove_accents(source_r)
  logo <- Logos[[source_r2]]
  logo_img   <- logo$img
  logo_height <- logo$height
  the_logo  <- glue("<br><img src={logo_img} height={logo_height}>")
  return(the_logo)
}

# Récupère le nom d'un site friche
# get_nom_site <- function(f) {
#   toupper(f$site_nom)
# }
get_nom_site <- function(f) {
  if(is.na(f$site_nom)) {
    if (f$nature == "Ademe") {
      "Friche avec potentiel solaire au sol"
    } else {
      glue("Friche {toupper(f$source_r)}")
    }
  } else {
    toupper(f$site_nom)
  }
}

get_nom_site_AVANTSTANDARD <- function(f) {
  if(is.na(f$site_nom)) {
    if(f$source_r == "Ademe") {
      "Friche identifiée étude Ademe"
    } else if (f$source_r == "MTE PV") {
      "Friche avec potentiel solaire au sol"
    } else {
      glue("Friche {toupper(f$source_r)}")
    }
  } else {
    toupper(f$site_nom)
  }
}

# > LEGENDE ----

# Filtre les friches selon la source choisie (observatoires locaux,...)
OLD_filtrer_friches <- function(f.xy, choices) {
  choices_s <- glue("is_{choices}") %>% paste(collapse=" | ")
  f.xy %>% filter(!! rlang::parse_expr(choices_s))
}

# Filtre les friches selon la source choisie (observatoires locaux,...)
filtrer_friches <- function(f.xy, choices) {
  message(">> filtrer_friches")
  f.xy %>% filter(site_statut %in% choices)
}


# Retourne le nombre de friches
get_n_friches <- function(f) {
  res <- list()
  
  res$avec_projet       <- f %>% 
    filter(site_statut == "friche avec projet") %>% nrow
  
  res$sans_projet                <- f %>% 
    filter(site_statut == "friche sans projet") %>% nrow
  
  res$reconverties             <- f %>% 
    filter(site_statut == "friche reconvertie") %>% nrow
  
  res$potentielles               <- f %>% 
    filter(site_statut == "friche potentielle") %>% nrow
  
  # TOTAL
  res$totales <- sum(unlist(res))
  
  return(res)
}

get_n_friches_AVANTSTANDARD <- function(f) {
  res <- list()
  
  res$observatoire       <- f %>% filter(is_observatoire) %>% nrow
  res$aap                <- f %>% filter(is_aap) %>% nrow
  res$mte_pv             <- f %>% filter(is_mte_pv) %>% nrow
  res$user               <- f %>% filter(is_user) %>% nrow
  
  res$ademe              <- f %>% filter(is_ademe) %>% nrow
  
  res$mte_qualifiees     <- nrow(f %>% filter((source_r == "MTE" & checked))) + res$ademe # Friches MTE et ADEME
  res$mte_non_qualifiees <- nrow(f %>% filter(source_r == "MTE" & !checked))
  
  # TOTAUX
  res$n_qualifiees <- res$mte_qualifiees + 
    res$observatoire + 
    res$aap + 
    res$user + 
    res$mte_pv
  
  message("Bon comptage des friches qualifiees : ", 
          res$n_qualifiees == nrow(f %>% filter(checked))) # Doit être à TRUE
  
  return(res)
}

# Trouve l'icône de légende adapté selon le type
get_icon_legende <- function(type_friche, 
                             type = "legende") {
  
  # get_icon_legende("avec projet")
  # get_icon_legende("sans projet")
  # get_icon_legende("potentielle")
  # get_icon_legende("reconvertie")
  
  # Valeurs par défaut
  
  if(type == "legende") {
    font_size <- "1em"
    
    margin_right    <- "10px"
    padding_right   <- "10px"
    padding_left    <- "10px"
    padding_top     <- "10px"
    padding_bottom  <- "10px"
    
  } else if(type == "popup") {
    font_size <- "1em"
    
    
    margin_right    <- "6px"
    padding_right   <- "6px"
    padding_left    <- "6px"
    padding_top     <- "6px"
    padding_bottom  <- "6px"
    
  } else if (type == "bouton") {
    
    font_size       <- ".8em"
    margin_right    <- "0px"
    padding_right   <- "1px"
    padding_left    <- "4px"
    padding_top     <- "10px"
    padding_bottom  <- "2px"
    
  } else if (type == "accueil") {
    
    font_size       <- ".8em"
    margin_right    <- "2px"
    padding_right   <- "6px"
    padding_left    <- "6px"
    padding_top     <- "6px"
    padding_bottom  <- "6px"
  }
  
  get_icon_element <- function(fa, 
                               color, 
                               margin_right, 
                               padding_right, padding_left, padding_top, padding_bottom) {
    tags$span(
      HTML(glue('<i class=\"fa {fa}\"></i>')), 
      style = glue("background-color: {color};
                    font-size: {font_size};
                    padding-right: {padding_right};
                    padding-left: {padding_left};
                    padding-top: {padding_top};
                    padding-bottom: {padding_bottom};
                    border-radius: 5px;
                    margin-right: {margin_right};
                   ")
    )
  }
  
  if(type_friche == "potentielles") {
    
    get_icon_element(icone_friche$potentielles, 
                     couleur_friche$potentielles, 
                     margin_right,
                     padding_right, padding_left, padding_top, padding_bottom)
    
  } else if (type_friche == "sans projet") {
    
    get_icon_element(icone_friche$sans_projet, 
                     couleur_friche$sans_projet, 
                     margin_right,
                     padding_right, padding_left, padding_top, padding_bottom)
    
  } else if (type_friche == "avec projet") {
    
    get_icon_element(icone_friche$avec_projet, 
                     couleur_friche$avec_projet, 
                     margin_right,
                     padding_right, padding_left, padding_top, padding_bottom)
    
  } else if (type_friche == "reconverties") {
    
    get_icon_element(icone_friche$reconverties, 
                     couleur_friche$reconverties, 
                     margin_right,
                     padding_right, padding_left, padding_top, padding_bottom)
  } 
}

# Retourne un élément de légende
get_elt_legende <- function(type, label, n = 10, popup) {
  
  # Styles
  margin_bottom <- ifelse(popup, "10px", "20px")
  font_size <- ifelse(popup, "1.1em",  "1.3em")
  
  # Bloc contenant le comptage d'éléments
  className <- ifelse(popup, "nb_friches_elt_popup", "nb_friches_elt")
  
  bloc_n <- NULL
  if (n > 0) {
    bloc_n <- tags$span(n, class = className)
  }
  
  # Bloc info
  if(!popup & type == "mte_pv") {
    bloc_info <- actionLink("lnk_mte_pv", label = "i", style="

    background-color: #337ab7;
    # background-color: #5ab1ce;
    border-radius: 50%;    
    color: white;
    font-weight: 900;
    font-size: 0.5em;
    padding-left: 5px;
    padding-right: 5px;
    padding-top: 3px;
    padding-bottom: 3px;
")
  } else {
    bloc_info <- NULL
  }
  
  type_interface <- ifelse(popup, "popup", "legende")
  
  fluidRow(
    column(10, offset = 1,
           get_icon_legende(type_friche = type, 
                            type          = type_interface),
           tags$span(label, style=glue("font-size:{font_size};")),
           bloc_info,
           bloc_n
           , style=glue("margin-bottom: {margin_bottom};")))
}

# Crée la légende de la carte + légende popups des cercles régions/départements
get_ui_legende <- function(stats, chk_all = FALSE, popup = FALSE) {
  
  # get_ui_legende(stats)
  
  # # Bloc sites non expertisés
  if(!chk_all) {
    bloc_potentielles <- ""
  } else {
    bloc_potentielles <- get_elt_legende("potentielles",
                                         "Friches potentielles",
                                         stats$potentielles,
                                         popup)
  }
  # 
  # bloc_sites_non_expertises <- ""
  
  if(chk_all) {
    nFriches <- stats$avec_projet + stats$sans_projet + stats$reconverties + stats$potentielles
  } else {
    nFriches <- stats$avec_projet + stats$sans_projet + stats$reconverties
  }
  
  # nFriches <- stats$avec_projet + stats$sans_projet + stats$reconverties + stats$potentielles
  
  # Bloc final
  res <- fluidRow(
    column(10, 
           offset = 1, 
           tags$p(nFriches %>% get_texte_nFriches,
                  class = "nb_friches"
           )),
    get_elt_legende("reconverties", "Friches reconverties", stats$reconverties, popup), # PV au sol
    get_elt_legende("avec projet", "Friches avec projet", stats$avec_projet, popup), # Sites industriels MTE et Ademe
    get_elt_legende("sans projet", "Friches sans projet", stats$sans_projet, popup), # Observatoires
    bloc_potentielles
    , style="padding-top:0px;padding-bottom:5px;color:black;font-size:0.9em;margin-bottom: -15px;")
  
  return(res)
}

# > PAGES ----

# Retourne le nombre de friches sur la page d'accueil
get_ui_nb_friches_accueil <- function() {
  
  n <- f.xy %>% filter(site_statut != "friche potentielle") %>% nrow
  
  res <- div(tags$p(n, " friches", style="
                              font-weight: 700;
                              font-size: 1.4em;
                    margin-bottom: -15px;"), 
             tags$p("Dernière mise à jour le ", LAST_UPDATE_DATE, 
                    style="color: #99999e;
                    font-size: 1em;"),
             tags$p(actionButton("btn_voir", 
                                 "VOIR LES FRICHES", 
                                 class="goto", 
                                 style="
                                           padding-top:20px;
                                           padding-bottom:20px;
                                           padding-left:30px;
                                           padding-right:30px;
                                           ")),
             style="margin-top: 18%;margin-left:50px;
")
  return(res)
}

# Retourne la page d'à propos de Cartofriches
get_ui_apropos_cartofriches <- function() {
  
  n_friches <- list()
  n_friches[["qualifiees"]]    <- f.xy %>% filter(is_mte) %>% nrow # Comprend les données Ademe puisqu'on n'a pas mis !is_ademe
  n_friches[["observatoires"]] <- f.xy %>% filter(is_observatoire) %>% nrow
  n_friches[["mte_pv"]]        <- f.xy %>% filter(is_mte_pv) %>% nrow
  n_friches[["user"]]          <- f.xy %>% filter(is_user) %>% nrow
  n_friches[["aap"]]           <- f.xy %>% filter(is_aap) %>% nrow
  
  # ui chiffres clés
  ui_chiffres_cles <- tagList(h2("Chiffres-clés"),
                              tags$p("À ce jour, l'application identifie ", 
                                     tags$span(sum(unlist(n_friches)) %>% format_number, 
                                               class = "nb_friches_elt_accueil"),
                                     # HTML("&nbsp"),
                                     " friches :"),
                              
                              tags$p(get_icon_legende("mte", type="accueil"),
                                     tags$span(n_friches$qualifiees %>% format_number, 
                                               class = "nb_friches_elt_accueil"), 
                                     "friches vérifiées ", 
                                     "c’est-à-dire issues d’une base nationale et ayant fait l’objet d’un filtre manuel pour être retenues"), 
                              
                              tags$p(get_icon_legende("observatoire", type="accueil"),
                                     tags$span(n_friches$observatoires %>% format_number, 
                                               class = "nb_friches_elt_accueil"),
                                     " friches transmises par des observatoires locaux"),
                              
                              tags$p(get_icon_legende("aap", type="accueil"),
                                     tags$span(n_friches$aap %>% format_number, 
                                               class = "nb_friches_elt_accueil"),
                                     " friches issues de l'Appel à Projet Fonds Friches (lauréats et candidats)"),
                              
                              tags$p(get_icon_legende("mte_pv", type="accueil"),
                                     tags$span(n_friches$mte_pv %>% format_number,
                                               class = "nb_friches_elt_accueil"),
                                     " friches retenues pour le photo-voltaïque au sol (étude Ademe 2021)"),
                              
                              tags$p(get_icon_legende("user", type="accueil"),
                                     tags$span(n_friches$user %>% format_number, 
                                               class = "nb_friches_elt_accueil"),
                                     " friches remontées par des utilisateurs")
  )
  
  # result
  div(
    h1("Cartofriches"),
    tags$p(icon("sync-alt"), 
           " Dernière mise à jour des données : ", 
           LAST_UPDATE_DATE, 
           style="color: #808285;font-size: 1em;"),
    includeMarkdown("www/textes/apropos_cartofriches-1.md"),
    
    # includeMarkdown("www/textes/apropos_cartofriches-2.md"),
    
    # includeMarkdown("www/textes/apropos_cartofriches-3.md"),
    
    ui_chiffres_cles, # on affiche les chiffres clés : nombre de friches qualifiées et d'observatoires
    
    includeMarkdown("www/textes/apropos_cartofriches-4.md")
  )
}

# Fenêtre d'avertissement
# Si, par exemple, pas de données trouvées
warning_dialog <- function(texte, type = "Commune") {
  
  showModal(modalDialog(title = NULL,
                        texte,
                        footer = NULL,
                        easyClose = T))
}

# Texte à afficher pour la partie zoom
get_txt_zoom <- function(zoom) {
  if(zoom > ZOOM_LEVELS["Département"]) {return()}
  
  zoom_delta <- (ZOOM_LEVELS["Département"] - zoom) + 1
  
  texte <- ifelse(zoom_delta == 1, 
                  "Ne zoomez plus qu'une fois pour afficher les friches",
                  glue("Zoomez encore {zoom_delta} fois pour afficher les friches")) %>% HTML
  
  div(
    icon("search-plus"),
    texte,
    style="color: #808285;font-style: normal;text-align:center")
}

# Récupère le footer de l'application
get_ui_footer <- function() {
  div(
    tags$img(src="logo2.png", width=200),
    HTML("&nbsp"),HTML("&nbsp"),HTML("&nbsp"),HTML("&nbsp"),
    
    "Copyright 2021 © - Cerema",
    HTML("&nbsp"),HTML("&nbsp"),HTML("&nbsp"),HTML("&nbsp"),
    
    actionLink("open_mentions", "Mentions légales"),
    HTML("&nbsp"),HTML("&nbsp"),HTML("&nbsp"),HTML("&nbsp"),
    
    actionLink("accessibilite", 
               "Accessibilité", icon("external-link"),
               onclick = glue("window.open('https://artificialisation.developpement-durable.gouv.fr/accessibilite', '_blank')")),
    HTML("&nbsp"),HTML("&nbsp"),HTML("&nbsp"),HTML("&nbsp"),
    
    actionLink("contact", 
               "Contact",icon("external-link"),
               onclick = glue("window.open('https://artificialisation.developpement-durable.gouv.fr/contact', '_blank')")),
    style="text-align: center;
                        padding-top: 20px;
                        /*box-shadow: 0px -5px 10px #d2d2d2;*/
                        height: 80px;")
  
}

# Petit trait décoratif utilisé dans getUiEncart
petitTrait <- function(width="10%") {
  div('', style=glue("
                            width: {width};
                            border-bottom: 3px solid #f5a64a;
                            margin-bottom: 30px;
                            "))
}

# Crée l'encart avec texte et image
get_ui_encart <- function(titre, titreSuite, body, image, isMobile) {
  
  if(isMobile) {
    fluidRow(
      column(12,
             div(
               # petit trait
               petitTrait("10%"),
               
               # Titre
               div(
                 tags$span(
                   tags$span(titre, style="color:#EF7757; font-weight:700;"),
                   titreSuite,
                   style="font-size:2em; color:#464749")),
               
               # Corps
               div(body, style="margin-bottom:40px;"),
               
               # Bouton
               div(actionButton("btn_chercher", 
                                "CHERCHER UNE FRICHE", 
                                class="goto", 
                                style="
                                           padding-top:20px;
                                           padding-bottom:20px;
                                           padding-left:30px;
                                           padding-right:30px;
                                           ")),
               style="
                      margin-bottom:8%;
                      margin-top:8%;                      
                      width:80%;
                          "), 
             offset = 1),
      style="background-color:white;"
    )
    
  } else {
    
    fluidRow(
      column(6, 
             div(
               petitTrait("10%"),
               div(
                 tags$span(
                   tags$span(titre, style="color:#EF7757; font-weight:700;"),
                   titreSuite,
                   style="font-size:2em; color:#464749")),
               div(body, style="margin-bottom:40px;"),
               div(actionButton("btn_chercher", 
                                "CHERCHER UNE FRICHE", 
                                class="goto", 
                                style="
                                           padding-top:20px;
                                           padding-bottom:20px;
                                           padding-left:30px;
                                           padding-right:30px;
                                           ")),
               style="
                      margin-bottom:8%;
                      margin-top:8%;
                      width:80%
                          "), 
             offset = 1),
      column(5,
             fluidRow(
               column(12,
                      div(get_ui_nb_friches_accueil()))),
             fluidRow(
               column(12,
                      div(
                        span(
                          tags$img(src=image,
                                   align="right",
                                   style="
                                      margin-right: -15px;
                                      margin-top:25%;")))))
      ), 
      
      style="background-color:white;"
    )
  }
}

# Crée l'URL du site friche
get_url_site <- function(site_id) {
  glue("https://cartofriches.cerema.fr/?site={site_id}")
}

# Formate le mailto pour retourner un courriel pré-construit
get_mailto <- function(site_id) {
  paste0("mailto:cartofriches@cerema.fr",
         "?subject=[Cartofriches] Informations sur le site n°", 
         site_id,
         "&body=Je vous contacte au sujet du site ", 
         get_url_site(site_id))
}

# Second bandeau
get_bandeau2 <- function(titre, sous_titre) {
  div(
    titre,
    tags$br(),
    span(sous_titre,
         style="font-family:'Marianne-BoldItalic';"),
    style="text-align:center;font-size:3em;color:white;")
}

# Bouton type
get_bouton <- function(input_id, input_lib, url = NULL) {
  actionButton(input_id, 
               input_lib, 
               icon  = icon("plus-circle"), 
               onclick = glue("window.open('{url}', '_blank')"),
               class = "home_button")
}

# Favicon
get_favicon <- function() {
  tags$link(rel="shortcut icon", href="https://www.cerema.fr/themes/custom/uas_base/favicon.ico")
}

# Lien CSS
get_css <- function() {
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
}

# Premier bandeau
get_bandeau1 <- function(titre, sous_titre, mobile, height = "400px") {
  
  if(mobile) {
    
    fluidRow(
      div(
        id="bandeau1",
        style="background-color:black;height:200px;padding:30px;background-image:url(fonds.jpg);background-size: 100%;",
        div(titre,
            style="font-size:2em;text-align:center;margin-bottom:10px;font-family:Marianne-Bold;color:white;"),
        div(tags$i(sous_titre),
            style="font-size:1.2em;text-align:center;margin-bottom:10px;color:white;"),
        modui_ban("search1", width="80%")
      )
    )
    
  } else {
    
    div(id="parent",
        div(id="sous_parent",
            # Enfant
            div(id="enfant",
                # titre
                div(
                  titre,
                  style="font-size:3em;margin-bottom:10px;font-family:Marianne-Bold;"),
                # Sous titre
                div(tags$i(sous_titre),
                    style="font-size:2em;margin-bottom:10px;"),
                # Barre de recherche
                modui_ban("search1", width="40%"),
                style = "
                  /*enfant*/
                  text-align: center;
                  color: white;
                  display: table-cell;
                  vertical-align: middle;"),
            style=glue("
                 /*sous_parent*/
                display: table;
                vertical-align: middle;
                text-align: center;
                width: 100%;
                height: {height};")),
        style=glue("
              /*parent*/
              height: {height};
              margin-left: -15px;
              margin-right: -15px;
              background-image: url(fonds.jpg);
              background-size: 100%;
              background-position: center center;"))
    
  }
}

# logo
get_logo <- function(appli) {
  div(actionLink("see_home", 
                 tags$img(src="logo.png", width=200, alt = "Cartofriches")),
      style="padding-top:15px;margin-top:-15px;margin-left:-40px;")
}

# UI - Retourne un vecteur à mettre dans un selectInput
get_slc <- function(v, label = "Veuillez sélectionner un élément dans la liste") {
  v2 <- c("", v)
  if(is.null(names(v))) {
    names(v2) <- c(label, v)
  } else {
    names(v2) <- c(label, names(v))
  }
  v2
}

# Retourne la couleur des sites en fonction de leur type
get_color <- function(f, map = FALSE) {
  
  # get_color(f.xy)
  
  if(map) {
    couleurs <- couleur_icone
  } else {
    couleurs <- couleur_friche
  }
  
  case_when(
    f$site_statut == "friche avec projet" ~ couleurs$avec_projet,
    f$site_statut == "friche sans projet" ~ couleurs$sans_projet,
    f$site_statut == "friche potentielle" ~ couleurs$potentielles,
    f$site_statut == "friche reconvertie" ~ couleurs$reconverties,
    TRUE              ~ couleurs$potentielles
  )
}


# > RECHERCHE ----

# Boîte modale associée à la recherche d'adresse
my_modal <- function(labels) {
  modalDialog(title = "Sélectionner une adresse", 
              div(
                awesomeRadio(
                  inputId = "adresses",
                  label = NULL, 
                  choices = labels,
                  selected = NULL,
                  status = "primary",
                  width = "100%"
                )
                , style="
              padding-left:20px;
              padding-left: 20px;
              padding-top: 20px;
              padding-bottom: 10px;"), 
              footer = actionButton("btn_gototheadresse", 
                                    tagList(icon("paper-plane"), "Aller vers cette adresse")), 
              easyClose = TRUE)
}

# Reconstruit les éléments depuis la valeur sélectionnée dans le inputRadio
# A savoir : retrouve le code INSEE, le nom, l'adresse, les coordonnées,...
interpret_label <- function(v) {
  
  s <- strsplit(v, "\\|")[[1]]
  
  type  <- s[1]
  value <- NULL
  label <- NULL
  
  if(type == "ban") {
    codeInsee <- s[2]
    name      <- s[3]
    coords    <- s[4]
    type      <- s[5]
    
    # Adresse
    if(type %in% c("street", 'housenumber')) {
      s2 <- strsplit(coords, ",")[[1]]
      x <- as.numeric(s2[1])
      y <- as.numeric(s2[2])
      coords <- c(x, y)
      type <- "adresse"
      # Commune
    } else { 
      codeInsee <- s[2]
      res <- getCommune(codeInsee)
      if(is.null(res)) {
        coords <- s[4]
        s2 <- strsplit(coords, ",")[[1]]
        x <- as.numeric(s2[1])
        y <- as.numeric(s2[2])
        coords <- c(x, y)
        type <- "commune-point"
      } else {
        bb <- res$geometry %>% st_bbox
        xmin <- as.numeric(bb$xmin)
        ymin <- as.numeric(bb$ymin)
        xmax <- as.numeric(bb$xmax)
        ymax <- as.numeric(bb$ymax)
        coords <- c(xmin, ymin, xmax, ymax)
        type <- "commune-surface"
      }
      value <- codeInsee
    }
    
    return(list(type   = type, 
                value  = value, 
                coords = coords, 
                label  = glue('{name} ({codeInsee})')))
    
    # Département ou Région
  } else {
    type   <- s[1]
    value  <- s[2]
    coords <- s[3]
    
    xmin <- gsub("(.*),(.*),(.*),(.*)", "\\1", coords) %>% as.numeric
    ymin <- gsub("(.*),(.*),(.*),(.*)", "\\2", coords) %>% as.numeric
    xmax <- gsub("(.*),(.*),(.*),(.*)", "\\3", coords) %>% as.numeric
    ymax <- gsub("(.*),(.*),(.*),(.*)", "\\4", coords) %>% as.numeric
    
    coords <- c(xmin, ymin, xmax, ymax)
    
    return(list(type   = type, 
                value  = value, 
                coords = coords,
                label  = label)) # !! intégrer le label à l'entité
  }
}

# Récupère les coordonnées long lat d'une adresse depuis le layerId
get_coords_adresse <- function(adresse) {
  
  adresse <- gsub("ban-", "", adresse)
  
  long <- gsub("(.*),(.*)-(.*)", "\\1", adresse) %>% as.numeric
  lat <- gsub("(.*),(.*)-(.*)", "\\2", adresse) %>% as.numeric
  
  type <- gsub("(.*),(.*)-(.*)", "\\3", adresse)
  
  message(">> on va vers l'adresse ", adresse, " de coordonnées ", long, ",", lat)
  zoom <- ifelse(type == "municipality", 14, 18)
  
  c(long, lat, zoom)
  
}

# Trouve le département et la région relatifs à une adresse retournée par l'API BAN
get_labels_depReg <- function(s) {
  
  message(">> get_labels_depReg : ", s)
  
  w.reg <- which(transform_string(regs$NOM_REG) == transform_string(s))
  w.dep <- which(transform_string(deps$NOM_DEP) == transform_string(s))
  
  domtom <- FALSE
  regs.sel <- regs[w.reg, ]
  deps.sel <- deps[w.dep, ]
  # Si l'objet est un DOM TOM, alors on retient seulement la région
  # w.dep est initialisé à NULL
  if(nrow(deps.sel) > 0) {
    if(all(grepl("97.+", deps.sel$INSEE_DEP))) {
      w.dep <- NULL
      domtom <- TRUE
    }    
  }
  
  # On va récupérer les résultats
  res <- list()
  
  res$dep <- list("nom" = ifelse(length(w.dep) > 0, as.character(deps.sel$NOM_DEP), NA),
                  "bbox" = st_bbox(deps.sel) %>% as.numeric,
                  "statut" = ifelse(length(w.dep) > 0, "OK", "KO"))
  
  
  res$reg <- list("nom" = ifelse(length(w.reg) > 0, as.character(regs.sel$NOM_REG), NA),
                  "bbox" = st_bbox(regs.sel) %>% as.numeric,
                  "statut" = ifelse(length(w.reg) > 0, "OK", "KO"))
  
  # On choisit les résultats qui renvoient des résultats
  w <- which(sapply(res, function(elt) elt$statut == "OK"))
  
  if(length(w) == 0) return()
  
  res <- res[[w]]
  
  # si le premier élément est OK, alors c'est un département, sinon une région
  # car res est une liste avec deux éléments : res$dep et res$reg
  # res$dep est le premier élément de la liste
  type    <- ifelse(w == 1, "dep", "reg")
  libelle <- res$nom
  bb      <- res$bbox
  bb_s    <- paste(bb, collapse=",")
  
  # On récupère le nombre de friches
  # On configure l'affichage des éléments dans la sélection de résultats
  if(type == "dep") { # on définit l'élément département
    n_friches <- find_friches(libelle, mode = "département")
    elt <- glue("dep|{libelle}|{bb_s}")
    names(elt) <- glue("{libelle} (Département) ({n_friches})")
  } else if(type == "reg") { # on définit l'élément région
    n_friches <- find_friches(libelle, mode="région")
    elt <- glue("reg|{libelle}|{bb_s}")
    reg <- ifelse(domtom, "Département ultra-marin", "Région")
    names(elt) <- glue("{libelle} ({reg}) ({n_friches})")
  }
  
  elt
}


# Retourne le texte associé au nombre de friches dans le module de recherche adresse, commune ET popup cercles régions/départements
get_texte_nFriches <- function(value) {
  if(value == 0) {
    "Aucune friche"
  } else if(value == 1) {
    "Une friche"
  } else {
    sprintf("%s friches", format_number(value))
  }
}

# On récupère les infos du cercle ou de l'emprise cliqués
get_dep_reg_stats_from_event_id <- function(id) {
  
  # On récupère le type et le code
  if(grepl("label", id)) {
    the_type <- gsub("stat_(.*)_(.*)__label", "\\1", id)
    the_code <- gsub("stat_(.*)_(.*)__label", "\\2", id)
  } else {
    the_type <- gsub("(?:stat|contour)_(.*)_(.*)", "\\1", id)
    the_code <- gsub("(?:stat|contour)_(.*)_(.*)", "\\2", id)
  }
  
  # On prend le fichier de stats qui convient en fonction du type de cercle cliqué
  if(the_type == "reg") {
    f <- Data$stats$regs
    the_type <- "Région"
  } else if(the_type == "dep") {
    f <- Data$stats$deps
    the_type <- "Département"
  } 
  
  # Eléments sélectionnés
  f <- f %>% filter(code == the_code)
  return(list(f = f, type = the_type))
}

# Permet d'aller vers une commune ou une région
goto <- function(proxy, id) {
  
  res <- get_dep_reg_stats_from_event_id(id)
  f <- res$f
  the_type <- res$type
  
  # Si on est au niveau région, alors le contrôle des couches est plié
  # Si département, alors on le déplie pour pouvoir cacher la couche Contours si besoin est
  collapsed <- (the_type == "Région")
  
  # Zoomer dans la carte
  long <- f$Long
  lat  <- f$Lat
  zoom <- ZOOM_LEVELS[which(names(ZOOM_LEVELS) == the_type)] + 1
  proxy %>% flyTo(as.numeric(long), 
                  as.numeric(lat), 
                  zoom = as.numeric(zoom)) %>% 
    
    # Ajout du groupe Contours dans le contrôle des couches
    addLayersControl(baseGroups    = baseGroups,
                     overlayGroups = c(overlayGroups, "Contours"),
                     options       = layersControlOptions(collapsed = collapsed)) %>% 
    showGroup("Ortho IGN") %>%
    hideGroup("Parcelles IGN")
}


# Récupère la commune avec le formatage qui va bien
get_labels_refCad <- function(adresse) {
  res <- get_parcelle(adresse)
  if(is.null(res)) return()
  n_friches <- find_friches(adresse, mode = 'refCad')
  coords <- res$bb
  value <- paste("refCad", adresse, paste(coords, collapse=","), sep = "|")
  names(value) <- glue("{adresse} (Référence cadastrale)")
  value
}

# Récupère la commune avec le formatage qui va bien
get_labels_commune <- function(adresse) {
  res <- getCommune(code_insee = adresse)
  if(is.null(res)) return()
  nom  <- res$nom
  geom <- res$geometry
  coords <- st_bbox(geom) %>% as.numeric
  value <- paste("comm", adresse, paste(coords, collapse=","), sep="|")
  n_friches <- find_friches(adresse, mode = 'commune')
  names(value) <- glue("{adresse} ({nom}) ({n_friches})")
  value
}


# Récupère les éléments de liste pour la BAN
get_labels_ban <- function(res) {
  # On définit le labels en fonction des coordonnées du site et son type
  labels <- glue("ban|{res$citycode}|{res$name}|{res$longitude},{res$latitude}|{res$type}")
  
  # Libellés des communes, ou des rues
  names(labels) <- sapply(1:nrow(res), function(i) {
    elt <- res[i, ]
    if(elt$type == "municipality") {
      message(">> Recherche des friches dans la commune ", elt$citycode)
      n_friches <- find_friches(elt$citycode, mode = 'commune')
      glue("{elt$label} ({elt$citycode}) (Commune) ({n_friches})")
    } else {
      message(">> Recherche des friches autour de la rue de coordonnées ", elt$longitude, "-",elt$latitude)
      coords <- c(elt$longitude, elt$latitude)
      n_friches <- find_friches(coords, mode = 'point', distance = SEARCH_DISTANCE)
      # ifelse(is.null(n_friches), elt$label, glue("{elt$label}"))  # Nicolas
      ifelse(is.null(n_friches), elt$label, glue("{elt$label} ({n_friches})"))
      
    }
  })
  
  return(labels)
}

# Trouve les friches dans un certain rayon ou dans une emprise (commune, département,...)
find_friches <- function(s, mode = "commune", distance = SEARCH_DISTANCE) {
  
  # Cas de région
  if(mode == "région") {
    f <- regs.pts %>% filter(libelle == s)
    n_friches <- f$n_friches_observatoires + 
      f$n_friches_mte_qualifiees + 
      f$n_friches_ademe + 
      f$n_friches_user + 
      f$n_friches_mte_pv + 
      f$n_friches_aap
  }
  
  # Cas de département
  if(mode == "département") {
    f <- deps.pts %>% filter(libelle == s) %>% slice(1) # !!! enlever après résolution du doublon Guyane
    n_friches <- f$n_friches_observatoires + 
      f$n_friches_mte_qualifiees + 
      f$n_friches_ademe +
      f$n_friches_user + 
      f$n_friches_mte_pv +
      f$n_friches_aap
  }
  
  # Cas de commune
  if(mode == "commune") {
    comm <- comms %>% filter(INSEE_COM == s)
    i <- st_intersects(comm, Data$points %>% filter(checked)) %>% unlist
    n_friches <- length(i)
    n_friches
  }
  
  # comm <- comms %>% filter(INSEE_COM == s)
  # value <- as.data.frame(comm$NOM_COM_M)
  # print(value)
  # i <- nrow(as.data.frame(Data$points) %>% filter(comm_nom == value))
  # n_friches <- length(i)
  
  
  
  # Cas de référence cadastrale
  if(mode == "refCad") {
    # nombre de friches sur la parcelle
    geom <- get_parcelle(s)$geometry
    i <- st_intersects(geom, Data$points %>% filter(checked)) %>% unlist
    n_friches_in <- length(i)
    
    # nombre de friches autour de la parcelle
    geom <- geom %>% st_buffer(distance)
    i <- st_intersects(geom, Data$points %>% filter(checked)) %>% unlist
    n_friches_out <- length(i)
    
    if(n_friches_in == 0) {
      if(n_friches_out == 0) {
        return("Pas de friche")
      } else if (n_friches_out == 1) {
        return("Une friche*")
      } else {
        return(glue("{n_friches_out} friches*"))
      }
    } else if (n_friches_in == 1) {
      return("Une friche")
    } else {
      return(glue("{n_friches_in} friches"))
    }
  }
  
  # Cas d'un point
  if(mode == "point") {
    buffer <- st_point(s) %>%
      st_sfc %>%
      st_set_crs(4326) %>% 
      st_buffer(0.022) # st_buffer(distance) # Nicolas
      
    

    i <- st_intersects(buffer, Data$points %>% filter(checked)) %>% unlist
    n_friches <- length(i)
    
    # On met le suffixe à proximité dans le cas des adresses
    if(n_friches == 0) {
      return()
    } else if (n_friches == 1) {
      return("Une friche*")
    } else {
      return(glue("{n_friches} friches*"))
    }
  }
  
  if(n_friches == 0) {
    return("Pas de friche")
  } else if (n_friches == 1) {
    return("Une friche")
  } else {
    return(glue("{n_friches} friches"))
  }
  
}

# Teste si la chaîne est un code INSEE
is_insee <- function(s) {
  grepl("^([013-9]\\d|2[AB1-9])\\d{3}$", s)
}

# Teste si la chaîne est une référence cadastrale
is_parcelle <- function(s) {
  grepl("^([013-9]\\d|2[AB1-9])\\d{3}(0|[A-Z])[A-Z][0-9]{4}[a-z]?$", s)
}

# Récupère le contour d'une commune
getCommune <- function(codeInsee) {
  my_fun <- function(codeInsee) {
    # geo.api.gouv.fr arguments
    # geometry = contour pour avoir le contour
    # fields = geometry car seule la géométrie nous intéresse
    url      <- glue("https://geo.api.gouv.fr/communes/{codeInsee}?format=geojson&geometry=contour&fields=geometry")
    coords   <- jsonlite::fromJSON(url)$geometry$coordinates[1,,]
    geometry <- st_polygon(list(coords)) %>% st_sfc %>% st_set_crs(4326)  
    nom      <- jsonlite::fromJSON(url)$properties$nom
    list(geometry = geometry, nom = nom)
  }
  
  tryCatch({
    result <- my_fun(codeInsee)
  }, error = function(e) {return()})
}

# Récupère la bbox d'une référence cadastrale
# get_parcelle("592200B0084")
get_parcelle <- function(ref_cad) {
  my_fun <- function(ref_cad) {
    code_insee <- substr(ref_cad, 1, 5)
    section    <- substr(ref_cad, 6, 7)
    numero     <- substr(ref_cad, 8, 11)
    
    url <- glue("https://apicarto.ign.fr/api/cadastre/parcelle?code_insee={code_insee}&section={section}&numero={numero}&source_ign=PCI")
    
    # Bbox
    res <- jsonlite::fromJSON(url)
    bb <- res$bbox
    
    # Géométrie
    coords <- res$features$geometry$coordinates[[1]]
    x <- coords[,,,1]
    y <- coords[,,,2]
    coords <- cbind(x, y)
    geometry <- st_polygon(list(coords)) %>% st_sfc %>% st_set_crs(4326)  
    
    list(geometry = geometry, 
         bb = bb, 
         code_insee = code_insee, 
         section = section, 
         numero = numero)
  }
  
  tryCatch({
    result <- my_fun(ref_cad)
  }, error = function(e) {return()})
}