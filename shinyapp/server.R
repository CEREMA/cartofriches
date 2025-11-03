server <- function(input, output, session) {
  
  # # Variable réactive pour suivre si on doit montrer la popup
  # popup_shown <- reactiveVal(FALSE)
  # 
  # # Observer pour afficher le modal au démarrage
  # observeEvent(input$cookie_hide_popup, {
  #   
  #   # Ne montrer qu'une seule fois
  #   if (!popup_shown()) {
  #     cookie_value <- input$cookie_hide_popup
  #     
  #     cat("Cookie reçu:", cookie_value, "\n")
  #     
  #     # Afficher la popup seulement si le cookie n'est pas "true"
  #     if (is.null(cookie_value) || cookie_value != "true") {
  #       cat("Affichage de la popup\n")
  #       showModal(modalDialog(
  #         title = "Bienvenue !",
  #         HTML("<p>Cartofriches fera l'objet d'une émission dans le mag radio territoria le 14 octobre à 11h.</p>"),
  #         HTML("<p>Pour en savoir plus : 
  #     <a href='https://radioterritoria.fr/podcasts/1-RadioTerritoria' target='_blank' style='color: #007bff; font-weight: bold;'>
  #     Emission Le Mag - 14 octobre 11h-12h</a></p>"),
  #         easyClose = TRUE,
  #         footer = tagList(
  #           checkboxInput("dont_show_again", "Ne plus afficher ce message", value = FALSE),
  #           actionButton("close_popup", "Fermer", class = "btn-primary")
  #         )
  #       ))
  #     } else {
  #       cat("Popup masquée (cookie = true)\n")
  #     }
  #     
  #     popup_shown(TRUE)
  #   }
  # }, ignoreNULL = FALSE)
  # 
  # # Gérer la fermeture de la popup
  # observeEvent(input$close_popup, {
  #   # Si la case est cochée, définir le cookie
  #   if (!is.null(input$dont_show_again) && input$dont_show_again) {
  #     cat("Définition du cookie\n")
  #     runjs("setCookie('hideWelcomePopup', 'true', 365); console.log('Cookie défini');")
  #   }
  #   removeModal()
  # })
  # 
  # 
  
  
  
  # Il s'agit du carroussel d'informations
  output$carousel <- renderSlickR({
    
    # f <- read_yaml("https://raw.githubusercontent.com/CEREMA/cartofriches/refs/heads/main/shinyapp/www/actus/actus.yaml")
    f <- read_yaml("www/actus/actus.yaml")
    
    actus <- render_actus(f)
    
    slickR(
      obj = actus
    ) +
      settings(
        autoplay = TRUE,        # Active le défilement automatique
        autoplaySpeed = 4000,   # Temps entre les slides
        slidesToShow = 3,       # Affiche une seule image à la fois
        slidesToScroll = 1,     # Une image à la fois lors du défilement
        dots = TRUE,            # Affiche les points de pagination
        arrows = TRUE
      )
  })
  
  
  load_data()   # on charge les données
  
  # Va directement sur l'onglet "Contribuer" lorsqu'il y a 'contribuer' dans l'url
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if("contribuer" %in% names(query) | "local" %in% names(query) | "observatoire" %in% names(query)){
      updateNavbarPage(session, "app_navbar", selected = "Monter son observatoire local")
    }
  })
  
  # Va directement sur l'onglet "Enrichissement de données" lorsqu'il y a 'enrichissement' dans l'url
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if("enrichissement" %in% names(query) | "qualification" %in% names(query)){
      updateNavbarPage(session, "app_navbar", selected = "Enrichissement de données")
    }
  })
  
  # Va directement sur l'onglet "Statistiques d'impact" lorsqu'il y a 'impact' dans l'url
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if("statistiques" %in% names(query) | "statistique" %in% names(query) | "impact" %in% names(query)){
      updateNavbarPage(session, "app_navbar", selected = "Statistiques d'impact")
    }
  })
  
  # Va directement sur l'onglet "Enrichissement de données" lorsqu'il y a 'enrichissement' dans l'url
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if("mutabilite" %in% names(query)){
      updateNavbarPage(session, "app_navbar", selected = "Indices de mutabilité")
    }
  })
  
  
  # > MODULES ----
  
  # Modules pour rechercher des adresses
  # retourne un reactive contenant les coordonnées XY
  # ou la BBOX xmin, ymin, xmax, ymax de là où aller
  # r_coords1 <- mod_ban("search1", session)
  r_coords2 <- mod_ban("search2", session)
  
  # > VARIABLES ----
  
  # rv_filtres ----
  rv_filtres <- reactiveValues(value = NA)
  
  # r_bbox() (bbox ou site issu de la query string) ----
  r_bbox <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    
    # Si la requête ne comporte ni bbox, ni site, alors on quitte
    if(!("bbox" %in% names(query)) & !("site" %in% names(query))) return()
    
    if("bbox" %in% names(query)) {
      # On extrait les éléments de l'URL
      bbox <- query$bbox %>% strsplit(split = ",") %>% {.[[1]]}
      
      xmin <- bbox[1] %>% as.numeric
      ymin <- bbox[2] %>% as.numeric
      xmax <- bbox[3] %>% as.numeric
      ymax <- bbox[4] %>% as.numeric
      
      bbox <- c(xmin, ymin, xmax, ymax)
      
      return(bbox)
      
    } else if("site" %in% names(query)) {
      # On récupère la BBOX du site
      f <- Data$polygons %>% filter(site_id == query$site)
      bbox <- st_bbox(f)
      
      xmin <- bbox$xmin  %>% as.numeric
      ymin <- bbox$ymin  %>% as.numeric
      xmax <- bbox$xmax  %>% as.numeric
      ymax <- bbox$ymax  %>% as.numeric
      
      bbox <- c(xmin, ymin, xmax, ymax)
      
      return(bbox)
    } 
    
  })
  
  # r_friches (il s'agit des friches d'un certain type pour le calcul des plus proches) ----
  r_friches <- reactive({
    
    req(input$mymap_zoom)
    
    if(input$mymap_zoom <= ZOOM_LEVELS["Département"]) return()
    
    sf_points <- Data$points
    
    # Toutes les friches (y compris celles potentielles)
    if(is.null(input$chk_all)) {
      sf_points <- sf_points %>% filter(site_statut != "friche potentielle")
    } else {
      # Si on ne prend pas les potentielles
      if(!input$chk_all) {
        sf_points <- sf_points %>% filter(site_statut != "friche potentielle")
      }
    }
    
    # Choix de friches
    choices <- rv_filtres$value
    if(!all(is.na(choices))) {
      sf_points <- sf_points %>% filtrer_friches(choices = choices)
    }
    
    return(sf_points)
  })
  
  # r_data() - Ajout des points, surfaces, cercles stats en fonction du niveau de zoom ----
  r_data <- reactive({
    
    req(input$mymap_bounds, 
        input$mymap_zoom)
    
    ## Bounding box
    map_bb <- input$mymap_bounds
    
    ## Load stats data
    message(">> Niveau de zoom : ", input$mymap_zoom)
    
    # REGION 
    if(input$mymap_zoom <= ZOOM_LEVELS["Région"]) {
      message(">> r_data : Cercles stats de région")
      return(list(regs = Data$stats$regs))
      
      # DEPARTEMENT
    } else if(input$mymap_zoom <= ZOOM_LEVELS["Département"]) {
      message(">> r_data : Cercles stats de département")
      return(list(deps = Data$stats$deps))
      
      # PINT
    } else if (input$mymap_zoom > ZOOM_LEVELS["Département"]) {
      message(">> r_data : Points")
      
      sf_points <- r_friches()
      
      # Filtre sur l'étendue de la carte
      sf_points <- sf_points %>% get_objects_bounds(map_bb)
      
      if(nrow(sf_points) == 0) {
        message("Pas de points")
        return(list(points = NULL))
      }
      if(input$mymap_zoom <= ZOOM_LEVELS["Marqueur"]){
        message(">> r_data : Points1")
        return(list(points = sf_points))
      } else {
        sf_polygons <- Data$polygons %>% filter(site_id %in% sf_points$site_id)
        if(nrow(sf_polygons) == 0) {
          message(">> r_data : Points2")
          return(list(points = sf_points))
        } else {
          message(">> r_data : Polygones")
          return(list(points = sf_points, polygons = sf_polygons))
        }
      }
    }
  })
  
  # r_closest_friche (Friche la plus proche) ----  PB NICOLAS FINDFRICHE
  r_closest_friche <- reactive({
    
    message(">> r_closest_friche()")
    
    req(input$mymap_bounds)
    req(!is.null(input$chk_all))
    req(r_friches())
    
    f <- r_friches()
    
    bb <- input$mymap_bounds
    f_in_bounds <- f %>% get_objects_bounds(bb)
    
    if(nrow(f_in_bounds) > 0) return()
    
    # Coordonnées du point central
    coords <- c(mean(bb$west, bb$east), mean(bb$south, bb$north))
    
    # On cherche les friches les plus proches
    res <- find_closest_friche(coords = coords,
                               f      = f)
    
    return(res)
  })
  
  # > OBSERVE ####
  
  # lnk_close ----
  observeEvent(input$lnk_close, {
    removeModal()
  })
  
  # rv_filtres ----
  observe({
    if(is.null(input$chk_filtres)) {
      rv_filtres$value <- NA
    } else {
      rv_filtres$value <- input$chk_filtres
    }
  })
  
  
  # input$filtrer ----
  observeEvent(input$filtrer, {
    
    showModal(modalDialog(title = NULL,
                          div(
                            tags$p(checkboxGroupInput("chk_filtres", 
                                                      "Friches à afficher :",
                                                      choices = Filtres,
                                                      selected = NULL))
                          ),
                          footer = modalButton("Valider la sélection"),
                          easyClose = TRUE,
                          size="s"))
  })
  
  # lnk_mte_pv ----
  observeEvent(input$lnk_mte_pv, {
    showModal(modalDialog(title = NULL,
                          tagList(tags$p(tags$img(src="logo-ademe.png", width="33%")),
                                  tags$p("Site potentiellement en friche, dont la destination pour du photovoltaïque au sol a été confirmée comme intéressante par une étude Ademe en 2021."),
                                  tags$div(tags$img(src="images/panneaux.jpeg", width="100%"), 
                                           style="margin-right: -31px;
                                         margin-left: -31px;"),
                                  tags$p(tags$a(href="https://www.ecologie.gouv.fr/solaire#scroll-nav__7", 
                                                "▶ Accéder aux résultats de l'étude",
                                                target = "_blank"))), 
                          footer = modalButton("Fermer la fenêtre"),
                          easyClose = TRUE
    ))
  })
  
  # shinydisconnect ----
  observeEvent(input$disconnect, {
    session$close()
  })
  
  # Aller vers un site : on active l'onglet "Cartographie des friches" ----
  # La carte est calculée dans output$mymap, et on y ajuste la BBOX
  observe({
    req(r_bbox())
    updateNavbarPage(session, "app_navbar", selected = "Cartographie des friches")
  })
  
  # url : rajoute bbox dans l'url ----
  observe({
    req(input$mymap_bounds)
    
    bbox <- input$mymap_bounds
    
    xmin <- bbox$west
    ymin <- bbox$south
    xmax <- bbox$east
    ymax <- bbox$north
    
    bbox <- glue("{xmin},{ymin},{xmax},{ymax}")
    
    updateQueryString(
      glue("?bbox={bbox}"),
      mode = c("replace")
    )
  })
  
  # btn_voir ----
  observeEvent(input$btn_voir, {
    updateNavbarPage(session, "app_navbar", selected = "Cartographie des friches")
  })
  
  # btn_chercher ----
  observeEvent(input$btn_chercher, {
    updateNavbarPage(session, "app_navbar", selected = "Cartographie des friches")
  })
  
  # mymap_zoom (enlève les contours associés à ContoursMouseOver) ----
  observe({
    req(input$mymap_zoom)
    zoom <- input$mymap_zoom
    if (zoom > ZOOM_LEVELS["Département"]) {
      proxy %>% clearGroup("ContoursMouseOver")
    }
  })
  
  # open_mentions ----
  observeEvent(input$open_mentions, {
    message(">> Ouverture des mentions")
    updateNavbarPage(session, "app_navbar", selected = "Mentions légales") 
  })
  
  # open_mutabilite ----
  # observeEvent(input$open_mutabilite, {
  #   message(">> Ouverture mutabilite")
  #   updateNavbarPage(session, "app_navbar", selected = "Indices de mutabilité", target="_blank")
  #   # runjs("window.open('https://cartofriches.cerema.fr/?mutabilite', '_blank');")
  #   # runjs("window.open('https://cerema-med.shinyapps.io/cartofriches-beta/?mutabilite', '_blank');")
  #   removeModal()
  # })
  
  # open_enrichissementdata ----
  observeEvent(input$open_enrichissementdata, {
    message(">> Ouverture Enrichissement de données")
    updateNavbarPage(session, "app_navbar", selected = "Enrichissement de données")
    removeModal()
  })
  
  # Affichage du bouton "Aller vers la friche la plus proche" ---- PB NICOLAS FINDFRICHE
  observe({
    
    # Friche la plus proche
    res <- r_closest_friche()
    
    if(is.null(res)) {
      proxy %>% clearControls()
      return()
    }
    
    # Distance à la friche
    distance_txt <- res$distance_txt
    
    # Friche concernée
    f <- Data$points %>% filter(site_id == res$site_id)
    
    # Output à ajouter à la carte
    ui <- actionButton("btn_see_friche",
                       tagList(tags$i(class="far fa-paper-plane", role="presentation", `aria-label`="paper-plane icon", style="color:white"),
                               glue("Friche à {distance_txt}")
                       ),
                       class = "goto_map"
    )
    proxy %>% clearControls() %>% addControl(ui)
  })
  
  # btn_see_friche ----  PB NICOLAS FINDFRICHE
  observeEvent(input$btn_see_friche, {
    res <- r_closest_friche()
    coords <- res$coords
    proxy %>% flyTo(coords[1], coords[2], 18)
  })
  
  # slc_secteurs (Choix de Métropole ou DOM TOM) ----
  observeEvent(input$slc_secteurs, {
    proxy %>% zoom_secteur(secteur = input$slc_secteurs)
  }, ignoreInit = TRUE)
  
  # slc_observatoires (Choix d'un observatoire) ----
  observeEvent(input$slc_observatoires, {
    req(input$slc_observatoires)
    proxy %>% zoom_observatoire(input$slc_observatoires)
  }, ignoreInit = TRUE)
  
  # Ajoute les sites sur la carte depuis r_data() ----
  observe({
    
    req(r_data())
    req(!is.null(input$chk_all))
    
    message(">> Observe : affichage des objets")
    message(">> names(r_data()) ", paste(names(r_data()), collapse = ", "))
    
    # Si pas de résultat, alors pas de friche à l'endroit souhaité
    # on enlève alors les marqueurs de sites et les unités foncières affichées précédemment
    
    # REGIONS 
    if(any(names(r_data()) == "regs")) {
      message(">> Affichage des cercles régionaux")
      
      f <- r_data()$regs
      
      chk_all <- input$chk_all
      
      proxy %>%
        clearGroup("Basias et Basol") %>%
        clearGroup("Unités foncières") %>%
        clearGroup("stat_dep") %>%
        add_circles(f, 
                    group = "stat_reg", 
                    chk_all = input$chk_all)
      message(">> Fin - Affichage des cercles régionaux")
      
      # DEPARTEMENTS
    } else if(any(names(r_data()) == "deps")) {
      message(">> Affichage des cercles stats départementaux")
      
      chk_all <- input$chk_all
      
      proxy %>%
        clearGroup("Basias et Basol") %>% 
        clearGroup("Unités foncières") %>% 
        clearGroup("stat_reg") %>% 
        add_circles(r_data()$deps, 
                    group = "stat_dep",
                    chk_all = chk_all)
      
      # FRICHES 
    } else if(any(names(r_data()) == "points")) {
      message(">> Affichage des points")
      proxy %>% 
        clearGroup("Basias et Basol") %>% 
        clearGroup("Unités foncières")
      
      if(!is.null(r_data()$points)) {
        f <- r_data()$points
        
        proxy %>%
          clearMarkers() %>% 
          add_points(f, replaceMarkers = TRUE)
        
        if("polygons" %in% names(r_data())) {
          message(">> Affichage des surfaces")
          proxy %>%
            clearGroup("Unités foncières") %>% 
            add_polygons(r_data()$polygons,
                         group = "Unités foncières des friches industrielles")
        }
      }
    }
  })
  
  # mymap_marker_click (Clic sur marqueur ou cercle statistique) ----
  observeEvent(input$mymap_marker_click, {
    event <- input$mymap_marker_click
    id <- event$id
    
    message(">> Marqueur cliqué d'id ", id)
    
    # Si id est nul, on quitte
    if(is.null(id)) return()
    
    # On met à jour l'URL avec le numéro du site
    f <- get_friche_from_id(id)
    site_id <- f$site_id
    updateQueryString(
      glue("?site={site_id}"),
      mode = c("replace")
    )
    
    
    ## __Mutabilité__ -----
    
    output$GRAPH_mutabilite <- renderPlot({
      
      FCT_rajoutmutabilite <- function(dataframe){
        
        dataframe %>%
          st_drop_geometry() %>%
          mutate(
            # Terrain viabilisé
            Score_terrainviabilise_residentiel = ifelse(input$INPUT_terrainviabilise == "Oui",2,
                                                        ifelse(input$INPUT_terrainviabilise == "Non",0,0)),
            Score_terrainviabilise_equipement = ifelse(input$INPUT_terrainviabilise == "Oui",2,
                                                       ifelse(input$INPUT_terrainviabilise == "Non",0,0)),
            Score_terrainviabilise_culturel = ifelse(input$INPUT_terrainviabilise == "Oui",2,
                                                     ifelse(input$INPUT_terrainviabilise == "Non",0,0)),
            Score_terrainviabilise_tertiaire = ifelse(input$INPUT_terrainviabilise == "Oui",2,
                                                      ifelse(input$INPUT_terrainviabilise == "Non",0,0)),
            Score_terrainviabilise_industriel = ifelse(input$INPUT_terrainviabilise == "Oui",2,
                                                       ifelse(input$INPUT_terrainviabilise == "Non",0,0)),
            Score_terrainviabilise_renaturation = ifelse(input$INPUT_terrainviabilise == "Oui",0.5,
                                                         ifelse(input$INPUT_terrainviabilise == "Non",2,0)),
            Score_terrainviabilise_PV = ifelse(input$INPUT_terrainviabilise == "Oui",0.5,
                                               ifelse(input$INPUT_terrainviabilise == "Non",1,0)),
            
            # négatif
            ScoreNegatif_terrainviabilise_residentiel = ifelse(input$INPUT_terrainviabilise == "Oui",0,
                                                               ifelse(input$INPUT_terrainviabilise == "Non",2,0)),
            ScoreNegatif_terrainviabilise_equipement = ifelse(input$INPUT_terrainviabilise == "Oui",0,
                                                              ifelse(input$INPUT_terrainviabilise == "Non",2,0)),
            ScoreNegatif_terrainviabilise_culturel = ifelse(input$INPUT_terrainviabilise == "Oui",0,
                                                            ifelse(input$INPUT_terrainviabilise == "Non",2,0)),
            ScoreNegatif_terrainviabilise_tertiaire = ifelse(input$INPUT_terrainviabilise == "Oui",0,
                                                             ifelse(input$INPUT_terrainviabilise == "Non",2,0)),
            ScoreNegatif_terrainviabilise_industriel = ifelse(input$INPUT_terrainviabilise == "Oui",0,
                                                              ifelse(input$INPUT_terrainviabilise == "Non",2,0)),
            ScoreNegatif_terrainviabilise_renaturation = ifelse(input$INPUT_terrainviabilise == "Oui",0.5,
                                                                ifelse(input$INPUT_terrainviabilise == "Non",0,0)),
            ScoreNegatif_terrainviabilise_PV = ifelse(input$INPUT_terrainviabilise == "Oui",0.5,
                                                      ifelse(input$INPUT_terrainviabilise == "Non",0,0))
          ) %>%
          mutate(
            # Espèce protégé ?
            Score_especeprotege_residentiel = ifelse(input$INPUT_especeprotege == "Oui",0,
                                                     ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            Score_especeprotege_equipement = ifelse(input$INPUT_especeprotege == "Oui",0,
                                                    ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            Score_especeprotege_culturel = ifelse(input$INPUT_especeprotege == "Oui",0,
                                                  ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            Score_especeprotege_tertiaire = ifelse(input$INPUT_especeprotege == "Oui",0,
                                                   ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            Score_especeprotege_industriel = ifelse(input$INPUT_especeprotege == "Oui",0,
                                                    ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            Score_especeprotege_renaturation = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                      ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            Score_especeprotege_PV = ifelse(input$INPUT_especeprotege == "Oui",0,
                                            ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            
            # négatif
            ScoreNegatif_especeprotege_residentiel = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                            ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            ScoreNegatif_especeprotege_equipement = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                           ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            ScoreNegatif_especeprotege_culturel = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                         ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            ScoreNegatif_especeprotege_tertiaire = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                          ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            ScoreNegatif_especeprotege_industriel = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                           ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            ScoreNegatif_especeprotege_renaturation = ifelse(input$INPUT_especeprotege == "Oui",0,
                                                             ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            ScoreNegatif_especeprotege_PV = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                   ifelse(input$INPUT_especeprotege == "Non",0.5,0))
          ) %>%
          mutate(
            # Zone humide ?
            Score_zonehumide_residentiel = ifelse(input$INPUT_zonehumide == "Oui",0,
                                                  ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            Score_zonehumide_equipement = ifelse(input$INPUT_zonehumide == "Oui",0,
                                                 ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            Score_zonehumide_culturel = ifelse(input$INPUT_zonehumide == "Oui",0,
                                               ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            Score_zonehumide_tertiaire = ifelse(input$INPUT_zonehumide == "Oui",0,
                                                ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            Score_zonehumide_industriel = ifelse(input$INPUT_zonehumide == "Oui",0,
                                                 ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            Score_zonehumide_renaturation = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                   ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            Score_zonehumide_PV = ifelse(input$INPUT_zonehumide == "Oui",0,
                                         ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            
            # négatif
            ScoreNegatif_zonehumide_residentiel = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                         ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            ScoreNegatif_zonehumide_equipement = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                        ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            ScoreNegatif_zonehumide_culturel = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                      ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            ScoreNegatif_zonehumide_tertiaire = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                       ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            ScoreNegatif_zonehumide_industriel = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                        ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            ScoreNegatif_zonehumide_renaturation = ifelse(input$INPUT_zonehumide == "Oui",0,
                                                          ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            ScoreNegatif_zonehumide_PV = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                ifelse(input$INPUT_zonehumide == "Non",0.5,0))
          ) %>%
          mutate(
            # Voie d'eau ?
            Score_voieeau_residentiel = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                               ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                      ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            Score_voieeau_equipement = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                              ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                     ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            Score_voieeau_culturel = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                            ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                   ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            Score_voieeau_tertiaire = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                             ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                    ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            Score_voieeau_industriel = ifelse(input$INPUT_voieeau == "Oui",1,
                                              ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                     ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            Score_voieeau_renaturation = ifelse(input$INPUT_voieeau == "Oui",1,
                                                ifelse(input$INPUT_voieeau == "Oui_nv",1,
                                                       ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            Score_voieeau_PV = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                      ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                             ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            
            # négatif
            ScoreNegatif_voieeau_residentiel = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                                      ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                             ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            ScoreNegatif_voieeau_equipement = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                                     ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                            ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            ScoreNegatif_voieeau_culturel = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                                   ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                          ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            ScoreNegatif_voieeau_tertiaire = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                                    ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                           ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            ScoreNegatif_voieeau_industriel = ifelse(input$INPUT_voieeau == "Oui",0,
                                                     ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                            ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            ScoreNegatif_voieeau_renaturation = ifelse(input$INPUT_voieeau == "Oui",0,
                                                       ifelse(input$INPUT_voieeau == "Oui_nv",0,
                                                              ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            ScoreNegatif_voieeau_PV = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                             ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                    ifelse(input$INPUT_voieeau == "Non",0.5,0)))
          ) %>%
          
          # Centre bourg
          mutate(Score_centrebourg_residentiel = 2*ifelse(input$INPUT_centrebourg == "Oui",2,0),
                 Score_centrebourg_equipement = 2*ifelse(input$INPUT_centrebourg == "Oui",1,0),
                 Score_centrebourg_culturel = 2*0.5,
                 Score_centrebourg_tertiaire = 2*ifelse(input$INPUT_centrebourg == "Oui",1,0),
                 Score_centrebourg_industriel = 2*ifelse(input$INPUT_centrebourg == "Oui",0,2),
                 Score_centrebourg_renaturation = 2*ifelse(input$INPUT_centrebourg == "Oui",0.5,1),
                 Score_centrebourg_PV = 2*ifelse(input$INPUT_centrebourg == "Oui",0,2),
                 
                 # negatif
                 ScoreNegatif_centrebourg_residentiel = 2*ifelse(input$INPUT_centrebourg == "Oui",0,2),
                 ScoreNegatif_centrebourg_equipement = 2*ifelse(input$INPUT_centrebourg == "Oui",0,1),
                 ScoreNegatif_centrebourg_culturel = 2*0.5,
                 ScoreNegatif_centrebourg_tertiaire = 2*ifelse(input$INPUT_centrebourg == "Oui",0,1),
                 ScoreNegatif_centrebourg_industriel = 2*ifelse(input$INPUT_centrebourg == "Oui",2,0),
                 ScoreNegatif_centrebourg_renaturation = 2*ifelse(input$INPUT_centrebourg == "Oui",0.5,0),
                 ScoreNegatif_centrebourg_PV = 2*ifelse(input$INPUT_centrebourg == "Oui",2,0),
          ) %>%
          
          # mutate(
          #   # Couvert végétal ?
          #   Score_couvertvege_residentiel = ifelse(input$INPUT_couvertvege == "Imper",1,
          #                                          ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                 ifelse(input$INPUT_couvertvege == "Vegefaible",0,
          #                                                        ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          #   Score_couvertvege_equipement = ifelse(input$INPUT_couvertvege == "Imper",1,
          #                                         ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                ifelse(input$INPUT_couvertvege == "Vegefaible",0,
          #                                                       ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          #   Score_couvertvege_culturel = ifelse(input$INPUT_couvertvege == "Imper",0.5,
          #                                       ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                              ifelse(input$INPUT_couvertvege == "Vegefaible",0.5,
          #                                                     ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          #   Score_couvertvege_tertiaire = ifelse(input$INPUT_couvertvege == "Imper",1,
          #                                        ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                               ifelse(input$INPUT_couvertvege == "Vegefaible",0,
          #                                                      ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          #   Score_couvertvege_industriel = ifelse(input$INPUT_couvertvege == "Imper",1,
          #                                         ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                ifelse(input$INPUT_couvertvege == "Vegefaible",0,
          #                                                       ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          #   Score_couvertvege_renaturation = ifelse(input$INPUT_couvertvege == "Imper",0,
          #                                           ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                  ifelse(input$INPUT_couvertvege == "Vegefaible",1,
          #                                                         ifelse(input$INPUT_couvertvege == "Vegepredominante",2,0)))),
          #   Score_couvertvege_PV = ifelse(input$INPUT_couvertvege == "Imper",0.5,
          #                                 ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                        ifelse(input$INPUT_couvertvege == "Vegefaible",0,
          #                                               ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          # 
          #   # négatif
          #   ScoreNegatif_couvertvege_residentiel = ifelse(input$INPUT_couvertvege == "Imper",0,
          #                                                 ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                        ifelse(input$INPUT_couvertvege == "Vegefaible",1,
          #                                                               ifelse(input$INPUT_couvertvege == "Vegepredominante",2,0)))),
          #   ScoreNegatif_couvertvege_equipement = ifelse(input$INPUT_couvertvege == "Imper",0,
          #                                                ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                       ifelse(input$INPUT_couvertvege == "Vegefaible",1,
          #                                                              ifelse(input$INPUT_couvertvege == "Vegepredominante",2,0)))),
          #   ScoreNegatif_couvertvege_culturel = ifelse(input$INPUT_couvertvege == "Imper",0.5,
          #                                              ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                     ifelse(input$INPUT_couvertvege == "Vegefaible",0.5,
          #                                                            ifelse(input$INPUT_couvertvege == "Vegepredominante",1,0)))),
          #   ScoreNegatif_couvertvege_tertiaire = ifelse(input$INPUT_couvertvege == "Imper",0,
          #                                               ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                      ifelse(input$INPUT_couvertvege == "Vegefaible",1,
          #                                                             ifelse(input$INPUT_couvertvege == "Vegepredominante",2,0)))),
          #   ScoreNegatif_couvertvege_industriel = ifelse(input$INPUT_couvertvege == "Imper",0,
          #                                                ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                       ifelse(input$INPUT_couvertvege == "Vegefaible",1,
          #                                                              ifelse(input$INPUT_couvertvege == "Vegepredominante",2,0)))),
          #   ScoreNegatif_couvertvege_renaturation = ifelse(input$INPUT_couvertvege == "Imper",1,
          #                                                  ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                         ifelse(input$INPUT_couvertvege == "Vegefaible",0,
          #                                                                ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          #   ScoreNegatif_couvertvege_PV = ifelse(input$INPUT_couvertvege == "Imper",0.5,
          #                                        ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                               ifelse(input$INPUT_couvertvege == "Vegefaible",1,
          #                                                      ifelse(input$INPUT_couvertvege == "Vegepredominante",2,0))))
          # ) %>%
          mutate(
            # valeur architecturale ?
            Score_archi_residentiel = ifelse(input$INPUT_archi == "Non",0.5,
                                             ifelse(input$INPUT_archi == "faible",0.5,
                                                    ifelse(input$INPUT_archi == "moyen",0.5,
                                                           ifelse(input$INPUT_archi == "fort",0.5,
                                                                  ifelse(input$INPUT_archi == "except",0.5,0))))),
            Score_archi_equipement = ifelse(input$INPUT_archi == "Non",0.5,
                                            ifelse(input$INPUT_archi == "faible",0.5,
                                                   ifelse(input$INPUT_archi == "moyen",0.5,
                                                          ifelse(input$INPUT_archi == "fort",0.5,
                                                                 ifelse(input$INPUT_archi == "except",0.5,0))))),
            Score_archi_culturel = ifelse(input$INPUT_archi == "Non",0,
                                          ifelse(input$INPUT_archi == "faible",0,
                                                 ifelse(input$INPUT_archi == "moyen",0.5,
                                                        ifelse(input$INPUT_archi == "fort",1,
                                                               ifelse(input$INPUT_archi == "except",2,0))))),
            Score_archi_tertiaire = ifelse(input$INPUT_archi == "Non",1,
                                           ifelse(input$INPUT_archi == "faible",1,
                                                  ifelse(input$INPUT_archi == "moyen",0.5,
                                                         ifelse(input$INPUT_archi == "fort",0,
                                                                ifelse(input$INPUT_archi == "except",0,0))))),
            Score_archi_industriel = ifelse(input$INPUT_archi == "Non",1,
                                            ifelse(input$INPUT_archi == "faible",1,
                                                   ifelse(input$INPUT_archi == "moyen",0.5,
                                                          ifelse(input$INPUT_archi == "fort",0,
                                                                 ifelse(input$INPUT_archi == "except",0,0))))),
            Score_archi_renaturation = ifelse(input$INPUT_archi == "Non",1,
                                              ifelse(input$INPUT_archi == "faible",0.5,
                                                     ifelse(input$INPUT_archi == "moyen",0,
                                                            ifelse(input$INPUT_archi == "fort",0,
                                                                   ifelse(input$INPUT_archi == "except",0,0))))),
            Score_archi_PV = ifelse(input$INPUT_archi == "Non",2,
                                    ifelse(input$INPUT_archi == "faible",1,
                                           ifelse(input$INPUT_archi == "moyen",0.5,
                                                  ifelse(input$INPUT_archi == "fort",0,
                                                         ifelse(input$INPUT_archi == "except",0,0))))),
            
            # négatif
            ScoreNegatif_archi_residentiel = ifelse(input$INPUT_archi == "Non",0.5,
                                                    ifelse(input$INPUT_archi == "faible",0.5,
                                                           ifelse(input$INPUT_archi == "moyen",0.5,
                                                                  ifelse(input$INPUT_archi == "fort",0.5,
                                                                         ifelse(input$INPUT_archi == "except",0.5,0))))),
            ScoreNegatif_archi_equipement = ifelse(input$INPUT_archi == "Non",0.5,
                                                   ifelse(input$INPUT_archi == "faible",0.5,
                                                          ifelse(input$INPUT_archi == "moyen",0.5,
                                                                 ifelse(input$INPUT_archi == "fort",0.5,
                                                                        ifelse(input$INPUT_archi == "except",0.5,0))))),
            ScoreNegatif_archi_culturel = ifelse(input$INPUT_archi == "Non",1,
                                                 ifelse(input$INPUT_archi == "faible",1,
                                                        ifelse(input$INPUT_archi == "moyen",0.5,
                                                               ifelse(input$INPUT_archi == "fort",0,
                                                                      ifelse(input$INPUT_archi == "except",0,0))))),
            ScoreNegatif_archi_tertiaire = ifelse(input$INPUT_archi == "Non",0,
                                                  ifelse(input$INPUT_archi == "faible",0,
                                                         ifelse(input$INPUT_archi == "moyen",0.5,
                                                                ifelse(input$INPUT_archi == "fort",1,
                                                                       ifelse(input$INPUT_archi == "except",2,0))))),
            ScoreNegatif_archi_industriel = ifelse(input$INPUT_archi == "Non",0,
                                                   ifelse(input$INPUT_archi == "faible",0,
                                                          ifelse(input$INPUT_archi == "moyen",0.5,
                                                                 ifelse(input$INPUT_archi == "fort",1,
                                                                        ifelse(input$INPUT_archi == "except",1,0))))),
            ScoreNegatif_archi_renaturation = ifelse(input$INPUT_archi == "Non",0,
                                                     ifelse(input$INPUT_archi == "faible",0.5,
                                                            ifelse(input$INPUT_archi == "moyen",1,
                                                                   ifelse(input$INPUT_archi == "fort",2,
                                                                          ifelse(input$INPUT_archi == "except",2,0))))),
            ScoreNegatif_archi_PV = ifelse(input$INPUT_archi == "Non",0,
                                           ifelse(input$INPUT_archi == "faible",0,
                                                  ifelse(input$INPUT_archi == "moyen",0.5,
                                                         ifelse(input$INPUT_archi == "fort",2,
                                                                ifelse(input$INPUT_archi == "except",2,0)))))
          ) %>%
          
          
          # pollution du sol ####
        mutate(Score_pollution_residentiel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                      ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                             ifelse(input$INPUT_pollution %in% c("pollution volatil"),0,
                                                                    ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5)))),
               Score_pollution_equipement =  2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                      ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                             ifelse(input$INPUT_pollution %in% c("pollution volatil"),0,
                                                                    ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5)))),
               Score_pollution_culturel =  2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                    ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                           ifelse(input$INPUT_pollution %in% c("pollution volatil"),0,
                                                                  ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5)))),
               Score_pollution_tertiaire = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                    ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                           ifelse(input$INPUT_pollution %in% c("pollution volatil"),0,
                                                                  ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5)))),
               Score_pollution_industriel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                     ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                            ifelse(input$INPUT_pollution %in% c("pollution volatil"),0,
                                                                   ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5)))),
               Score_pollution_renaturation = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),1,
                                                       ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                              ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0.5,0.5))),
               Score_pollution_PV = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),1,
                                             ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                    ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0.5,0.5))),
               
               # negatif
               ScoreNegatif_pollution_residentiel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                             ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                    ifelse(input$INPUT_pollution %in% c("pollution volatil"),2,
                                                                           ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5)))),
               ScoreNegatif_pollution_equipement = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                            ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                   ifelse(input$INPUT_pollution %in% c("pollution volatil"),2,
                                                                          ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5)))),
               ScoreNegatif_pollution_culturel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                          ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                 ifelse(input$INPUT_pollution %in% c("pollution volatil"),2,
                                                                        ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5)))),
               ScoreNegatif_pollution_tertiaire = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                           ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                  ifelse(input$INPUT_pollution %in% c("pollution volatil"),1,
                                                                         ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5)))),
               ScoreNegatif_pollution_industriel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                            ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                   ifelse(input$INPUT_pollution %in% c("pollution volatil"),1,
                                                                          ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5)))),
               ScoreNegatif_pollution_renaturation = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                              ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                     ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0.5,0.5))),
               ScoreNegatif_pollution_PV = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                    ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                           ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0.5,0.5)))
               
        ) %>%
          
          # risques naturels ####
        mutate(Score_pprmouppri_residentiel = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                     ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               Score_pprmouppri_equipement = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                    ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               Score_pprmouppri_culturel = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                  ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               Score_pprmouppri_tertiaire = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                   ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               Score_pprmouppri_industriel = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                    ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               Score_pprmouppri_renaturation = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                      ifelse(input$INPUT_risquesnaturels == "risque fort",2,1)),
               Score_pprmouppri_PV = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                            ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               
               # négatif
               ScoreNegatif_pprmouppri_residentiel = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                            ifelse(input$INPUT_risquesnaturels == "risque fort",2,1)),
               ScoreNegatif_pprmouppri_equipement = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                           ifelse(input$INPUT_risquesnaturels == "risque fort",2,1)),
               ScoreNegatif_pprmouppri_culturel = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                         ifelse(input$INPUT_risquesnaturels == "risque fort",2,1)),
               ScoreNegatif_pprmouppri_tertaire = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                         ifelse(input$INPUT_risquesnaturels == "risque fort",2,1)),
               ScoreNegatif_pprmouppri_industriel = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                           ifelse(input$INPUT_risquesnaturels == "risque fort",2,1)),
               ScoreNegatif_pprmouppri_renaturation = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                             ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               ScoreNegatif_pprmouppri_PV = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                   ifelse(input$INPUT_risquesnaturels == "risque fort",1,1)),
               
        )     %>%
          
          
          mutate(Score_rajout_residentiel = Score_terrainviabilise_residentiel + Score_especeprotege_residentiel + Score_zonehumide_residentiel + Score_voieeau_residentiel + Score_archi_residentiel + Score_centrebourg_residentiel + Score_pollution_residentiel + Score_pprmouppri_residentiel,
                 ScoreNegatif_rajout_residentiel = ScoreNegatif_terrainviabilise_residentiel + ScoreNegatif_especeprotege_residentiel + ScoreNegatif_zonehumide_residentiel + ScoreNegatif_voieeau_residentiel + ScoreNegatif_archi_residentiel + ScoreNegatif_centrebourg_residentiel + ScoreNegatif_pollution_residentiel + ScoreNegatif_pprmouppri_residentiel,
                 
                 Score_rajout_equipement = Score_terrainviabilise_equipement + Score_especeprotege_equipement + Score_zonehumide_equipement + Score_voieeau_equipement + Score_archi_equipement + Score_centrebourg_equipement + Score_pollution_equipement + Score_pprmouppri_equipement, 
                 ScoreNegatif_rajout_equipement = ScoreNegatif_terrainviabilise_equipement + ScoreNegatif_especeprotege_equipement + ScoreNegatif_zonehumide_equipement + ScoreNegatif_voieeau_equipement + ScoreNegatif_archi_equipement + ScoreNegatif_centrebourg_equipement + ScoreNegatif_pollution_equipement + ScoreNegatif_pprmouppri_equipement,
                 
                 Score_rajout_culturel = Score_terrainviabilise_culturel + Score_especeprotege_culturel + Score_zonehumide_culturel + Score_voieeau_culturel+ Score_archi_culturel + Score_centrebourg_culturel + Score_pollution_culturel + Score_pprmouppri_culturel,
                 ScoreNegatif_rajout_culturel = ScoreNegatif_terrainviabilise_culturel + ScoreNegatif_especeprotege_culturel + ScoreNegatif_zonehumide_culturel + ScoreNegatif_voieeau_culturel + ScoreNegatif_archi_culturel + ScoreNegatif_centrebourg_culturel + ScoreNegatif_pollution_culturel + ScoreNegatif_pprmouppri_culturel,
                 
                 Score_rajout_tertiaire = Score_terrainviabilise_tertiaire + Score_especeprotege_tertiaire + Score_zonehumide_tertiaire + Score_voieeau_tertiaire + Score_archi_tertiaire + Score_centrebourg_tertiaire + Score_pollution_tertiaire + Score_pprmouppri_tertiaire,
                 ScoreNegatif_rajout_tertiaire = ScoreNegatif_terrainviabilise_tertiaire + ScoreNegatif_especeprotege_tertiaire + ScoreNegatif_zonehumide_tertiaire + ScoreNegatif_voieeau_tertiaire + ScoreNegatif_archi_tertiaire + ScoreNegatif_centrebourg_tertiaire + ScoreNegatif_pollution_tertiaire + ScoreNegatif_pprmouppri_tertaire,
                 
                 Score_rajout_industriel = Score_terrainviabilise_industriel + Score_especeprotege_industriel + Score_zonehumide_industriel + Score_voieeau_industriel + Score_archi_industriel + Score_centrebourg_industriel + Score_pollution_industriel + Score_pprmouppri_industriel,
                 ScoreNegatif_rajout_industriel = ScoreNegatif_terrainviabilise_industriel + ScoreNegatif_especeprotege_industriel + ScoreNegatif_zonehumide_industriel + ScoreNegatif_voieeau_industriel + ScoreNegatif_archi_industriel + ScoreNegatif_centrebourg_industriel + ScoreNegatif_pollution_industriel + ScoreNegatif_pprmouppri_industriel,
                 
                 Score_rajout_renaturation = Score_terrainviabilise_renaturation + Score_especeprotege_renaturation + Score_zonehumide_renaturation + Score_voieeau_renaturation + Score_archi_renaturation + Score_centrebourg_renaturation + Score_pollution_renaturation + Score_pprmouppri_renaturation,
                 ScoreNegatif_rajout_renaturation = ScoreNegatif_terrainviabilise_renaturation + ScoreNegatif_especeprotege_renaturation + ScoreNegatif_zonehumide_renaturation + ScoreNegatif_voieeau_renaturation + ScoreNegatif_archi_renaturation + ScoreNegatif_centrebourg_renaturation + ScoreNegatif_pollution_renaturation + ScoreNegatif_pprmouppri_renaturation, 
                 
                 Score_rajout_PV = Score_terrainviabilise_PV + Score_especeprotege_PV + Score_zonehumide_PV + Score_voieeau_PV + Score_archi_PV + Score_centrebourg_PV + Score_pollution_PV + Score_pprmouppri_PV,
                 ScoreNegatif_rajout_PV = ScoreNegatif_terrainviabilise_PV + ScoreNegatif_especeprotege_PV + ScoreNegatif_zonehumide_PV + ScoreNegatif_voieeau_PV + ScoreNegatif_archi_PV + ScoreNegatif_centrebourg_PV + ScoreNegatif_pollution_PV + ScoreNegatif_pprmouppri_PV
          )
        
      }
      
      data_mutatserveur <-  f %>%
        FCT_rajoutmutabilite() %>%
        mutate("Résidentiel" = (ScorePositif_residentiel+Score_rajout_residentiel)/(ScorePositif_residentiel+Score_rajout_residentiel+ScoreNegatif_residentiel+ScoreNegatif_rajout_residentiel),
               "Equipement public" = (ScorePositif_equipement+Score_rajout_equipement)/(ScorePositif_equipement+Score_rajout_equipement+ScoreNegatif_equipement+ScoreNegatif_rajout_equipement),
               # "Culturel" = (ScorePositif_tertiaire+Score_rajout_tertiaire)/(ScorePositif_tertiaire+Score_rajout_tertiaire+ScoreNegatif_tertiaire+ScoreNegatif_rajout_tertiaire),
               "Tertiaire" = (ScorePositif_tertiaire+Score_rajout_tertiaire)/(ScorePositif_tertiaire+Score_rajout_tertiaire+ScoreNegatif_tertiaire+ScoreNegatif_rajout_tertiaire),
               "Industriel" = (ScorePositif_industriel+Score_rajout_industriel)/(ScorePositif_industriel+Score_rajout_industriel+ScoreNegatif_industriel+ScoreNegatif_rajout_industriel),
               "Renaturation" = (ScorePositif_renaturation+Score_rajout_renaturation)/(ScorePositif_renaturation+Score_rajout_renaturation+ScoreNegatif_renaturation+ScoreNegatif_rajout_renaturation),
               "Photovoltaïque au sol" = (ScorePositif_PV+Score_rajout_PV)/(ScorePositif_PV+Score_rajout_PV+ScoreNegatif_PV+ScoreNegatif_rajout_PV)) %>%
        select("Résidentiel","Equipement public",#"Culturel",
               "Tertiaire","Industriel","Renaturation","Photovoltaïque au sol")
      
      radarchart(rbind(rep(1,6) , rep(0,6) , data_mutatserveur),
                 # rbind(rep(1,7) , rep(0,7) , data_mutatserveur),
                 
                 #custom polygon
                 pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,
                 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
      )
      
    })
    
    output$GT_mutabilite <- render_gt({
      
      FCT_rajoutmutabilite <- function(dataframe){
        
        dataframe %>%
          st_drop_geometry() %>%
          mutate(
            # Terrain viabilisé
            Score_terrainviabilise_residentiel = ifelse(input$INPUT_terrainviabilise == "Oui",2,
                                                        ifelse(input$INPUT_terrainviabilise == "Non",0,0)),
            Score_terrainviabilise_equipement = ifelse(input$INPUT_terrainviabilise == "Oui",2,
                                                       ifelse(input$INPUT_terrainviabilise == "Non",0,0)),
            Score_terrainviabilise_culturel = ifelse(input$INPUT_terrainviabilise == "Oui",2,
                                                     ifelse(input$INPUT_terrainviabilise == "Non",0,0)),
            Score_terrainviabilise_tertiaire = ifelse(input$INPUT_terrainviabilise == "Oui",2,
                                                      ifelse(input$INPUT_terrainviabilise == "Non",0,0)),
            Score_terrainviabilise_industriel = ifelse(input$INPUT_terrainviabilise == "Oui",2,
                                                       ifelse(input$INPUT_terrainviabilise == "Non",0,0)),
            Score_terrainviabilise_renaturation = ifelse(input$INPUT_terrainviabilise == "Oui",0.5,
                                                         ifelse(input$INPUT_terrainviabilise == "Non",2,0)),
            Score_terrainviabilise_PV = ifelse(input$INPUT_terrainviabilise == "Oui",0.5,
                                               ifelse(input$INPUT_terrainviabilise == "Non",1,0)),
            
            # négatif
            ScoreNegatif_terrainviabilise_residentiel = ifelse(input$INPUT_terrainviabilise == "Oui",0,
                                                               ifelse(input$INPUT_terrainviabilise == "Non",2,0)),
            ScoreNegatif_terrainviabilise_equipement = ifelse(input$INPUT_terrainviabilise == "Oui",0,
                                                              ifelse(input$INPUT_terrainviabilise == "Non",2,0)),
            ScoreNegatif_terrainviabilise_culturel = ifelse(input$INPUT_terrainviabilise == "Oui",0,
                                                            ifelse(input$INPUT_terrainviabilise == "Non",2,0)),
            ScoreNegatif_terrainviabilise_tertiaire = ifelse(input$INPUT_terrainviabilise == "Oui",0,
                                                             ifelse(input$INPUT_terrainviabilise == "Non",2,0)),
            ScoreNegatif_terrainviabilise_industriel = ifelse(input$INPUT_terrainviabilise == "Oui",0,
                                                              ifelse(input$INPUT_terrainviabilise == "Non",2,0)),
            ScoreNegatif_terrainviabilise_renaturation = ifelse(input$INPUT_terrainviabilise == "Oui",0.5,
                                                                ifelse(input$INPUT_terrainviabilise == "Non",0,0)),
            ScoreNegatif_terrainviabilise_PV = ifelse(input$INPUT_terrainviabilise == "Oui",0.5,
                                                      ifelse(input$INPUT_terrainviabilise == "Non",0,0))
          ) %>%
          mutate(
            # Espèce protégé ?
            Score_especeprotege_residentiel = ifelse(input$INPUT_especeprotege == "Oui",0,
                                                     ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            Score_especeprotege_equipement = ifelse(input$INPUT_especeprotege == "Oui",0,
                                                    ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            Score_especeprotege_culturel = ifelse(input$INPUT_especeprotege == "Oui",0,
                                                  ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            Score_especeprotege_tertiaire = ifelse(input$INPUT_especeprotege == "Oui",0,
                                                   ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            Score_especeprotege_industriel = ifelse(input$INPUT_especeprotege == "Oui",0,
                                                    ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            Score_especeprotege_renaturation = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                      ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            Score_especeprotege_PV = ifelse(input$INPUT_especeprotege == "Oui",0,
                                            ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            
            # négatif
            ScoreNegatif_especeprotege_residentiel = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                            ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            ScoreNegatif_especeprotege_equipement = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                           ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            ScoreNegatif_especeprotege_culturel = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                         ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            ScoreNegatif_especeprotege_tertiaire = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                          ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            ScoreNegatif_especeprotege_industriel = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                           ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            ScoreNegatif_especeprotege_renaturation = ifelse(input$INPUT_especeprotege == "Oui",0,
                                                             ifelse(input$INPUT_especeprotege == "Non",0.5,0)),
            ScoreNegatif_especeprotege_PV = ifelse(input$INPUT_especeprotege == "Oui",2,
                                                   ifelse(input$INPUT_especeprotege == "Non",0.5,0))
          ) %>%
          mutate(
            # Zone humide ?
            Score_zonehumide_residentiel = ifelse(input$INPUT_zonehumide == "Oui",0,
                                                  ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            Score_zonehumide_equipement = ifelse(input$INPUT_zonehumide == "Oui",0,
                                                 ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            Score_zonehumide_culturel = ifelse(input$INPUT_zonehumide == "Oui",0,
                                               ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            Score_zonehumide_tertiaire = ifelse(input$INPUT_zonehumide == "Oui",0,
                                                ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            Score_zonehumide_industriel = ifelse(input$INPUT_zonehumide == "Oui",0,
                                                 ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            Score_zonehumide_renaturation = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                   ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            Score_zonehumide_PV = ifelse(input$INPUT_zonehumide == "Oui",0,
                                         ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            
            # négatif
            ScoreNegatif_zonehumide_residentiel = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                         ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            ScoreNegatif_zonehumide_equipement = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                        ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            ScoreNegatif_zonehumide_culturel = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                      ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            ScoreNegatif_zonehumide_tertiaire = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                       ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            ScoreNegatif_zonehumide_industriel = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                        ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            ScoreNegatif_zonehumide_renaturation = ifelse(input$INPUT_zonehumide == "Oui",0,
                                                          ifelse(input$INPUT_zonehumide == "Non",0.5,0)),
            ScoreNegatif_zonehumide_PV = ifelse(input$INPUT_zonehumide == "Oui",2,
                                                ifelse(input$INPUT_zonehumide == "Non",0.5,0))
          ) %>%
          mutate(
            # Voie d'eau ?
            Score_voieeau_residentiel = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                               ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                      ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            Score_voieeau_equipement = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                              ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                     ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            Score_voieeau_culturel = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                            ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                   ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            Score_voieeau_tertiaire = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                             ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                    ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            Score_voieeau_industriel = ifelse(input$INPUT_voieeau == "Oui",1,
                                              ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                     ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            Score_voieeau_renaturation = ifelse(input$INPUT_voieeau == "Oui",1,
                                                ifelse(input$INPUT_voieeau == "Oui_nv",1,
                                                       ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            Score_voieeau_PV = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                      ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                             ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            
            # négatif
            ScoreNegatif_voieeau_residentiel = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                                      ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                             ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            ScoreNegatif_voieeau_equipement = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                                     ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                            ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            ScoreNegatif_voieeau_culturel = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                                   ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                          ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            ScoreNegatif_voieeau_tertiaire = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                                    ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                           ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            ScoreNegatif_voieeau_industriel = ifelse(input$INPUT_voieeau == "Oui",0,
                                                     ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                            ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            ScoreNegatif_voieeau_renaturation = ifelse(input$INPUT_voieeau == "Oui",0,
                                                       ifelse(input$INPUT_voieeau == "Oui_nv",0,
                                                              ifelse(input$INPUT_voieeau == "Non",0.5,0))),
            ScoreNegatif_voieeau_PV = ifelse(input$INPUT_voieeau == "Oui",0.5,
                                             ifelse(input$INPUT_voieeau == "Oui_nv",0.5,
                                                    ifelse(input$INPUT_voieeau == "Non",0.5,0)))
          ) %>%
          
          # Centre bourg
          mutate(Score_centrebourg_residentiel = 2*ifelse(input$INPUT_centrebourg == "Oui",2,0),
                 Score_centrebourg_equipement = 2*ifelse(input$INPUT_centrebourg == "Oui",1,0),
                 Score_centrebourg_culturel = 2*0.5,
                 Score_centrebourg_tertiaire = 2*ifelse(input$INPUT_centrebourg == "Oui",1,0),
                 Score_centrebourg_industriel = 2*ifelse(input$INPUT_centrebourg == "Oui",0,2),
                 Score_centrebourg_renaturation = 2*ifelse(input$INPUT_centrebourg == "Oui",0.5,1),
                 Score_centrebourg_PV = 2*ifelse(input$INPUT_centrebourg == "Oui",0,2),
                 
                 # negatif
                 ScoreNegatif_centrebourg_residentiel = 2*ifelse(input$INPUT_centrebourg == "Oui",0,2),
                 ScoreNegatif_centrebourg_equipement = 2*ifelse(input$INPUT_centrebourg == "Oui",0,1),
                 ScoreNegatif_centrebourg_culturel = 2*0.5,
                 ScoreNegatif_centrebourg_tertiaire = 2*ifelse(input$INPUT_centrebourg == "Oui",0,1),
                 ScoreNegatif_centrebourg_industriel = 2*ifelse(input$INPUT_centrebourg == "Oui",2,0),
                 ScoreNegatif_centrebourg_renaturation = 2*ifelse(input$INPUT_centrebourg == "Oui",0.5,0),
                 ScoreNegatif_centrebourg_PV = 2*ifelse(input$INPUT_centrebourg == "Oui",2,0),
          ) %>%
          
          # mutate(
          #   # Couvert végétal ?
          #   Score_couvertvege_residentiel = ifelse(input$INPUT_couvertvege == "Imper",1,
          #                                          ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                 ifelse(input$INPUT_couvertvege == "Vegefaible",0,
          #                                                        ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          #   Score_couvertvege_equipement = ifelse(input$INPUT_couvertvege == "Imper",1,
          #                                         ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                ifelse(input$INPUT_couvertvege == "Vegefaible",0,
          #                                                       ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          #   Score_couvertvege_culturel = ifelse(input$INPUT_couvertvege == "Imper",0.5,
          #                                       ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                              ifelse(input$INPUT_couvertvege == "Vegefaible",0.5,
          #                                                     ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          #   Score_couvertvege_tertiaire = ifelse(input$INPUT_couvertvege == "Imper",1,
          #                                        ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                               ifelse(input$INPUT_couvertvege == "Vegefaible",0,
          #                                                      ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          #   Score_couvertvege_industriel = ifelse(input$INPUT_couvertvege == "Imper",1,
          #                                         ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                ifelse(input$INPUT_couvertvege == "Vegefaible",0,
          #                                                       ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          #   Score_couvertvege_renaturation = ifelse(input$INPUT_couvertvege == "Imper",0,
          #                                           ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                  ifelse(input$INPUT_couvertvege == "Vegefaible",1,
          #                                                         ifelse(input$INPUT_couvertvege == "Vegepredominante",2,0)))),
          #   Score_couvertvege_PV = ifelse(input$INPUT_couvertvege == "Imper",0.5,
          #                                 ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                        ifelse(input$INPUT_couvertvege == "Vegefaible",0,
          #                                               ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          # 
          #   # négatif
          #   ScoreNegatif_couvertvege_residentiel = ifelse(input$INPUT_couvertvege == "Imper",0,
          #                                                 ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                        ifelse(input$INPUT_couvertvege == "Vegefaible",1,
          #                                                               ifelse(input$INPUT_couvertvege == "Vegepredominante",2,0)))),
          #   ScoreNegatif_couvertvege_equipement = ifelse(input$INPUT_couvertvege == "Imper",0,
          #                                                ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                       ifelse(input$INPUT_couvertvege == "Vegefaible",1,
          #                                                              ifelse(input$INPUT_couvertvege == "Vegepredominante",2,0)))),
          #   ScoreNegatif_couvertvege_culturel = ifelse(input$INPUT_couvertvege == "Imper",0.5,
          #                                              ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                     ifelse(input$INPUT_couvertvege == "Vegefaible",0.5,
          #                                                            ifelse(input$INPUT_couvertvege == "Vegepredominante",1,0)))),
          #   ScoreNegatif_couvertvege_tertiaire = ifelse(input$INPUT_couvertvege == "Imper",0,
          #                                               ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                      ifelse(input$INPUT_couvertvege == "Vegefaible",1,
          #                                                             ifelse(input$INPUT_couvertvege == "Vegepredominante",2,0)))),
          #   ScoreNegatif_couvertvege_industriel = ifelse(input$INPUT_couvertvege == "Imper",0,
          #                                                ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                       ifelse(input$INPUT_couvertvege == "Vegefaible",1,
          #                                                              ifelse(input$INPUT_couvertvege == "Vegepredominante",2,0)))),
          #   ScoreNegatif_couvertvege_renaturation = ifelse(input$INPUT_couvertvege == "Imper",1,
          #                                                  ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                                         ifelse(input$INPUT_couvertvege == "Vegefaible",0,
          #                                                                ifelse(input$INPUT_couvertvege == "Vegepredominante",0,0)))),
          #   ScoreNegatif_couvertvege_PV = ifelse(input$INPUT_couvertvege == "Imper",0.5,
          #                                        ifelse(input$INPUT_couvertvege == "Solnu",0.5,
          #                                               ifelse(input$INPUT_couvertvege == "Vegefaible",1,
          #                                                      ifelse(input$INPUT_couvertvege == "Vegepredominante",2,0))))
          # ) %>%
          mutate(
            # valeur architecturale ?
            Score_archi_residentiel = ifelse(input$INPUT_archi == "Non",0.5,
                                             ifelse(input$INPUT_archi == "faible",0.5,
                                                    ifelse(input$INPUT_archi == "moyen",0.5,
                                                           ifelse(input$INPUT_archi == "fort",0.5,
                                                                  ifelse(input$INPUT_archi == "except",0.5,0))))),
            Score_archi_equipement = ifelse(input$INPUT_archi == "Non",0.5,
                                            ifelse(input$INPUT_archi == "faible",0.5,
                                                   ifelse(input$INPUT_archi == "moyen",0.5,
                                                          ifelse(input$INPUT_archi == "fort",0.5,
                                                                 ifelse(input$INPUT_archi == "except",0.5,0))))),
            Score_archi_culturel = ifelse(input$INPUT_archi == "Non",0,
                                          ifelse(input$INPUT_archi == "faible",0,
                                                 ifelse(input$INPUT_archi == "moyen",0.5,
                                                        ifelse(input$INPUT_archi == "fort",1,
                                                               ifelse(input$INPUT_archi == "except",2,0))))),
            Score_archi_tertiaire = ifelse(input$INPUT_archi == "Non",1,
                                           ifelse(input$INPUT_archi == "faible",1,
                                                  ifelse(input$INPUT_archi == "moyen",0.5,
                                                         ifelse(input$INPUT_archi == "fort",0,
                                                                ifelse(input$INPUT_archi == "except",0,0))))),
            Score_archi_industriel = ifelse(input$INPUT_archi == "Non",1,
                                            ifelse(input$INPUT_archi == "faible",1,
                                                   ifelse(input$INPUT_archi == "moyen",0.5,
                                                          ifelse(input$INPUT_archi == "fort",0,
                                                                 ifelse(input$INPUT_archi == "except",0,0))))),
            Score_archi_renaturation = ifelse(input$INPUT_archi == "Non",1,
                                              ifelse(input$INPUT_archi == "faible",0.5,
                                                     ifelse(input$INPUT_archi == "moyen",0,
                                                            ifelse(input$INPUT_archi == "fort",0,
                                                                   ifelse(input$INPUT_archi == "except",0,0))))),
            Score_archi_PV = ifelse(input$INPUT_archi == "Non",2,
                                    ifelse(input$INPUT_archi == "faible",1,
                                           ifelse(input$INPUT_archi == "moyen",0.5,
                                                  ifelse(input$INPUT_archi == "fort",0,
                                                         ifelse(input$INPUT_archi == "except",0,0))))),
            
            # négatif
            ScoreNegatif_archi_residentiel = ifelse(input$INPUT_archi == "Non",0.5,
                                                    ifelse(input$INPUT_archi == "faible",0.5,
                                                           ifelse(input$INPUT_archi == "moyen",0.5,
                                                                  ifelse(input$INPUT_archi == "fort",0.5,
                                                                         ifelse(input$INPUT_archi == "except",0.5,0))))),
            ScoreNegatif_archi_equipement = ifelse(input$INPUT_archi == "Non",0.5,
                                                   ifelse(input$INPUT_archi == "faible",0.5,
                                                          ifelse(input$INPUT_archi == "moyen",0.5,
                                                                 ifelse(input$INPUT_archi == "fort",0.5,
                                                                        ifelse(input$INPUT_archi == "except",0.5,0))))),
            ScoreNegatif_archi_culturel = ifelse(input$INPUT_archi == "Non",1,
                                                 ifelse(input$INPUT_archi == "faible",1,
                                                        ifelse(input$INPUT_archi == "moyen",0.5,
                                                               ifelse(input$INPUT_archi == "fort",0,
                                                                      ifelse(input$INPUT_archi == "except",0,0))))),
            ScoreNegatif_archi_tertiaire = ifelse(input$INPUT_archi == "Non",0,
                                                  ifelse(input$INPUT_archi == "faible",0,
                                                         ifelse(input$INPUT_archi == "moyen",0.5,
                                                                ifelse(input$INPUT_archi == "fort",1,
                                                                       ifelse(input$INPUT_archi == "except",2,0))))),
            ScoreNegatif_archi_industriel = ifelse(input$INPUT_archi == "Non",0,
                                                   ifelse(input$INPUT_archi == "faible",0,
                                                          ifelse(input$INPUT_archi == "moyen",0.5,
                                                                 ifelse(input$INPUT_archi == "fort",1,
                                                                        ifelse(input$INPUT_archi == "except",1,0))))),
            ScoreNegatif_archi_renaturation = ifelse(input$INPUT_archi == "Non",0,
                                                     ifelse(input$INPUT_archi == "faible",0.5,
                                                            ifelse(input$INPUT_archi == "moyen",1,
                                                                   ifelse(input$INPUT_archi == "fort",2,
                                                                          ifelse(input$INPUT_archi == "except",2,0))))),
            ScoreNegatif_archi_PV = ifelse(input$INPUT_archi == "Non",0,
                                           ifelse(input$INPUT_archi == "faible",0,
                                                  ifelse(input$INPUT_archi == "moyen",0.5,
                                                         ifelse(input$INPUT_archi == "fort",2,
                                                                ifelse(input$INPUT_archi == "except",2,0)))))
          ) %>%
          
          
          # pollution du sol ####
        mutate(Score_pollution_residentiel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                      ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                             ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5))),
               Score_pollution_equipement = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                     ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                            ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5))),
               Score_pollution_culturel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                   ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                          ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5))),
               Score_pollution_tertiaire = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                    ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                           ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5))),
               Score_pollution_industriel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                     ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                            ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5))),
               Score_pollution_renaturation = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),1,
                                                       ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                              ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0.5,0.5))),
               Score_pollution_PV = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),1,
                                             ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                    ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0.5,0.5))),
               
               # negatif
               ScoreNegatif_pollution_residentiel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                             ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                    ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),2,0.5))),
               ScoreNegatif_pollution_equipement = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                            ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                   ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5))),
               ScoreNegatif_pollution_culturel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                          ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                 ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5))),
               ScoreNegatif_pollution_tertiaire = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                           ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                  ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5))),
               ScoreNegatif_pollution_industriel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                            ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                   ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5))),
               ScoreNegatif_pollution_renaturation = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                              ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                     ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0.5,0.5))),
               ScoreNegatif_pollution_PV = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                    ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                           ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0.5,0.5)))
               
        ) %>%
          
          # pollution du sol ####
        mutate(Score_pollution_residentiel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                      ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                             ifelse(input$INPUT_pollution %in% c("pollution volatil"),0,
                                                                    ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5)))),
               Score_pollution_equipement =  2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                      ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                             ifelse(input$INPUT_pollution %in% c("pollution volatil"),0,
                                                                    ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5)))),
               Score_pollution_culturel =  2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                    ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                           ifelse(input$INPUT_pollution %in% c("pollution volatil"),0,
                                                                  ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5)))),
               Score_pollution_tertiaire = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                    ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                           ifelse(input$INPUT_pollution %in% c("pollution volatil"),0,
                                                                  ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5)))),
               Score_pollution_industriel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),2,
                                                     ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                            ifelse(input$INPUT_pollution %in% c("pollution volatil"),0,
                                                                   ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0,0.5)))),
               Score_pollution_renaturation = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),1,
                                                       ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                              ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0.5,0.5))),
               Score_pollution_PV = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),1,
                                             ifelse(input$INPUT_pollution %in% c("pollution traitée"),1,
                                                    ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0.5,0.5))),
               
               # negatif
               ScoreNegatif_pollution_residentiel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                             ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                    ifelse(input$INPUT_pollution %in% c("pollution volatil"),2,
                                                                           ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5)))),
               ScoreNegatif_pollution_equipement = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                            ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                   ifelse(input$INPUT_pollution %in% c("pollution volatil"),2,
                                                                          ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5)))),
               ScoreNegatif_pollution_culturel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                          ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                 ifelse(input$INPUT_pollution %in% c("pollution volatil"),2,
                                                                        ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5)))),
               ScoreNegatif_pollution_tertiaire = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                           ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                  ifelse(input$INPUT_pollution %in% c("pollution volatil"),1,
                                                                         ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5)))),
               ScoreNegatif_pollution_industriel = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                            ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                   ifelse(input$INPUT_pollution %in% c("pollution volatil"),1,
                                                                          ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),1,0.5)))),
               ScoreNegatif_pollution_renaturation = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                              ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                                     ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0.5,0.5))),
               ScoreNegatif_pollution_PV = 2*ifelse(input$INPUT_pollution %in% c("pollution inexistante","pollution peu probable"),0,
                                                    ifelse(input$INPUT_pollution %in% c("pollution traitée"),0,
                                                           ifelse(input$INPUT_pollution %in% c("pollution avérée","pollution supposée"),0.5,0.5)))
               
        ) %>%
          
          # risques naturels ####
        mutate(Score_pprmouppri_residentiel = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                     ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               Score_pprmouppri_equipement = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                    ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               Score_pprmouppri_culturel = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                  ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               Score_pprmouppri_tertiaire = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                   ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               Score_pprmouppri_industriel = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                    ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               Score_pprmouppri_renaturation = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                      ifelse(input$INPUT_risquesnaturels == "risque fort",2,1)),
               Score_pprmouppri_PV = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                            ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               
               # négatif
               ScoreNegatif_pprmouppri_residentiel = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                            ifelse(input$INPUT_risquesnaturels == "risque fort",2,1)),
               ScoreNegatif_pprmouppri_equipement = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                           ifelse(input$INPUT_risquesnaturels == "risque fort",2,1)),
               ScoreNegatif_pprmouppri_culturel = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                         ifelse(input$INPUT_risquesnaturels == "risque fort",2,1)),
               ScoreNegatif_pprmouppri_tertaire = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                         ifelse(input$INPUT_risquesnaturels == "risque fort",2,1)),
               ScoreNegatif_pprmouppri_industriel = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                           ifelse(input$INPUT_risquesnaturels == "risque fort",2,1)),
               ScoreNegatif_pprmouppri_renaturation = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                             ifelse(input$INPUT_risquesnaturels == "risque fort",0,0)),
               ScoreNegatif_pprmouppri_PV = ifelse(input$INPUT_risquesnaturels == "hors_risques",0.5,
                                                   ifelse(input$INPUT_risquesnaturels == "risque fort",1,1)),
               
        )     %>%
          
          
          mutate(Score_rajout_residentiel = Score_terrainviabilise_residentiel + Score_especeprotege_residentiel + Score_zonehumide_residentiel + Score_voieeau_residentiel + Score_archi_residentiel + Score_centrebourg_residentiel + Score_pollution_residentiel + Score_pprmouppri_residentiel,
                 ScoreNegatif_rajout_residentiel = ScoreNegatif_terrainviabilise_residentiel + ScoreNegatif_especeprotege_residentiel + ScoreNegatif_zonehumide_residentiel + ScoreNegatif_voieeau_residentiel + ScoreNegatif_archi_residentiel + ScoreNegatif_centrebourg_residentiel + ScoreNegatif_pollution_residentiel + ScoreNegatif_pprmouppri_residentiel,
                 
                 Score_rajout_equipement = Score_terrainviabilise_equipement + Score_especeprotege_equipement + Score_zonehumide_equipement + Score_voieeau_equipement + Score_archi_equipement + Score_centrebourg_equipement + Score_pollution_equipement + Score_pprmouppri_equipement, 
                 ScoreNegatif_rajout_equipement = ScoreNegatif_terrainviabilise_equipement + ScoreNegatif_especeprotege_equipement + ScoreNegatif_zonehumide_equipement + ScoreNegatif_voieeau_equipement + ScoreNegatif_archi_equipement + ScoreNegatif_centrebourg_equipement + ScoreNegatif_pollution_equipement + ScoreNegatif_pprmouppri_equipement,
                 
                 Score_rajout_culturel = Score_terrainviabilise_culturel + Score_especeprotege_culturel + Score_zonehumide_culturel + Score_voieeau_culturel+ Score_archi_culturel + Score_centrebourg_culturel + Score_pollution_culturel + Score_pprmouppri_culturel,
                 ScoreNegatif_rajout_culturel = ScoreNegatif_terrainviabilise_culturel + ScoreNegatif_especeprotege_culturel + ScoreNegatif_zonehumide_culturel + ScoreNegatif_voieeau_culturel + ScoreNegatif_archi_culturel + ScoreNegatif_centrebourg_culturel + ScoreNegatif_pollution_culturel + ScoreNegatif_pprmouppri_culturel,
                 
                 Score_rajout_tertiaire = Score_terrainviabilise_tertiaire + Score_especeprotege_tertiaire + Score_zonehumide_tertiaire + Score_voieeau_tertiaire + Score_archi_tertiaire + Score_centrebourg_tertiaire + Score_pollution_tertiaire + Score_pprmouppri_tertiaire,
                 ScoreNegatif_rajout_tertiaire = ScoreNegatif_terrainviabilise_tertiaire + ScoreNegatif_especeprotege_tertiaire + ScoreNegatif_zonehumide_tertiaire + ScoreNegatif_voieeau_tertiaire + ScoreNegatif_archi_tertiaire + ScoreNegatif_centrebourg_tertiaire + ScoreNegatif_pollution_tertiaire + ScoreNegatif_pprmouppri_tertaire,
                 
                 Score_rajout_industriel = Score_terrainviabilise_industriel + Score_especeprotege_industriel + Score_zonehumide_industriel + Score_voieeau_industriel + Score_archi_industriel + Score_centrebourg_industriel + Score_pollution_industriel + Score_pprmouppri_industriel,
                 ScoreNegatif_rajout_industriel = ScoreNegatif_terrainviabilise_industriel + ScoreNegatif_especeprotege_industriel + ScoreNegatif_zonehumide_industriel + ScoreNegatif_voieeau_industriel + ScoreNegatif_archi_industriel + ScoreNegatif_centrebourg_industriel + ScoreNegatif_pollution_industriel + ScoreNegatif_pprmouppri_industriel,
                 
                 Score_rajout_renaturation = Score_terrainviabilise_renaturation + Score_especeprotege_renaturation + Score_zonehumide_renaturation + Score_voieeau_renaturation + Score_archi_renaturation + Score_centrebourg_renaturation + Score_pollution_renaturation + Score_pprmouppri_renaturation,
                 ScoreNegatif_rajout_renaturation = ScoreNegatif_terrainviabilise_renaturation + ScoreNegatif_especeprotege_renaturation + ScoreNegatif_zonehumide_renaturation + ScoreNegatif_voieeau_renaturation + ScoreNegatif_archi_renaturation + ScoreNegatif_centrebourg_renaturation + ScoreNegatif_pollution_renaturation + ScoreNegatif_pprmouppri_renaturation, 
                 
                 Score_rajout_PV = Score_terrainviabilise_PV + Score_especeprotege_PV + Score_zonehumide_PV + Score_voieeau_PV + Score_archi_PV + Score_centrebourg_PV + Score_pollution_PV + Score_pprmouppri_PV,
                 ScoreNegatif_rajout_PV = ScoreNegatif_terrainviabilise_PV + ScoreNegatif_especeprotege_PV + ScoreNegatif_zonehumide_PV + ScoreNegatif_voieeau_PV + ScoreNegatif_archi_PV + ScoreNegatif_centrebourg_PV + ScoreNegatif_pollution_PV + ScoreNegatif_pprmouppri_PV
          )
        
      }
      
      data_mutatserveur <-  f %>%
        FCT_rajoutmutabilite() %>%
        mutate("Résidentiel" = (ScorePositif_residentiel+Score_rajout_residentiel)/(ScorePositif_residentiel+Score_rajout_residentiel+ScoreNegatif_residentiel+ScoreNegatif_rajout_residentiel),
               "Equipement public" = (ScorePositif_equipement+Score_rajout_equipement)/(ScorePositif_equipement+Score_rajout_equipement+ScoreNegatif_equipement+ScoreNegatif_rajout_equipement),
               # "Culturel" = (ScorePositif_culturel+Score_rajout_culturel)/(ScorePositif_culturel+Score_rajout_culturel+ScoreNegatif_culturel+ScoreNegatif_rajout_culturel),
               "Tertiaire" = (ScorePositif_tertiaire+Score_rajout_tertiaire)/(ScorePositif_tertiaire+Score_rajout_tertiaire+ScoreNegatif_tertiaire+ScoreNegatif_rajout_tertiaire),
               "Industriel" = (ScorePositif_industriel+Score_rajout_industriel)/(ScorePositif_industriel+Score_rajout_industriel+ScoreNegatif_industriel+ScoreNegatif_rajout_industriel),
               "Renaturation" = (ScorePositif_renaturation+Score_rajout_renaturation)/(ScorePositif_renaturation+Score_rajout_renaturation+ScoreNegatif_renaturation+ScoreNegatif_rajout_renaturation),
               "Photovoltaïque au sol" = (ScorePositif_PV+Score_rajout_PV)/(ScorePositif_PV+Score_rajout_PV+ScoreNegatif_PV+ScoreNegatif_rajout_PV)) %>%
        select("Résidentiel","Equipement public",#"Culturel",
               "Tertiaire","Industriel","Renaturation","Photovoltaïque au sol")
      
      # print(data_mutatserveur)
      
      data_mutatserveur_t <- data.frame(
        typo = c("Résidentiel","Equipement public",#"Culturel",
                 "Tertiaire","Industriel","Renaturation","Photovoltaïque au sol"),
        Valeur = 100*round(t(data_mutatserveur[1,]),3)
      ) %>%
        arrange(desc(X1))
      
      gt(data_mutatserveur_t) %>%
        fmt(
          columns = c(X1),
          fns = function(x) ifelse(x == 0, "-", paste0(format(round(x,0), big.mark = " ", decimal.mark = ",", trim = TRUE, scientific = FALSE)," %"))
        ) %>%
        cols_label(
          typo = "Type de reconversion",
          X1 = "Indice de mutabilité"
        ) %>%
        opt_table_font(
          font = "marianne"
        )
      
    })
    
    
    output$Texte_friche3lignes <- renderText({
      
      # test
      # f <- f.xy %>% filter(site_id == "01166_11982")
      # f <- f.xy %>% filter(site_id == "13073_10570")
      
      print(f)
      
      text_statut <- if(is.na(f$site_statut)) {
        ""
      } else if (f$site_statut == "friche sans projet") {
        "Il n'y a aucun projet connu à ce jour sur la friche. "
      } else if(f$site_statut == "friche avec projet"){
        "Un projet de reconversion est à l'étude ou en cours. "
      } else if (f$site_statut == "friche potentielle") {
        "Il s'agit d'un site potentiellement en friche. "
      } else {""}
      
      
      # proprio
      text_proprio <- if (is.na(f$proprio_mutafriches)) {
        ""
      } else if(f$proprio_mutafriches == "Public"){
        "La friche appartient à un propriétaire public. "
      } else if(f$proprio_mutafriches == "Privé"){
        "La friche appartient à un propriétaire privé. "
      } else if (f$proprio_mutafriches == "Mixte public privé") {
        "La friche appartient à des propriétaires publics et privés. "
      }  else {""}
      
      # proprio
      text_zonageabc <- if (is.na(f$zonageabc)) {
        ""
      } else if(f$zonageabc %in% c("Abis","A")){
        " La friche est située dans une commune classée en zone A au titre du zonage ABC."
      } else if(f$zonageabc  %in% c("B1","B2")){
        " La friche est située dans une commune classée en zone B au titre du zonage ABC."
      } else if (f$zonageabc == "C") {
        " La friche est située dans une commune classée en zone C au titre du zonage ABC."
      }  else {""}
      
      
      print("text_centrebourg")
      # localisation
      text_centrebourg <-  if(f$site_centrebourg == "Non" | is.na(f$site_centrebourg)){
        "Elle ne fait pas partie du centre-bourg "
      } else if(f$site_centrebourg == "Oui"){
        "Elle fait partie du centre-bourg "
      } else {""}
      
      text_commerce <- if(f$distancecommerce == "Plus500m" | is.na(f$distancecommerce)){
        "et n'est pas située à proximité de commerces. "
      } else if(f$distancecommerce == "moins500m"){
        "et est située à proximité de commerces. "
      } else {""}
      
      # surface et bâti
      print("text_surface")
      text_surface <- if(f$site_surface < 10000){
        "Sa surface est inférieure à 1 hectare, "
      } else if(f$site_surface < 50000){
        "Sa surface est comprise entre 1 et 5 hectares, "
      } else if (f$site_surface >= 50000) {
        "Sa surface est supérieure à 5 hectares, "
      } else {""}
      
      text_emprisebati <-  if (as.numeric(f$emprise_sol_bati) == 0 | is.na(f$emprise_sol_bati)) {
        "sans bâti. "
      } else if(as.numeric(f$emprise_sol_bati) < 10000){
        "avec une emprise de bâti inférieur à 1ha"
      } else if (as.numeric(f$emprise_sol_bati) >= 10000) {
        "avec une emprise de bâti supérieure à 1ha"
      } else {""}
      
      text_etatbati <- if (is.na(f$bati_etat)) {
        ""
      } else if(f$bati_etat == "dégradation très importante"){
        ", dont l'état est très dégradé. "
      } else if (f$bati_etat == "dégradation inexistante ou faible") {
        ", dont l'état est peu dégradé. "
      } else if (f$bati_etat == "dégradation moyenne") {
        ", dont l'état est moyennement dégradé. "
      } else if (f$emprise_sol_bati == 0) {
        ""
      } else {"."}
      
      
      text_pollution <- if (is.na(f$sol_pollution_existe) | f$sol_pollution_existe == "inconnu"){
        ""
      } else if (f$sol_pollution_existe %in% c("pollution inexistante","pollution peu probable")) {
        "Les sols de la friche ne sont pas polluées (pollution inexistante ou peu probable)."
      }  else if (f$sol_pollution_existe %in% c("pollution traitée")) {
        "La friche a fait l'objet d'une dépollution des sols (pollution traitée)."
      }  else if (f$sol_pollution_existe %in% c("pollution avérée","pollution supposée")) {
        "La friche présente une pollution des sols (pollution avérée ou supposée)."
      }  else {""}
      
      
      # zonage PLU et zone activité
      print("text_zone_activites")
      # text_zone_activites <- if(is.na(f$urba_zone_type)){
      #   " Elle est située dans une zone d'activité."
      # } else if(f$zone_activites == "oui" | f$urba_site_eco == "oui"){
      #   " Elle est située dans une zone d'activité."
      # } else if(f$urba_zone_type %in% c("U")) {
      #   " Elle est située en zonage U."
      # } else if(f$urba_zone_type %in% c("AUc","AUs")) {
      #   " Elle est située en zonage AU."
      # } else if(f$urba_zone_type %in% c("N","Nh")) {
      #   " Elle est située en zonage N."
      # } else if(f$urba_zone_type %in% c("A","Ah")) {
      #   " Elle est située en zonage A."
      # } else {""}
      text_zone_activites <- if(f$zone_activites == "oui"){
        " Elle est située dans une zone d'activité."
      } else if(f$urba_site_ux == "oui"){
        " Elle est située en zonage UX ou UI (zone d'activités économiques)."
      } else if(f$urba_site_ue == "oui"){
        " Elle est située en zonage UE (zone d'équipements publics)."
      } else if(f$urba_site_uabcd == "oui"){
        " Elle est située en zonage UA, UB, UC ou UD (vocation d'habitation)."
      } else if(is.na(f$urba_zone_type)){
        ""
      } else if(f$urba_zone_type %in% c("U")) {
        " Elle est située en zonage U."
      } else if(f$urba_zone_type %in% c("AUc","AUs")) {
        " Elle est située en zonage AU."
      } else if(f$urba_zone_type %in% c("N","Nh")) {
        " Elle est située en zonage N."
      } else if(f$urba_zone_type %in% c("A","Ah")) {
        " Elle est située en zonage A."
      } else {""}
      
      print("text_zonage_enviro")
      text_zonage_enviro <- if(is.na(f$zonage_enviro)){
        "La friche est située"
      } else if(f$zonage_enviro == "hors zone"){
        "La friche est située hors périmètre de ZNIEFF, Natura2000 ou réserve naturelle"
      } else if(str_detect(f$zonage_enviro,"proximite_zone")) {
        "La friche est située à proximité d'un périmètre de ZNIEFF, Natura2000 ou réserve naturelle"
      } else if(f$zonage_enviro == "natura_2000") {
        "La friche est située dans un périmètre de Natura2000"
      } else if(f$zonage_enviro == "znieff") {
        "La friche est située dans un périmètre de ZNIEFF"
      } else {""}
      
      print("text_tvb")
      text_tvb <- if(is.na(f$tramevb_2)) {
        ""
      } else if(f$tramevb_2 == "hors_tvb"){
        " et hors trame verte et bleue connue. "
      } else if(f$tramevb_2 == "tvb") {
        " au sein d'une trame verte et bleue connue. "
      } else {""}
      
      # risques
      print("risques")
      text_risque_enviro <- if(is.na(f$pprmouppri)) {
        ""
      } else if(f$pprmouppri == "hors_risques"){
        "elle n'est pas soumise à un risque naturel"
      } else if(f$pprmouppri == "pprm_ou_ppri") {
        "elle est soumis à un risque naturel"
      } else {""}
      
      text_risque_techno <- if(is.na(f$pprt_2)) {
        ""
      } else if(f$pprt_2 == "hors_pprt"){
        " et n'est pas soumise à un risque technologique. "
      } else if(f$pprt_2 == "pprt") {
        " et soumise à un risque technologique. "
      } else {""}
      
      # desserte
      print("desserte")
      text_desserte_route <- if(is.na(f$desserte_distance_route)) {
        ""
      } else if(as.numeric(f$desserte_distance_route) < 1){
        "elle est localisée à moins d'1km d'un échangeur autoroutier, "
      } else if(as.numeric(f$desserte_distance_route) < 5){
        "elle est localisée à moins de 5km d'un échangeur autoroutier, "
      } else {"elle est à plus de 5km d'un échangeur autoroutier, "}
      
      text_desserte_ferroviaire <- if(is.na(f$desserte_distance_ferroviaire)) {
        ""
      } else if(as.numeric(f$desserte_distance_ferroviaire < 1)){
        "à moins d'1km d'une gare"
      } else {"à plus d'1km d'une gare"}
      
      text_desserte_ite <- if(is.na(f$distance_ite)) {
        ". "
      } else if(f$distance_ite == "Moins1kmITEbon"){
        " et à moins d'1km d'une Installation Terminale Embranchée fret en bon état. "
      } else if(f$distance_ite == "Moins1kmITEmauvais"){
        " et à moins d'1km d'une Installation Terminale Embranchée fret en mauvais état. "
      } else if(f$distance_ite == "RAS"){
        " et à moins d'1km d'une Installation Terminale Embranchée fret en mauvais état. "
      } else {"."}
      
      # print("text_posteelect")
      # text_posteelect <- if (f$distanceposteelect != "moins1km" | is.na(f$distanceposteelect)){
      #   "et à plus d'1km d'un point de raccordement BT/HT. "
      # } else if(f$distanceposteelect == "moins1km"){
      #   "et à moins d'1km d'un point de raccordement BT/HT. "
      # } else {""} 
      
      print("text_zaer")
      text_zaer <- if(is.na(f$site_zaer)) {
        ""
      } else if(f$site_zaer == "oui"){
        "La friche est située dans une Zone d'Accélération d'Energie Renouvelable."
      } else {""}
      
      print("text_pogeis")
      text_pogeis <- if(is.na(f$PGE_SurfTot_QualiEnvTot)) {
        ""
      }  else if(f$PGE_SurfTot_QualiEnvTot == "FORT"){
        "La friche possède un fort potentiel de gain écologique."
      } else {""}
      
      HTML(
        paste0(text_statut,text_proprio,
               # text_centrebourg,text_commerce,
               "\n",text_surface,text_emprisebati,text_etatbati,text_pollution,
               text_zone_activites, text_zonageabc,
               "\n",text_zonage_enviro,text_tvb,text_pogeis,
               "\n","Concernant l'exposition aux risques, ",text_risque_enviro,text_risque_techno,
               "\n","Concernant l'accessibilité aux transports, ",text_desserte_route, text_desserte_ferroviaire, text_desserte_ite,#text_posteelect,
               text_zaer)
      )
      
      
    })
    
    # On affiche la popup associée au site
    isolate({
      proxy %>%
        show_info(id = id)
    })
  })
  
  # mymap_shape_click (Clic sur surface) ----
  observeEvent(input$mymap_shape_click, {
    event <- input$mymap_shape_click
    if(is.null(event)) return()
    message(">> Polygone cliqué d'id ", event$id)
    isolate({proxy %>% show_info(id = event$id)}) # On affiche la popup associée au site
  })
  
  # mymap_marker_mouseover ----
  observeEvent(input$mymap_marker_mouseover, {
    event <- input$mymap_marker_mouseover
    event_id <- event$id
    if(grepl('address', event_id)) return()
    message(">> Marqueur survolé d'id ", event_id)
    if(grepl("reg|dep", event_id) & !grepl("label", event_id)) {
      res <- get_polygons_from_id(event_id)
      f <- res$f
      proxy %>% add_contour(f, label = NULL, group = "ContoursMouseOver")
    } 
  })
  
  # r_coords2() (Zoom vers l'adresse choisie dans la seconde barre d'adresse) ----
  observe({
    req(r_coords2())
    message(">> r_coords2 a changé")
    
    # Coordonnées du point où aller
    message(">> zoom_to")
    res <- r_coords2()
    proxy %>% zoom_to(res$coords,
                      res$type,
                      res$value)
  })
  
  
  # > OUTPUT ----
  
  # filtres ----
  output$filtres <- renderUI({
    
    req(input$mymap_zoom)
    
    if(input$mymap_zoom <= ZOOM_LEVELS["Département"]) return()
    
    # Afficher le nombre de filtres activés
    if(all(is.na(rv_filtres$value))) {
      n_filtres <- 0
    } else {
      n_filtres <- rv_filtres$value %>% length
    }
    
    if(n_filtres == 0) {
      txt <- "Filtrer"
    } else {
      txt <- glue("Filtrer ({n_filtres})")
    }
    
    # Bloc final
    tags$p(actionLink("filtrer", 
                      txt,
                      icon = icon("filter")),
           style="text-align:center;font-size:1em")
  })
  
  # ui_pave ----
  output$ui_pave <- renderUI({
    
    get_ui_encart(titre    = "Cartofriches", 
                  titreSuite = ", un inventaire national des friches qui s'appuie sur la connaissance locale", 
                  body       = includeMarkdown("www/textes/accueil.md"),
                  isMobile   = input$isMobile)
    
  })
  
  # ui_apropos_cartofriches ----
  output$ui_apropos_cartofriches <- renderUI({
    get_ui_apropos_cartofriches()
  })
  
  output$ui_publier_une_friche <- renderUI({
    div(
      h1("Monter son observatoire local"),
      
      includeMarkdown("www/textes/publier_une_friche.md")
      
    )
  })
  
  output$ui_publier_une_friche_2 <- renderUI({
    div(
      includeMarkdown("www/textes/publier_une_friche_2.md")
    )
  })
  
  
  # zoom ----
  # Affichage du niveau de zoom de la carte
  # notamment, du nombre de zooms restant avant l'affichage des marqueurs
  output$zoom <- renderUI({
    req(input$mymap_zoom <= ZOOM_LEVELS[["Département"]])
    tagList(tags$hr(), 
            get_txt_zoom(input$mymap_zoom))
  })
  
  # ui_legende : légende de la carte avec le nombre de friches ----
  # Légende de la carte
  output$ui_legende <- renderUI({
    
    req(input$mymap_zoom)
    req(input$mymap_bounds)
    
    if(is.null(input$chk_all)) {
      chk_all <- FALSE
    } else {
      chk_all <- input$chk_all
    }
    
    f <- f.xy %>% get_objects_bounds(input$mymap_bounds)
    
    stats <- get_n_friches(f)
    get_ui_legende(stats, 
                   chk_all = chk_all, 
                   popup = FALSE)
    
  })
  
  # ui_bandeau ----
  # output$ui_bandeau <- renderUI({
  #   get_bandeau1(titre      = "Cartofriches", 
  #                sous_titre = "Trouvez les friches près de chez vous", 
  #                mobile     = input$isMobile,
  #                height     = "400px")
  # })
  
  # mymap ----
  output$mymap <- renderLeaflet({
    
    m <- leaflet(options = leafletOptions(zoomControl = TRUE, 
                                          minZoom = 0, 
                                          maxZoom = 18)) %>%
      
      # Tiles
      add_tiles() %>%
      
      
      # Boutons
      addEasyButton(
        easyButton(
          icon=icon("fullscreen", lib = "glyphicon"),
          title="Zoom France entière",
          onClick=JS(sprintf("function(btn, mymap){ mymap.flyToBounds([[%f, %f],[%f, %f]]); }", bb_ini[2], bb_ini[1], bb_ini[4], bb_ini[3])))) %>%
      
      # Layers Control Friches
      addLayersControl(baseGroups    = baseGroups,
                       overlayGroups = overlayGroups,
                       options       = layersControlOptions(collapsed = TRUE)) %>%
      
      # Show or Hide
      # showGroup("Ortho IGN") %>%
      hideGroup("Parcelles IGN")
    
    # Zoom
    # if(!is.null(r_coords1())) {
    #   coords <- r_coords1()$coords
    #   value  <- r_coords1()$value
    #   type   <- r_coords1()$type
    #   label  <- r_coords1()$label
    #   proxy %>% zoom_to(coords, type, value, label)
    # } else {
    m <- m %>% zoom_to(coords_fr, type = NULL, value = NULL)
    # }
    
    # Site ou BBOX
    if(!is.null(r_bbox())) {
      message(">> Aller vers une BBOX")
      m <- m %>% zoom_to_coords(r_bbox(), type = "site")
    }
    
    m
  })
  
  # leafletProxy ----
  proxy <- leafletProxy('mymap', session)
  
  
  observeEvent(input$see_home, {
    updateNavbarPage(session, "app_navbar", selected = "Accueil")
  })
  
  
  
  
  # Téléchargement des données 
  output$downloadZIP <- downloadHandler(
    filename <- "Donnees_Cartofriches.zip",
    content <- function(file) {
      file.copy("www/data_export/Donnees_Cartofriches.zip", file)
    },
    contentType = "application/zip"
  )
  
  
  # Téléchargement du kit Contribuez 
  output$downloadZIP_kit <- downloadHandler(
    filename <- "Kit_Cartofriches.zip",
    content <- function(file) {
      file.copy("www/Kit_Cartofriches.zip", file)
    },
    contentType = "application/zip"
  )
  
  
  
  # waiter ----
  waiter_hide() # on ferme la fenêtre d'attente
  
  
  
  
  
  
  # > ONGLET STATS ----
  
  REACT_DataTableau <- reactive({
    
    # print(as.numeric(input$INPUT_FILTRE_SURFACE_TABLEAU_slider[1]))
    # print(as.numeric(input$INPUT_FILTRE_SURFACE_TABLEAU_slider[2]))
    # print(as.numeric(input$INPUT_FILTRE_Site_type))
    
    Dataframe <- f.xy %>%
      mutate(urba_zone_type_regroup = ifelse(urba_zone_type %in% c("A","Ah"),"A",
                                             ifelse(urba_zone_type %in% c("AUs","AUc"),"AU",
                                                    ifelse(urba_zone_type %in% c("N","Nh"),"N",
                                                           ifelse(urba_zone_type %in% c("U"),"U",
                                                                  ifelse(is.na(urba_zone_type)|urba_zone_type %in% c("99"),"Autre",
                                                                         ifelse(urba_zone_type %in% c("ZC","ZCa"),"CC-ZC",
                                                                                ifelse(urba_zone_type %in% c("ZnC"),"CC-ZnC",
                                                                                       "Autre")))))))) %>%
      mutate(site_type_regroup = ifelse(site_type %notin% c("friche agro-industrielle","friche commerciale","friche d'équipement public","friche d'habitat",
                                                            "friche enseignement","friche ferroviaire","friche hospitalière","friche industrielle","friche logistique",
                                                            "friche loisir tourisme hôtellerie","friche militaire","mixte","inconnu","autre"), "autre",site_type)
      ) %>%
      mutate(site_surface = round(as.numeric(site_surface)/10000,2)) %>%
      mutate(site_surface_tri = ifelse(site_surface > 49, 50, site_surface)) %>%
      filter(dep %in% input$INPUT_ChoixDep) %>%
      filter(site_statut %in% input$INPUT_Site_statut) %>%
      filter(site_surface_tri >= as.numeric(input$INPUT_FILTRE_SURFACE_TABLEAU_slider[1]),
             site_surface_tri <= as.numeric(input$INPUT_FILTRE_SURFACE_TABLEAU_slider[2])) %>%
      filter(nature %in% input$INPUT_FILTRE_Producteur) %>%
      filter(site_type_regroup %in% input$INPUT_FILTRE_Site_type) %>%
      filter(urba_zone_type_regroup %in% input$INPUT_FILTRE_ZONEURBA_TABLEAU)
    
    Dataframe <- Dataframe %>% st_set_geometry(NULL) %>%
      select(site_id,site_nom,nom_prodcartofriches,site_statut,site_type,urba_zone_type_regroup,site_surface,
             dep,comm_nom,comm_insee, Long, Lat) %>%
      mutate(Commune = paste0(comm_nom, " (",comm_insee,")")) %>%
      mutate(LienCartofriches2 = sprintf('<a href="%s" target="_blank">Lien direct du site vers Cartofriches</a>',
                                         paste0("https:cartofriches.cerema.fr/cartofriches/?site=",site_id)),
             LienCartofriches3 = paste0(site_id, " - ",
                                        sprintf(paste0('<a href="%s" target="_blank">Voir sur la carte</a>'), #, " ",icon("eye")
                                                paste0("/?site=",site_id))
             )) %>%
      mutate(nom_court =  ifelse(nom_prodcartofriches ==  "Appel à projet Fonds Friches" , 
                                 paste0(str_split(site_nom," - ", simplify = TRUE)[,1], " - ",str_split(site_nom," - ", simplify = TRUE)[,6]), 
                                 site_nom)) %>%
      mutate(urba_zone_type_regroup = ifelse(urba_zone_type_regroup == "U","U - zone urbaine",
                                             ifelse(urba_zone_type_regroup == "AU","AU - zone à urbaniser",
                                                    ifelse(urba_zone_type_regroup == "A","A - zone agricole",
                                                           ifelse(urba_zone_type_regroup == "N","N - zone naturelle","Hors PLU/PLUi"))))) %>%
      select(LienCartofriches3,dep,Commune,nom_court,nom_prodcartofriches,site_statut,site_type,urba_zone_type_regroup,site_surface, Long, Lat) %>%
      rename(
        "Nom du site" = nom_court,
        # "Département" = dep,
        "Producteur de la donnée"  = nom_prodcartofriches,
        "Statut" = site_statut,
        "Type" = site_type,
        "Zonage d'urbanisme" = urba_zone_type_regroup,
        "Surface (en Ha)" = site_surface,
        "Identifiant et lien Cartofriches" = LienCartofriches3)
    
    Dataframe[is.na(Dataframe)] <- "Non renseigné"
    
    
    Dataframe$dep <- fct_recode(Dataframe$dep,
                                "01 - Ain"="01","02 - Aisne"="02","03 - Allier"="03","04 - Alpes de Haute-Provence"="04","05 - Hautes-Alpes"="05",
                                "06 - Alpes-Maritimes"="06","07 - Ardêche"="07","08 - Ardennes"="08","09 - Ariège"="09","10 - Aube"="10",
                                "11 - Aude"="11","12 - Aveyron"="12","13 - Bouches-du-Rhône"="13","14 - Calvados"="14","15 - Cantal"="15",
                                "16 - Charente"="16","17 - Charente-Maritime"="17","18 - Cher"="18","19 - Corrèze"="19",
                                "2A - Corse-du-Sud"="2A","2B - Haute-Corse"="2B",
                                "21 - Côte-d'Or"="21","22 - Côtes d'Armor"="22","23 - Creuse"="23","24 - Dordogne"="24","25 - Doubs"="25",
                                "26 - Drôme"="26","27 - Eure"="27","28 - Eure-et-Loir"="28","29 - Finistère"="29","30 - Gard"="30",
                                "31 - Haute-Garonne"="31","32 - Gers"="32","33 - Gironde"="33","34 - Hérault"="34","35 - Île-et-Vilaine"="35",
                                "36 - Indre"="36","37 - Indre-et-Loire"="37","38 - Isère"="38","39 - Jura"="39","40 - Landes"="40",
                                "41 - Loir-et-Cher"="41","42 - Loire"="42","43 - Haute-Loire"="43","44 - Loire-Atlantique"="44","45 - Loiret"="45",
                                "46 - Lot"="46","47 - Lot-et-Garonne"="47","48 - Lozère"="48","49 - Maine-et-Loire"="49","50 - Manche"="50",
                                "51 - Marne"="51","52 - Haute-Marne"="52","53 - Mayenne"="53","54 - Meurthe-et-Moselle"="54","55 - Meuse"="55",
                                "56 - Morbihan"="56","57 - Moselle"="57","58 - Nièvre"="58","59 - Nord"="59","60 - Oise"="60",
                                "61 - Orne"="61","62 - Pas-de-Calais"="62","63 - Puy-de-Dôme"="63","64 - Pyrénées-Atlantique"="64","65 - Haute-Pyrénées"="65",
                                "66 - Pyrénées-Orientales"="66","67 - Bas-Rhin"="67","68 - Haut-Rhin"="68","69 - Rhône"="69","70 - Haute-Saône"="70",
                                "71 - Saône-et-Loire"="71","72 - Sarthe"="72","73 - Savoie"="73","74 - Haute-Savoie"="74","75 - Paris"="75",
                                "76 - Seine-Maritime"="76","77 - Seine-et-Marne"="77","78 - Yvelines"="78","79 - Deux-Sèvres"="79","80 - Somme"="80",
                                "81 - Tarn"="81","82 - Tarn-et-Garone"="82","83 - Var"="83","84 - Vaucluse"="84","85 - Vendée"="85",
                                "86 - Vienne"="86","87 - Haute-Vienne"="87","88 - Vosges"="88","89 - Yonne"="89","90 - Territoire-de-Belfort"="90",
                                "91 - Essonne"="91","92 - Hauts-de-Seine"="92","93 - Seine-Saint-Denis"="93","94 - Val-de-Marne"="94","95 - Val-dOise"="95",
                                "971 - Guadeloupe"="971","972 - Martinique"="972","973 - Guyane"="973","974 - La Réunion"="974","976 - Mayotte"="976"
    )
    
    res <- Dataframe %>% arrange(dep) %>% rename("Département" = dep) %>%
      mutate(Long = as.numeric(ifelse(is.na(Long),0,Long)),
             Lat = as.numeric(ifelse(is.na(Lat),0,Lat)))
    
    # Longitude, Latitude
    Long <- round(res$Long, 5)
    Lat <- round(res$Lat, 5)
    res[["Coordonnées GPS (Longitude,Latitude)"]] <- paste(Long, Lat, sep = ",")
    res <- res %>% select(-Long, -Lat)
    
    return(res)
    
  })
  
  
  output$tableau <- DT::renderDataTable({
    REACT_DataTableau()
  },
  escape = FALSE,
  rownames = FALSE,
  options = list(pageLength = 20,
                 language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'))
  )
  
  
  output$downloadData_Tableau <- downloadHandler(
    filename = function() {
      paste("Cartofriches_export_filtres", ".csv", sep="")
    },
    content = function(file) {
      write.csv(REACT_DataTableau(), file)
    }
  )
  
  
  
  
  
  
  # ONGLET STATS. IMPACTS ####
  # # Initialiser les données au démarrage
  # data_stats_impact <- reactive({
  # 
  #     list(
  #       ca = 950000,
  #       benefices = 95000,
  #       marge = 12.5,
  #       commandes = 3200,
  #       clients = 1150,
  #       satisfaction = 92.3
  #     )
  # 
  # })
  
  # Indicateurs d'usage ####
  output$stats_nombre_friches <- renderText({
    paste0(format(data_stats_usage$nombre_friches, big.mark = " "), "")
  })
  
  output$stats_nombres_obslocaux <- renderText({
    paste0(format(data_stats_usage$nombres_obslocaux, big.mark = " "), "")
  })
  
  output$stats_visites_site <- renderText({
    paste0(format(data_stats_usage$visites_site, big.mark = " "), "")
  })
  
  output$stats_telechargement_datagouv <- renderText({
    paste0(format(data_stats_usage$telechargement_datagouv, big.mark = " "), "")
  })
  
  output$stats_appel_API <- renderText({
    paste0(format(data_stats_usage$appel_API, big.mark = " "), "")
  })
  
  output$stats_enrichissementdata <- renderText({
    paste0(data_stats_usage$enrichissementdata, " ")
  })
  
  
  # Indicateurs d'impacts ####
  output$stats_superficie_couverte <- renderText({
    paste0(data_stats_impacts$superficie_couverte, " %")
  })
  
  output$stats_gaintemps_montage <- renderText({
    format(data_stats_impacts$gaintemps_montage, big.mark = " ")
  })
  
  output$stats_gaintemps_qualification <- renderText({
    format(data_stats_impacts$gaintemps_qualification, big.mark = " ")
  })
  
  output$stats_contacts_uv <- renderText({
    format(data_stats_impacts$contacts_uv, big.mark = " ")
  })
  
  output$stats_reut_outils <- renderText({
    paste0(data_stats_impacts$reut_outils, " ")
  })
  
  output$stats_utilisation_OHF <- renderText({
    paste0(data_stats_impacts$utilisation_OHF, " ")
  })
  
  output$stats_visu_public <- renderText({
    format(data_stats_impacts$visu_public, big.mark = " ")
  })
  
  
  # > ONGLET FONDS VERT ----
  
  
  # output$ui_fondsvert <- renderUI({
  #   
  #   h4("en cours")
  # 
  #   
  # })
  
  modFondsfriches_server("fonds_friches")
  
  
  
  
}