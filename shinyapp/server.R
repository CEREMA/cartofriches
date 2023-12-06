server <- function(input, output, session) {
  
  load_data()   # on charge les données
  
  
  # > MODULES ----
  
  # Modules pour rechercher des adresses
  # retourne un reactive contenant les coordonnées XY
  # ou la BBOX xmin, ymin, xmax, ymax de là où aller
  r_coords1 <- mod_ban("search1", session)
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
    
    # OFF
    # Toutes les friches (y compris celles non vérifiées) ?
    # if(is.null(input$chk_all)) {
    #   sf_points <- sf_points %>% filter(checked)
    # } else {
    #   if(!input$chk_all) {
    #     sf_points <- sf_points %>% filter(checked)
    #   }
    # }
    
    # Choix de friches
    choices <- rv_filtres$value
    if(!is.na(choices)) {
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
  
  # OFF
  # # r_closest_friche (Friche la plus proche) ----
  # r_closest_friche <- reactive({
  #   message(">> r_closest_friche()")
  #   req(input$mymap_bounds)
  #   req(!is.null(input$chk_all))
  #   req(r_friches())
  #   
  #   bb <- input$mymap_bounds
  #   
  #   # f <- Data$points %>% get_objects_bounds(bb)
  #   f <- r_friches()
  #   
  #   f_in_bounds <- f %>% get_objects_bounds(bb)
  #   
  #   if(nrow(f_in_bounds) > 0) return()
  #   
  #   # Coordonnées du point central
  #   coords <- c(mean(bb$west, bb$east), mean(bb$south, bb$north))
  #   
  #   # On cherche les friches les plus proches
  #   res <- find_closest_friche(coords = coords, 
  #                              f      = f)
  #   
  #   return(res)
  # })
  
  
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
  
  # lnk_filtre ----
  observeEvent(input$lnk_filtre, {
    
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
  
  # Aller vers un site : on active l'onglet "Rechercher une friche" ----
  # La carte est calculée dans output$mymap, et on y ajuste la BBOX
  observe({
    req(r_bbox())
    updateNavbarPage(session, "app_navbar", selected = "Rechercher une friche")
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
    updateNavbarPage(session, "app_navbar", selected = "Rechercher une friche")
  })
  
  # btn_chercher ----
  observeEvent(input$btn_chercher, {
    updateNavbarPage(session, "app_navbar", selected = "Rechercher une friche")
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
  
  # OFF
  # # Affichage du bouton "Aller vers la friche la plus proche" ----
  # observe({
  #   res <- r_closest_friche()
  #   if(is.null(res)) {
  #     proxy %>% clearControls()
  #     return()
  #   }
  #   
  #   distance_txt <- res$distance_txt
  #   
  #   f <- Data$points %>% filter(site_id == res$site_id)
  #   
  #   type_friche <- case_when(f$is_mte ~ "mte", 
  #                     f$is_observatoire ~ "observatoire",
  #                     f$is_aap ~ "aap",
  #                     f$is_user ~ "user",
  #                     f$is_mte_non_expertise ~ "mte_non_expertise",
  #                     f$is_ademe ~ "ademe",
  #                     f$is_mte_pv ~  "mte_pv")
  #   
  #   type_friche <- case_when(
  #     f$is_mte ~ "Donnée nationale",
  #     f$is_observatoire ~ "Donnée locale",
  #     f$is_aap ~ "Appel à projets",
  #     f$is_mte_pv ~ "Potentiel solaire au sol",
  #     f$is_user ~ "Retour utilisateur",
  #     f$is_mte_non_expertise ~ "Site industriel non vérifié",
  #   )
  #   
  #   type_friche <- strwrap(type_friche, width = 20) %>% paste(collapse=br_code)
  #   
  #   ui <- actionButton("btn_see_friche", 
  #                      tagList(icon("paper-plane"),
  #                              glue("Friche à {distance_txt}"),
  #                              HTML(br_code),
  #                              tags$span(HTML(glue(("{type_friche}"))), 
  #                              style="font-size: 0.9em;
  #                                     color: #ffcec1;
  #                                     margin-left: 20px;
  #                                     text-align: left;
  #                                     display: block;
  #                                     margin-top: 5px;"),
  #                              ),
  #                      class = "goto_map"
  #                      )
  #   proxy %>% clearControls() %>% addControl(ui)
  # })
  
  # OFF
  # # btn_see_friche ----
  # observeEvent(input$btn_see_friche, {
  #   res <- r_closest_friche()
  #   coords <- res$coords
  #   proxy %>% flyTo(coords[1], coords[2], 18)
  # })
  
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
    message(">> Observe : affichage des objets")
    
    # Si pas de résultat, alors pas de friche à l'endroit souhaité
    # on enlève alors les marqueurs de sites et les unités foncières affichées précédemment
    
    # REGIONS 
    if(names(r_data()) == "regs") {
      message(">> Affichage des cercles régionaux")
      
      f <- r_data()$regs
      
      proxy %>%
        clearGroup("Basias et Basol") %>%
        clearGroup("Unités foncières") %>%
        clearGroup("stat_dep") %>%
        add_circles(f, group = "stat_reg")
      message(">> Fin - Affichage des cercles régionaux")
      
    # DEPARTEMENTS
    } else if(names(r_data()) == "deps") {
      message(">> Affichage des cercles stats départementaux")
      proxy %>%
        clearGroup("Basias et Basol") %>% 
        clearGroup("Unités foncières") %>% 
        clearGroup("stat_reg") %>% 
        add_circles(r_data()$deps, group = "stat_dep")
     
    # FRICHES 
    } else if(names(r_data()) == "points") {
      message(">> Affichage des points")
      proxy %>% clearGroup("Basias et Basol") %>% clearGroup("Unités foncières")
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
  
  # ui_filtres ----
  output$ui_filtres <- renderUI({
    
    req(input$mymap_zoom)
    
    if(input$mymap_zoom <= ZOOM_LEVELS["Département"]) return()
    
    # Afficher le nombre de filtres activés
    if(is.na(rv_filtres$value)) {
      n_filtres <- 0
    } else {
      n_filtres <- rv_filtres$value %>% length
    }
    
    if(n_filtres == 0) {
      label <- "Filtrer"
    } else {
      label <- glue("Filtrer ({n_filtres})")
    }
    
    # Bloc final
    fluidRow(
      column(8, offset=2,
        tags$p(actionLink("lnk_filtre", label, icon=icon("filter")),
               style="text-align:center;font-size:1em"),
        tags$p(checkboxInput("chk_all", 
                             "Afficher les sites non vérifiés", 
                             value = FALSE))))
  })
  
  # ui_pave ----
  output$ui_pave <- renderUI({
    
    get_ui_encart(titre    = "Cartofriches", 
                titreSuite = ", un inventaire national des friches qui s'appuie sur la connaissance locale", 
                body       = includeMarkdown("www/textes/accueil.md"),
                image      = "encart.jpg",
                isMobile   = input$isMobile)
    
  })
  
  # ui_apropos_cartofriches ----
  output$ui_apropos_cartofriches <- renderUI({
    get_ui_apropos_cartofriches()
  })
  
  # ui_txt_zoom ----
  # Affichage du niveau de zoom de la carte
  # notamment, du nombre de zooms restant avant l'affichage des marqueurs
  output$ui_txt_zoom <- renderUI({
    req(input$mymap_zoom <= ZOOM_LEVELS[["Département"]])
    tagList(tags$hr(), 
            get_txt_zoom(input$mymap_zoom))
  })
  
  # ui_legende : légende de la carte avec le nombre de friches ----
  # Légende de la carte
  output$ui_legende <- renderUI({
    
    req(input$mymap_zoom)
    req(input$mymap_bounds)
    
    # if(input$mymap_zoom <= ZOOM_LEVELS[["Département"]]) return()
    
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
  output$ui_bandeau <- renderUI({
    get_bandeau1(titre      = "Cartofriches", 
                 sous_titre = "Trouvez les friches près de chez vous", 
                 mobile     = input$isMobile,
                 height     = "400px")
  })
  
  # mymap ----
  output$mymap <- renderLeaflet({
    
    m <- leaflet(options = leafletOptions(zoomControl = TRUE, 
                                          minZoom = 0, 
                                          maxZoom = 19)) %>%
      
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
      showGroup("Ortho IGN") %>%
      hideGroup("Parcelles IGN")
    
    # Zoom
    if(!is.null(r_coords1())) {
      coords <- r_coords1()$coords
      value  <- r_coords1()$value
      type   <- r_coords1()$type
      label  <- r_coords1()$label
      proxy %>% zoom_to(coords, type, value, label)
    } else {
      m <- m %>% zoom_to(coords_fr, type = NULL, value = NULL)
    }
    
    # Site ou BBOX
    if(!is.null(r_bbox())) {
      message(">> Aller vers une BBOX")
      m <- m %>% zoom_to_coords(r_bbox(), type = "site")
    }
    
    m
  })
  
  # leafletProxy ----
  proxy <- leafletProxy('mymap', session)
  
  # waiter ----
  waiter_hide() # on ferme la fenêtre d'attente
}