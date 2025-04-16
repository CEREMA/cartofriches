server <- function(input, output, session) {
  
  load_data()   # on charge les données
  
  
  
  
  # Va directement sur l'onglet "Contribuer"
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if("contribuer" %in% names(query)){
      updateNavbarPage(session, "app_navbar", selected = "Contribuer")
    }
  })
  
  
  
  
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
    
    # Toutes les friches (y compris celles potentielles) ?
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
    
  #   print(rv_filtres$surface_min)
  #   
  #   if(!is.na(rv_filtres$surface_min)) {
  #   sf_points <- sf_points %>%
  #     filter(unite_fonciere_surface > rv_filtres$surface_min)
  # }
      
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

    # f <- Data$points %>% get_objects_bounds(bb)
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
  
  # observe({
  #     rv_filtres$surface_min <- input$INPUT_FILTRE_SURFACE[1]
  #     rv_filtres$surface_max <- input$INPUT_FILTRE_SURFACE[2]
  # })
  
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
                image      = "encart.jpg",
                isMobile   = input$isMobile)
    
  })
  
  # ui_apropos_cartofriches ----
  output$ui_apropos_cartofriches <- renderUI({
    get_ui_apropos_cartofriches()
  })
  
  output$ui_publier_une_friche <- renderUI({
    div(
      h1("Cartofriches"),
      
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
                                                                  ifelse(is.na(urba_zone_type),"CC",urba_zone_type)))))) %>%
      mutate(site_surface = round(as.numeric(site_surface)/10000,2)) %>%
      mutate(site_surface_tri = ifelse(site_surface > 49, 50, site_surface)) %>%
      filter(dep %in% input$INPUT_ChoixDep) %>%
      filter(site_statut %in% input$INPUT_Site_statut) %>%
      filter(site_surface_tri >= as.numeric(input$INPUT_FILTRE_SURFACE_TABLEAU_slider[1]),
             site_surface_tri <= as.numeric(input$INPUT_FILTRE_SURFACE_TABLEAU_slider[2])) %>%
      filter(nature %in% input$INPUT_FILTRE_Producteur) %>%
      filter(site_type %in% input$INPUT_FILTRE_Site_type) %>%
      filter(urba_zone_type_regroup %in% input$INPUT_FILTRE_ZONEURBA_TABLEAU)
  
  Dataframe <- Dataframe %>% st_set_geometry(NULL) %>%
    select(site_id,site_nom,nom_prodcartofriches,site_statut,site_type,site_surface,
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
    select(LienCartofriches3,dep,Commune,nom_court,nom_prodcartofriches,site_statut,site_type,site_surface, Long, Lat) %>%
    rename(
      "Nom du site" = nom_court,
      # "Département" = dep,
      "Producteur de la donnée"  = nom_prodcartofriches,
      "Statut" = site_statut,
      "Type" = site_type,
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
  
  res <- Dataframe %>% arrange(dep) %>% rename("Département" = dep)
  
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
  
  
  
  
  
  
  
  
  
}