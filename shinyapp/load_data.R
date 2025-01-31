load_data <- function() {
  
  ## LECTURE DE LA DATA ############################################################
  
  # > FRICHES ----
  # f.xy <<- readRDS("data/friches/f.xy.rds")
  # f.tup <<- readRDS("data/friches/f.tup.rds")
  f.xy <<- readRDS("data/friches/f.xy.rds") %>% # st_set_crs(2154) %>% st_transform(4326) %>% 
    # mutate(Long = st_coordinates(.)[, 1], Lat = st_coordinates(.)[, 2]) %>%
    mutate(Long = long,
           Lat = lat,
           site_numero = site_id,
           bati_surface = as.numeric(bati_surface),
           desserte_distance_ferroviaire = as.numeric(desserte_distance_ferroviaire))  
  
  f.xy <- f.xy %>%
    mutate(site_type = ifelse(site_type == "agro-industrielle", "friche agro-industrielle",
                              ifelse(site_type == "friche hospitaliere", "friche hospitalière",site_type))) %>% 
    mutate(nom_prodcartofriches = ifelse(nom_prodcartofriches == "urban vitaliz", "UrbanVitaliz",nom_prodcartofriches),
           source_nom = ifelse(source_nom == "urban vitaliz", "UrbanVitaliz",source_nom)) %>%
    filter(pk != '28015',
           nom_prodcartofriches != "DDT de la Marne")
  
  f.tup <<- readRDS("data/friches/f.tup.rds") %>% # st_set_crs(2154) %>% st_transform(4326) %>%
    mutate(nom_prodcartofriches = ifelse(nom_prodcartofriches == "urban vitaliz", "UrbanVitaliz",nom_prodcartofriches),
           source_nom = ifelse(source_nom == "urban vitaliz", "UrbanVitaliz",source_nom)) %>%
    mutate(site_numero = site_id) %>%
    filter(nom_prodcartofriches != "DDT de la Marne")
  
  # LAYER IDS
  f.xy$layerId      <- paste0("friche_xy_", f.xy$site_id)
  f.tup$layerId     <- paste0("friche_tup_", f.tup$site_id)
  
  Surface_max <<- max(f.xy$unite_fonciere_surface, na.rm = TRUE)
  
  # > STATS ---
  # Affichage des stats régionales
  # sous la forme de cercles 
  # au lancement de l'application
  # regs.pts  <<- readRDS("data/stats/regs.pts.rds")
  # deps.pts  <<- readRDS("data/stats/deps.pts.rds")
  regs.pts  <<- readRDS("data/stats/regs.pts_MAJNico.rds")
  deps.pts  <<- readRDS("data/stats/deps.pts_MAJNico.rds")
  regs.pts$layerId  <- glue("stat_reg_{regs.pts$code}")
  deps.pts$layerId  <- glue("stat_dep_{deps.pts$code}")
  
  
  # > LISTE ----
  # On construit une liste avec les différents éléments
  Data <<- list(points = f.xy,
               polygons = f.tup,
               stats = list(regs = regs.pts,
                            deps = deps.pts))
  
  # > CONTOURS ----
  deps  <<- readRDS("data/contours/deps.rds")
  regs  <<- readRDS("data/contours/regs.rds")
  comms <<- readRDS("data/contours/comms.rds")
  
  # EMPRISES ####
  emprises <<- readRDS("data/contours/emprises.rds")
}



  

