load_data <- function() {
  
  ## LECTURE DE LA DATA ############################################################
  
  # > FRICHES ----
  
  cf_mutafriches <- read.csv2("data/friches/cf_mutafriches.csv")
  echangeurs <-  read.csv2("data/friches/echangeurs_oct25.csv", sep = ",") %>% mutate(distance = round(as.numeric(distance)/1000,1))
  head(echangeurs)
  
  # f.xy <<- readRDS("data/friches/f.xy.rds")
  # f.tup <<- readRDS("data/friches/f.tup.rds")
  f.xy <<- readRDS("data/friches/f.xy.rds") %>% # st_set_crs(2154) %>% st_transform(4326) %>% 
    # mutate(Long = st_coordinates(.)[, 1], Lat = st_coordinates(.)[, 2]) %>%
    mutate(Long = long,
           Lat = lat,
           site_numero = site_id,
           bati_surface = as.numeric(bati_surface),
           desserte_distance_ferroviaire = as.numeric(desserte_distance_ferroviaire))  %>% 
    mutate(CES = round(as.numeric(emprise_sol_bati) / as.numeric(site_surface),3)) %>%
    left_join(echangeurs, by = "site_id") %>%
    mutate(desserte_distance_route = distance) %>% select(-distance,-row_number)
  
  f.xy <- f.xy %>%
    mutate(site_type = ifelse(site_type == "agro-industrielle", "friche agro-industrielle",
                              ifelse(site_type == "friche hospitaliere", "friche hospitalière",site_type))) %>% 
    mutate(nom_prodcartofriches = ifelse(nom_prodcartofriches == "urban vitaliz", "UrbanVitaliz",nom_prodcartofriches),
           source_nom = ifelse(source_nom == "urban vitaliz", "UrbanVitaliz",source_nom)) %>%
    # filter(#pk != '28015',
    #   nom_prodcartofriches != "DDT de la Marne")  %>%
    left_join(cf_mutafriches, by = "site_id")
  
  
  f.xy %>% st_drop_geometry() %>%
    filter(site_statut%in% c("friche avec projet","friche sans projet") & site_type != "friche carrière ou mine") %>%
    summarise(nbre_friches = n(),
              surface = sum(site_surface) / 10000
    )
  
  # f.xy %>% st_drop_geometry() %>%
  #   filter(site_statut != "friche potentielle") %>%
  #   filter(dep %in% c('09','11','12','30','31','32','34','46','48','65','66','81','82')) %>%
  #   # select(dep,nature,site_statut,source_producteur) %>%
  #   # group_by(nature) %>%
  #   group_by(site_type) %>%
  #   summarise(nbre_friches = n(),
  #             friche_reconvertie =sum(site_statut == "friche reconvertie"),
  #             friche_avec_projet = sum(site_statut == "friche avec projet"),
  #             friche_sans_projet = sum(site_statut == "friche sans projet"),
  #             )
  
  
  # f.xy %>% filter(nature == "observatoire local" | site_statut %in% c("friche sans projet", "friche avec projet")) %>% nrow()
  
  # test 
  # sapply(f.xy, function(x) sum(is.na(x)))
  
  
  # table(f.xy$nature)
  # head(f.xy$desserte_distance_route)
  # 
  # f.xy %>% as.data.frame() %>%
  #   filter(nature == "observatoire local") %>%
  #   summarise(somme = sum(site_surface, na.rm = TRUE)/10000)  
  # 
  # f.xy %>% as.data.frame() %>%
  #   filter(site_statut == "friche potentielle") %>%
  #   summarise(somme = sum(site_surface, na.rm = TRUE)/10000)  
  # 
  # f.xy %>% as.data.frame() %>%
  #   filter(#P_industriel > 0.6,
  #          site_statut == "friche avec projet",
  #          nature == "observatoire local") %>%
  #   summarise(nombre = n(),
  #             somme = sum(site_surface, na.rm = TRUE)/10000) 
  # 
  # 
  # f.xy %>% as.data.frame() %>%
  #   filter(site_statut == "friche sans projet") %>%
  #   summarise(nombre = n(),
  #             somme = sum(site_surface, na.rm = TRUE)/10000) 
  # 
  # 
  # # Stats pour juin 2025
  # # 
  # #   Export oour Magali avant congés juin 2025
  # #   st_write(f.xy %>% select(-long,-lat),
  # #            glue("../shinyapp/data/friches/exportmagali_juin2025",".gpkg"), delete_dsn = T)
  # #
  # # table(f.xy$P_industriel)
  # hist(f.xy$P_industriel)
  # hist(f.xy$P_residentiel)
  # hist(f.xy$P_PV)
  # hist(f.xy$P_renaturation)
  # # 
  # # 
  f.xy %>% filter(site_statut == "friche sans projet") %>% nrow()
  tot <- f.xy %>% filter(site_statut == "friche sans projet")
  sum(tot$site_surface)/10000
  
  f.xy %>% filter(site_statut == "friche sans projet", P_industriel>0.6) %>% nrow()
  indus <- f.xy %>% filter(site_statut == "friche sans projet", P_industriel>0.6)
  sum(indus$site_surface)/10000
  f.xy %>% filter(site_statut == "friche sans projet", P_industriel>0.6) %>% nrow() / f.xy %>% filter(site_statut == "friche sans projet") %>% nrow()  * 100
  
  f.xy %>% filter(site_statut == "friche sans projet", P_residentiel>0.6) %>% nrow()
  res <- f.xy %>% filter(site_statut == "friche sans projet", P_residentiel>0.6)
  sum(res$site_surface)/10000
  f.xy %>% filter(site_statut == "friche sans projet", P_residentiel>0.6) %>% nrow() / f.xy %>% filter(site_statut == "friche sans projet") %>% nrow()  * 100
  
  f.xy %>% filter(site_statut == "friche sans projet", P_renaturation>0.6) %>% nrow()
  f.xy %>% filter(site_statut == "friche sans projet", P_renaturation>0.6) %>% nrow() / f.xy %>% filter(site_statut == "friche sans projet") %>% nrow()  * 100
  
  f.xy %>% filter(site_statut == "friche sans projet", P_PV>0.6) %>% nrow()
  pv <- f.xy %>% filter(site_statut == "friche sans projet", P_PV>0.6)
  sum(pv$site_surface)/10000
  f.xy %>% filter(site_statut == "friche sans projet", P_PV>0.6) %>% nrow() / f.xy %>% filter(site_statut == "friche sans projet") %>% nrow() * 100
  
  
  ggplot(f.xy) +
    geom_point(aes(x = P_industriel, y = P_residentiel))
  
  ggplot(f.xy) +
    geom_point(aes(x = P_industriel, y = P_PV))
  
  ggplot(f.xy) +
    geom_point(aes(x = P_renaturation, y = P_PV))
  
  
  f.tup <<- readRDS("data/friches/f.tup.rds") %>% # st_set_crs(2154) %>% st_transform(4326) %>%
    mutate(nom_prodcartofriches = ifelse(nom_prodcartofriches == "urban vitaliz", "UrbanVitaliz",nom_prodcartofriches),
           source_nom = ifelse(source_nom == "urban vitaliz", "UrbanVitaliz",source_nom)) %>%
    mutate(site_numero = site_id) %>%
    filter(nom_prodcartofriches != "DDT de la Marne") %>% 
    mutate(CES = round(as.numeric(emprise_sol_bati) / as.numeric(site_surface),3)) %>%
    left_join(cf_mutafriches, by = "site_id")
  
  
  # LAYER IDS
  f.xy$layerId      <- paste0("friche_xy_", f.xy$site_id)
  f.tup$layerId     <- paste0("friche_tup_", f.tup$site_id)
  
  Surface_max <<- max(f.xy$unite_fonciere_surface, na.rm = TRUE)
  
  # > STATS ---
  # Affichage des stats régionales
  # sous la forme de cercles 
  # au lancement de l'application
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





