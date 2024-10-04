suppressMessages(library(tidyverse))
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(bslib))
suppressMessages(library(plotly))
suppressMessages(library(sf))
suppressMessages(library(glue))
suppressMessages(library(janitor))
suppressMessages(library(DT))


# HELPERS ----

# load("../shinyapp/data/fondsfriches.rds")
# fr_g_fdfriche_r <- fr_g_fdfriche_r %>%
#   st_drop_geometry() %>%
#   select(libreg,num_demarche,laureat_fv,num_dossier,
#          loc_abc,logique_intervention,intervention,mo_requal,bilan,deficit,nat_friche,site_area,friche_area)
# depense_t_amenagement_r <- depense_t_amenagement_r %>%
#   select(num_dossier,daa_ht,dab_ht,dac_ht,dad_ht,daf_ht,dag1_ht,dag2_ht,dag3_ht,dag4_ht,dag5_ht,dag6_ht,dag7_ht)
# save(fr_g_fdfriche_r,
#      depense_t_amenagement_r,
#      liste_editions,liste_regions,
#      file = "../shinyapp/data/fondsfriches_light.rds")
load("data/fondsfriches_light.rds")

# regions_rappro <- st_read('data/contours/regions_rapprochees.shp') %>%
#   left_join(regs.pts , by = "reg")
# regions_rappro_centroide <- st_read('data/contours/regions_rapprochees_centroide.shp') %>%
#   left_join(regs.pts , by = "reg")
# 
# regions_rappro_centroide$Long[14]=-4.01
# regions_rappro_centroide$Lat[14]=41.76818
# 
# regions_rappro_centroide$Long[15]=-2.439187
# regions_rappro_centroide$Lat[15]=41.89234
# 
# regions_rappro_centroide$Long[16]=-1.165385
# regions_rappro_centroide$Lat[16]=41.89574
# 
# regions_rappro_centroide$Long[17]=0.2303484
# regions_rappro_centroide$Lat[17]= 41.94279
# 
# regions_rappro_centroide$Long[18]=1.253103
# regions_rappro_centroide$Lat[18]=41.97768
# 
# library(BAMMtools)
# quantileNum = 5
# probs <- seq(0, 1, length.out = quantileNum + 1)
# bins_brute <- quantile(regions_rappro$nb_friches, probs, na.rm = TRUE, names = FALSE)
# bins <- round(bins_brute/10,0)*10
# bins[1] <- 0
# bins[6] <- bins[6]+10
# 
# pal <- colorBin("Greens", bins = bins)









GRAPH <- 1

liste_rapports <- list(
  "Dépenses moyennes" = list("fn" = "depenses_moyennes", "type" = "graph"),
  "Logique d'intervention" = list("fn" = "logique_intervention", "type" = "graph"),
  "Zonage Pinel" = list("fn" = "zonage_pinel", "type" = "graph"),
  "Typologie des friches" = list("fn" = "typologie_friches", "type" = "graph"),
  "Répartition des porteurs de projets" = list("fn" = "repartition_porteurs", "type" = "graph"),
  "Taille des projets" = list("fn" = "taille_projets", "type" = "graph"),
  "Part en friche dans les projets" = list("fn" = "part_friche_projets", "type" = "graph"),
  "Les porteurs de projets par nature de friche" = list("fn" = "porteurs_nature_friche", "type" = "graph"),
  "Les filières principales par la nature de friche" = list("fn" = "filieres_nature_friche", "type" = "graph"),
  "Répartition des porteurs de projets par filière de recyclage" = list("fn" = "repartition_porteurs_recyclage", "type" = "graph"),
  "Répartition des déficits déclarés par les candidats" = list("fn" = "repartition_deficits", "type" = "graph")
)


simplifier_liste <- function(liste) {
  nouvelle_liste <- list()
  for (nom in names(liste)) {
    nouvelle_liste[[nom]] <- liste[[nom]]$fn
  }
  return(nouvelle_liste)
}

liste_rapports_simplifiees <- simplifier_liste(liste_rapports)

filtre_data <- function(choix_region, choix_edition, laureats) {
  data <- fr_g_fdfriche_r %>%
    filter(
      libreg %in% choix_region,
      num_demarche %in% choix_edition,
      laureat_fv %in% laureats
    )

  data
}

table_stats <- function(data, ..., pc = TRUE, total = TRUE) {
  res <- data %>%
    st_drop_geometry() %>%
    tabyl(..., show_na = TRUE)

  if (total) {
    res <- res %>% adorn_totals(c("col", "row"))
  }

  if (pc) {
    res <- res %>%
      adorn_percentages("col") %>%
      adorn_pct_formatting(digits = 0) %>%
      adorn_ns()
  }

  res
}




classer_friches <- function(data) {
  data %>%
    mutate(
      grp_friche = case_when(
        str_detect(nat_friche, "Friche urbaine|Friche commerciale") ~ "Urbaine",
        str_detect(nat_friche, "industrielle|minière") ~ "Friche industrielle ou minière",
        str_detect(nat_friche, "militaire|administrative|hospitalière|portuaire|aéroportuaire|routière|ferroviaire") ~ "Autre",
        str_detect(nat_friche, "Autre| \\(Préciser\\)") ~ "Autre",
        is.na(nat_friche) ~ "Inconnue",
        nat_friche == "0" ~ "Inconnue",
        TRUE ~ "Autre"
      )
    )
}

classer_friches_2 <- function(data) {
  data %>%
    mutate(
      # on extrait tout le texte avant la premiere virgule ou la fin de la chaine
      # et si ca continue avec "friche" ou "autre" sinon on prend tout
      grp_friche = str_extract(nat_friche, "^(.*?)(?=, Friche|,Friche|, autre|$)"),
      grp_friche = gsub("\\t", "", grp_friche)
    ) %>%
    mutate(
      grp_friche = case_when(
        str_detect(grp_friche, "utre") ~ "Autre",
        str_detect(nat_friche, "dégradé") ~ "Friche urbaine - Îlots anciens dégradés",
        is.na(nat_friche) ~ "Inconnue",
        nat_friche == "0" ~ "Inconnue",
        TRUE ~ grp_friche
      )
    )
}

classer_tailles <- function(data) {
  data %>%
    mutate(taille = case_when(
      site_area < 1 ~ "< 1 ha",
      site_area >= 1 & site_area < 9 ~ "1 à 9 ha",
      site_area >= 9 & site_area < 25 ~ "9 à 25 ha",
      site_area >= 25 ~ "> 25 ha",
      TRUE ~ "Inconnu"
    )) %>%
    mutate(
      taille = factor(taille, levels = c("< 1 ha", "1 à 9 ha", "9 à 25 ha", "> 25 ha", "Inconnu"))
    )
}

classer_part_friches <- function(data) {
  data %>%
    mutate(
      part = case_when(
        100 * friche_area / site_area < 25 ~ "< 25 %",
        100 * friche_area / site_area >= 25 & 100 * friche_area / site_area < 50 ~ "25 à 50 %",
        100 * friche_area / site_area >= 50 & 100 * friche_area / site_area < 75 ~ "50 à 75 %",
        100 * friche_area / site_area >= 75 ~ "75 % à 100%",
        is.na(100 * friche_area / site_area) ~ "Inconnu"
      )
    ) %>%
    mutate(part = factor(part, levels = c("Inconnu", "< 25 %", "25 à 50 %", "50 à 75 %", "75 % à 100%")))
}

classer_moa <- function(data) {
  data %>%
    mutate(mo_requal = case_when(
      str_detect(mo_requal, "Collectivité") ~ "Collectivité locale",
      str_detect(mo_requal, "ailleur") ~ "Bailleur social",
      str_detect(mo_requal, "EPA") ~ "Étalissement Public d'Aménagment \n (EPA)",
      str_detect(mo_requal, "EPF") ~ "Étalissement Public Foncier \n (EPF)",
      str_detect(mo_requal, "SEM") ~ "SEM/SPL",
      str_detect(mo_requal, "Autre") ~ "Autre",
      is.na(mo_requal) ~ "Inconnu",
      TRUE ~ mo_requal
    ))
}

classer_logique_intervention <- function(data) {
  data %>%
    mutate(logique_intervention = case_when(
      str_detect(logique_intervention, "Remettre en") ~ "Remettre",
      TRUE ~ logique_intervention
    ))
}

classer_deficits <- function(data) {
  data %>%
    mutate(deficit = case_when(
      deficit < 500000 ~ "< 500 k€",
      deficit <= 1000000 ~ "[500k€ - 1M€]",
      deficit <= 5000000 ~ "[1M€ - 5M€]",
      deficit > 5000000 ~ "> 5M€",
      TRUE ~ "Inconnu"
    )) %>%
    mutate(deficit = factor(deficit, levels = c("< 500 k€", "[500k€ - 1M€]", "[1M€ - 5M€]", "> 5M€", "Inconnu")))
}

###################################################################### ""
# GENERATES OUTPUTS ----


ajout_caption <- function(data) {
  markdown(glue("_Source : traitements Cerema 2023 à partir de démarches simplifiées sur {nrow(data)} dossiers_"))
}

depenses_moyennes <- function(data) {
  data %>%
    st_drop_geometry() %>%
    left_join(depense_t_amenagement_r, ., by = "num_dossier") %>%
    group_by(loc_abc) %>%
    summarize(
      acquisition = mean(daa_ht / ifelse(site_area == 0, 1, site_area), na.rm = T),
      études = mean(dab_ht / ifelse(site_area == 0, 1, site_area), na.rm = T),
      travaux = mean(dac_ht / ifelse(site_area == 0, 1, site_area), na.rm = T),
      "contributions et participations" = mean(dad_ht / ifelse(site_area == 0, 1, site_area), na.rm = T),
      "honoraires MOA" = mean(daf_ht / ifelse(site_area == 0, 1, site_area), na.rm = T),
      "frais financiers" = mean(dag1_ht / ifelse(site_area == 0, 1, site_area), na.rm = T),
      "frais de communicadtion" = mean(dag2_ht / ifelse(site_area == 0, 1, site_area), na.rm = T),
      "gestion foncière et immobilière" = mean(dag3_ht / ifelse(site_area == 0, 1, site_area), na.rm = T),
      assurances = mean(dag4_ht / ifelse(site_area == 0, 1, site_area), na.rm = T),
      marges = mean(dag5_ht / ifelse(site_area == 0, 1, site_area), na.rm = T),
      "provisions pour aléas" = mean(dag6_ht / ifelse(site_area == 0, 1, site_area), na.rm = T),
      "autres dépenses" = mean(dag7_ht / ifelse(site_area == 0, 1, site_area), na.rm = T)
    ) %>%
    pivot_longer(-loc_abc, names_to = "Postes", values_to = "Dépenses") %>%
    drop_na()
}

logique_intervention <- function(data) {
  # table_stats(data, intervention)
  data %>%
    st_drop_geometry() %>%
    classer_logique_intervention() %>% # Test Nico
    group_by(intervention) %>%
    summarise(n = n()) %>%
    ungroup()
}

zonage_pinel <- function(data) {
  data %>%
    st_drop_geometry() %>%
    group_by(loc_abc, intervention) %>%
    summarise(n = n()) %>%
    ungroup()
}

typologie_friches <- function(data) {
  data %>%
    st_drop_geometry() %>%
    classer_friches_2() %>%
    group_by(grp_friche) %>%
    summarise(n = n()) %>%
    ungroup()
}


repartition_porteurs <- function(data) {
  data %>%
    st_drop_geometry() %>%
    classer_moa() %>%
    group_by(mo_requal) %>%
    summarise(n = n()) %>%
    ungroup()
}


taille_projets <- function(data) {
  data_bilan <- data %>%
    st_drop_geometry() %>%
    mutate(
      bilan = case_when(
        is.na(bilan) ~ "Inconnu",
        bilan == "0" ~ "Inconnu",
        TRUE ~ bilan
      )
    ) %>%
    classer_tailles() %>%
    group_by(bilan, taille) %>%
    summarise(n = n()) %>%
    ungroup()

  data_total <- data %>%
    st_drop_geometry() %>%
    classer_tailles() %>%
    group_by(bilan = "Total", taille) %>%
    summarise(n = n()) %>%
    ungroup()

  bind_rows(data_bilan, data_total)
}

part_friche_projets <- function(data) {
  data %>%
    st_drop_geometry() %>%
    classer_part_friches() %>%
    group_by(part) %>%
    summarise(n = n()) %>%
    ungroup()
}

porteurs_nature_friche <- function(data) {
  data %>%
    st_drop_geometry() %>%
    classer_moa() %>%
    classer_friches_2() %>%
    group_by(mo_requal, grp_friche) %>%
    summarise(n = n()) %>%
    group_by(mo_requal) %>%
    mutate(pc = n / sum(n, na.rm = TRUE)) %>%
    ungroup()
}

filieres_nature_friche <- function(data) {
  data %>%
    st_drop_geometry() %>%
    classer_friches_2() %>%
    group_by(grp_friche, intervention) %>%
    summarise(n = n()) %>%
    group_by(intervention) %>%
    mutate(pc = n / sum(n, na.rm = TRUE)) %>%
    ungroup()
}


repartition_porteurs_recyclage <- function(data) {
  data %>%
    st_drop_geometry() %>%
    classer_moa() %>%
    group_by(mo_requal, intervention) %>%
    summarise(n = n()) %>%
    group_by(mo_requal) %>%
    mutate(pc = n / sum(n, na.rm = TRUE)) %>%
    ungroup()
}

repartition_deficits <- function(data) {
  data %>%
    st_drop_geometry() %>%
    classer_deficits() %>%
    group_by(deficit) %>%
    summarise(n = n()) %>%
    ungroup()
}




# UI ----

modFondsfriches_ui <- function(id) {
  ns <- NS(id)

  column(10,
    offset = 1,
    tagList(
      h1("Analyse des données Fonds Friches / Fonds Vert"),
      tags$br(),
      
      fluidRow(
        layout_column_wrap(
          pickerInput(ns("choix_region"),
            "Région :",
            choices = liste_regions,
            options = list(
              `live-search` = TRUE,
              `actions-box` = TRUE,
              `deselect-all-text` = "Tout déselectionner",
              `select-all-text` = "Tout sélectionner",
              `none-selected-text` = "Aucune région sélectionnée",
              `selected-text-format` = "count > 2",
              `count-selected-text` = "{0} regions sélectionnées"
            ),
            multiple = TRUE,
            selected = liste_regions
          ),
          pickerInput(ns("laureats"),
            "Lauréat :",
            choices = c("Oui", "Non"),
            options = list(
              # `live-search` = TRUE,
              `actions-box` = TRUE,
              `select-all-text` = "Tous",
              `deselect-all-text` = "Aucun",
              `none-selected-text` = "Aucun statut sélectionné",
              `selected-text-format` = "count > 1",
              `count-selected-text` = "Tout statuts"
            ), # nolint: indentation_linter.
            multiple = TRUE,
            selected = c("Oui", "Non")
          ),
          pickerInput(ns("choix_edition"),
            "Édition :",
            choices = liste_editions,
            # choices = c(`1` = "1ère édition",
            #             `2` = "2ème édition",
            #             `3` = "3ème édition",
            #             `4` = "4ème édition"),
            options = list(
              # `live-search` = TRUE,
              `actions-box` = TRUE,
              `select-all-text` = "Toutes",
              `deselect-all-text` = "Aucune",
              `none-selected-text` = "Aucune édition sélectionnée",
              `selected-text-format` = "count > 3",
              `count-selected-text` = "Toutes les éditions sélectionnées"
            ),
            multiple = TRUE,
            selected = liste_editions
          ),
          pickerInput(ns("Rapports_a_afficher"),
            "Rapports à afficher :",
            choices = liste_rapports_simplifiees,
            options = list(
              # `live-search` = TRUE,
              `actions-box` = TRUE,
              `select-all-text` = "Tous",
              `deselect-all-text` = "Aucun",
              `none-selected-text` = "Aucun type sélectionné",
              `selected-text-format` = "count > 14",
              `count-selected-text` = "Tous les types sélectionnés"
            ),
            multiple = FALSE,
            selected = liste_rapports_simplifiees[GRAPH]
          )
        ),
        hr(),
        div(
          uiOutput(ns("rapport")),
          align = "center"
        )
      )
    )
  )
}


# SERVER ----

modFondsfriches_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    gen_ui <- function(data, id) {
      type <- liste_rapports[[which(sapply(liste_rapports, function(x) x$fn) == id)]]$type

      tagList(
        {
          if (type == "graph") {
            plotlyOutput(ns(glue("{id}_ui")), width = "80%", height = "600px")
          } else if (type == "tableau") {
            dataTableOutput(ns(glue("{id}_ui")))
          }
        },
        br(),
        ajout_caption(data)
      )
    }

    data_filtree <- reactive({
      filtre_data(
        choix_region = input$choix_region,
        choix_edition = input$choix_edition,
        laureats = input$laureats
      )
    })

    data_a_afficher <- reactive({
      do.call(input$Rapports_a_afficher, list(data_filtree()))
    })

    output$rapport <- renderUI({
      gen_ui(data_filtree(), input$Rapports_a_afficher)
    })

    output$test1 <- renderPrint({
      gen_ui(data_filtree(), input$Rapports_a_afficher)
      # data_a_afficher()
    })
    output$test2 <- renderPrint({
      # input$Rapports_a_afficher
      data_a_afficher()
    })

    
    output$depenses_moyennes_ui <- renderPlotly({
      
      data_a_afficher() %>%
        # calcul du text a afficher sur le graph
        group_by(loc_abc) %>%
        mutate(text = sum(Dépenses, na.rm = TRUE)) %>%
        plot_ly(
          x = ~loc_abc,
          y = ~Dépenses,
          text = ~ scales::number(text),
          color = ~ fct_reorder(Postes, `Dépenses`, sum, .desc = TRUE), # ~Postes,
          textposition = "outside",
          type = "bar"
        ) %>%
        layout(
          title = "Postes de dépenses moyennes à l'hectare",
          xaxis = list(title = ""),
          yaxis = list(title = "Dépenses"),
          barmode = "stack",
          showlegend = TRUE,
          #legend = list(yanchor = "middle", y = 0.5)
          legend = list(orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        traceorder = "normal")
        ) %>%
        config(displaylogo = FALSE)
    })

    output$logique_intervention_ui <- renderPlotly({
      data_a_afficher() %>%
        # calcul du text a afficher sur le graph
        group_by(intervention) %>%
        mutate(text = sum(n, na.rm = TRUE)) %>%
        plot_ly(
          x = ~n,
          y = ~ fct_reorder(intervention, n, sum),
          text = ~ scales::number(text),
          color = ~intervention,
          textposition = "outside",
          type = "bar"
        ) %>%
        layout(
          title = "La place dans la filière des projets candidats",
          xaxis = list(title = "Nombre de projets"),
          yaxis = list(title = ""),
          barmode = "dodge",
          showlegend = FALSE,
          legend = list(orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        traceorder = "normal")
        ) %>%
        config(displaylogo = FALSE)
    })

    output$zonage_pinel_ui <- renderPlotly({
      data_a_afficher() %>%
        # calcul du text a afficher sur le graph
        group_by(loc_abc, intervention) %>%
        mutate(text = sum(n, na.rm = TRUE)) %>%
        plot_ly(
          x = ~loc_abc,
          y = ~n,
          text = ~ scales::number(text),
          color = ~intervention,
          textposition = "outside",
          type = "bar"
        ) %>%
        layout(
          title = "Nombre de candidats par zone de tension du marché locatif",
          xaxis = list(title = ""),
          yaxis = list(title = "Nombre de projets"),
          barmode = "dodge",
          showlegend = TRUE,
          legend = list(orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        traceorder = "normal")
        ) %>%
        config(displaylogo = FALSE)
    })

    output$typologie_friches_ui <- renderPlotly({
      data_a_afficher() %>%
        # calcul du text a afficher sur le graph
        group_by(grp_friche) %>%
        mutate(text = sum(n, na.rm = TRUE)) %>%
        plot_ly(
          x = ~n,
          y = ~ fct_reorder(grp_friche, n, sum),
          text = ~ scales::number(text),
          color = ~grp_friche,
          textposition = "outside",
          type = "bar"
        ) %>%
        layout(
          title = "Typologie des friches",
          xaxis = list(title = "Nombre de projets"),
          yaxis = list(title = ""),
          barmode = "dodge",
          showlegend = FALSE,
          legend = list(orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        traceorder = "normal")
        ) %>%
        config(displaylogo = FALSE)
    })

    output$repartition_porteurs_ui <- renderPlotly({
      data_a_afficher() %>%
        # calcul du text a afficher sur le graph
        group_by(mo_requal) %>%
        mutate(text = sum(n, na.rm = TRUE)) %>%
        plot_ly(
          x = ~n,
          y = ~ fct_reorder(mo_requal, n, sum),
          text = ~ scales::number(text),
          color = ~mo_requal,
          textposition = "outside",
          type = "bar"
        ) %>%
        layout(
          title = "Typologie des porteurs",
          xaxis = list(title = "Nombre de projets"),
          yaxis = list(title = ""),
          barmode = "dodge",
          showlegend = FALSE,
          legend = list(orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        traceorder = "normal")
        ) %>%
        config(displaylogo = FALSE)
    })

    output$taille_projets_ui <- renderPlotly({
      data_a_afficher() %>%
        # calcul du text a afficher sur le graph
        group_by(bilan, taille) %>%
        mutate(text = sum(n, na.rm = TRUE)) %>%
        plot_ly(
          x = ~bilan,
          y = ~n,
          text = ~ scales::number(text),
          color = ~taille,
          colors = "Reds",
          textposition = "outside",
          type = "bar"
        ) %>%
        layout(
          title = "Taille des projets",
          xaxis = list(title = ""),
          yaxis = list(title = "Nombre de projets"),
          barmode = "dodge",
          showlegend = TRUE,
          legend = list(orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        traceorder = "normal")
        ) %>%
        config(displaylogo = FALSE)
    })

    output$part_friche_projets_ui <- renderPlotly({
      data_a_afficher() %>%
        # calcul du text a afficher sur le graph
        group_by(part) %>%
        mutate(text = sum(n, na.rm = TRUE)) %>%
        plot_ly(
          x = ~part,
          y = ~n,
          text = ~ scales::number(text),
          color = "Blue",
          textposition = "outside",
          type = "bar"
        ) %>%
        layout(
          title = "Part en friche dans les projets",
          xaxis = list(title = ""),
          yaxis = list(title = "Nombre de projets"),
          barmode = "dodge",
          showlegend = FALSE,
          legend = list(orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        traceorder = "normal")
        ) %>%
        config(displaylogo = FALSE)
    })

    output$porteurs_nature_friche_ui <- renderPlotly({
      data_a_afficher() %>%
        # calcul du text a afficher sur le graph
        group_by(mo_requal, grp_friche) %>%
        mutate(text = sum(n, na.rm = TRUE)) %>%
        plot_ly(
          x = ~pc,
          y = ~mo_requal,
          text = ~ scales::number(text),
          color = ~grp_friche,
          textposition = "auto",
          type = "bar",
          textfont = list(color = "white")
        ) %>%
        layout(
          title = "Les porteurs de projets par nature de friche",
          xaxis = list(title = "", tickformat = ".0%"),
          yaxis = list(title = ""),
          barmode = "stack",
          showlegend = TRUE,
          legend = list(orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        traceorder = "normal")
        ) %>%
        config(displaylogo = FALSE)
    })

    output$filieres_nature_friche_ui <- renderPlotly({
      data_a_afficher() %>%
        # calcul du text a afficher sur le graph
        group_by(grp_friche, intervention) %>%
        mutate(text = sum(n, na.rm = TRUE)) %>%
        plot_ly(
          x = ~pc,
          y = ~intervention,
          text = ~ scales::number(text),
          color = ~grp_friche,
          textposition = "auto",
          type = "bar",
          textfont = list(color = "white")
        ) %>%
        layout(
          title = "Les filières principales par la nature de friche",
          xaxis = list(title = "", tickformat = ".0%"),
          yaxis = list(title = ""),
          barmode = "stack",
          showlegend = TRUE,
          legend = list(orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        traceorder = "normal")
        ) %>%
        config(displaylogo = FALSE)
    })

    output$repartition_porteurs_recyclage_ui <- renderPlotly({
      data_a_afficher() %>%
        # calcul du text a afficher sur le graph
        group_by(mo_requal, intervention) %>%
        mutate(text = sum(n, na.rm = TRUE)) %>%
        plot_ly(
          x = ~pc,
          y = ~mo_requal,
          text = ~ scales::number(text),
          color = ~intervention,
          textposition = "auto",
          type = "bar",
          textfont = list(color = "white")
        ) %>%
        layout(
          title = "Les porteurs de projets par filière de recyclage",
          xaxis = list(title = "", tickformat = ".0%"),
          yaxis = list(title = ""),
          barmode = "stack",
          showlegend = TRUE,
          legend = list(orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        traceorder = "normal")
        ) %>%
        config(displaylogo = FALSE)
    })

    output$repartition_deficits_ui <- renderPlotly({
      data_a_afficher() %>%
        # calcul du text a afficher sur le graph
        group_by(deficit) %>%
        mutate(text = sum(n, na.rm = TRUE)) %>%
        plot_ly(
          x = ~deficit,
          y = ~n,
          text = ~ scales::number(text),
          color = "Blue",
          textposition = "outside",
          type = "bar"
        ) %>%
        layout(
          title = "Répartition des déficits déclarés par les candidats",
          xaxis = list(title = ""),
          yaxis = list(title = "Nombre de projets"),
          barmode = "dodge",
          showlegend = FALSE,
          legend = list(orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        traceorder = "normal")
        ) %>%
        config(displaylogo = FALSE)
    })
  })
}
