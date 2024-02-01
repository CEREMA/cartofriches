mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

ui <- 
  
  # NAVBARPAGE ####
  navbarPage(
    id = "app_navbar",
    windowTitle = "Cartofriches",
    collapsible = TRUE,
    title = tagList(
      
      # WAITER ----
      # Utilisation de waiter pour l'écran d'attente
      # nécessaire pour activer waiter. waiter_hide() utilisé dans server.R
      use_waiter(), 
      waiterPreloader(html = tagList(
        includeHTML(path = "www/spinner/spinner.html")
      ), color = transparent(1)),
      
      # SHINY DISCONNECT ----
      disconnectMessage(
        text = "Votre session a pris fin",
        refresh = "Cliquez ici pour vous reconnecter",
        background = "#EF7757",
        colour = "#FFFFFF",
        refreshColour = "#337AB7",
        overlayColour = "#EF7757",
        overlayOpacity = 1,
        width = "full",
        top = "center",
        size = 24,
        css = "padding: 15px !important; box-shadow: none !important;"
      ),
      # actionButton('disconnect', 'Disconnect the app'),
      
      # LOGO, .... ----
      get_logo(appli = "Cartofriches"),
      get_css(),
      tags$head(
        # tags$title("Rapport de mobilité du Cerema"),
        get_favicon(),
        get_matomo() # // Matomo
      )
    ),
    position="static-top",
    
    # __ONGLET HOME ####
    tabPanel(" ",
             title = "Accueil",
             # icon("home", lib = "glyphicon"),
             icon = icon("home", lib = "glyphicon"),
             
             # DETECTION MOBILE ----
             mobileDetect('isMobile'),
             
              
             # BANDEAU DU HAUT 1 ----
             uiOutput("ui_bandeau"),
             
             # PAVES ----
             uiOutput("ui_pave"),
             
             # BOUTONS ----
             fluidRow(
               column(10,
                      offset=1,
                      fluidRow(
                        get_bandeau2(titre = "Valoriser vos friches", 
                                     sous_titre = "et rejoignez la communauté")
                 ),
                 tags$br(),
                 tags$br(),
                 
                fluidRow(
                  column(4, div(get_bouton("button1", "Publier une friche", url = 'https://artificialisation.biodiversitetousvivants.fr/cartofriches/contribuer'), style="text-align:center;margin-top:5px;")),
                  column(4, div(get_bouton("button2", "Voir les contributeurs", url = "https://artificialisation.biodiversitetousvivants.fr/cartofriches/observatoires-locaux"), style="text-align:center;margin-top:5px;")),
                  column(4, div(get_bouton("button3", "Découvrir le Cerema", url = "https://www.cerema.fr/fr"), style="text-align:center;margin-top:5px;"))
                 )
             ), 
             style="background-color:#EF7757;
                    padding-top:70px;
                    padding-bottom:70px;"),
  ),
  
  # __ONGLET RECHERCHE UNE FRICHE ----
  tabPanel(
    "Rechercher une friche",
    icon = icon("search"),
    fluidRow(
      column(4,
             div(
             tabsetPanel(id="tabs",
                        type = "tabs",
                         tabPanel(tagList(icon("map-marker", lib="glyphicon"),"Adresse"), 
                                  div( 
                                    div(
                                      div(
                                        modui_ban("search2", width = "100%")
                                      ),
                                      tags$hr(style="border-top:1px solid #ffae99;"),
                                      selectInput("slc_secteurs", 
                                                  label = NULL, 
                                                  choices = Secteurs, 
                                                  selected = NULL, 
                                                  width = "80%", 
                                                  selectize = FALSE)),
                                    class = "search"),                                  
                                  ),
                         tabPanel(tagList(icon("binoculars"), "Observatoires"), 
                                  div( 
                                    selectInput("slc_observatoires",
                                                NULL,
                                                choices = Observatoires, 
                                                width = "80%",
                                                selectize = FALSE), class = "search")                                  
                                  )
             )),
             
             fluidRow(
               column(12,
                      # LEGENDE ----
                      div(uiOutput("ui_legende"),
                          tags$p(checkboxInput("chk_all", 
                                               "Afficher les friches potentielles", 
                                               value = FALSE)),
                          uiOutput("filtres"),
                          uiOutput("zoom"),
                          class="information"),
                      
               )),
             
             class = "carte"),
      
      # Carte ----
      column(8, 
             leafletOutput("mymap"))
    )
  ),
  
  
  # # __ONGLET STATISTIQUES ----
  tabPanel("Explorer les données",
           icon = icon("table"),

           tags$br(),tags$br(),

           fluidRow(
             column(3, offset=2,

  # https://www.axl.cefan.ulaval.ca/europe/france_departements.htm
                           pickerInput("INPUT_ChoixDep",
                                       "Départements :",
                                       choices = c(c("01 - Ain"="01","02 - Aisne"="02","03 - Allier"="03","04 - Alpes de Haute-Provence"="04","05 - Hautes-Alpes"="05",
                                                     "06 - Alpes-Maritimes"="06","07 - Ardêche"="07","08 - Ardennes"="08","09 - Ariège"="09","10 - Aube"="10",
                                                     "11 - Aude"="11","12 - Aveyron"="12","13 - Bouches-du-Rhône"="13","14 - Calvados"="14","15 - Cantal"="15",
                                                     "16 - Charente"="16","17 - Charente-Maritime"="17","18 - Cher"="18","19 - Corrèze"="19",
                                                     "2A - Corse-du-Sud"="2A","2B - Haute-Corse"="2B"),
                                                   c("21 - Côte-d'Or"="21","22 - Côtes d'Armor"="22","23 - Creuse"="23","24 - Dordogne"="24","25 - Doubs"="25",
                                                     "26 - Drôme"="26","27 - Eure"="27","28 - Eure-et-Loir"="28","29 - Finistère"="29","30 - Gard"="30",
                                                     "31 - Haute-Garonne"="31","32 - Gers"="32","33 - Gironde"="33","34 - Hérault"="34","35 - Ille-et-Vilaine"="35",
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
                                                     "91 - Essonne"="91","92 - Hauts-de-Seine"="92","93 - Seine-Saint-Denis"="93","94 - Val-de-Marne"="94","95 - Val-dOise"="95"
                                                     ),
                                                   c("971 - Guadeloupe"="971","972 - Martinique"="972","973 - Guyane"="973","974 - La Réunion"="974","976 - Mayotte"="976")),
                                             options = list(
                                         `live-search` = TRUE,
                                         `actions-box` = TRUE,
                                         `deselect-all-text` = "Tout déselectionner",
                                         `select-all-text` = "Tout sélectionner",
                                         `none-selected-text` = "Aucun département sélectionné",
                                         `selected-text-format` = "count > 100",
                                         `count-selected-text` = "Tous les départements sélectionnés"),
                                       multiple = TRUE,
                                      selected = c(c("01","02","03","04","05","06","07","08","09"),
                                                   seq(10,19,1),
                                                   c("2A","2B"),
                                                   seq(21,95,1),
                                                   c(971,972,973,974,976))
                           )
           )
                           ,

           column(2, offset=0,

                  pickerInput("INPUT_Site_statut",
                              "Statut :",
                              choices = c("friche reconvertie","friche avec projet","friche sans projet","inconnu","friche potentielle"),
                              options = list(
                                # `live-search` = TRUE,
                                `actions-box` = TRUE,
                                `select-all-text` = "Tous",
                                `deselect-all-text` = "Aucun",
                                `none-selected-text` = "Aucun statut sélectionné",
                                `selected-text-format` = "count > 4",
                                `count-selected-text` = "Tout statuts"),
                              multiple = TRUE,
                              selected = c("friche avec projet","friche sans projet")
                  )

           )

           ,
  
  column(3, offset=0,
         
         pickerInput("INPUT_FILTRE_Site_type",
                     "Type :",
                     choices = c("friche agro-industrielle","friche commerciale","friche d'équipement public","friche d'habitat",
                                 "friche enseignement","friche ferroviaire","friche hospitalière","friche industrielle","friche logistique",
                                 "friche loisir tourisme hôtellerie","friche militaire","mixte","inconnu","autre","sans objet"),
                     options = list(
                       # `live-search` = TRUE,
                       `actions-box` = TRUE,
                       `select-all-text` = "Tous",
                       `deselect-all-text` = "Aucun",
                       `none-selected-text` = "Aucun type sélectionné",
                       `selected-text-format` = "count > 14",
                       `count-selected-text` = "Tous les types sélectionnés"),
                     multiple = TRUE,
                     selected = c("friche agro-industrielle","friche commerciale","friche d'équipement public","friche d'habitat",
                                  "friche enseignement","friche ferroviaire","friche hospitalière","friche industrielle","friche logistique",
                                  "friche loisir tourisme hôtellerie","friche militaire","mixte","inconnu","autre","sans objet")
         )
  )
           )
  ,
  br(),

  fluidRow(
    
    column(3, offset=2,
           pickerInput("INPUT_FILTRE_Producteur",
                       "Producteur :",
                       choices = c("Observatoire local" = "observatoire local",
                                   "Appels à projets Fonds friches" = "fond friche",
                                   "Basias/Basol" = "MTE",
                                   "Basias/Basol non vérifié" = "MTE non vérifié",
                                   "Potentiel solaire au sol de l'Ademe" = "Ademe",
                                   "Retours utilisateurs" = "retour utilisateur"),
                       options = list(
                         # `live-search` = TRUE,
                         `actions-box` = TRUE,
                         `select-all-text` = "Tous",
                         `deselect-all-text` = "Aucun",
                         `none-selected-text` = "Aucun producteur sélectionné",
                         `selected-text-format` = "count > 5",
                         `count-selected-text` = "Tous les producteurs sélectionnés"),
                       multiple = TRUE,
                       selected = c("Observatoire local" = "observatoire local",
                                    "Appels à projets Fonds friches" = "fond friche",
                                    "Basias/Basol" = "MTE",
                                    "Basias/Basol non vérifié" = "MTE non vérifié",
                                    "Potentiel solaire au sol de l'Ademe" = "Ademe",
                                    "Retours utilisateurs" = "retour utilisateur")
           )
    )
    
    
    ,
    
    column(2, offset=0,
           pickerInput("INPUT_FILTRE_ZONEURBA_TABLEAU",
                       "Zonage d'urbanisme :",
                       choices = c("U - zone urbaine" = "U",
                                   "AU - zone à urbaniser" = "AU",
                                   "A - zone agricole" = "A",
                                   "N - zone naturelle" = "N",
                                   "Hors PLU/PLUi" = "CC"),
                       options = list(
                         # `live-search` = TRUE,
                         `actions-box` = TRUE,
                         `select-all-text` = "Tous",
                         `deselect-all-text` = "Aucun",
                         `none-selected-text` = "Aucun zonage sélectionné",
                         `selected-text-format` = "count > 4",
                         `count-selected-text` = "Tous zonages sélectionnés"),
                       multiple = TRUE,
                       selected = c("U - zone urbaine" = "U",
                                    "AU - zone à urbaniser" = "AU",
                                    "A - zone agricole" = "A",
                                    "N - zone naturelle" = "N",
                                    "Hors PLU/PLUi" = "CC")
           )
    )
    
    
    ,
    
    column(3, offset=0,
           sliderInput("INPUT_FILTRE_SURFACE_TABLEAU_slider",
                       "Surface (en Ha) :",
                       value = c(0,5),
                       min = 0,
                       max = 50,
                       step = 1
           )
    )
    
    )

           ,
  
           tags$br(),

           fluidRow(column(8, offset=1,
                           
                           div('', style=glue("
                            width: 100;
                            border-bottom: 3px solid #BABABA;
                            margin-bottom: 30px;
                            ")),
                           
                           DT::dataTableOutput("tableau"),
                           
                           br(),
                           
                           downloadButton("downloadData_Tableau", "Télécharger les données du tableau (.csv)")

           ))

           ,
           tags$br(),tags$br()

  ) # fin tabPanel ONGLET STATISTIQUES

  ,


  # __ONGLET CONTRIBUER ----
  tabPanel("Contribuer",
           icon = icon("paper-plane"),
           
           fluidRow(column(8, offset=2, 
                           uiOutput("ui_publier_une_friche")
                           )
                    )
           ,

           fluidRow(
             column(6, offset=2,
                    downloadButton("downloadZIP_kit",
                                   role = "button",`arial-label` = "Télécharger le kit Cartofriches",
                                   class="btn-download",
                                   "Télécharger le kit Cartofriches (dossier .zip)")
             )
           )
           
           ,
           fluidRow(column(8, offset=2, 
                           uiOutput("ui_publier_une_friche_2")
           )
           )
             
             ,
           tags$br(),tags$br()
           
           ) # fin tabPanel CONTRIBUER
  ,
  
  # __ONGLET DONNEES ----
  tabPanel(
    "Données",
    icon = icon("database"),
    
    fluidRow(
      column(8 ,offset = 2,
             div(
               h1("Téléchargement des données"),
               tags$p(icon("sync-alt"), " Dernière mise à jour des données le ", LAST_UPDATE_DATE, style="color: #464749;font-size: 1.2em;font-style: italic;"),
               br(),
               h2("Télécharger les données sur le site data.gouv.fr"),
               
               includeMarkdown("www/textes/download_data.md"),
               
               div(
                 div(tags$span("Télécharger les données sur data.gouv.fr")),
                 div(tags$a(tags$img(src="logo-datagouv.png", width = "150px", style="margin-top:10px;"), 
                            href="https://www.data.gouv.fr/fr/datasets/sites-references-dans-cartofriches/",
                            target="_blank")),
                 style="
          color:#144391;
          margin-top: 5px;
          padding-top: 10px;
          text-align: center;
          padding-bottom: 10px;
          border-radius: 5px;
          background-color: whitesmoke;
          font-size: 1.2em;
          width:400px;
          margin:auto;
            ")
               
             )
             
#              ,
#              
#              HTML('
# <div class="fr-tile fr-tile--download fr-enlarge-link" id="tile-6735">    <div class="fr-tile__body">        <div class="fr-tile__content">            <h3 class="fr-tile__title">                <a href="#" download>Télécharger le document XX</a>            </h3>            <p class="fr-tile__detail">Détail (optionel)</p>        </div>    </div>    <div class="fr-tile__header">        <div class="fr-tile__pictogram">            <svg aria-hidden="true" class="fr-artwork" viewBox="0 0 80 80" width="80px" height="80px">                <use class="fr-artwork-decorative" href="/img/artwork/pictograms/city-hall.svg#artwork-decorative"></use>                <use class="fr-artwork-minor" href="/img/artwork/pictograms/city-hall.svg#artwork-minor"></use>                <use class="fr-artwork-major" href="/img/artwork/pictograms/city-hall.svg#artwork-major"></use>            </svg>        </div>    </div></div>
# ')
             
      ))
    ,tags$br(),tags$br(),
    
    fluidRow(
      column(8 ,offset = 2,
    div('', style=glue("
                            width: 100%;
                            border-bottom: 2px solid #e4e4e4;
                            margin-bottom: 10px;
                            "))
      ))
    ,
    
    # # Via le zip
    # fluidRow(
    #   column(6, offset=2,
    #          downloadButton("downloadZIP", 
    #                         role = "button",`arial-label` = "Télécharger les données",
    #                         class="btn-download",
    #                         "Télécharger les données (dossier .zip)")
    #   )
    #   # ,
    #   # column(2, 
    #   #        offset=1,
    #   #        fluidRow(
    #   #          column(6, tags$span("Données publiées sous", 
    #   #                              tags$a(href="https://www.etalab.gouv.fr/licence-ouverte-open-licence", "Licence Ouverte", target="_blank"))),
    #   #          column(6, tags$img(src="img/lo.png", 
    #   #                             alt = "",
    #   #                             width=60)),
    #   #        ),
    #   #        style="font-size:1.1em;"
    #   # )
    # )
    # ,

    
    
    fluidRow(
      column(8 ,offset = 2,
             div(
               h2("Visualiser les friches sur l'outil UrbanSIMUL"),
               includeMarkdown("www/textes/download_data_UrbanSIMUL.md")
               # tags$span("Les données présentes dans l'application Cartofriches sont en open data. Vous pouvez télécharger les données via le lien ci-dessous, ou via le portail data.gouv.fr .")
             ),
             div(
               div(tags$span("Accéder à l'outil UrbanSIMUL")),
               div(tags$a(tags$img(src="logo_UrbanSIMUL.jpg", width = "350px", style="margin-top:10px;"), 
                          href="https://urbansimul.cerema.fr/",   
                          target="_blank")),
               style="
          color:#144391;
          margin-top: 5px;
          padding-top: 10px;
          text-align: center;
          padding-bottom: 10px;
          border-radius: 5px;
          background-color: whitesmoke;
          font-size: 1.2em;
          width:400px;
          margin:auto;
            ")
    )
    )
    
    ,tags$br(),tags$br(),
    fluidRow(
      column(8 ,offset = 2,
             div('', style=glue("
                            width: 100%;
                            border-bottom: 2px solid #e4e4e4;
                            margin-bottom: 10px;
                            "))
      ))
    ,
    
    
    fluidRow(
      column(8 ,offset = 2,
             div(
               h2("Accéder aux données via l'API Données foncières"),
               includeMarkdown("www/textes/download_data_API.md")
               # tags$span("Les données présentes dans l'application Cartofriches sont en open data. Vous pouvez télécharger les données via le lien ci-dessous, ou via le portail data.gouv.fr .")
             ),
             div(
               div(tags$span("Accéder à l'API Données foncières du Cerema")),
               div(tags$a(tags$img(src="logo_apidonneesfoncieres.jpg", width = "400px", style="margin-top:10px;"), 
                          href="https://apidf-preprod.cerema.fr/swagger/#/Cartofriches%20(acc%C3%A8s%20libre)/",
                          target="_blank")),
               style="
          color:#144391;
          margin-top: 5px;
          padding-top: 10px;
          text-align: center;
          padding-bottom: 10px;
          border-radius: 5px;
          background-color: whitesmoke;
          font-size: 1.2em;
          width:500px;
          margin:auto;
            ")
      )
    )
    
    
    
    ,
    
    
    
    
    tags$br(),tags$br(),tags$br()
    
  ) # fin tabPanel DONNEES
  
  ,
    
    navbarMenu("À propos",
        
    # __ONGLETS A PROPOS / Cartofriches ----       
    tabPanel("Cartofriches",
             fluidRow(column(8, offset=2, uiOutput("ui_apropos_cartofriches")))),
    
    # Un clic amène vers la page observatoires du portail artificialisation.biodiversitetousvivants
    tabPanel(tags$a(href = "https://artificialisation.biodiversitetousvivants.fr/cartofriches/observatoires-locaux",
    "Observatoires locaux", 
    target="_blank")),
    
    # Un clic amène vers la page données du portail artificialisation.biodiversitetousvivants
    tabPanel(tags$a(href = "https://artificialisation.biodiversitetousvivants.fr/cartofriches/donnees-utilisees",
    "Données utilisées",
    target = "_blank")),
    
    tabPanel("Mentions légales",
             fluidRow(column(8, offset=2, includeMarkdown("www/textes/mentions_legales.md"))))
    ),
    
    
    # FOOTER ---------------------------
    tags$hr(),
    footer = get_ui_footer()
  )