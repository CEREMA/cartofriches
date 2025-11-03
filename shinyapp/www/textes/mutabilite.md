# Indices de mutabilité des friches

Cartofriches propose depuis peu des informations relatives à la **mutabilité des friches**.
Cette page explicite la méthode qui a été utilisée.


# Principes généraux de la méthode

A l'issue de la méthodologie, un indice de mutabilité est proposé pour 6 types de reconversion, pour l'ensemble des friches n'ayant pas de projet : **résidentiel, équipement public, tertiaire, industriel, renaturation, photovoltaïque au sol**.
L'indice de mutabilité correspond au niveau de pertinence de chaque usage pour cette friche. 
Plus il est élevé, plus une reconversion du site vers cet usage est pertinent.
Les indices de mutabilité ne sont pas proposés pour les friches faisant déjà l'objet d'un projet ni pour les friches déjà reconverties. 

Cette méthode est inspirée des travaux de la <a href=https://www.ardennes.gouv.fr/Actions-de-l-Etat/Amenagement-du-territoire.-construction-et-logement/Amenagement-et-urbanisme/Les-friches-des-Ardennes/Mutabilite-des-Friches/ target=_blank>**DDT des Ardennes** <i class="fa fa-external-link"></i></a> et de ceux de <a href=https://librairie.ademe.fr/urbanisme-territoires-et-sols/7241-evaluer-la-mutabilite-des-friches-outil-mutafriches.html/ target=_blank>**l'Ademe** <i class="fa fa-external-link"></i></a>.
Elle se base sur le remplissage de nombreux indicateurs, qui ont des effets positifs ou négatifs sur le futur usage d'une friche.

Les spécificités par rapport à la méthode de l'Ademe sont : 
- l'ajout de l'indicateur de potentiel de gain écologique issu de la méthode POGEIS ;
- l'ajout du classement de la commune selon le zonage ABC ; 
- l'ajout de la proximité à une Installation Terminale Embranchée fret ;
- des modifications dans le zonage du document d'urbanisme (détail de la zone U et pondération plus importante).

L'intérêt majeur de l'intégration de ces indices de mutabilité dans Cartofriches est que **la grande majorité des indicateurs sont automatiquement calculés par les équipes de Cartofriches**.
Les autres peuvent être renseignés ou modifiés par l'utilisateur.

## Indicateurs calculés automatiquement par Cartofriches

**22 indicateurs** sont calculés automatiquement : 

- Type de propriétaire : calculé à partir des <a href=https://datafoncier.cerema.fr/fichiers-fonciers/ target=_blank>**fichiers fonciers** <i class="fa fa-external-link"></i></a>, en croisant avec les unités foncières qui intersectent la friche
- Surface de la friche : calculée à partir de la géométrie de la friche
- Emprise au sol du bâti : calculée à partir de la BDTopo, en croisant avec les unités foncières qui intersectent la friche
- Etat du bâti : information renseignée par les observatoires locaux
- Monument historique : calculée à partir des données data.gouv.fr
- Zonage d'urbanisme : calculée à partir du Géoportail de l'urbanisme et du croisement avec la base de données <a href=https://datafoncier.cerema.fr/fusac/ target=_blank>**FUSAC** <i class="fa fa-external-link"></i></a>
- Pollution du sol : information renseignée par les observatoires locaux et enrichis par les SIS
- Accessibilité à une gare : calculée à partir des données data.gouv.fr
- Accessibilité à une Installation Terminale Embranchée fret (ITE) : calculée à partir des données <a href=https://www.data.gouv.fr/datasets/base-de-donnees-des-installations-terminales-embranchees-fret-en-france-ite-3000/ target=_blank>**data.gouv.fr** <i class="fa fa-external-link"></i></a>. Une ITE correspond à un site de chargement ou de déchargement de marchandises chez un industriel.
- Accessibilité à un échangeur autoroutier : calculée à partir des données data.gouv.fr
- Appartenance au centre-bourg : calculée à partir de la distance à la mairie
- Proximité aux commerces : calculée à partir des données de la Base Permanente des Equipements de l'INSEE
- Distance à un poste électrique : calculée à partir des données ENEDIS
- Taux de logements vacants à la commune : calculée à partir des données <a href=https://datafoncier.cerema.fr/lovac/ target=_blank>**LOVAC** <i class="fa fa-external-link"></i></a>
- Trame Verte et Bleue : calculée à partir des données du Géoportail de l'urbanisme
- Risques naturels : calculée à partir des données du Géoportail de l'urbanisme
- Risques Technologiques : calculée à partir des données du Géoportail de l'urbanisme
- Zonages environnementaux : calculée à partir des données du Géoportail de l'urbanisme et INPN
- Présence de la friche dans une zone d'accélération d'énergie renouvelable : calculée à partir des données du <a href=https://geoservices.ign.fr/portail-cartographique-enr/ target=_blank>**portail des EnR** <i class="fa fa-external-link"></i></a>
- Couvert végétal : calculé à partir des données de <a href=https://artificialisation.developpement-durable.gouv.fr/bases-donnees/ocs-ge/ target=_blank>**l'OCS-GE** <i class="fa fa-external-link"></i></a> 
- Potentiel de gain écologique : issu des travaux méthodologiques de l'application <a href=https://pogeis.ofb.fr/ target=_blank>**POGEIS** <i class="fa fa-external-link"></i></a> (méthodologie développée par l'OFB et le Cerema)
- Zonage ABC : information renseignée à partir des données <a href=https://www.ecologie.gouv.fr/politiques-publiques/zonage-b-c/ target=_blank>**data.gouv.fr** <i class="fa fa-external-link"></i></a>  

## Indicateurs à renseigner par l'utilisateur

5 indicateurs peuvent être renseignés par l'utilisateur : 
- Présence d'une zone humide
- Voie d'eau à proximité
- Terrain viabilisé
- Présence d'une espèce protégée
- Valeur architecturale et/ou sociale

L'appartenance au centre-bourg peut être modifiée si besoin par l'utilisateur. Cette information importante a été estimée à partir de la distance à la mairie. Il est possible de la corriger pour affiner les indices de mutabilité.
La pollution du site et la présence de risque naturels peuvent également être modifiées si besoin pour les mêmes raisons.


# Détail de la méthodologie

La section ci-dessous détaille le mode de calcul des indices de mutabilité, selon les usages (en colonne dans le tableau) et les caractéristiques du site (en ligne).

### Type de propriétaire

Source : <a href=https://datafoncier.cerema.fr/fichiers-fonciers/ target=_blank>**fichiers fonciers** <i class="fa fa-external-link"></i></a> (Cerema)

| Type de propriétaire | Résidentiel | Equipement public | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Public             | Très positif | Très positif | Neutre | Neutre | Positif | Positif |
| Privé              | Neutre | Négatif | Neutre | Neutre | Négatif | Négatif |
| Mixte public/privé | Neutre | Neutre | Neutre | Neutre | Très négatif | Neutre |
| Copropriété        | Négatif | Négatif | Négatif | Négatif | Très négatif | Négatif |

Cet indicateur a une pondération de 1.


### Surface de la friche

Source : géométrie de la friche

| Surface de la friche | Résidentiel | Equipement public | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Moins de 1ha       | Très positif | Positif | Neutre | Négatif | Neutre | Très négatif |
| Entre 1ha et 1.5ha | Négatif | Négatif | Neutre | Positif | Neutre | Très négatif |
| Entre 1.5ha et 5ha | Négatif | Négatif | Négatif | Très positif | Neutre | Positif |
| Plus de 5ha        | Négatif | Négatif | Négatif | Très positif | Neutre | Très positif |

Cet indicateur a une pondération de 2.

### Zonage du document d'urbanisme

Source : Géoportail de l’urbanisme et données <a href=https://datafoncier.cerema.fr/fusac/ target=_blank>**FUSAC** <i class="fa fa-external-link"></i></a> (Cerema)

| Zonage du document d'urbanisme | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Site d'activité    | Très négatif | Négatif | Négatif | Très positif | Très négatif | Neutre |
| UX                 | Très négatif | Négatif | Très positif | Positif | Négatif | Neutre |
| UE                 | Très négatif | Très positif | Neutre | Très négatif | Neutre | Neutre |
| UA ou UB ou UC ou UD| Très positif | Positif | Négatif | Très négatif | Neutre | Neutre |
| Autre U ou AU      | Très positif | Positif | Positif | Très négatif | Neutre | Neutre |
| ZC                 | Positif | Positif | Positif | Positif | Neutre | Neutre |
| N                  | Très négatif | Très négatif | Très négatif | Très négatif | Très positif | Très négatif |
| A                  | Très négatif | Très négatif | Très négatif | Très négatif| Très positif | Neutre |
| ZnC                | Très négatif | Très négatif | Très négatif | Très négatif | Très positif | Positif |

Cet indicateur a une pondération de 2.

### Localisation en centre-bourg

Source : estimation à partir de la distance à la mairie (data.gouv.fr)

| Localisation en centre-bourg | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Oui       | Très positif | Positif | Positif | Très négatif | Neutre | Très négatif |
| Non        | Très négatif | Négatif | Négatif | Très positif | Positif | Très positif |

Cet indicateur a une pondération de 2.

### Emprise au sol du bâti

Source : BDTopo (IGN)

| Emprise au sol du bâti | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Inférieur à 10000 m²       | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |
| Supérieur à 10000 m²       | Neutre | Neutre | Neutre | Positif | Négatif | Négatif |

Cet indicateur a une pondération de 2.

### Etat du bâti

Source : observatoires locaux de friches

| Etat du bâti | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Dégradation très importante             | Négatif | Négatif | Négatif | Neutre | Positif | Positif |
| Dégradation moyenne ou hétérogène       | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |
| Dégradation inexistante ou faible       | Positif | Positif | Positif | Positif | Négatif | Négatif |
| Absence de bâti                         | Neutre | Neutre | Neutre | Positif | Très positif | Positif |

Cet indicateur a une pondération de 2.

### Monument historique

Source : <a href=https://www.data.gouv.fr/datasets/immeubles-proteges-au-titre-des-monuments-historiques-2/ target=_blank>**data.gouv.fr** <i class="fa fa-external-link"></i></a>

| Monument historique | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Site classé            | Neutre | Neutre | Très négatif | Très négatif | Très négatif | Très négatif |
| Périmètre 500m         | Neutre | Neutre | Négatif | Négatif | Neutre | Négatif |
| Hors périmètre 500m    | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 1.

### Valeur architecturale

Source : information à renseigner par l'utilisateur

| Valeur architecturale | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Exceptionnel   | Neutre | Neutre | Très négatif | Négatif | Très négatif | Très négatif |
| Fort           | Neutre | Neutre | Négatif | Négatif | Très négatif | Très négatif |
| Moyen          | Neutre | Neutre | Neutre | Neutre | Négatif | Neutre |
| Faible         | Neutre | Neutre | Positif | Positif | Neutre | Positif |

Cet indicateur a une pondération de 1.

### Pollution du sol

Source : observatoires locaux de friches ou base de données des <a href=https://www.georisques.gouv.fr/donnees/bases-de-donnees/secteurs-dinformations-sur-les-sols-sis/ target=_blank>**SIS** <i class="fa fa-external-link"></i></a>. Information pouvant être modifiée par l'utilisateur.

| Pollution du sol | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Pollution traitée                      | Positif | Positif | Positif | Positif | Positif | Positif |
| Pollution avérée - composants volatils | Très négatif | Très négatif | Négatif | Négatif | Neutre | Neutre |
| Pollution avérée ou pollution supposée | Négatif | Négatif | Négatif | Négatif | Neutre | Neutre |
| Pollution inexistante ou peu probable  | Très positif | Très positif | Très positif | Très positif | Positif | Positif |
| Inconnu                                | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 2.

### Accessibilité à un échangeur autoroutier

Source : <a href=https://www.data.gouv.fr/datasets/echangeurs-du-reseau-routier-national-concede/ target=_blank>**data.gouv.fr** <i class="fa fa-external-link"></i></a>

| Distance à un échangeur autoroutier | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Moins de 2km    | Neutre | Neutre | Neutre | Positif | Neutre | Négatif |
| Entre 2 et 5km  | Neutre | Neutre | Neutre | Neutre  | Neutre | Neutre |
| Plus de 5km     | Neutre | Neutre | Neutre | Négatif | Neutre | Positif |

Cet indicateur a une pondération de 0,5.

### Accessibilité à une gare

Source : <a href=https://www.data.gouv.fr/datasets/liste-des-gares/ target=_blank>**data.gouv.fr** <i class="fa fa-external-link"></i></a>

| Distance à une gare | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Moins de 500m    | Très positif | Positif | Positif | Positif | Neutre | Neutre |
| Plus de 500m     | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 0,5.

### Accessibilité à une Installation Terminale Embranchée fret (ITE)

Source : <a href=https://www.data.gouv.fr/datasets/base-de-donnees-des-installations-terminales-embranchees-fret-en-france-ite-3000/ target=_blank>**data.gouv.fr** <i class="fa fa-external-link"></i></a> (Cerema)

| Distance à une ITE | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Moins de 1km d'une ITE en bon état    | Neutre | Neutre | Neutre | Très positif | Neutre | Neutre |
| Moins de 1km d'une ITE en mauvais état | Neutre | Neutre | Neutre | Positif | Neutre | Neutre |
| Plus de 1km d'une ITE              | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 0,5.

### Proximité d'une voie d'eau

Source : information à renseigner par l'utilisateur

| Proximité d'une voie d'eau | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Oui et navigable  | Neutre | Neutre | Neutre | Positif | Positif | Neutre |
| Oui non navigable | Neutre | Neutre | Neutre | Neutre | Positif | Neutre |
| Non               | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 1.

### Proximité à un commerce

Source : <a href=https://www.insee.fr/fr/metadonnees/source/serie/s1161/ target=_blank>**Base Permanente des Equipements** <i class="fa fa-external-link"></i></a>  (INSEE)

| Proximité à un commerce | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Moins de 500m    | Très positif | Très positif | Neutre | Neutre | Neutre | Neutre |
| Plus de 500m     | Négatif | Négatif | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 1.

### Potentiel de gain écologique - Méthodologie POGEIS

Source : <a href=https://pogeis.ofb.fr/ target=_blank>**POGEIS** <i class="fa fa-external-link"></i></a> (OFB/Cerema)

| Potentiel de gain écologique | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Fort       | Neutre | Neutre | Neutre | Neutre | Très positif | Neutre |
| Moyen | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |
| Faible | Neutre | Neutre | Neutre | Neutre | Négatif | Neutre |

Cet indicateur a une pondération de 2.

### Zonages environnementaux

Source : Géoportail de l’urbanisme et INPN

| Zonages environnementaux | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Réserve naturelle      | Très négatif | Très négatif | Très négatif | Très négatif | Très positif | Très négatif |
| Natura 2000 ou ZNIEFF  | Négatif | Négatif | Négatif | Négatif | Positif | Négatif |
| Moins de 5km d'une zone| Neutre | Neutre | Neutre | Neutre | Positif | Neutre |
| Hors zone              | Positif | Positif | Positif | Positif | Neutre | Positif |

Cet indicateur a une pondération de 1.

### Terrain viabilisé

Source : information à renseigner par l'utilisateur

| Terrain viabilisé | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Oui  | Très positif | Très positif | Très positif | Très positif | Neutre | Neutre |
| Non  | Très négatif | Très négatif | Très négatif | Très négatif | Très positif | Positif |

Cet indicateur a une pondération de 1.

### Présence d'espèces protéges

Source : information à renseigner par l'utilisateur

| Présence d'espèces protéges | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Oui  | Très négatif | Très négatif | Très négatif | Très négatif | Très positif | Très négatif |
| Non  | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 1.

### Présence d'une zone humide

Source : information à renseigner par l'utilisateur

| Présence d'une zone humide | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Oui  | Très négatif | Très négatif | Très négatif | Très négatif | Très positif | Très négatif |
| Non  | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 1.

### Couverture arborée

Source : <a href=https://artificialisation.developpement-durable.gouv.fr/bases-donnees/ocs-ge/ target=_blank>**OCS-GE** <i class="fa fa-external-link"></i></a>  (IGN/Cerema)

| Couverture arborée | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Sol imperméabilisé      | Positif | Positif | Positif | Positif | Négatif | Neutre |
| Sol nu                  | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |
| Végétation non ligneuse | Négatif | Négatif | Négatif | Négatif | Positif | Négatif |
| Végétation ligneuse     | Très négatif | Très négatif | Très négatif | Très négatif | Très positif | Très négatif |

Cet indicateur a une pondération de 1.

### Trame verte et bleue

Source : Géoportail de l’urbanisme et INPN

| Trame verte et bleue | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Oui      | Négatif | Négatif | Négatif | Négatif | Très positif | Négatif |
| Non      | Positif | Positif | Positif | Positif | Neutre | Positif |

Cet indicateur a une pondération de 1.

### Risques naturels

Source : Géoportail de l’urbanisme et INPN. Information pouvant être modifiée par l'utilisateur.

| Risques naturels | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Oui - risque fort      | Très négatif | Très négatif | Très négatif | Très négatif | Très positif | Négatif |
| Oui      | Négatif | Négatif | Négatif | Négatif | Positif | Négatif |
| Non      | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 1.

### Risques technologiques

Source : Géoportail de l’urbanisme et INPN. 

| Risques technologiques | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Oui      | Très négatif | Très négatif | Très négatif | Très positif | Positif | Positif |
| Non      | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 1.

### Zonage ABC

Source : <a href=https://www.ecologie.gouv.fr/politiques-publiques/zonage-b-c/ target=_blank>**data.gouv.fr** <i class="fa fa-external-link"></i></a>

| Zonage ABC | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Abis ou A    | Très positif | Neutre | Neutre | Neutre | Neutre | Neutre |
| B1 ou B2     | Positif | Neutre | Neutre | Neutre | Neutre | Neutre |
| C            | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 1.

### Taux de logements vacants dans la commune

Source : <a href=https://datafoncier.cerema.fr/lovac/ target=_blank>**LOVAC** <i class="fa fa-external-link"></i></a> (Cerema)

| Taux de logements vacants dans la commune | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Moins de 4%    | Très positif | Neutre | Neutre | Neutre | Neutre | Neutre |
| Entre 4 et 6%  | Positif | Neutre | Neutre | Neutre | Neutre | Neutre |
| Entre 6 et 10% | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |
| Entre 10 et 13%| Négatif | Neutre | Neutre | Neutre | Neutre | Neutre |
| Plus de 13%    | Très négatif | Neutre | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 1.

### Localisation sur une Zone d'Accélération d'Energies Renouvelables (ZAER) PV au sol

Source : <a href=https://geoservices.ign.fr/portail-cartographique-enr/ target=_blank>**Portail cartographique des EnR** <i class="fa fa-external-link"></i></a> 

| ZAER PV au sol | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Oui      | Neutre | Neutre | Neutre | Neutre | Neutre | Très positif |
| Non      | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |

Cet indicateur a une pondération de 1.

### Proximité d'un point de raccordement Enedis

Source : <a href=https://data.enedis.fr/pages/opendatagrd/ target=_blank>**ENEDIS** <i class="fa fa-external-link"></i></a>  

| Proximité d'un point de raccordement Enedis | Résidentiel | Equipement public  | Tertiaire | Industriel | Renaturation | PV au sol |
|:---:|:------:|:---:|:-----:|:----:|:------:|:-------:|
| Moins de 1km    | Neutre | Neutre | Neutre | Neutre | Neutre | Neutre |
| Entre 1 et 5km  | Neutre | Neutre | Neutre | Neutre | Neutre | Négatif |
| Plus de 5km     | Neutre | Neutre | Neutre | Neutre | Neutre | Très négatif |

Cet indicateur a une pondération de 1.



