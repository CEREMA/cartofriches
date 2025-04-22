# L'enrichissement de données de Cartofriches

Cartofriches réalise de nombreux enrichissement de données, en proposant plus de **30 indicateurs**, que l'on peut regrouper de la manière suivante : 

### Description de la friche : 
- **Type de propriétaire :** *Qui détient la friche ?* - calculé à partir des fichiers fonciers ;
- **Surface de la friche :** *Quelle est la superficie de la friche et des parcelles qui la compose ?* - calculée à partir de la géométrie de la friche ;
- **Nombre de bâtis, emprise au sol et coefficient d'emprise au sol :** *La friche est-elle bâtie ?* - calculé à partir de la BDTopo ;
- **Pollution du sol :** *Le sol est-il pollué ?* -  information renseignée par les observatoires locaux et enrichies par les SIS ;

### Description réglementaire de l'environnement de la friche :
- **Zonage d'urbanisme :** *La friche est-elle en zone urbanisée ou urbanisable ?* - calculée à partir des données du Géoportail de l'urbanisme ;
- **Risques naturels :** *La friche est-elle soumise à des risques naturels ?* - calculée à partir des données du Géoportail de l'urbanisme
- **Risques Technologiques :** *La friche est-elle soumise à des risques technologiques ?* - calculée à partir des données du Géoportail de l'urbanisme
- **Zonages environnementaux :** *La friche est-elle sur le périmètre d'une ZNIEFF, une réserve naturelle ou une zone Natura2000 ?* - calculée à partir des données du Géoportail de l'urbanisme
- **Monument historique :** *La friche est-elle sur un périmètre de 500m autour d’un monument historique ?* - calculée à partir des données data.gouv.fr
- **Zone d'accélération d'énergie renouvelable :** *La friche est-elle située dans une zone d'accélération d'énergie renouvelable PV au sol ?* - calculée à partir des données du portail des EnR
- **Trame Verte et Bleue :** *La friche est-elle sur une trame verte et bleue ?* - calculée à partir des données du Géoportail de l'urbanisme

### Autres descriptions de l'environnement de la friche :
- **Accessibilité aux transports :** gare, échangeur autoroutier - calculée à partir des données data.gouv.fr
- **Appartenance au centre-bourg :** calculée à partir de la distance à la mairie
- **Proximité aux commerces :** calculée à partir des données de la Base Permanente des Equipements de l'INSEE
- **Distance à un poste électrique :** calculée à partir des données ENEDIS

### Potentiel de gain écologique : 
A venir grâce au calcul de potentiel de gain écologique issu d'une méthodologie OFB/Cerema : https://professionnels.ofb.fr/fr/node/1662