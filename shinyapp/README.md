# Cartofriches

![](wiki/images/cartofriches.png)

Ceci est le répertoire de l'application Cartofriches, plateforme de recensement et d'inventaire des friches industrielles, contenant le code opensource de l'application.

Cartofriches a été construite sur la technologie R-Shiny et est publiée sous Licence xxx.

Sur cette page, vous aurez accès aux informations sur Cartofriches, ferez connaissance de ses particularités, accèderez à la documentation technique (wiki) et verrez comment installer puis lancer l'application Cartofriches.

# A propos
❔ [En savoir plus sur Cartofriches](https://artificialisation.developpement-durable.gouv.fr/cartofriches)

# Particularités de l'appli
xxx

leaflet
Recherche du site le plus proche du centre de la carte
banR

# Wiki
🤔 [Accéder au wiki pour savoir comment Cartofriches fonctionne](xxx)

# Dataviz Masterclass
Le contenu de cette masterclass vous permettra de comprendre davantage le code, et savoir comment développer une application Shiny comme Cartofriches :

xxxlogo

[Accéder à la Dataviz Masterclass du Cerema dans le cadre du Mois de l'Innovation Publique 2021](https://datagistips.github.io/dataviz-masterclass/)

# Installer et lancer Cartofriches
Voici comment installer et lancer Cartofriches :

## Installer R
Dans un premier temps, [installez **R**](https://cran.r-project.org/bin/windows/base/). C'est le moteur d'exécution de l'application

## Installer RStudio
Ensuite, vous pouvez [installer **RStudio**](https://rstudio.com/products/rstudio/download/). RStudio est l'environnement de développement de R, son IDE, l'interface dans laquelle éditer le code.

## Télécharger Cartofriches
Pour installer Cartofriches, vous pouvez passer, soit par un téléchargement depuis le répertoire github soit par la commande git

### En zip
[Vous pouvez aussi télécharger l'application sous forme de zip](url)

![](images/install/zip.png)

### Via git

- [Installez **git**](https://git-scm.com/downloads)
- Positionnez-vous dans le dossier dans lequel vous souhaitez avoir l'appli Cartofriches

		cd <votre-dossier>
- Ouvrez une Invite de commandes
- Exécutez la commande de clonage pour récupérer l'application


		git clone xxx.git

Cela va créer un dossier `cartofriches` sur votre poste de travail


## Installer les librairies
- Lancez **RStudio**
- Ouvrez le script `install-packages.R` et lancez-le afin d'installer les librairies nécessaifres à l'exécution de l'application

> [Plus d'infos sur les librairies utilisées](xxx)

## Lancer Cartofriches
- Ouvrez le fichier `cartofriches.RProj`, ce qui ouvre l'environnement de développement de Cartofriches dans RStudio que vous avez installé précédemment

![](images/install/projet-cartofriches.png)

- Positionnez-vous, soit sur le script `ui.R`, soit `server.R`, soit `global.R`. Cliquez sur `Run`

![](images/install/run.png)

- Cela lance Cartofriches dans RStudio.

- Pour le lancer dans le navigateur, choisir ces préférences :

![](images/install/prefs.png)

Voici Cartofriches dans le navigateur :

![](images/cartofriches.png)