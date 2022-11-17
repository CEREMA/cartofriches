# Cartofriches

![](https://raw.githubusercontent.com/wiki/CEREMA/cartofriches/images/cartofriches.png)

Ceci est le répertoire de l'application [Cartofriches](cartofriches.cerema.fr).

Cartofriches est une plateforme de recensement et d'inventaire des friches industrielles.

Vous trouverez dans ce répertoire le code opensource de l'application.

Cartofriches a été construite sur la technologie R-Shiny et est publiée sous [License MIT](https://spdx.org/licenses/MIT.html#licenseText).

[En savoir plus sur Cartofriches](https://artificialisation.developpement-durable.gouv.fr/cartofriches)  

## ⚙️ Installation
Voici comment installer et lancer Cartofriches :

### Installer R
Dans un premier temps, [installez **R**](https://cran.r-project.org/bin/windows/base/). C'est le moteur d'exécution de l'application

### Installer RStudio
Ensuite, vous pouvez [installer **RStudio**](https://rstudio.com/products/rstudio/download/). RStudio est l'environnement de développement de R, son IDE, l'interface dans laquelle éditer le code.

### Télécharger Cartofriches
Pour installer Cartofriches, vous pouvez passer, soit par un téléchargement depuis le répertoire github soit par la commande git

#### Télécharger en zip
![](https://raw.githubusercontent.com/wiki/CEREMA/cartofriches/images/install/zip.png)

#### Télécharger avec git

[Installez **git**](https://git-scm.com/downloads)

Positionnez-vous dans le dossier dans lequel vous souhaitez avoir l'appli Cartofriches

	cd <votre-dossier>


Exécutez la commande de clonage pour récupérer l'application


	git clone https://github.com/CEREMA/cartofriches.git

Cela va créer un dossier cartofriches sur votre poste de travail


### Installer les librairies
Lancez [**RStudio**](https://rstudio.com/products/rstudio/download/)

Ouvrez le script [`install-packages.R`](https://github.com/CEREMA/cartofriches/blob/main/shinyapp/install-packages.R) et lancez-le afin d'installer les librairies nécessaifres à l'exécution de l'application

[Plus d'infos sur les librairies utilisées](https://github.com/CEREMA/cartofriches/wiki/Librairies)

### ⚡ Lancer Cartofriches
Ouvrez le fichier `cartofriches.RProj`, ce qui ouvre l'environnement de développement de Cartofriches dans RStudio que vous avez installé précédemment

![](https://raw.githubusercontent.com/wiki/CEREMA/cartofriches/images/install/rproj.png)

Positionnez-vous, soit sur le script `ui.R`, soit `server.R`, soit `global.R`. Cliquez sur `Run`

![](https://raw.githubusercontent.com/wiki/CEREMA/cartofriches/images/install/run.png)

Cela lance Cartofriches dans RStudio.

Pour le lancer dans le navigateur, choisir ces préférences :

![](https://raw.githubusercontent.com/wiki/CEREMA/cartofriches/images/install/prefs.png)

## 💡 Wiki
[Accéder au wiki (code expliqué, contenu, données, éléments d'interface,...)](https://github.com/CEREMA/cartofriches/wiki)

✉️ Vous pouvez nous contacter à [cartofriches@cerema.fr](mailto:cartofriches.cerema.fr)
