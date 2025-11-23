// Récupérer les éléments du DOM
const openModalBtn = document.getElementById('openModalBtn');
const myModal = document.getElementById('Modal');
const closeButton = document.getElementsByClassName('close-button')[0];
const closeModalInsideBtn = document.getElementById('closeModalInsideBtn');

// Quand l'utilisateur clique sur le bouton, ouvrir la modale
openModalBtn.onclick = function() {
    myModal.style.display = 'flex'; // Utilise 'flex' pour centrer le contenu
}

// Quand l'utilisateur clique sur le bouton de fermeture (x), fermer la modale
closeButton.onclick = function() {
    myModal.style.display = 'none';
}

// Quand l'utilisateur clique sur le bouton "Fermer" à l'intérieur, fermer la modale
closeModalInsideBtn.onclick = function() {
    myModal.style.display = 'none';
}

// Quand l'utilisateur clique n'importe où en dehors de la modale, la fermer
window.onclick = function(event) {
    if (event.target == myModal) {
        myModal.style.display = 'none';
    }
}