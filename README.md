# Extraction-des-donnees

L’objectif de ce projet est d’extraire tous les guides touristiques de France sur le site Decitre. Plus précisément, je souhaiterais recueillir les informations suivantes : le titre, l’auteur, l’édition, la date de parution, la région et le prix. Puis afficher le livre le moins cher et disponible immédiatement (en stock) pour chaque région.

Le lien du site est le suivant : https://www.decitre.fr/livres/loisirs-nature-voyages/guides-de-voyage/guides-france-regions.html

Ce projet a été réalisé sous R. Pour extraire les données de la page HTML, nous utilisons le langage de requêtes XPATH qui permet de sélectionner les noeuds d’un document (XML).Ensuite, nous avons nettoyer les données obtenues à partir du langage de requêtes XPATH à l’aide des expressions régulières REGEX.

Dans ce répertoire, vous trouverez le rapport qui contient le code R à l'intérieur.

