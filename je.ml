(***************************************************************************) 
(* Jeu d'essai pour sysevenements_gratuites - TP2 - ÉTÉ 2015               *) 
(***************************************************************************)

(* On charge le fichier ml du Tp après avoir implanté 
les méthodes demandées pour realiser les tests  *)
#use "TP2-E2015.ml";;

(* Résultat:
module type TP2E15 =
  sig
    class evenement :
      string list ->
      object
        val adresse_evenement : string
        val categorie_evenement : string
        val complement_lieu_evenement : string
        val courriel_evenement : string
        val cout_evenement : string
        val debut_evenement : string
        val description_evenement : string
        val fin_evenement : string
        val horaire_evenement : string
        val nom_arrondissement : string
        val nomlieu_evenement : string
        val renseignement_evenement : string
        val tel1_evenement : string
        val tel2_evenement : string
        val tel_lieu : string
        val titre_evenement : string
        val url_evenement : string
        method afficher_evenement : unit
        method get_adresse_evenement : string
        method get_categorie_evenement : string
        method get_complement_lieu_evenement : string
        method get_courriel_evenement : string
        method get_cout_evenement : string
        method get_debut_evenement : string
        method get_description_evenement : string
        method get_fin_evenement : string
        method get_horaire_evenement : string
        method get_nom_arrondissement : string
        method get_nomlieu_evenement : string
        method get_renseignement_evenement : string
        method get_tel1_evenement : string
        method get_tel2_evenement : string
        method get_tel_lieu : string
        method get_titre_evenement : string
        method get_url_evenement : string
      end
    class sysevenements :
      string ->
      object
        val origine_donnees : string
        method get_origine_donnees : string
      end
    class syseve_quebec :
      string ->
      string ->
      object
        val mutable liste_evenements : evenement list
        val origine_donnees : string
        val ville_concernee : string
        method afficher_systeme_evenements : unit
        method ajouter_evenement : evenement -> unit
        method ajouter_liste_evenements : string list list -> unit
        method charger_donnees_sysevenements : string -> unit
        method evenement_existe : evenement -> bool
        method get_liste_evenements : evenement list
        method get_origine_donnees : string
        method get_ville_concernee : string
        method lister_arrondissements : string list
        method lister_categories_evenements : string list
        method retourner_nbr_evenements : int
        method set_liste_evenements : evenement list -> unit
        method supprimer_evenement : evenement -> unit
        method trier_evenements : int -> unit
        method trouver_selon_arrondissement : string -> evenement list
        method trouver_selon_categorie : string -> evenement list
      end
    class app_sysevenements :
      string ->
      bool ->
      object
        val interface : bool
        val nom_fichier : string
        method lancer_interface_sevenements : unit
        method lancer_systeme_evenements : unit
        method sauvegarder_liste_evenements :
          evenement list -> out_channel -> unit
      end
  end
module Tp2e15 : TP2E15
*)

(* On ouvre le module du TP *) 
open Tp2e15;;

(* On exécute maintenant les méthodes une à une *)

let se = new syseve_quebec "les donnees ouvertes" "la ville de Quebec";;

(* Résultat:
Recherche dans les donnees ouvertes de la ville de Quebec.
val se : Tp2e15.syseve_quebec = <obj>
*)

let e = new evenement ["Exposition";"Vivre au Trait-Carre au temps des moulins";"2015-06-24";
		       "2015-09-07";"Tous les jours de 10 h a 17 h";
		       "2";"L'exposition raconte la vie au quotidien dans le Trait-Carre de Charlesbourg.";
		       "";"418-624-7720";"";"moulindesjesuites@bellnet.ca";
		       "http://www.moulindesjesuites.org";"Moulin des Jesuites";"";
		       "7960, boulevard Henri-Bourassa";"418-624-7720";"Charlesbourg"];;

(* Résultat:
val e : Tp2e15.evenement = <obj>
*)

se#ajouter_evenement e;;

(* Résultat:
- : unit = ()
*)

se#evenement_existe e;;

(* Résultat:
- : bool = true
*)

let ne = se#retourner_nbr_evenements;;

(* Résultat:
val ne : int = 1
*)


e#afficher_evenement;;

(*
Titre: Vivre au Trait-Carre au temps des moulins.
Categorie: Exposition.
Lieu: Moulin des Jesuites.
Adresse: 7960, boulevard Henri-Bourassa.
Arrondissement: Charlesbourg.
Telephone: 418-624-7720.
Dates: 2015-06-24 au 2015-09-07.
Horaire: Tous les jours de 10 h a 17 h.
Cout: Autre.

- : unit = ()
*)


se#supprimer_evenement e;;

(* Résultat:
- : unit = ()
*)

se#ajouter_liste_evenements [["Exposition";"Vivre au Trait-Carre au temps des moulins";"2015-06-24";
			      "2015-09-07";"Tous les jours de 10 h a 17 h";
			      "0";"L'exposition raconte la vie au quotidien dans le Trait-Carre de Charlesbourg.";
			      "";"418-624-7720";"";"moulindesjesuites@bellnet.ca";
			      "http://www.moulindesjesuites.org";"Moulin des Jesuites";"";
			      "7960, boulevard Henri-Bourassa";"418-624-7720";"Charlesbourg"];
			     ["Animation";"Journee de la nature";"2015-07-05";"2015-07-25";"Tous les jours de 10 h a 15 h";"0";
			      "Animation lors des randonnees, presence de stands d'information sur l'interpretation de la nature";
			      "";"418-641-6282";"";"";"";"Base de plein air de Sainte-Foy";"";"3180, rue Laberge";
			      "418-641-6282";"Sainte-Foy-Sillery-Cap-Rouge"];
			     ["Activites estivales";"Dans la serie Messes d'artistes";"2015-07-26";"2015-07-26";"11 h 15";"1";
			      "La messe dominicale sera agrementee par des artistes de la region : chant, violon et guitare";"0";
			      "";"418-653-5643";"";"";"Eglise Saint-Felix de Cap-Rouge";"";"1460, rue Provancher";"";
			      "Sainte-Foy-Sillery-Cap-Rouge"]];;


(* Résultat:
- : unit = ()
*)


let (selon_date_deb, selon_date_fin, selon_cout) = (1,2,3);;

se#trier_evenements selon_date_deb;;
se#afficher_systeme_evenements;;

(* Résultat:
Titre: Vivre au Trait-Carre au temps des moulins.
Categorie: Exposition.
Lieu: Moulin des Jesuites.
Adresse: 7960, boulevard Henri-Bourassa.
Arrondissement: Charlesbourg.
Telephone: 418-624-7720.
Dates: 2015-06-24 au 2015-09-07.
Horaire: Tous les jours de 10 h a 17 h.
Cout: Gratuit.

Titre: Journee de la nature.
Categorie: Animation.
Lieu: Base de plein air de Sainte-Foy.
Adresse: 3180, rue Laberge.
Arrondissement: Sainte-Foy-Sillery-Cap-Rouge.
Telephone: 418-641-6282.
Dates: 2015-07-05 au 2015-07-25.
Horaire: Tous les jours de 10 h a 15 h.
Cout: Gratuit.

Titre: Dans la serie Messes d'artistes.
Categorie: Activites estivales.
Lieu: Eglise Saint-Felix de Cap-Rouge.
Adresse: 1460, rue Provancher.
Arrondissement: Sainte-Foy-Sillery-Cap-Rouge.
Telephone: .
Dates: 2015-07-26 au 2015-07-26.
Horaire: 11 h 15.
Cout: Cout a l'entree.

- : unit = ()
*)

se#trier_evenements selon_date_fin;;
se#afficher_systeme_evenements;;

(* Résultat:
Titre: Journee de la nature.
Categorie: Animation.
Lieu: Base de plein air de Sainte-Foy.
Adresse: 3180, rue Laberge.
Arrondissement: Sainte-Foy-Sillery-Cap-Rouge.
Telephone: 418-641-6282.
Dates: 2015-07-05 au 2015-07-25.
Horaire: Tous les jours de 10 h a 15 h.
Cout: Gratuit.

Titre: Dans la serie Messes d'artistes.
Categorie: Activites estivales.
Lieu: Eglise Saint-Felix de Cap-Rouge.
Adresse: 1460, rue Provancher.
Arrondissement: Sainte-Foy-Sillery-Cap-Rouge.
Telephone: .
Dates: 2015-07-26 au 2015-07-26.
Horaire: 11 h 15.
Cout: Cout a l'entree.

Titre: Vivre au Trait-Carre au temps des moulins.
Categorie: Exposition.
Lieu: Moulin des Jesuites.
Adresse: 7960, boulevard Henri-Bourassa.
Arrondissement: Charlesbourg.
Telephone: 418-624-7720.
Dates: 2015-06-24 au 2015-09-07.
Horaire: Tous les jours de 10 h a 17 h.
Cout: Gratuit.

- : unit = ()
*)

se#trier_evenements selon_cout;;
se#afficher_systeme_evenements;;

(* Résultat:
Titre: Journee de la nature.
Categorie: Animation.
Lieu: Base de plein air de Sainte-Foy.
Adresse: 3180, rue Laberge.
Arrondissement: Sainte-Foy-Sillery-Cap-Rouge.
Telephone: 418-641-6282.
Dates: 2015-07-05 au 2015-07-25.
Horaire: Tous les jours de 10 h a 15 h.
Cout: Gratuit.

Titre: Vivre au Trait-Carre au temps des moulins.
Categorie: Exposition.
Lieu: Moulin des Jesuites.
Adresse: 7960, boulevard Henri-Bourassa.
Arrondissement: Charlesbourg.
Telephone: 418-624-7720.
Dates: 2015-06-24 au 2015-09-07.
Horaire: Tous les jours de 10 h a 17 h.
Cout: Gratuit.

Titre: Dans la serie Messes d'artistes.
Categorie: Activites estivales.
Lieu: Eglise Saint-Felix de Cap-Rouge.
Adresse: 1460, rue Provancher.
Arrondissement: Sainte-Foy-Sillery-Cap-Rouge.
Telephone: .
Dates: 2015-07-26 au 2015-07-26.
Horaire: 11 h 15.
Cout: Cout a l'entree.

- : unit = ()
*)

let se = new syseve_quebec "les donnees ouvertes" "la ville de Quebec";;

se#charger_donnees_sysevenements "EVENEMENT.CSV";;

(* Résultat:
- : unit = ()
*)

let ne = se#retourner_nbr_evenements;;

(* Résultat:
val ne : int = 1413
*)

let le = se#trouver_selon_arrondissement "Sainte-Foy-Sillery-Cap-Rouge";;
let ne = List.length le;;

(* Résultat:
val le : Tp2e15.evenement list =
  [<obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; ...]
# val ne : int = 463
*)

let lc = se#trouver_selon_categorie "Musique";;
let nc = List.length lc;;

(*
val lc : Tp2e15.evenement list =
  [<obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
   <obj>; <obj>; <obj>; <obj>; <obj>; <obj>]
# val nc : int = 126
*)

let liste_arrond = se#lister_arrondissements;;
let na = List.length liste_arrond;;

(*
val liste_arrond : string list =
  ["Les Rivieres"; "Beauport"; "La Haute-Saint-Charles";
   "Sainte-Foy-Sillery-Cap-Rouge"; "La Cite-Limoilou"; "Charlesbourg"]
# val na : int = 6
*)

let liste_categories = se#lister_categories_evenements;;
let nc = List.length liste_categories;;

(*
val liste_categories : string list =
  ["Divers"; "Musique"; "Activites estivales"; "Magie"; "Activite familiale";
   "Activite petit budget"; "Animation"; "Artisanat"; "Arts visuels";
   "Atelier artistique"; "Exposition"; "Patrimoine"; "Atelier culinaire";
   "Fete nationale du Quebec"; "Festival"; "Contes"; "Danse";
   "Histoire animee"; "Theatre"; "Activite multiculturelle";
   "Arts de la rue"; "Cinema"; "Visite guidee"; "Archeologie";
   "Atelier scientifique"; "Fete de quartier"; "Conference"; "Opera";
   "Parc Lineaire"; "Sports"; "Jeu"; "Marionnettes"; "Activite litteraire";
   "Eveil a la lecture"; "Vente de livres"; "Humour"; "Cirque";
   "Atelier d'ecriture"; "Fete"; "Defi Sante"]
# val nc : int = 40
*)

let app = new app_sysevenements "EVENEMENT.CSV" false;;

(* Résultat:
Bienvenue a l'outil de recherche d'événements
Recherche dans les donnees ouvertes de la ville de Quebec.
Quelle categorie d'evenements vous interessent?
0 - Divers
1 - Musique
2 - Activites estivales
3 - Magie
4 - Activite familiale
5 - Activite petit budget
6 - Animation
7 - Artisanat
8 - Arts visuels
9 - Atelier artistique
10 - Exposition
11 - Patrimoine
12 - Atelier culinaire
13 - Fete nationale du Quebec
14 - Festival
15 - Contes
16 - Danse
17 - Histoire animee
18 - Theatre
19 - Activite multiculturelle
20 - Arts de la rue
21 - Cinema
22 - Visite guidee
23 - Archeologie
24 - Atelier scientifique
25 - Fete de quartier
26 - Conference
27 - Opera
28 - Parc Lineaire
29 - Sports
30 - Jeu
31 - Marionnettes
32 - Activite litteraire
33 - Eveil a la lecture
34 - Vente de livres
35 - Humour
36 - Cirque
37 - Atelier d'ecriture
38 - Fete
39 - Defi Sante
40 - Tous 
Veuillez entrer un nombre entre 0 et 40:? 1

Quel arrondissement vous interesse?
0 - Les Rivieres
1 - Beauport
2 - La Haute-Saint-Charles
3 - Sainte-Foy-Sillery-Cap-Rouge
4 - La Cite-Limoilou
5 - Charlesbourg
6 - Tous 
Veuillez entrer un nombre entre 0 et 6:? 1

Voici le resultat de la recherche:
Titre: For me Formidable.
Categorie: Musique.
Lieu: Centre de loisirs Monseigneur-De Laval.
Adresse: 2, rue du Fargy.
Arrondissement: Beauport.
Telephone: 418-666-3349.
Dates: 2015-08-07 au 2015-08-07.
Horaire: 20 h.
Cout: Autre.

Titre: Barbara, le temps d'une chanson.
Categorie: Musique.
Lieu: Centre de loisirs Monseigneur-De Laval.
Adresse: 2, rue du Fargy.
Arrondissement: Beauport.
Telephone: 418-666-3349.
Dates: 2015-08-21 au 2015-08-21.
Horaire: 20 h.
Cout: Autre.

Titre: Fin de semaine chantante.
Categorie: Musique.
Lieu: Centre de loisirs Monseigneur-De Laval.
Adresse: 2, rue du Fargy.
Arrondissement: Beauport.
Telephone: 418-627-9802.
Dates: 2015-06-19 au 2015-06-21.
Horaire: De 19 h a 22 h.
Cout: Gratuit.

Titre: Soiree musicale et dansante pour le 35e anniversaire de l'Harmonie des Cascades et le lancement de sa nouvelle image corporative.
Categorie: Musique.
Lieu: Centre de loisirs Monseigneur-De Laval.
Adresse: 2, rue du Fargy.
Arrondissement: Beauport.
Telephone: 418-664-0989.
Dates: 2015-05-29 au 2015-05-29.
Horaire: 19 h.
Cout: Autre.

Titre: Jazz'Art 2015.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-664-0989.
Dates: 2015-07-24 au 2015-07-26.
Horaire: De 19 h a 22 h Animation.
Cout: Gratuit.

Titre: Un bestiaire en contes et chansons.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6045.
Dates: 2015-07-27 au 2015-07-27.
Horaire: 19 h.
Cout: Gratuit.

Titre: Quebec en Harmonies.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-880-2848.
Dates: 2015-06-26 au 2015-06-28.
Horaire: Vendredi de 19 h a 22 h Samedi et dimanche de 12 h a 17 h et de 19 h a 22 h.
Cout: Gratuit.

Titre: Spectacle La fievre des Balkans de Boris & les Gitans de Sarajevo.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6045.
Dates: 2015-07-11 au 2015-07-11.
Horaire: 19 h 30.
Cout: Gratuit.

Titre: Spectacle de Nadja.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6045.
Dates: 2015-07-18 au 2015-07-18.
Horaire: 19 h 30.
Cout: Gratuit.

Titre: Fete de la Ville de Quebec - Spectacle Les Studebakers.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6045.
Dates: 2015-07-03 au 2015-07-03.
Horaire: Spectacle a 19 h 30 (Il y aura de l'animation pour les enfants avant le spectacle).
Cout: Gratuit.

Titre: Fin de semaine chantante.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-627-9802.
Dates: 2015-06-19 au 2015-06-21.
Horaire: De 19 h a 22 h.
Cout: Gratuit.

Titre: Musiques pour flute et harpe: itineraires entre orient et occident.
Categorie: Musique.
Lieu: Eglise Saint-Louis-de-Courville.
Adresse: 2315, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6471-3.
Dates: 2015-07-15 au 2015-07-15.
Horaire: 19 h.
Cout: Gratuit.

Titre: Confidence de la musique romantique.
Categorie: Musique.
Lieu: Eglise Saint-Louis-de-Courville.
Adresse: 2315, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6471-3.
Dates: 2015-08-05 au 2015-08-05.
Horaire: 19 h.
Cout: Gratuit.

Titre: M'accorderiez-vous cette danse?.
Categorie: Musique.
Lieu: Eglise Saint-Louis-de-Courville.
Adresse: 2315, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6471-3.
Dates: 2015-07-29 au 2015-07-29.
Horaire: 19 h.
Cout: Gratuit.

Titre: Czardas.
Categorie: Musique.
Lieu: Eglise Saint-Louis-de-Courville.
Adresse: 2315, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6471-3.
Dates: 2015-07-22 au 2015-07-22.
Horaire: 19 h.
Cout: Gratuit.

Titre: Cordes a l'Opera.
Categorie: Musique.
Lieu: Eglise Saint-Louis-de-Courville.
Adresse: 2315, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6471-3.
Dates: 2015-07-08 au 2015-07-08.
Horaire: 19 h.
Cout: Gratuit.

Titre: Fete nationale du Quebec du Pivot.
Categorie: Musique.
Lieu: Centre communautaire des Chutes.
Adresse: 4054, boulevard Sainte-Anne.
Arrondissement: Beauport.
Telephone: 418-666-2371.
Dates: 2015-06-24 au 2015-06-24.
Horaire: De 11 h a 22 h.
Cout: Gratuit.

Titre: L'Amour, la poesie.
Categorie: Musique.
Lieu: Centre Monseigneur-De Laval.
Adresse: 35, avenue du Couvent G1E 6R9 Quebec.
Arrondissement: Beauport.
Telephone: 418-666-3349.
Dates: 2015-06-05 au 2015-06-05.
Horaire: 20 h.
Cout: Autre.

Titre: Souper-concert de l'Association lyrique de Beauport.
Categorie: Musique.
Lieu: Centre Monseigneur-De Laval.
Adresse: 35, avenue du Couvent G1E 6R9 Quebec.
Arrondissement: Beauport.
Telephone: 418-666-3349.
Dates: 2015-06-12 au 2015-06-12.
Horaire: 18 h.
Cout: Autre.

Titre: Soiree dansante sous les etoiles.
Categorie: Musique.
Lieu: Eglise de la Nativite de Notre-Dame de Beauport, perron.
Adresse: 25, Avenue du Couvent Beauport, QC G1E 6R9.
Arrondissement: Beauport.
Telephone: 418-641-6045.
Dates: 2015-07-10 au 2015-07-10.
Horaire: De 19 h a 21 h (spectacle et soiree dansante a l'exterieur ) De 21 h a 23 h (Soiree au CLML).
Cout: Gratuit.


Nombre d'evenements trouves: 20

Voulez-vous trier le resultat de la recherche?
1- Selon la date de debut.
2- Selon la date de fin.
3- Selon le cout de l'evenement.
4- Non, merci!.
Veuillez choisir une option (1 a 4):? 1

Voici la liste triee:

Titre: Soiree musicale et dansante pour le 35e anniversaire de l'Harmonie des Cascades et le lancement de sa nouvelle image corporative.
Categorie: Musique.
Lieu: Centre de loisirs Monseigneur-De Laval.
Adresse: 2, rue du Fargy.
Arrondissement: Beauport.
Telephone: 418-664-0989.
Dates: 2015-05-29 au 2015-05-29.
Horaire: 19 h.
Cout: Autre.

Titre: L'Amour, la poesie.
Categorie: Musique.
Lieu: Centre Monseigneur-De Laval.
Adresse: 35, avenue du Couvent G1E 6R9 Quebec.
Arrondissement: Beauport.
Telephone: 418-666-3349.
Dates: 2015-06-05 au 2015-06-05.
Horaire: 20 h.
Cout: Autre.

Titre: Souper-concert de l'Association lyrique de Beauport.
Categorie: Musique.
Lieu: Centre Monseigneur-De Laval.
Adresse: 35, avenue du Couvent G1E 6R9 Quebec.
Arrondissement: Beauport.
Telephone: 418-666-3349.
Dates: 2015-06-12 au 2015-06-12.
Horaire: 18 h.
Cout: Autre.

Titre: Fin de semaine chantante.
Categorie: Musique.
Lieu: Centre de loisirs Monseigneur-De Laval.
Adresse: 2, rue du Fargy.
Arrondissement: Beauport.
Telephone: 418-627-9802.
Dates: 2015-06-19 au 2015-06-21.
Horaire: De 19 h a 22 h.
Cout: Gratuit.

Titre: Fin de semaine chantante.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-627-9802.
Dates: 2015-06-19 au 2015-06-21.
Horaire: De 19 h a 22 h.
Cout: Gratuit.

Titre: Fete nationale du Quebec du Pivot.
Categorie: Musique.
Lieu: Centre communautaire des Chutes.
Adresse: 4054, boulevard Sainte-Anne.
Arrondissement: Beauport.
Telephone: 418-666-2371.
Dates: 2015-06-24 au 2015-06-24.
Horaire: De 11 h a 22 h.
Cout: Gratuit.

Titre: Quebec en Harmonies.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-880-2848.
Dates: 2015-06-26 au 2015-06-28.
Horaire: Vendredi de 19 h a 22 h Samedi et dimanche de 12 h a 17 h et de 19 h a 22 h.
Cout: Gratuit.

Titre: Fete de la Ville de Quebec - Spectacle Les Studebakers.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6045.
Dates: 2015-07-03 au 2015-07-03.
Horaire: Spectacle a 19 h 30 (Il y aura de l'animation pour les enfants avant le spectacle).
Cout: Gratuit.

Titre: Cordes a l'Opera.
Categorie: Musique.
Lieu: Eglise Saint-Louis-de-Courville.
Adresse: 2315, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6471-3.
Dates: 2015-07-08 au 2015-07-08.
Horaire: 19 h.
Cout: Gratuit.

Titre: Soiree dansante sous les etoiles.
Categorie: Musique.
Lieu: Eglise de la Nativite de Notre-Dame de Beauport, perron.
Adresse: 25, Avenue du Couvent Beauport, QC G1E 6R9.
Arrondissement: Beauport.
Telephone: 418-641-6045.
Dates: 2015-07-10 au 2015-07-10.
Horaire: De 19 h a 21 h (spectacle et soiree dansante a l'exterieur ) De 21 h a 23 h (Soiree au CLML).
Cout: Gratuit.

Titre: Spectacle La fievre des Balkans de Boris & les Gitans de Sarajevo.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6045.
Dates: 2015-07-11 au 2015-07-11.
Horaire: 19 h 30.
Cout: Gratuit.

Titre: Musiques pour flute et harpe: itineraires entre orient et occident.
Categorie: Musique.
Lieu: Eglise Saint-Louis-de-Courville.
Adresse: 2315, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6471-3.
Dates: 2015-07-15 au 2015-07-15.
Horaire: 19 h.
Cout: Gratuit.

Titre: Spectacle de Nadja.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6045.
Dates: 2015-07-18 au 2015-07-18.
Horaire: 19 h 30.
Cout: Gratuit.

Titre: Czardas.
Categorie: Musique.
Lieu: Eglise Saint-Louis-de-Courville.
Adresse: 2315, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6471-3.
Dates: 2015-07-22 au 2015-07-22.
Horaire: 19 h.
Cout: Gratuit.

Titre: Jazz'Art 2015.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-664-0989.
Dates: 2015-07-24 au 2015-07-26.
Horaire: De 19 h a 22 h Animation.
Cout: Gratuit.

Titre: Un bestiaire en contes et chansons.
Categorie: Musique.
Lieu: Maison Girardin.
Adresse: 600, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6045.
Dates: 2015-07-27 au 2015-07-27.
Horaire: 19 h.
Cout: Gratuit.

Titre: M'accorderiez-vous cette danse?.
Categorie: Musique.
Lieu: Eglise Saint-Louis-de-Courville.
Adresse: 2315, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6471-3.
Dates: 2015-07-29 au 2015-07-29.
Horaire: 19 h.
Cout: Gratuit.

Titre: Confidence de la musique romantique.
Categorie: Musique.
Lieu: Eglise Saint-Louis-de-Courville.
Adresse: 2315, avenue Royale.
Arrondissement: Beauport.
Telephone: 418-641-6471-3.
Dates: 2015-08-05 au 2015-08-05.
Horaire: 19 h.
Cout: Gratuit.

Titre: For me Formidable.
Categorie: Musique.
Lieu: Centre de loisirs Monseigneur-De Laval.
Adresse: 2, rue du Fargy.
Arrondissement: Beauport.
Telephone: 418-666-3349.
Dates: 2015-08-07 au 2015-08-07.
Horaire: 20 h.
Cout: Autre.

Titre: Barbara, le temps d'une chanson.
Categorie: Musique.
Lieu: Centre de loisirs Monseigneur-De Laval.
Adresse: 2, rue du Fargy.
Arrondissement: Beauport.
Telephone: 418-666-3349.
Dates: 2015-08-21 au 2015-08-21.
Horaire: 20 h.
Cout: Autre.


Voulez-vous sauvegarder le resultat de recherche?
1- Dans un fichier 'Resultats.txt'.
2- Non merci!.
Veuillez choisir une option (1 ou 2):? 1

Veuillez consulter le fichier 'Resultats.txt' dans votre repertoire courant!

Merci est au revoir!

val app : Tp2e15.app_sysevenements = <obj>
*)

let app = new app_sysevenements "EVENEMENT.CSV" true;;

(* Résultat:
"Affichage d'une fenêtre graphique (voir énoncé du TP)."
Recherche dans les donnees ouvertes de la ville de Quebec.
Merci et au revoir!
val app : Tp2e15.app_sysevenements = <obj>
*)



(***************************************)
(* Verification des messages d'erreurs *)
(***************************************)


let se = new syseve_quebec "les donnees ouvertes" "la ville de Quebec";;
let e2 = new evenement ["Animation";"Journee de la nature";"2015-07-05";"2015-07-25";"Tous les jours de 10 h a 15 h";"0";
			"Animation lors des randonnees, presence de stands d'information sur l'interpretation de la nature";
			"";"418-641-6282";"";"";"";"Base de plein air de Sainte-Foy";"";"3180, rue Laberge";
			"418-641-6282";"Sainte-Foy-Sillery-Cap-Rouge"];;

se#ajouter_evenement e;;

try 
  se#supprimer_evenement e2
with
  Failure s -> print_endline s;;

(* Résultat:
Le systeme d'evenements ne contient pas cet evenement
- : unit = ()
*)

let se = new syseve_quebec "les donnees ouvertes" "la ville de Quebec";;

try 
  se#afficher_systeme_evenements
with
  Failure s -> print_endline s;;

(* Résultat:
Le systeme d'evenements est vide
- : unit = ()
*)


try 
  se#charger_donnees_sysevenements "TestTP2.CSV"
with
  Sys_error s -> print_endline s;;

(* Résultat:
TestTP2.CSV: No such file or directory
- : unit = ()
*)

try 
  ignore (se#trouver_selon_arrondissement "test")
with
  Failure s -> print_endline s;;

(* Résultat:
Le systeme d'evenements est vide
- : unit = ()
*)


try 
  ignore (se#trouver_selon_categorie "test")
with
  Failure s -> print_endline s;;

(* Résultat:
Le systeme d'evenements est vide
- : unit = ()
*)


try 
  ignore (se#lister_arrondissements)
with
  Failure s -> print_endline s;;

(* Résultat:
Le systeme d'evenements est vide
- : unit = ()
*)


try 
  ignore (se#lister_categories_evenements)
with
  Failure s -> print_endline s;;

(* Résultat:
Le systeme d'evenements est vide
- : unit = ()
*)


try 
  se#trier_evenements 4
with
  Failure s -> print_endline s;;

(* Résultat:
trier_evenements: ordre incorrect!
- : unit = ()
*)

let fluxg = open_out "Resultats2.txt";;

try 
  app#sauvegarder_liste_evenements [] fluxg
with
  Failure s -> print_endline s;;

close_out fluxg;;

(* Résultat:
val fluxg : out_channel = <abstr>
La liste d'evenements est vide
- : unit = ()
- : unit = ()
*)


try 
  app#lancer_systeme_evenements
with
  Failure s -> print_endline s;;

(* Résultat:
Bienvenue a l'outil de recherche d'événements
Recherche dans les donnees ouvertes de la ville de Quebec.
Quelle categorie d'evenements vous interessent?
0 - Divers
1 - Musique
2 - Activites estivales
3 - Magie
4 - Activite familiale
5 - Activite petit budget
6 - Animation
7 - Artisanat
8 - Arts visuels
9 - Atelier artistique
10 - Exposition
11 - Patrimoine
12 - Atelier culinaire
13 - Fete nationale du Quebec
14 - Festival
15 - Contes
16 - Danse
17 - Histoire animee
18 - Theatre
19 - Activite multiculturelle
20 - Arts de la rue
21 - Cinema
22 - Visite guidee
23 - Archeologie
24 - Atelier scientifique
25 - Fete de quartier
26 - Conference
27 - Opera
28 - Parc Lineaire
29 - Sports
30 - Jeu
31 - Marionnettes
32 - Activite litteraire
33 - Eveil a la lecture
34 - Vente de livres
35 - Humour
36 - Cirque
37 - Atelier d'ecriture
38 - Fete
39 - Defi Sante
40 - Tous 
Veuillez entrer un nombre entre 0 et 40:? 41
Nombre incorrect, veuillez recommencer!
- : unit = ()
*)
