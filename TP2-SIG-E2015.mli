(***********************************************************************)
(* Langages de Programmation: IFT 3000 NRC 51158                       *)
(* TP2 ÉTÉ 2015. Date limite: Mercredi 15 juillet à 17h                *) 
(* Implanter un système permettant de chercher des événements          *)
(* en utilisant les données ouvertes de la ville de Québec             *)
(***********************************************************************) 
(* Signature du TP2                                                    *)  
(***********************************************************************)

module type TP2E15 = sig

  class evenement : string list ->
    object
      val categorie_evenement : string
      val titre_evenement : string
      val debut_evenement : string
      val fin_evenement : string
      val horaire_evenement : string
      val cout_evenement : string
      val description_evenement : string
      val renseignement_evenement : string
      val tel1_evenement : string
      val tel2_evenement : string
      val courriel_evenement : string
      val url_evenement : string
      val nomlieu_evenement : string
      val complement_lieu_evenement : string
      val adresse_evenement : string
      val tel_lieu : string
      val nom_arrondissement : string
      method get_categorie_evenement : string
      method get_titre_evenement : string
      method get_debut_evenement : string
      method get_fin_evenement : string
      method get_horaire_evenement : string
      method get_cout_evenement : string
      method get_description_evenement : string
      method get_renseignement_evenement : string
      method get_tel1_evenement : string
      method get_tel2_evenement : string
      method get_courriel_evenement : string
      method get_url_evenement : string
      method get_nomlieu_evenement : string
      method get_complement_lieu_evenement : string
      method get_adresse_evenement : string
      method get_tel_lieu : string
      method get_nom_arrondissement : string
      method afficher_evenement : unit
    end

   class sysevenements : string ->
     object
	val origine_donnees : string 
	method get_origine_donnees : string
     end

   class syseve_quebec : string -> string ->
     object
      inherit sysevenements
      val ville_concernee : string
      val mutable liste_evenements : evenement list
      method get_ville_concernee : string
      method get_liste_evenements : evenement list
      method set_liste_evenements : evenement list -> unit
      method evenement_existe : evenement -> bool
      method retourner_nbr_evenements : int
      method ajouter_evenement : evenement -> unit
      method supprimer_evenement : evenement -> unit
      method afficher_systeme_evenements : unit
      method ajouter_liste_evenements : string list list -> unit
      method charger_donnees_sysevenements : string -> unit
      method trouver_selon_arrondissement : string -> evenement list      
      method trouver_selon_categorie : string -> evenement list
      method lister_arrondissements : string list
      method lister_categories_evenements : string list
      method trier_evenements : int -> unit
    end

  class app_sysevenements : string -> bool ->
    object
      val nom_fichier : string
      val interface : bool
      method sauvegarder_liste_evenements : evenement list -> out_channel -> unit
      method lancer_systeme_evenements : unit
      method lancer_interface_sevenements : unit
    end

end
