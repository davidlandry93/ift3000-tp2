(***********************************************************************)
(* Langages de Programmation: IFT 3000 NRC 51158                       *)
(* TP2 ÉTÉ 2015. Date limite: Mercredi 15 juillet à 17h                *) 
(* Implanter un système permettant de chercher des événements          *)
(* en utilisant les données ouvertes de la ville de Québec             *)
(***********************************************************************)
(*                                                                     *)
(* NOM: Landry_____________________ PRÉNOM: David_____________________ *) 
(* MATRICULE: 111 040 494__________ PROGRAMME: IFT____________________ *)
(*                                                                     *)
(***********************************************************************)
(*                                                                     *)
(* NOM: ___________________________ PRÉNOM:___________________________ *) 
(* MATRICULE: _____________________ PROGRAMME: _______________________ *)
(*                                                                     *)
(***********************************************************************)

#load "unix.cma";; (* Charger le module unix *)
#load "str.cma";;  (* Charger le module Str  *)
#directory "+labltk";; 
#load "labltk.cma";;  (* Charger le module labltk  *)

(* Charger la signature du système d'activités *)
#use "TP2-SIG-E2015.mli";;

(********************************************************************) 
(* Implantation du système en utilisant                             *)
(* la programmation orientée objet                       	    *) 
(********************************************************************)

module Tp2e15 : TP2E15 = struct

  open List
  open Str
  open Tk  

  (* Fonctions manipulant les listes et/ou les chaînes de caractères *)

  (* appartient : 'a -> 'a list -> bool                   *)
  (* Retourner si un élément existe ou non dans une liste *)

  let appartient e l = exists (fun x -> x = e) l

  (* enlever : 'a -> 'a list -> 'a list *)
  (* Enlever un élément dans une liste  *)

  let enlever e l = 
    let (l1, l2) = partition (fun x -> x = e) l
    in l2

  (* remplacer : 'a -> 'a -> 'a list -> 'a list       *)
  (* Remplacer un élément par un autre dans une liste *)

  let remplacer e e' l =
    map (fun x -> (if (x = e) then e' else x)) l 

  (* uniques : string list -> string list                         *)
  (* Retourner une liste ne contenant que des éléments uniques    *) 
  (* Les chaînes vides sont également enlevées de la liste        *)
  (* ainsi que les espaces inutiles avant et/ou après les chaînes *)

  let uniques liste =
    let ltrim = map (fun ch -> String.trim ch) liste in
    let res = ref [] in
    let rec fct l = match l with
     | [] -> !res
     | x::xs -> if (not (mem x !res) && (x <> "")) then res := (!res)@[x]; fct xs
    in fct ltrim

  (* decouper_chaine : string -> string -> string list                          *)
  (* Retourner une liste en découpant une chaîne selon un séparateur (p.ex "|") *)

  let decouper_chaine chaine separateur = split (regexp separateur) chaine

  (* formater_chaine : string list -> string                                  *)
  (* Construire une chaîne selon un certain formatage pour les besoins du TP  *)

  let formater_chaine liste = 
    let res = ref "" in
    let n = (length liste) - 1  in
      for i = 0 to n do
	res := !res ^ ((string_of_int i) ^ " - " ^ (nth liste i) ^ "\n")
      done;
      res := !res ^ ((string_of_int (n+1)) ^ " - Tous \n"); !res

   (* lire_fichier : in_channel -> string -> string list list                     *)
   (* Lire un fichier CSV et retourne une lite de listes de chaînes de caractères *)
   (* en spécifiant le séparateur qu'il faut utiliser pour délimiter les chaînes  *)

   let rec lire_fichier (flux:in_channel) (separateur:string) =
     let read_line ic =
       try
  	input_line ic (* Lire la ligne sans le retour de chariot *)
       with End_of_file -> "" 
     in
       let ligne = read_line flux in
       match ligne with
	 | "" -> []
	 | s -> (decouper_chaine s separateur)::(lire_fichier flux separateur)

  (* retourner_epoque_secondes : string -> string -> string -> string -> float       *)
  (* Retourne le nombre de secondes depuis l'année 1970 jusqu'à une date en prenant  *)
  (* comme heure par défaut minuit (0:00:00).                                        *)
  (* Exemple: let ep = retourner_epoque_secondes "2015-06-23" "-";;                  *)
  (* val ep : float = 1435032000.                                                    *)

  let retourner_epoque_secondes (date:string) (sdate: string) =
    let d = decouper_chaine date sdate in
    let yyyy = int_of_string (nth d 0) and mm = int_of_string (nth d 1) and dd = int_of_string (nth d 2) in
    let eg = {Unix.tm_sec = 0; tm_min = 0; tm_hour = 0; tm_mday = dd; tm_mon = mm-1;
	      tm_year = yyyy-1900; tm_wday = 0; tm_yday = 0; tm_isdst = false} in fst(Unix.mktime eg)


    let string_of_evenement x = "Titre: " ^ x#get_titre_evenement ^ 
        "\nCategorie: " ^ x#get_categorie_evenement ^ 
        "\nLieu: " ^ x#get_nomlieu_evenement ^ 
        "\nAdresse: " ^ x#get_adresse_evenement ^ 
        "\nArrondissement: " ^ x#get_nom_arrondissement ^ 
        "\nTelephone: " ^ x#get_tel1_evenement ^ 
        "\nDates: " ^ x#get_debut_evenement ^ " au " ^ x#get_fin_evenement ^
        "\nHoraire: " ^ x#get_horaire_evenement ^ 
        "\nCout: " ^ x#get_cout_evenement ^ "\n\n"

  (* Classes du TP *)

  class evenement (lch:string list) = 
    object(self)
      val categorie_evenement : string = nth lch 0
      val titre_evenement : string = nth lch 1
      val debut_evenement : string = nth lch 2
      val fin_evenement : string = nth lch 3
      val horaire_evenement : string = nth lch 4
      val cout_evenement : string = nth lch 5
      val description_evenement : string = nth lch 6
      val renseignement_evenement : string = nth lch 7
      val tel1_evenement : string = nth lch 8
      val tel2_evenement : string = nth lch 9
      val courriel_evenement : string = nth lch 10
      val url_evenement : string = nth lch 11
      val nomlieu_evenement : string = nth lch 12
      val complement_lieu_evenement : string = nth lch 13
      val adresse_evenement : string = nth lch 14
      val tel_lieu : string = nth lch 15
      val nom_arrondissement : string = nth lch 16

      method get_categorie_evenement = categorie_evenement
      method get_titre_evenement = titre_evenement
      method get_debut_evenement = debut_evenement
      method get_fin_evenement = fin_evenement
      method get_horaire_evenement = horaire_evenement
      method get_cout_evenement = cout_evenement
      method get_description_evenement = description_evenement
      method get_renseignement_evenement = renseignement_evenement
      method get_tel1_evenement = tel1_evenement
      method get_tel2_evenement = tel2_evenement
      method get_courriel_evenement = courriel_evenement
      method get_url_evenement = url_evenement
      method get_nomlieu_evenement = nomlieu_evenement
      method get_complement_lieu_evenement = complement_lieu_evenement
      method get_adresse_evenement = adresse_evenement
      method get_tel_lieu = tel_lieu
      method get_nom_arrondissement = nom_arrondissement

      (* Méthode à implanter *)
      
      (* afficher_evenement : unit *)
      method afficher_evenement = 
        print_string (string_of_evenement self)
    end

  class sysevenements (od:string) =
    object
	val origine_donnees : string = od 
	method get_origine_donnees = origine_donnees
    end


  class syseve_quebec (od:string) (vc:string) =
    object(self)
      inherit sysevenements od as parent
      val ville_concernee : string = vc
      val mutable liste_evenements : evenement list = []
      method get_ville_concernee = ville_concernee
      method get_liste_evenements = liste_evenements
      method set_liste_evenements (le:evenement list) = liste_evenements <- le
      method evenement_existe (e:evenement) = appartient e liste_evenements
      method retourner_nbr_evenements = length liste_evenements

      (* Méthodes à implanter *)
      
      (* ajouter_evenement : evenement -> unit *)
      method ajouter_evenement (e:evenement) = 
          liste_evenements <- (e :: liste_evenements)

      (* supprimer_evenement : evenement -> unit *)
      method supprimer_evenement (e:evenement) = 
          liste_evenements <- (enlever e liste_evenements)

      (* afficher_systeme_evenements : unit *)
      method afficher_systeme_evenements = 
          iter (fun x -> x#afficher_evenement) liste_evenements

      (* ajouter_liste_evenements : string list list -> unit *)
      method ajouter_liste_evenements (lle:string list list) = 
          let f x = liste_evenements <- (new evenement x) :: liste_evenements in
          iter f lle

      (* charger_donnees_sysevenements : string -> unit *)
      method charger_donnees_sysevenements (fichier:string) = 
          let flux = open_in fichier in
          let liste = 
              match (lire_fichier flux "|") with 
              | [] -> [] 
              | x::xs -> xs  (* Retirer la tete de la liste parce que c'est le header du csv *)
          in
          self#ajouter_liste_evenements liste

      (* trouver_selon_arrondissement : string -> evenement list *)
      method trouver_selon_arrondissement (na:string) = 
          filter (fun x -> x#get_nom_arrondissement = na) liste_evenements

      (* trouver_selon_categorie : string -> evenement list *)
      method trouver_selon_categorie (ge:string) = 
          filter (fun x -> x#get_categorie_evenement = ge) liste_evenements

      (* lister_arrondissements : string list *)
      method lister_arrondissements = 
          uniques (map (fun e -> e#get_nom_arrondissement) liste_evenements)

      (* lister_categories_evenements : string list *)
      method lister_categories_evenements = 
          uniques (map (fun e -> e#get_categorie_evenement) liste_evenements)

      (* trier_evenements : int -> unit *)
      method trier_evenements (ordre:int) = 
          let f = match ordre with
            | 1 -> fun lhs rhs -> 
                    compare 
                        (retourner_epoque_secondes lhs#get_debut_evenement "-")
                        (retourner_epoque_secondes rhs#get_debut_evenement "-")
            | 2 -> fun lhs rhs -> 
                    compare 
                        (retourner_epoque_secondes lhs#get_fin_evenement "-")
                        (retourner_epoque_secondes rhs#get_fin_evenement "-")
            | 3 -> fun lhs rhs -> 
                    compare lhs#get_cout_evenement rhs#get_cout_evenement
            | _ -> failwith "trier_evenements: ordre incorrect!"
          in
          liste_evenements <- sort f liste_evenements
      
      initializer print_string ("Recherche dans " ^ (parent#get_origine_donnees) ^ 
				" de " ^ (self#get_ville_concernee) ^ ".");
				print_newline()


    end

  class app_sysevenements (nf:string) (i:bool) =
    object(self)
      val nom_fichier = nf
      val interface = i
      val sys_evenements = new syseve_quebec "toto" "tata"

       (* Méthodes à implanter *)
      
      (* sauvegarder_liste_evenements : evenement list -> out_channel -> unit *)     
      method sauvegarder_liste_evenements (le:evenement list) (flux:out_channel) = 
          match le with
          | [] -> failwith "La liste d'evenements est vide"
          | _ -> 
            iter (fun y -> output_string flux (string_of_evenement y)) le

      (* lancer_systeme_evenements : unit *)
      method lancer_systeme_evenements = ()

      (* lancer_interface_sevenements : unit *)
      method lancer_interface_sevenements =
        (* À compléter *)
        let top = openTk () in
        Wm.title_set top "Système d'événements";
        Wm.geometry_set top "370x580";
        let tv1 = Textvariable.create () 
        and tv2 = Textvariable.create () in
        let l1 = Label.create ~text:"Bienvenue a l'outil de recherche d'événements" top in
        let lb1 = Listbox.create ~selectmode:`Single top 
        and lb2 = Listbox.create ~selectmode:`Single top in

        (* The buttons to update the selected values from the lisboxes *)
        let b1 = 
            let liste = sys_evenements#lister_arrondissements in
            Button.create 
                ~text:"Afficher arrondissement" 
                ~command:(fun () ->
                    try 
                        let n = 
                            match (hd (Listbox.curselection lb1)) with
                            | `Num y -> y
                            | _ -> failwith "pas de selection"
                        in
                        Textvariable.set tv1 (nth liste n)
                    with _ -> failwith "pas de selection")
                top in
        let b2 = 
            let liste = sys_evenements#lister_categories_evenements in
            Button.create 
                ~text:"Afficher categorie" 
                ~command:(fun () ->
                    try 
                        let n = 
                            match (hd (Listbox.curselection lb2)) with
                            | `Num y -> y
                            | _ -> failwith "pas de selection"
                        in
                        Textvariable.set tv2 (nth liste n)
                    with _ -> failwith "pas de selection")
                top in

        (* The button to show the results. *)

        (* Text labels *)
        let l2 = Label.create ~text:"Arrondissement selectionne" top 
        and l3 = Label.create ~text:"Categorie selectionnee" top in
        let labelArron = Label.create ~textvariable:tv1 top
        and labelCat = Label.create ~textvariable:tv2 top in

        (* Fill the listboxes *)
        Listbox.insert ~index:`End ~texts:sys_evenements#lister_arrondissements lb1;
        Listbox.insert ~index:`End ~texts:sys_evenements#lister_categories_evenements lb2;

        (* Initialize the selected values *)
        Textvariable.set tv1 "?"; Textvariable.set tv2 "?";

        (* Fill the window *)
        pack [coe l1; coe lb1; coe lb2; coe b1; coe b2; coe l2; coe labelArron;
                coe l3; coe labelCat];
        let _ = Printexc.print mainLoop () in
        print_endline "Merci et au revoir!"

        initializer 
            (sys_evenements#charger_donnees_sysevenements nf);
            if interface 
            then self#lancer_interface_sevenements 
            else self#lancer_systeme_evenements

    end

end
