(***********************************************************************)
(* Langages de Programmation: IFT 3000 NRC 51158                       *)
(* TP2 �T� 2015. Date limite: Mercredi 15 juillet � 17h                *) 
(* Implanter un syst�me permettant de chercher des �v�nements          *)
(* en utilisant les donn�es ouvertes de la ville de Qu�bec             *)
(***********************************************************************)
(*                                                                     *)
(* NOM: Landry_____________________ PR�NOM: David_____________________ *) 
(* MATRICULE: 111 040 494__________ PROGRAMME: IFT____________________ *)
(*                                                                     *)
(***********************************************************************)
(*                                                                     *)
(* NOM: ___________________________ PR�NOM:___________________________ *) 
(* MATRICULE: _____________________ PROGRAMME: _______________________ *)
(*                                                                     *)
(***********************************************************************)

#load "unix.cma";; (* Charger le module unix *)
#load "str.cma";;  (* Charger le module Str  *)
#directory "+labltk";; 
#load "labltk.cma";;  (* Charger le module labltk  *)

(* Charger la signature du syst�me d'activit�s *)
#use "TP2-SIG-E2015.mli";;

(********************************************************************) 
(* Implantation du syst�me en utilisant                             *)
(* la programmation orient�e objet                       	    *) 
(********************************************************************)

module Tp2e15 : TP2E15 = struct

  open List
  open Str
  open Tk  

  (* Fonctions manipulant les listes et/ou les cha�nes de caract�res *)

  (* appartient : 'a -> 'a list -> bool                   *)
  (* Retourner si un �l�ment existe ou non dans une liste *)

  let appartient e l = exists (fun x -> x = e) l

  (* enlever : 'a -> 'a list -> 'a list *)
  (* Enlever un �l�ment dans une liste  *)

  let enlever e l = 
    let (l1, l2) = partition (fun x -> x = e) l
    in l2

  (* remplacer : 'a -> 'a -> 'a list -> 'a list       *)
  (* Remplacer un �l�ment par un autre dans une liste *)

  let remplacer e e' l =
    map (fun x -> (if (x = e) then e' else x)) l 

  (* uniques : string list -> string list                         *)
  (* Retourner une liste ne contenant que des �l�ments uniques    *) 
  (* Les cha�nes vides sont �galement enlev�es de la liste        *)
  (* ainsi que les espaces inutiles avant et/ou apr�s les cha�nes *)

  let uniques liste =
    let ltrim = map (fun ch -> String.trim ch) liste in
    let res = ref [] in
    let rec fct l = match l with
     | [] -> !res
     | x::xs -> if (not (mem x !res) && (x <> "")) then res := (!res)@[x]; fct xs
    in fct ltrim

  (* decouper_chaine : string -> string -> string list                          *)
  (* Retourner une liste en d�coupant une cha�ne selon un s�parateur (p.ex "|") *)

  let decouper_chaine chaine separateur = split (regexp separateur) chaine

  (* formater_chaine : string list -> string                                  *)
  (* Construire une cha�ne selon un certain formatage pour les besoins du TP  *)

  let formater_chaine liste = 
    let res = ref "" in
    let n = (length liste) - 1  in
      for i = 0 to n do
	res := !res ^ ((string_of_int i) ^ " - " ^ (nth liste i) ^ "\n")
      done;
      res := !res ^ ((string_of_int (n+1)) ^ " - Tous \n"); !res

   (* lire_fichier : in_channel -> string -> string list list                     *)
   (* Lire un fichier CSV et retourne une lite de listes de cha�nes de caract�res *)
   (* en sp�cifiant le s�parateur qu'il faut utiliser pour d�limiter les cha�nes  *)

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
  (* Retourne le nombre de secondes depuis l'ann�e 1970 jusqu'� une date en prenant  *)
  (* comme heure par d�faut minuit (0:00:00).                                        *)
  (* Exemple: let ep = retourner_epoque_secondes "2015-06-23" "-";;                  *)
  (* val ep : float = 1435032000.                                                    *)

  let retourner_epoque_secondes (date:string) (sdate: string) =
    let d = decouper_chaine date sdate in
    let yyyy = int_of_string (nth d 0) and mm = int_of_string (nth d 1) and dd = int_of_string (nth d 2) in
    let eg = {Unix.tm_sec = 0; tm_min = 0; tm_hour = 0; tm_mday = dd; tm_mon = mm-1;
	      tm_year = yyyy-1900; tm_wday = 0; tm_yday = 0; tm_isdst = false} in fst(Unix.mktime eg)

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

      (* M�thode � implanter *)
      
      (* afficher_evenement : unit *)
      method afficher_evenement = 
        print_string ("Titre: " ^ titre_evenement ^ 
                "\nCategorie: " ^ categorie_evenement ^ 
                "\nLieu: " ^ nomlieu_evenement ^ 
                "\nAdresse: " ^ adresse_evenement ^ 
                "\nArrondissement: " ^ nom_arrondissement ^ 
                "\nTelephone: " ^ tel1_evenement ^ 
                "\nDates: " ^ debut_evenement ^ " au " ^ fin_evenement ^
                "\nHoraire: " ^ horaire_evenement ^ 
                "\nCout: " ^ cout_evenement ^ "\n\n")
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

      (* M�thodes � implanter *)
      
      (* ajouter_evenement : evenement -> unit *)
      method ajouter_evenement (e:evenement) = ()

      (* supprimer_evenement : evenement -> unit *)
      method supprimer_evenement (e:evenement) = ()

      (* afficher_systeme_evenements : unit *)
      method afficher_systeme_evenements = ()

      (* ajouter_liste_evenements : string list list -> unit *)
      method ajouter_liste_evenements (lle:string list list) = ()

      (* charger_donnees_sysevenements : string -> unit *)
      method charger_donnees_sysevenements (fichier:string) = ()

      (* trouver_selon_arrondissement : string -> evenement list *)
      method trouver_selon_arrondissement (na:string) = []

      (* trouver_selon_categorie : string -> evenement list *)
      method trouver_selon_categorie (ge:string) = []

      (* lister_arrondissements : string list *)
      method lister_arrondissements = []

      (* lister_categories_evenements : string list *)
      method lister_categories_evenements = []

      (* trier_evenements : int -> unit *)
      method trier_evenements (ordre:int) = ()
      
      initializer print_string ("Recherche dans " ^ (parent#get_origine_donnees) ^ 
				" de " ^ (self#get_ville_concernee) ^ ".");
				print_newline()


    end

  class app_sysevenements (nf:string) (i:bool) =
    object(self)
      val nom_fichier = nf
      val interface = i

       (* M�thodes � implanter *)
      
      (* sauvegarder_liste_evenements : evenement list -> out_channel -> unit *)     
      method sauvegarder_liste_evenements (le:evenement list) (flux:out_channel) = ()

      (* lancer_systeme_evenements : unit *)
      method lancer_systeme_evenements = ()

      (* lancer_interface_sevenements : unit *)
      method lancer_interface_sevenements =
	     (* � compl�ter *)
	     let top = openTk () in
	     Wm.title_set top "Syst�me d'�v�nements";
	     Wm.geometry_set top "370x580";
	     let l1 = Label.create ~text:"Bienvenue a l'outil de recherche d'�v�nements" top in
             pack [l1];
	     let _ = Printexc.print mainLoop () in
	     print_endline "Merci et au revoir!"

      initializer if interface then self#lancer_interface_sevenements else self#lancer_systeme_evenements

    end

end
