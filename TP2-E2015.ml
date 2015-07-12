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
(* NOM: Bouchard___________________ PRÉNOM: Frédéric__________________ *) 
(* MATRICULE: 111 086 988__________ PROGRAMME: IFT____________________ *)
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

  (* Classes du TP *)

  
  let string_of_evenement x = 
    let cout y = match y with
    | "0" -> "Gratuit"
    | "1" -> "Coût à l'entrée"
    | "2" -> "Autre"
    | _ -> "Pas affiché"
    in
       "Titre: " ^ x#get_titre_evenement ^ ".\n" ^
       "Categorie: " ^ x#get_categorie_evenement ^ ".\n" ^ 
       "Lieu: " ^ x#get_nomlieu_evenement ^ ".\n" ^
       "Adresse: " ^ x#get_adresse_evenement ^ ".\n" ^
       "Arrondissement: " ^ x#get_nom_arrondissement ^ ".\n" ^
       "Telephone: " ^ x#get_tel1_evenement ^ ".\n" ^
       "Dates: " ^ x#get_debut_evenement ^ " au " ^ x#get_fin_evenement ^ ".\n" ^
       "Horaire: " ^ x#get_horaire_evenement ^ ".\n" ^
       "Cout: " ^ cout(x#get_cout_evenement) ^ ".\n\n"

  let string_of_event_list x = 
      let init_acc = "Nombre d'évènements trouvés: " ^ 
            (string_of_int (length x)) ^ "\n\n"
      in
      fold_left (fun a -> (fun e -> a ^ (string_of_evenement e))) init_acc x 

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
	print_string(string_of_evenement self)
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
         if(appartient e liste_evenements) then liste_evenements <- (enlever e liste_evenements)
	 else failwith "Le systeme d'evenements ne contient pas cet evenement"

      (* afficher_systeme_evenements : unit *)
      method afficher_systeme_evenements = 
	  if(length liste_evenements <> 0) then iter (fun x -> x#afficher_evenement) liste_evenements
	  else failwith "Le systeme d'evenements est vide"

      (* ajouter_liste_evenements : string list list -> unit *)
      method ajouter_liste_evenements (lle:string list list) = 
          let f x = liste_evenements <- liste_evenements @ [new evenement x] in
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
	  if(length liste_evenements <> 0) then filter (fun x -> x#get_nom_arrondissement = na) liste_evenements
	  else failwith "Le systeme d'evenements est vide"

      (* trouver_selon_categorie : string -> evenement list *)
      method trouver_selon_categorie (ge:string) = 
          if(length liste_evenements <> 0) then filter (fun x -> x#get_categorie_evenement = ge) liste_evenements
	  else failwith "Le systeme d'evenements est vide"

      (* lister_arrondissements : string list *)
      method lister_arrondissements = 
          if(length liste_evenements <> 0) then uniques (map (fun e -> e#get_nom_arrondissement) liste_evenements)
	  else failwith "Le systeme d'evenements est vide"

      (* lister_categories_evenements : string list *)
      method lister_categories_evenements = 
         if(length liste_evenements <> 0) then uniques (map (fun e -> e#get_categorie_evenement) liste_evenements)
	 else failwith "Le systeme d'evenements est vide"

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
      val sys_evenements = new syseve_quebec "les donnees ouvertes" "la ville de Quebec"

       (* Méthodes à implanter *)
      
      (* sauvegarder_liste_evenements : evenement list -> out_channel -> unit *)     
      method sauvegarder_liste_evenements (le:evenement list) (flux:out_channel) = 
          match le with
          | [] -> failwith "La liste d'evenements est vide"
          | _ ->  iter (fun y -> output_string flux (string_of_evenement y)) le

      (* lancer_systeme_evenements : unit *)
      method lancer_systeme_evenements =
	ignore (print_string "Quelle categorie d'evenements vous interessent?\n");
	let arrondissements = sys_evenements#lister_arrondissements
        and categories = sys_evenements#lister_categories_evenements
	and failPhrase = "Nombre incorrect, veuillez recommencer!" in
	   ignore (print_string (formater_chaine categories));
	   ignore (print_string ("Veuillez entrer un nombre entre 0 et "));
	   ignore (print_string (string_of_int (length categories) ^ ":? "));
	   flush stdout;

	  (* choix = Numéro de la catégorie désirée *)
	  let choix = 
	    int_of_string(input_line stdin)
	  in
	    if(choix > length categories || choix < 0)
		then failwith failPhrase

	    (* choix2 = Numéro de l'arrondissement désiré *)
	    else let choix2 =
	      ignore (print_string "Quel arrondissement vous interesse?\n");
	      ignore (print_string (formater_chaine arrondissements));
	      ignore (print_string ("Veuillez entrer un nombre entre 0 et "));
	      ignore (print_string (string_of_int (length arrondissements) ^ ":? "));
	      flush stdout;
	      int_of_string(input_line stdin)
	    in
	      if(choix2 > length arrondissements || choix2 < 0)
		  then failwith failPhrase
	      else
		let requete c1 c2 = match c1, c2 with
                    | (x, y) when (x = length categories && y = length arrondissements) -> sys_evenements#get_liste_evenements
		    | (x, _) when (x = length categories) -> sys_evenements#trouver_selon_arrondissement (nth arrondissements choix2)
		    | (_, y) when (y = length arrondissements) -> sys_evenements#trouver_selon_categorie (nth categories choix)
		    | (_, _) -> fst(partition(fun x -> appartient x (sys_evenements#trouver_selon_categorie (nth categories choix))) (sys_evenements#trouver_selon_arrondissement (nth arrondissements choix2)))
		in let resultats = requete choix choix2 in
		   if(length resultats < 1)
		       then failwith "Votre recherche n'a donnée aucun résultat"
		   else
		     iter(fun x -> x#afficher_evenement) resultats;
		     ignore (print_string ("Nombre d'evenements trouves: " ^ (string_of_int (length resultats)) ^ "\n\n"));
		     ignore (print_string ("Voulez-vous trier le resultat de la recherche?\n" ^					                              "1 - Selon la date de debut.\n" ^
					   "2 - Selon la date de fin.\n" ^
					   "3 - Selon le cout de l'evenement.\n" ^
					   "4 - Non, merci!.\n" ^
					   "Veuillez choisir une option (1 a 4) :? "));

		     (* choix3 = Option de tri *)
		     let choix3 = 
		       flush stdout;
		       int_of_string(input_line stdin)

		     (* backup_event est une variable utilisé pour sauvegarder l'ancienne liste d'événements
			car cette liste sera altéré par le traitement suivant. Ainsi, à la fin de l'exécution
			pour s'assurer que la recherche ne commette pas d'effets de bord, nous redéclareront la
			liste d'événements comme étant cette liste de soutient... 
			
			Il aurait été préférable de faire d'autres fonctions pour l'accès, mais pour ne pas
			ajouter d'autres fonctions que celles requises pas le TP, nous utiliserons cette
			technique. *)
		     and backup_events = sys_evenements#get_liste_evenements
		     in
		       if(choix3 > 4 || choix3 < 0)
			   then failwith failPhrase
		       else
			 if(choix3 < 4)
			     then begin
			        ignore (print_string "Voici la liste triee:\n\n");
		                sys_evenements#set_liste_evenements resultats;
		                sys_evenements#trier_evenements choix3;
		                sys_evenements#afficher_systeme_evenements
			     end;

			 ignore (print_string ("Voulez-vous sauvegarder le resultat de recherche?\n" ^
					       "1 - Dans un fichier 'Resultat.txt'.\n" ^
					       "2 - Non merci!.\n" ^
					       "Veuillez choisir une option (1 ou 2) :? "));

		         (* choix4 = Exportation des résultats de recherche *)
		         let choix4 = 
			   flush stdout;
			   int_of_string(input_line stdin)
			 in
			    if(choix4 > 2 || choix4 < 0)
				then
			             if(choix3 < 4) then begin
				       sys_evenements#set_liste_evenements backup_events;
			               failwith failPhrase
				     end else failwith failPhrase
			    else 
			      if(choix4 < 2) then begin
				 let monFichier = open_out "Resultat.txt" in
			             self#sauvegarder_liste_evenements sys_evenements#get_liste_evenements monFichier;
                                     close_out monFichier;
                                     print_string "\nVeuillez consulter le fichier 'Resultats.txt' dans votre repertoire courant!\n"
			     end;

			     print_string "\nMerci et au revoir!\n\n";
				
			     if(choix3 < 4) then sys_evenements#set_liste_evenements backup_events

      (* lancer_interface_sevenements : unit *)
      method lancer_interface_sevenements =
        (* À compléter *)
        let top = openTk () in
        Wm.title_set top "Système d'événements";
        Wm.geometry_set top "370x580";
        let tv1 = Textvariable.create () 
        and tv2 = Textvariable.create () in
        let l1 = Label.create ~text:"Bienvenue a l'outil de recherche d'événements" top in
        let lb1 = Listbox.create
		 ~selectmode:`Single
		 ~width:370
                 top
        and lb2 = Listbox.create 
		  ~selectmode:`Single 
		  ~width:370 
                  top in
        let arrondissement_selectionne = ref "" 
        and categorie_selectionnee = ref "" in

        let daughter =
            let d = Toplevel.create top in
            begin destroy d; ref d end
        and b_spawn_daughter = Button.create ~text:"Afficher le résultat" top
        in
        let make_daughter () =
            let d = Toplevel.create top in
            begin
                let scr = Scrollbar.create ~width:10 d in
                let topLabel = Label.create ~text:"Résultats de recherche" d
                and resultBox = Text.create 
                    ~width:570 
                    ~height:80
                    ~yscrollcommand:(Scrollbar.set scr)
                    d
                in
                let eventList = 
                    match !arrondissement_selectionne, !categorie_selectionnee with
                    | "","" -> sys_evenements#get_liste_evenements
                    | "", cat -> (sys_evenements#trouver_selon_categorie cat)
                    | arr, "" -> 
                        (sys_evenements#trouver_selon_arrondissement arr)
                    | arr, cat -> 
                        filter 
                            (fun x -> x#get_nom_arrondissement = arr)
                            (sys_evenements#trouver_selon_categorie cat)
                in
                Wm.title_set d "Résultat de recherche";
                Wm.geometry_set d "500x780";
                Text.insert (`End,[]) (string_of_event_list eventList) resultBox;
                Scrollbar.configure ~command:(Text.yview resultBox) scr;
		pack ~side:`Left [coe scr] ~fill:`Y;
                pack ~side:`Top [topLabel] ~fill:`X;
                pack ~side:`Right [coe resultBox] ~fill:`X;
                daughter := d
            end
        in

        (* The button to show the results. *)

        (* Text labels *)
        let l2 = Label.create ~text:"Arrondissement selectionné" top 
        and l3 = Label.create ~text:"Catégorie selectionnée" top in
        let labelArron = Label.create ~textvariable:tv1 top
        and labelCat = Label.create ~textvariable:tv2 top in

	(* The buttons to update the selected values from the lisboxes *)
        let b1 = 
            let liste = sys_evenements#lister_arrondissements in
            Button.create 
                ~text:"Afficher arrondissement" 
                ~width:20
                ~command:(fun () ->
                    try 
                        let n = 
                            match (hd (Listbox.curselection lb1)) with
                            | `Num y -> y
                            | _ -> failwith "Pas de selection"
                        in
                        arrondissement_selectionne := (nth liste n);
                        Textvariable.set tv1 (nth liste n)
                    with _ -> failwith "Pas de selection")
                top in
        let b2 = 
            let liste = sys_evenements#lister_categories_evenements in
            Button.create 
                ~text:"Afficher categorie" 
                ~width:15
                ~command:(fun () ->
                    try 
                        let n = 
                            match (hd (Listbox.curselection lb2)) with
                            | `Num y -> y
                            | _ -> failwith "Pas de selection"
                        in
                        categorie_selectionnee := (nth liste n);
                        Textvariable.set tv2 (nth liste n)
                    with _ -> failwith "Pas de selection")
                top in

        (* Configure the buttons. *)
        Button.configure b_spawn_daughter 
               ~command:(make_daughter)
               ~width:15; 

        (* Fill the listboxes *)
        Listbox.insert ~index:`End ~texts:sys_evenements#lister_arrondissements lb1;
        Listbox.insert ~index:`End ~texts:sys_evenements#lister_categories_evenements lb2;

        (* Initialize the selected values *)
        Textvariable.set tv1 "?"; Textvariable.set tv2 "?";

        (* Fill the window *)
        pack [coe l1; coe lb1; coe lb2; coe l2; coe labelArron;
                coe l3; coe labelCat] ~fill:`X;
	pack [coe b1; coe b2; coe b_spawn_daughter] ~side:`Left ~expand:true ;

        let _ = Printexc.print mainLoop () in
        print_endline "Merci et au revoir!"

        initializer 
            (if (not interface)
            then begin
	      (print_string "Bienvenue a l'outil de recherche d'événements\n");
              (sys_evenements#charger_donnees_sysevenements nf);
	      self#lancer_systeme_evenements
            end else begin
	      (sys_evenements#charger_donnees_sysevenements nf);
	      self#lancer_interface_sevenements
            end);
    end

end
