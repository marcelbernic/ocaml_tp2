(* --------------------------------------------------------------------------- *)
(* ----------------------- TP2 - IFT-3000 - Hiver 2016 ----------------------- *)
(* --------------------------------------------------------------------------- *)

#use "utiles.ml";;

#directory "+ocamlgraph";;
#load "graph.cmo";;

open Utiles;;
open Date;;

module L = List;;
module H = Hashtbl;;
module G = Graph.Pack.Digraph;;

let poids_max_arete = 24*3600 
(* 1jr pour quitter une station à une autre est impossible *)

let distance_max_pieds = 0.5
(* en km *)

let marche_max_pieds = 1.5
(* en km *)

(* -- Arrêts - stop_times.txt                                                  *)
(* ----------------------------------------------                              *)
class arret liste = 

  object

    val station_id = int_of_string (L.nth liste 3)
    val voyage_id = L.nth liste 0
    val arrivee = heure_a_nbsecs (L.nth liste 1)
    val depart = heure_a_nbsecs (L.nth liste 2)
    val num_sequence = int_of_string (L.nth liste 4)
    val embarque_client = 
      if int_of_string (L.nth liste 5) = 0 then true else false
    val debarque_client = 
      if int_of_string (L.nth liste 6) = 0 then true else false

    method get_station_id = station_id
    method get_voyage_id = voyage_id
    method get_arrivee = arrivee
    method get_depart = depart
    method get_num_sequence = num_sequence
    method get_embarque_client = embarque_client
    method get_debarque_client = debarque_client
  
  end;; 

(* -- Voyages - trips.txt                                                      *)
(* --------------------------------------------------                          *)
type direction = AllerVers of string | RetourVers of string;;

class voyage liste (liste_arrets : arret list) =

  object (self)

    val voyage_id = L.nth liste 2
    val ligne_id = int_of_string (L.nth liste 0)
    val service_id = L.nth liste 1
    val direction = 
      let d = enlever_guillemets (L.nth liste 3) in
      if int_of_string (L.nth liste 5) = 0 then AllerVers d else RetourVers d
    val itineraire_id = int_of_string (L.nth liste 7)
    val embarque_chaise = 
      if int_of_string (L.nth liste 8) = 1 then true else false
    val arrets = 
      L.sort 
        (fun a1 a2 -> compare a1#get_num_sequence a2#get_num_sequence)
        liste_arrets
    val heure_depart = top_liste min (L.map (fun a -> a#get_depart) liste_arrets)
    val heure_fin = top_liste max (L.map (fun a -> a#get_depart) liste_arrets)
    val table_arrets = H.create 100 (* Pour des raisons de performances *)

    method get_voyage_id = voyage_id
    method get_ligne_id = ligne_id
    method get_service_id = service_id
    method get_direction = direction
    method get_itineraire_id = itineraire_id
    method get_embarque_chaise = embarque_chaise
    method get_arrets = arrets
    method get_stations_des_arrets = L.map (fun a -> a#get_station_id) arrets
    method get_heure_depart = heure_depart
    method get_heure_fin = heure_fin
    method get_arret_de_station sid = H.find table_arrets sid
    
    initializer 
      L.iter (fun a -> H.add table_arrets a#get_station_id a) arrets
		
  end;;
	
(* -- Lignes - route.txt ----------------------------------------------------- *)
type type_ligne = MetroBus | Express | LeBus | CoucheTard;;

let new_type_ligne couleur = match couleur with
  | "013888" -> LeBus
  | "E04503" -> Express
  | "97BF0D" -> MetroBus
  | "1A171B" -> CoucheTard
  | "003888" -> CoucheTard
  | other -> print_string other; 
    raise (Failure "La couleur du bus est invalide");;

class ligne liste (liste_voyages: voyage list) =
  
  let reconstruire_itineraire liste_arrets_des_voyages =
    let temp = Hashtbl.create 1000 in 
    let _ = 
      L.iter 
        (fun l -> 
           let len = L.length l in 
  	   let l' = List.combine l (range len) in 
  	   L.iter (fun (k, v) -> H.add temp k v) l'
        ) liste_arrets_des_voyages in
    let cls = cles temp in
    let temp' = L.map (fun c -> c, (top_liste max (H.find_all temp c))) cls
    in let ordre = L.sort (fun (st1, a) (st2, b) -> compare a b) temp' 
    in L.map (fun (st, _) -> st) ordre
  in
  
  let lister_stations_sur_itineraire l_voyages = 
    let temp = H.create 3000 in
    let _ =     
      L.iter 
        (fun v -> match v#get_direction with
  	   | AllerVers d | RetourVers d -> H.add temp d v#get_stations_des_arrets
        ) l_voyages in
    let directions = cles temp in
    L.map 
      (fun direction -> 
         let arr_dir = uniques (H.find_all temp direction)
	 in direction, reconstruire_itineraire arr_dir
      ) directions in
  
  object (self)

    val ligne_id = int_of_string (L.nth liste 0)
    val numero = enlever_guillemets (L.nth liste 2)
    val terminaux = 
      let temp = decouper_chaine 
          (enlever_guillemets 
	     (L.nth liste 4)) " - " in (L.nth temp 0), (L.nth temp 1)
    val le_type = new_type_ligne (L.nth liste 7)
    val stations_sur_itineraire = lister_stations_sur_itineraire liste_voyages
    val voyages = L.map (fun v -> v#get_voyage_id) liste_voyages
    
    method get_type = le_type
    method get_ligne_id = ligne_id
    method get_terminaux = terminaux
    method get_numero = numero
    method get_stations_sur_itineraire = stations_sur_itineraire
    method get_voyages = voyages
    method stations_sur_itineraire_des_voyages (lv: voyage list) = 
      lister_stations_sur_itineraire lv
  
  end;;


(* -- Stations - stops.txt                                                     *)
(* -------------------------------------------------                           *)
class coordonnees lat long =

 object (self: 'a)

   val latitude = (lat : float)
   val longitude = (long : float)

   method get_lat = latitude
   method get_long = longitude
   method is_valid_gps = 
     latitude >= 0. && latitude <= 90. && 
     longitude >= -180. && longitude <= 180.
   method distance (autre_coord: 'a) = 
     let pi = 3.14159265358979323846 in 
     let pi_rad = pi /. 180. in
     let lat1 = latitude *. pi_rad in
     let lon1 = longitude *. pi_rad in
     let lat2 = autre_coord#get_lat *. pi_rad in
     let lon2 = autre_coord#get_long *. pi_rad in
     6378.137 *. acos( (cos lat1) *. (cos lat2) 
              *. (cos (lon2 -. lon1)) +.(sin lat1) *. (sin lat2))
 
 end;;

class station liste (liste_voyages: voyage list) =

  object (self: 'a)

    val station_id = int_of_string (L.nth liste 0)
    val nom = enlever_guillemets (L.nth liste 1)
    val description = enlever_guillemets (L.nth liste 2)
    val coords = 
      let lat = float_of_string (L.nth liste 3) 
      and long = float_of_string (L.nth liste 4) in 
      new coordonnees lat long
    val embarque_chaise = 
      if int_of_string (L.nth liste 7) = 1 then true else false
    val lignes_passantes_ids = 
      uniques (L.map (fun v -> v#get_ligne_id) liste_voyages)
    val voyages_passants = L.map (fun v -> v#get_voyage_id) liste_voyages

    method get_station_id = station_id
    method get_nom = nom
    method get_description = description
    method get_coords = coords
    method get_embarque_chaise = embarque_chaise
    method get_lignes_passantes_ids = lignes_passantes_ids
    method get_voyages_passants = voyages_passants
    method distance (station' : 'a) =
      self#get_coords#distance station'#get_coords 

  end;;
  
	
(* -- Services - calendar_dates.txt                                            *)
(* ----------------------------------------                                    *)
class service liste = 

  object

    val service_id = L.nth liste 0;
    val date = date_a_nbjours (L.nth liste 1);
    
    method get_service_id = service_id
    method get_date = date
      
  end;;

(* -- Modules pour le graphe des stations                                      *)
(* ----------------------------------------                                    *)
module Poids = 
struct
  type edge = G.E.t
  type t = int
  let weight (e : edge) : t = G.E.label e
  let compare = compare
  let add = (+)
  let zero = 0
end

module BF = Graph.Path.BellmanFord (G) (Poids)


(* -- Principale classe du Tp                                                  *)
(* ----------------------------------------                                    *)
class gestionnaireReseau 
   ?(rep = "/home/marcus/Developer/tp2/googletransit/") () =
   (*"/home/etudiant/workspace/tp2/googletransit/";;*)
  
  (* Abréviations *)
  let new_noeud = G.V.create 
  and new_arete = G.E.create 
                    
  (* -- Principale fonctions pour chargement de données                        *)
  and charger_donnees fichier constructor = 
    let f = try open_in fichier with _ -> raise (Erreur "Fichier inacessible") in
    (* on ignore l'entête qui correspond aux titres des colonnes *)
    let _ = try input_line f with _ -> raise (Erreur "Fichier vide") in 
    let rec aux acc =
      match input_line f with
      | s -> aux ((constructor (decouper_chaine s ",")) :: acc)
      | exception End_of_file -> L.rev acc 
    in aux []	
  in
  
  object(self)
        
    (* -- Tables de hachage   *)
    val lignes = H.create 5000
    val stations = H.create 100000
    val voyages = H.create 500000 
    val voyages_par_date = H.create 500000 
    val noeuds_stations = H.create 5000

    (* -- Graphe de stations   *)
    val graphe_stations = G.create () ~size:5000

    initializer 			
      let voyages_par_service = H.create 500000 
      and voyages_par_ligne = H.create 500000 
      and arrets_par_voyage = H.create 3000000 
      and arrets_par_station = H.create 3000000 
      and services = H.create 3000000 
      and identite = (fun x -> x) in
      begin   
        (* Chargement des données au niveaux des arrêts *)
        L.iter 
          (fun (a: arret) -> 
             H.add arrets_par_voyage a#get_voyage_id a;
	     H.add arrets_par_station a#get_station_id a#get_voyage_id
          )
     	  (charger_donnees (rep ^ "stop_times.txt") (new arret));

        (* Chargement des données au niveaux des services *)
        L.iter 
          (fun (v: service) -> H.add services v#get_date v#get_service_id)    
          (charger_donnees (rep ^ "calendar_dates.txt") (new service));

        (* Chargement des données au niveaux des voyages *)
        L.iter 
          (fun lv -> 
             let vid = L.nth lv 2 in
	     let v = new voyage lv (H.find_all arrets_par_voyage vid) in
             begin
	       H.add voyages v#get_voyage_id v;
               H.add voyages_par_service v#get_service_id v#get_voyage_id;
	       H.add voyages_par_ligne v#get_ligne_id v
             end
          ) (charger_donnees (rep ^ "trips.txt") identite);

        (* Ajouts des éléments de la TH voyages_par_date *)
        L.iter 
          (fun date -> 
             let sids = H.find_all services date in 
	     H.add voyages_par_date 
                   date 
	           (L.concat(L.map (H.find_all voyages_par_service) sids))
          ) (cles services);

        (* Chargement des données au niveaux des stations *)
        L.iter 
          (fun ls -> 
             let sid = int_of_string (L.nth ls 0) in
	     let s = new station ls 
                                 (L.map (fun vid -> H.find voyages vid) 
			                (H.find_all arrets_par_station sid)
                                 ) in 
	     H.add stations s#get_station_id s
          ) (charger_donnees (rep ^ "stops.txt") identite);      

        (* Chargement des données au niveaux des lignes *)
        L.iter 
          (fun ll -> 
             let lid = int_of_string (L.nth ll 0) in
	     let l = new ligne ll (H.find_all voyages_par_ligne lid) in
	     H.add lignes l#get_numero l
          ) (charger_donnees (rep ^ "routes.txt") identite);

        (* Construction du graphe de stations *)
        let aretes = 
          H.fold 
            (fun vid v acc -> 
               (tuples_de_voisins v#get_stations_des_arrets)@acc
            ) voyages [] in
        begin
          L.iter 
            (fun sid -> H.add noeuds_stations sid (new_noeud sid)) 
	    self#lister_stations;
          L.iter 
            (fun (s1, s2) -> 
               G.add_edge graphe_stations (H.find noeuds_stations s1) 
                                          (H.find noeuds_stations s2)
            ) (uniques aretes)
        end;	
     
        (* Affichage d'informations utiles *)
        self#print_stats;
        print_endline "CHARGEMENT DES DONNÉES ET CONSTRUCTION DU GRAPHE TERMINÉS" 
      end
    (* Mines *)
      method get_stations = stations
      method get_lignes = lignes
    (*------ *)
    method lister_lignes = cles lignes
    method lister_stations = cles stations
    method nb_lignes = H.length lignes
    method nb_stations = H.length stations
    method nb_aretes = G.nb_edges graphe_stations
    method get_graphe = graphe_stations
    method get_noeuds = noeuds_stations
    method print_stats =
      let open Printf in 
      begin
        printf "Nb stations : %i\n" (self#nb_stations);
        printf "Nb aretes : %i\n" (self#nb_aretes);
        printf "Nb lignes : %i\n" (self#nb_lignes)
      end


    (* ----------------------------------------------------------------------  *)
    (* @Fonction      : ?types:type_ligne list ->(type_ligne * string list)list*)  
    (* @Description   : liste tous les numeros des lignes du réseau  groupés   *)	
    (*			par type dans la liste de types en paramètre           *)
    (* @Precondition  : aucune 						       *)
    (* @Postcondition : la liste retournée est correcte et l'état de l'objet   *)
    (*  		reste inchangé					       *)
    (* ----------------------------------------------------------------------  *)
    method lister_lignes_par_type
        ?(types = [ MetroBus; Express; LeBus; CoucheTard ]) () = 
      (* Traitement correspondant à la fonction *)
      let cles = self#lister_lignes in
      let values = L.map (fun cle -> H.find lignes cle) cles in 
      let valeurs_par_type t = L.find_all (fun l -> l#get_type = t) values in
      L.map (fun t -> t,L.map (fun l -> l#get_numero) (valeurs_par_type t)) types

         
    (* ----------------------------------------------------------------------  *)
    (* @Fonction      : trouver_voyages_par_date : ?date:int ->  string list   *)  
    (* @Description   : trouve tous les numéros de voyages associés à une date *)
    (* @Precondition  : la date doit être une clé existante dans la table      *)
    (* 			voyages_par_date  				       *)
    (* @Postcondition : la liste retournée est correcte et l'état de l'objet   *)
    (*  		reste inchangé					       *)
    (* ----------------------------------------------------------------------  *)
    method (*private*) trouver_voyages_par_date ?(date = date_actuelle ()) () =
      (* Traitement correspondant aux préconditions *)
      if not (H.mem voyages_par_date date) then
        raise (Erreur "Date invalide ou pas prise en charge");
      (* Traitement correspondant à la fonction *)
      H.find voyages_par_date date

           
    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : trouver_voyages_sur_la_ligne : ?date:int option ->     *)
    (*						       string -> string list   *)
    (* @Description   : trouve tous les numéros de voyages associés à une      *)
    (* 			ligne pour une date	         	               *)
    (* @Precondition  : la date doit être une clé existante dans la table      *)
    (* 		        voyages_par_date  			   	       *)
    (* @Postcondition : la liste retournée est correcte et l'état de l'objet   *)
    (*  		reste inchangé		                               *)
    (* ----------------------------------------------------------------------- *)
    method (*private*) trouver_voyages_sur_la_ligne 
	?(date = Some (date_actuelle ())) l_num = 
      (* Traitement correspondant aux préconditions *)
      if not (H.mem lignes l_num) then raise (Erreur "Ligne invalide");
      (* Traitement correspondant à la fonction *)
      let l = H.find lignes l_num in
      let voyages_ligne = l#get_voyages in
      match date with 
      | None -> voyages_ligne
      | Some d -> 
        let voyages_date = self#trouver_voyages_par_date ~date: d () in 
	voyages_ligne ++ voyages_date
                
    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : trouver_stations_environnantes: coordonnees -> float ->*)
    (*                                                  (G.V.label * float)list*)
    (* @Description   : trouve toutes les stations dans un rayon précisé en km *)
    (*  		par rapport à une coordonnée GPS.                      *)
    (* @Precondition  : 1- la coordonneée doit être valide - voir methode      *)
    (* 			   is_valide_gps de la classe coordonnees              *)
    (* 			2- le rayon est positif ou nul 	                       *)
    (* @Postcondition : la liste retournée est correcte et l'état de l'objet   *)
    (*  		reste inchangé				               *)
    (* ----------------------------------------------------------------------- *)
    method trouver_stations_environnantes pos rayon =
      (* Traitement correspondant aux préconditions *)
      if not(pos#is_valid_gps) then raise (Erreur "Position GPS invalide");
      if rayon < 0. then raise (Erreur "Rayon négatif");
      (* Traitement correspondant à la fonction *)
      let liste_st =
        H.fold 
          (fun id s acc -> 
             let d = s#get_coords#distance pos in
             if d <= rayon then (id, d)::acc else acc
          ) stations [] 
      in
      L.sort (fun (_, d) (_, d') -> compare d d') liste_st
	
    
    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : lister_lignes_passantes : G.V.label -> string list     *)
    (* @Description   : Lister les lignes passantes par une station en tout    *)
    (* 			temps 						       *)
    (* @Precondition  : l'id de la station existe dans la table stations       *)
    (* @Postcondition : la liste retournée est correcte et l'état de l'objet   *)
    (*  	        reste inchangé                                         *)
    (* ----------------------------------------------------------------------- *)
    method lister_lignes_passantes sid = 
      (* Traitement correspondant aux préconditions *)
      if not (H.mem stations sid) then raise (Erreur "Station inexistante ");
      (* Traitement correspondant à la fonction *)
      let s = H.find stations sid in 
      H.fold 
        (fun k l acc -> 
	   if L.mem l#get_ligne_id s#get_lignes_passantes_ids 
           then l#get_numero::acc
           else acc
        ) lignes []
	


    (* -- À IMPLANTER/COMPLÉTER (10 PTS) ------------------------------------- *)
    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : lister_stations_sur_itineraire: ?date:int option ->    *)
    (* 							string ->              *)
    (*                                                  (string * int list)list*)
    (* @Description   : liste les stations sur l'itineraire d'une ligne selon  *)
    (* 		        une date donnée ou quelque soit la date                *)
    (* @Precondition  : 1 - la date doit être une clé existante dans la table  *)
    (* 			    voyages_par_date  				       *)
    (* 		        2- le numéro de la ligne existe                        *)
    (* @Postcondition : la liste retournée est correcte et l'état de l'objet   *)
    (*  		reste inchangé                                         *)
    (* ----------------------------------------------------------------------- *)
     method lister_stations_sur_itineraire 
	?(date = Some(date_actuelle ())) 
        (l_num : string) : (string * int list) list =
      (* Traitement correspondant aux  préconditions  *)
       if not (H.mem lignes l_num) then raise (Erreur "Ligne inexistante");
        (* !!! il reste la precondition pour la date !!! *)
      (* Traitement correspondant à la fonction *)
       let l = H.find lignes l_num in
       let cles = self#trouver_voyages_sur_la_ligne l_num ~date in
       let liste_voyages = L.map (fun x -> H.find voyages x) cles in      
       l#stations_sur_itineraire_des_voyages liste_voyages


				
    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : ligne_passe_par_station :?date:int option -> string    *)
    (*                                           -> G.V.label -> bool          *)
    (* @Description   : vérifie si une ligne passe par une station donnée à,   *)
    (* 			éventuellement, une date donnée                        *)
    (* @Precondition  : 1 - la date doit être une clé existante dans la table  *)
    (* 			    voyages_par_date                                   *)
    (* 			2- le numéro de la ligne existe                        *)
    (* 			3- la station existe dans la table stations            *)
    (* @Postcondition : le resultat retourné est correct et l'état de l'objet  *)
    (*  		reste inchangé	                                       *)
    (* ----------------------------------------------------------------------- *)
    method ligne_passe_par_station 
        ?(date = Some (date_actuelle ())) l_num sid =
      (* Traitement correspondant aux préconditions *)
      if not (H.mem lignes l_num) then raise (Erreur "Ligne inexistante");
      if not (H.mem stations sid) then raise (Erreur "Station inexistante");
      (* Traitement correspondant à la fonction *)
      let vids_ligne = self#trouver_voyages_sur_la_ligne l_num ~date:date in
      let vids_station = let s = H.find stations sid in s#get_voyages_passants in
      let vids = vids_station ++ vids_ligne  in
      (L.length vids) != 0
      

    (* -- À IMPLANTER/COMPLÉTER (15 PTS) ------------------------------------- *)
    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : prochain_arret : ?date:int -> ?heure:int -> direction  *)
    (*                                   -> string -> G.V.label -> arret       *)
    (* @Description   : trouve le prochain arret de la ligne à la station,     *)
    (* 			étant données une date, une heure et une direction     *)
    (* @Precondition  : 1- la date doit être une clé existante dans la table   *)
    (* 			    voyages_par_date  			               *)
    (* 			2- le numéro de la ligne existe                        *)
    (* 			3- la station existe dans la table stations            *)
    (* 			4- la direction existe pour cette ligne	               *)
    (* 			5- l'heure est positive ou nulle                       *)
    (* 			6- plus d'arrets de la ligne à cette station           *)
    (* @Postcondition : le resultat retourné est correct et l'état de l'objet  *)
    (*  		reste inchangé                                         *)
    (* ----------------------------------------------------------------------- *)
    method (*private*) prochain_arret 
        ?(date = date_actuelle ())
        ?(heure = heure_actuelle ()) 
        (direction : direction) 
        (l_num : string) 
        (sid : G.V.label) : arret  =
     (* Traitement correspondant aux préconditions *)
     if not (H.mem lignes l_num) then raise (Erreur "Ligne inexistante");
     if heure < 0 then raise (Erreur "Heure négative");
     if not (H.mem stations sid) then raise (Erreur "Station inexistante");
     (* Traitement correspondant à la fonction *)
    let vids_ligne=self#trouver_voyages_sur_la_ligne l_num ~date:(Some date) in
	let vids_station = let s=H.find stations sid in s#get_voyages_passants in
    let vids = vids_ligne ++ vids_station in
    let arrets = 
      L.concat
        (L.map 
           (fun vid -> 
              let v = H.find voyages vid in 
	      if v#get_direction = direction then v#get_arrets else []
           ) vids
        ) in
    let arrets_station = L.filter (fun a -> a#get_station_id = sid) arrets in 
    let arrets_horaires =
     L.fold_left 
        (fun acc arr -> 
           let t = arr#get_arrivee in 
	   if t >= heure then arr::acc else acc
        ) [] arrets_station in
    let sorted_list = L.sort (fun x y -> if x#get_arrivee < y#get_arrivee then -1 else 1)  arrets_horaires in
    List.hd sorted_list

    
    (* -- À IMPLANTER/COMPLÉTER (15 PTS) ------------------------------------- *)
    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : prochaines_lignes_entre : ?date:int -> ?heure:int ->   *)
    (*                                            G.V.label -> G.V.label ->    *)
    (*                                            (string * int * int) list    *)
    (* @Description   : trouver toutes les lignes allant d'une station à une   *)
    (* 		        autre, étant données une date et une heure. Le résultat*)
    (*                  doit être une liste de tuples (un par ligne passante). *)
    (* 		        Chaque tuple  identifiera une ligne, son heure de      *)
    (*  	        passage au depart et son heure de passage à la         *)
    (*                  destination                                            *)
    (* @Precondition  : 1- la date doit être une clé existante dans la table   *)
    (* 			   voyages_par_date                                    *)
    (* 			2- les deux stations existent dans la table stations   *)
    (* 			3- l'heure est positive ou nulle                       *)
    (* @Postcondition : la liste retournée est correcte et l'état de l'objet   *)
    (*  		reste inchangé                                         *)
    (* ----------------------------------------------------------------------  *)
    method (*private*) prochaines_lignes_entre 
        ?(date = date_actuelle ())
        ?(heure = heure_actuelle ()) 
        (sid_dep : G.V.label) 
        (sid_dest : G.V.label) : (string * int * int) list =
      (* Traitement correspondant aux préconditions *)
      (* Traitement correspondant à la fonction *)
      raise (Non_Implante "prochaines_lignes_entre")
	

    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : trouver_horaire : ?date:int -> ?heure:int -> direction *)
    (*                                    -> string -> G.V.label -> string list*)
    (* @Description   : trouve l'horaire entier des arrivées d'une ligne à une *)
    (*                  station donnée, à partir d'une date, d'une heure       *)
    (*                  dans une direction donnée			       *)
    (* @Precondition  : 1- la date doit être une clé existante dans la table   *)
    (* 			    voyages_par_date                                   *)
    (* 			2- le numéro de la ligne existe                        *)
    (* 			3- la station existe dans la table stations            *)
    (* 			4- la direction existe pour cette ligne	               *)
    (* 			5- l'heure est positive ou nulle                       *)
    (* @Postcondition : le resultat retourné est correct et l'état de l'objet  *)
    (*  		reste inchangé                                         *)
    (* ----------------------------------------------------------------------- *)
  method trouver_horaire ?(date = date_actuelle ())
      ?(heure = heure_actuelle ()) direction l_num st_id = 
    (* Traitement correspondant aux préconditions *)
    if heure < 0 then raise (Erreur "Heure négative");
    if not (H.mem stations st_id) then raise (Erreur "Station inexistante ");
    if not (self#ligne_passe_par_station ~date: (Some date) l_num st_id) 
    then raise (Erreur "La ligne ne passe pas par la station");
    (* Traitement correspondant à la fonction *)
    let vids_ligne=self#trouver_voyages_sur_la_ligne l_num ~date:(Some date) in
    let vids_station = let s=H.find stations st_id in s#get_voyages_passants in
    let vids = vids_ligne ++ vids_station in
    let arrets = 
      L.concat
        (L.map 
           (fun vid -> 
              let v = H.find voyages vid in 
	      if v#get_direction = direction then v#get_arrets else []
           ) vids
        ) in
    if ((L.length arrets) = 0) then raise (Erreur "Direction invalide");
    let arrets_station = L.filter (fun a -> a#get_station_id = st_id) arrets in 
    let horaires = 
      L.fold_left 
        (fun acc arr -> 
           let t = arr#get_arrivee in 
	   if t >= heure then t::acc else acc
        ) [] arrets_station in
    L.map secs_a_heure (L.sort compare horaires)
      

    (* -- À IMPLANTER/COMPLÉTER (15 PTS) ------------------------------------- *)
    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : maj_etiquette_arete :?date:int -> ?heure:int ->        *)
    (*                                       G.V.label -> G.V.label -> unit    *)
    (* @Description   : mettre à jour le poids de l'arete entre deux stations  *)
    (* 	                dans le reseau.                                        *)
    (* 	                Le nouveau poids est le temps maximum que peut prendre *)
    (*                  un voyage pour parcourir cette arête, peu importe le   *)
    (*                  bus, et les multiples passages de celui-ci. Si aucun   *)    
    (*                  voyage ne passe, le poids est égal à poids_max_arete   *)
    (* @Precondition  : 1- la date doit être une clé existante dans la table   *)
    (* 			   voyages_par_date  	                               *)
    (* 			2- les deux stations existent dans la table stations   *)
    (* 		       	3- l'heure est positive ou nulle                       *)
    (* 			4- Aucune arete n'existe entre s1 et s2	               *)
    (* @Postcondition : seule l'etiquette de l'arete concernée est modifiée    *)
    (*                  dans le graphe.                                        *)
    (* Remarque       : Fonctions du module G utilisées dans le corrigé:       *)
    (*                  G.mem_edge, G.remove_edge, G.add_edge_e                *) 
    (* ----------------------------------------------------------------------- *)

    method (*private*) maj_etiquette_arete 
      ?(date = date_actuelle ())
      ?(heure = heure_actuelle ())
      (sid1 : G.V.label) (sid2 : G.V.label) : unit =
    (* Traitement correspondant aux préconditions *)
    if not (H.mem voyages_par_date date) then
      raise (Erreur "Date invalide ou pas prise en charge");
    if not (H.mem stations sid1) && not (H.mem stations  sid2) then
      raise (Erreur "Une station est inexistante");
    if (heure < 0) then
      raise (Erreur "Heure negative");
  (*  if not (G.mem_edge  (H.find stations sid1) (H.find stations sid2)) then
      raise (Erreur "L'arete n'existe pas"); *)
    (* Traitement correspondant à la fonction *)
   (*  print_int  (sid1) ; print_endline " " ; print_int  (sid2);*)
    let vd = self#trouver_voyages_par_date ~date:date () in
    let st1 = (H.find stations sid1)#get_voyages_passants in
    let st2 = (H.find stations sid2)#get_voyages_passants in
    let st_list = st1 ++ st2 in
    let vdstr_list = vd ++ st_list in
    let vd_list = List.map (fun x -> self#get_obj_voyage x) vdstr_list in
    let arret_list = List.map (fun x -> x#get_arrets) vd_list in
    let arret_list_st = List.map (fun x -> List.filter (fun y ->
        if y#get_station_id == sid1 || y#get_station_id == sid2 
        then true 
        else false) x) arret_list in
    let arret_list_st_tps = List.map (fun x -> List.filter (fun y -> 
        if y#get_depart < heure
        then false
        else true) x) arret_list_st in
    let arret_list_st_tps_filtered = List.filter (fun x -> 
        if x <> [] && L.length x = 2
        then true
        else false) arret_list_st_tps in
    let weigth = 
      if arret_list_st_tps_filtered == [] 
      then poids_max_arete
      else
        let weight_list = List.map (fun x -> 
            (List.nth x 1)#get_arrivee - (List.nth x 0)#get_depart) arret_list_st_tps_filtered in
        List.hd (List.sort (fun x y -> if x<y then 1 else -1) weight_list) in
    let n1 = H.find self#get_noeuds sid1 in
    let n2 = H.find self#get_noeuds sid2 in
    let new_edge = G.E.create n1 weigth n2 in
    G.remove_edge graphe_stations n1 n2 ; G.add_edge_e graphe_stations new_edge 
     

    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : maj_toutes_aretes_reseau : ?date:int -> ?heure:int ->  *)
    (* 						   unit -> unit	               *)
    (* @Description   : mettre à jour toutes les arêtes du réseau en fonction  *)
    (* 			de la date et de l'heure                               *)
    (* @Precondition  : 1- la date doit être une clé existante dans la table   *)
    (* 			   voyages_par_date                                    *)
    (* 			2- l'heure est positive ou nulle                       *)
    (* @Postcondition : les etiquettes des aretes du graphe sont modifiées;    *)
    (*                  le nombre de noeuds et d'arêtes reste inchangé, et il  *)
    (*                  en est de même pour tous les autres attributs de la    *)
    (*                  classe                                                 *)
    (* ----------------------------------------------------------------------- *)
  method (*private*) maj_toutes_aretes_reseau 
      ?(date = date_actuelle ())
      ?(heure = heure_actuelle ()) () =
    (* Traitement correspondant aux préconditions *)
    if heure < 0 then raise (Erreur "Heure négative");
    (* Traitement correspondant à la fonction *) 
    G.iter_edges 
      (fun n1 n2 -> 
         let s1 = G.V.label n1 and s2 = G.V.label n2 in
	 self#maj_etiquette_arete ~date: date ~heure: heure s1 s2
      ) graphe_stations


    (* -- À IMPLANTER/COMPLÉTER (15 PTS) ------------------------------------- *)
    (* ----------------------------------------------------------------------- *)
    (* @Fonction     : paires_stations_possibles : ?rmax:float -> coordonnees  *)
    (*  	       -> coordonnees ->                                       *)
    (*                 (G.V.label * G.V.label * float * float) list            *)  
    (* @Description  : trouver le top20 des paires de stations permettant      *)
    (*                 de joindre deux points gps,  étant donnée un rayon      *)
    (*                 maximum de marche autour de chacun des points. Pour ce  *)
    (*                 faire, vous devez trouver toutes les stations dans un   *)
    (*                 rayon max du point de départ. Pour chacunes d'elle      *)
    (*                 Bellman-Ford (BF.all_shortest_paths) vous aidera à      *)
    (*                 trouver toutes les autres stations atteignables.        *)
    (*                 On s'intéresse seulement aux stations atteignables dans *)
    (*                 un rayon rmax du point de destination. De ces           *)
    (*                 combinaisons possibles, on veut seulement les 20 paires *)
    (*                 ayant les coûts minimaux selon l'algorithme de bellman  *)
    (*                 et les poids actuels du graphe.                         *)
    (*                 Le résultat final, sous forme de liste de tuples, doit  *)
    (*                 être de la forme:                                       *)
    (*                 (sid1, sid2, distance sid1 coord1, distance sid2 coord2)*)
    (* @Precondition : 1- les coordonneées doivent être valides                *)	
    (* 		       2- le rayon est positif ou nul                          *)
    (* @Postcondition: la liste retournée est correcte et l'état de l'objet    *)
    (*  	       reste inchangé	                                       *)
    (* Remarque      : - Fonctions du module G utilisées dans le corrigé:      *)
    (*                   G.V.label                                             *) 
    (*                 - Fonctions du module BF utilisées dans le corrigé:     *)
    (*                   BF.all_shortest_paths, BF.H.fold                      *) 
    (* ----------------------------------------------------------------------- *)		
							
  method paires_stations_possibles 
      ?(rmax = distance_max_pieds) 
      (coord1 : coordonnees) 
      (coord2 : coordonnees) : (G.V.label * G.V.label * float * float) list =
    (* Traitement correspondant aux préconditions *)
    (* Traitement correspondant à la fonction *)
    raise (Non_Implante "paires_stations_possible")
      
		   
    (* -- À IMPLANTER/COMPLÉTER (10 PTS) ------------------------------------- *)
    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : plus_court_chemin : G.V.label -> G.V.label ->          *)
    (* 				            G.V.label list                     *)
    (* @Description   : étant données deux stations, on veut le plus court     *)
    (*                  chemin selon l'agorithme de dijsktra (G.shortest_path).*)
    (*                  Le chemin doit être la liste des stations parcourues   *)
    (*                  de la station de départ vers celle à destination       *)
    (* @Precondition  : les deux stations existent dans la table stations      *)
    (* @Postcondition : le chemin retourné est correct mais l'état de l'objet  *)
    (* 			reste inchangé 	                                       *)
    (* Remarque       : Fonctions du module G utilisées dans le corrigé:       *)
    (*                  G.shortest_path, G.V.label, G.E.src, G.E.dst           *) 
    (* ----------------------------------------------------------------------- *)
    method plus_court_chemin 
      (sid_dep : G.V.label) 
      (sid_dest : G.V.label) : G.V.label list = 
    (* Traitement correspondant aux préconditions *)
    if not (H.mem stations sid_dep) then raise (Erreur "Station inexistante");
    if not (H.mem stations sid_dest) then raise (Erreur "Station inexistante");
    (* Traitement correspondant à la fonction *)
    let st1 = H.find self#get_noeuds sid_dep in  (*trouver premiere station*)
    let st2 = H.find self#get_noeuds sid_dest in (*trouver deuxieme station*)
    let liste = fst(G.shortest_path (self#get_graphe) st1 st2) in
    let tailListe = L.nth liste ((L.length liste)-1) in (*dernier el. de liste*)
    let lastStation = [G.V.label (G.E.dst tailListe)] in  (*derniere station*)
    (L.map (fun x -> G.V.label (G.E.src x)) liste) @ lastStation

    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : plans_possibles_pour_chemin : ?date:int ->             *)
    (*                  ?heure:int ->  G.V.label list ->                       *)
    (*                  (string * int * int * G.V.label list) list list        *)
    (* @Description   : trouver tous les plans possibles pour le chemin en     *)
    (*                  parametre; un chemin est une liste de tuple de la forme*)
    (*                  (bus à prendre, heure_depart, heure_fin, liste des     *)
    (*                   stations d'arrets)                                    *)
    (* @Precondition  : 1- la date doit être une clé existante dans la table   *)
    (* 			   voyages_par_date                                    *)
    (* 			2- l'heure est positive ou nulle                       *)
    (* @Postcondition : l'état actuel de l'objet reste inchangé	               *)
    (* ----------------------------------------------------------------------- *)
    method plans_possibles_pour_chemin 
      ?(date = date_actuelle ())
      ?(heure = heure_actuelle ()) chemin =
    let aretes = tuples_de_voisins chemin in 
    let first_arete = L.hd aretes and reste_aretes = L.tl aretes in
    let init_acc = 
      let s1, s2 = first_arete in
      let choix = self#prochaines_lignes_entre ~date:date ~heure:heure s1 s2 in
      L.map (fun (a,b,c) -> [(a, b, c, s1, s2)]) choix 
    in
    let res = 
      L.fold_left 
        (fun acc (s1, s2) ->
           L.concat
             (L.map 
                (fun ch_deb -> 
	           let lnum, _, h, _, _ = L.hd ch_deb in
                   let temp = self#prochaines_lignes_entre ~date:date 
                                                           ~heure:h s1 s2 in
                   let ltemp = L.map (fun (a,_,_) -> a) temp in
                   if (L.mem lnum ltemp) then
                     let (x, y, z) = L.find (fun (a,_,_) -> a = lnum) temp in
                     [(x, y, z, s1, s2)::ch_deb]
                   else L.map (fun (x, y, z) -> (x, y, z, s1, s2)::ch_deb) temp
                ) acc
             )
        ) init_acc reste_aretes in
    L.map 
      (fun l ->
	 L.rev
           (L.fold_left 
              (fun acc (act_num, d, f, x, y) ->
                 if acc = [] then (act_num, d, f, [x;y])::acc else
		   let last_num, dep, _, l = L.hd acc in
                   if act_num = last_num 
                   then (act_num, dep, f, l@[y])::(L.tl acc)
                   else (act_num, d, f, [x;y])::acc
              ) [] (L.rev l)
           )
      ) res
      

    (* -- À IMPLANTER/COMPLÉTER (20 PTS) ------------------------------------- *)
    (* ----------------------------------------------------------------------- *)
    (* @Fonction      : trouver_trajet_optimal : ?date:int -> ?heure:int ->    *)
    (*                  ?rmax:float -> coordonnees -> coordonnees ->           *)
    (*                  (float * float                                         *)
    (*                   * (string * int * int * G.V.label list) list) list    *)
    (* @Description   : Trouver trajet optimal selon (dans l'ordre de priorité)*)
    (*                  a- le nombre minimal de transferts (de lignes)         *)
    (*                  b- l'heure de début du trajet                          *)
    (*                  c- la durée totale du trajet                           *)
    (*                  d- l'heure d'arrivée à la fin du trajet                *)
    (*                  e- la distance totale parcourue à pieds                *)
    (* @Precondition  : 1- la date doit être une clé existante dans la table   *)
    (* 			   voyages_par_date                                    *)
    (* 			2- l'heure est positive ou nulle                       *)
    (* 			3- les coods sont valides                              *)
    (*                  4- rmax >= 0                                           *)
    (* @Postcondition : l'état actuel de l'objet reste inchangé	               *)
    (* ----------------------------------------------------------------------- *)

  method trouver_trajet_optimal 
      ?(date = date_actuelle ())
      ?(heure = heure_actuelle ())
      ?(rmax = marche_max_pieds) 
      (coord1 : coordonnees) (coord2 : coordonnees) : 
      (float * float * (string * int * int * G.V.label list) list) list =
    (* Traitement correspondant aux préconditions *)
    (* Traitement correspondant à la fonction *)
    raise (Non_Implante "trouver_trajet_optimal")
			            
  end;;

