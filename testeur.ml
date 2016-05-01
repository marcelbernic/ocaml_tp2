#use "tp2.ml";;

open String;;
open Utiles ;;
open Date;;
open Hashtbl ;;

let rep = "/home/etudiant/workspace/tp2/googletransit/";; 

(* --------------------------------------------------------------------------- *)
(* -- FONCTIONS UTILES ------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let print_list l  =
  print_string "[ "; List.iter (fun x -> print_int x; print_string "; ") l;
  print_endline "] ";;

let fail () = false 
and pass () = true;;

let assert_equal nom_test a b = 
  if a = b then pass() else fail();;

let assert_equal_list nom_test a b = 
  if (a ++ b) = a then pass() else fail();;

let assert_throw_exception nom_test lazy_expression =
   match lazy_expression () with
  | exception (Erreur _) -> pass()
  | exception _ -> fail()
  |  _ -> fail();;

let assert_true nom_test lazy_expression =
  match lazy_expression () with
  | true -> pass()
  | false -> fail();;  

(* --------------------------------------------------------------------------- *)
(* -- CHARGEMENT DES DONNÉES ------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let app, t = timeRun (new gestionnaireReseau ~rep:rep) () ;;
let chez_nous = new coordonnees 46.760074  (-71.319867)
and bureau = new coordonnees 46.816699  (-71.206776)
and pouliot = new coordonnees 46.778826 (-71.275169) 
and desjardins = new coordonnees 46.778808 (-71.270014) ;;


(* -- TEST ------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let test1() =
  let comment = ref [] in
  try
    let obtenu  = app#lister_stations_sur_itineraire "11" ~date:None in 
    let direction1_attendu = 
      ("Place D'Youville / Vieux-Qu\195\169bec (Est)",
       [2070; 2071; 3998; 1905; 1906; 1907; 1908; 1909; 1911; 1807; 1808; 1809;
        1810; 1811; 1812; 1813; 1814; 1815; 1816; 1817; 1818; 1820; 1441; 1823;
        1824; 1826; 1828; 1830; 1831; 1832; 1833; 1834; 1835; 1836; 1837; 1838;
        1839; 1840; 1841; 1073; 1074; 1075; 1076; 1077; 1078; 1844; 1845; 1846;
        1847; 1848; 1849; 1850; 1851; 1852; 1037; 1853; 1583; 1854; 1584; 1560;
        1855; 1856; 1271; 1272; 1274; 1275; 1276; 1134; 1124; 1259; 2625]) in
    let direction2_attendu = 
      ("Pointe-de-Sainte-Foy (Ouest)",
       [1271; 1272; 1517; 1274; 1275; 1519; 1276; 2447; 1134; 1549; 1124; 1259;
        2625; 1190; 1270; 1762; 1763; 1764; 1765; 1766; 1767; 1768; 1769; 1770;
        1771; 1772; 1773; 1774; 1056; 1057; 1058; 1059; 1060; 1061; 1063; 1065;
        1066; 1777; 1778; 1779; 1780; 1781; 1782; 1783; 1999; 1787; 1776; 1790;
        1791; 1793; 1794; 1795; 1796; 1797; 1798; 1799; 1800; 1801; 1802; 1804;
        1803; 1805; 1806; 3099; 1440; 1882; 1883; 1884; 1885; 2070; 2071; 3998;
        1905]) in
    let attendu = [ direction1_attendu; direction2_attendu ] in
    if (assert_equal "lister_stations_sur_itineraire@longueur_correct" 
	  (List.length obtenu) (List.length attendu)) 
    then () else comment := (!comment) @ [ "longueur incorrecte" ];

    if (assert_true "lister_stations_sur_itineraire@direction1_valide" 
	  (fun () -> List.mem direction1_attendu obtenu)) 
    then () else comment := (!comment) @ [ "direction1 invalide" ];

    if (assert_true "lister_stations_sur_itineraire@direction2_valide" 
	  (fun () -> List.mem direction2_attendu obtenu)) 
    then () else comment := (!comment) @ [ "direction2 invalide" ];

    if (assert_throw_exception "lister_stations_sur_itineraire@ligne_invalide" 
	  (fun () -> app#lister_stations_sur_itineraire "807")) 
    then () else comment := (!comment) @ [ "precondition ligne"];

    let obtenu  = app#lister_stations_sur_itineraire "11" 
	~date:(Some(date_a_nbjours "20160327"))  in 
    let direction1_attendu = 
      ("Place D'Youville / Vieux-Qu\195\169bec (Est)",
       [2070; 2071; 3998; 1905; 1906; 1907; 1908; 1909; 1911; 1807; 1808; 1809;
        1810; 1811; 1812; 1813; 1814; 1815; 1816; 1817; 1818; 1820; 1441; 1823;
        1824; 1826; 1828; 1830; 1831; 1832; 1833; 1834; 1835; 1836; 1837; 1838;
        1839; 1840; 1841; 1073; 1074; 1075; 1076; 1077; 1078; 1844; 1845; 1846;
        1847; 1848; 1849; 1850; 1851; 1037; 1583; 1584; 1560; 2625]) in
    let direction2_attendu = 
      ("Pointe-de-Sainte-Foy (Ouest)",
       [2625; 1190; 1517; 1519; 2447; 1549; 1767; 1768; 1769; 1770; 1771; 1772;
        1773; 1774; 1056; 1057; 1058; 1059; 1060; 1061; 1063; 1065; 1066; 1777;
        1778; 1779; 1780; 1781; 1782; 1783; 1999; 1787; 1776; 1790; 1791; 1793;
        1794; 1795; 1796; 1797; 1798; 1799; 1800; 1801; 1802; 1804; 1803; 1805;
        1806; 3099; 1440; 1882; 1883; 1884; 1885; 2070; 2071; 3998; 1905]) in
    let attendu = [ direction1_attendu; direction2_attendu ] in
    
    if (assert_equal "lister_stations_sur_itineraire@longueur_correct" 
	  (List.length obtenu) (List.length attendu)) 
    then () else comment := (!comment) @ [ "longueur incorrecte" ];

    if (assert_true "lister_stations_sur_itineraire@direction1_valide" 
	  (fun () -> List.mem direction1_attendu obtenu)) 
    then () else comment := (!comment) @ [ "direction1 invalide" ];

    if (assert_true "lister_stations_sur_itineraire@direction2_valide" 
	  (fun () -> List.mem direction2_attendu obtenu)) 
    then () else comment := (!comment) @ [ "direction2 invalide" ];


    let obtenu  = app#lister_stations_sur_itineraire "980" ~date:None  in 
    let direction_attendu = 
      ("L'Ancienne-Lorette (Nord)",
       [2878; 1191; 1032; 1034; 1035; 1036; 1038; 1039; 1040; 1041; 1042; 1043;
        4445; 4446; 1160; 1161; 4447; 2643; 4449; 4451; 4452; 4453; 4454; 4455;
        4456; 3348; 4458; 4459; 4460; 4461; 4462; 3962; 3963; 3964; 3997; 3999;
        4470; 4471; 4472; 4473; 4474; 4475; 4476; 4477; 4478; 1210; 5734; 4365;
        4367; 4368; 4479; 4480; 4481; 4482; 4483; 4484; 4485; 5039; 4486; 4487;
        4488; 4489; 4490; 4491; 4381; 4382; 4383; 4492; 4493; 4494; 4495; 4496;
        2952; 4497; 4498; 4499; 4500; 4501; 4524]) in
    let attendu = [ direction_attendu ] in
    if (assert_equal "lister_stations_sur_itineraire@longueur_correct" 
	  (List.length obtenu) (List.length attendu)) 
    then () else comment := (!comment) @ [ "longueur incorrecte" ];

    if (assert_true "lister_stations_sur_itineraire@direction_valide" 
	  (fun () -> List.mem direction_attendu obtenu)) 
    then () else comment := (!comment) @ [ "direction invalide" ];

    let obtenu  = app#lister_stations_sur_itineraire "980" 
	~date:(Some(date_a_nbjours "20160329"))  in 
    let attendu = [] in
    if (assert_equal "lister_stations_sur_itineraire@longueur_correct" 
	  (List.length obtenu) (List.length attendu)) 
    then () else comment := (!comment) @ [ "longueur incorrecte" ];
    !comment

  with 
  | Non_Implante _ -> comment := (!comment) @ [ "Fonction non_implantee" ]; 
    !comment
  | _ -> comment := (!comment) @ [ "Test non complete" ]; !comment;;


(* -- TEST ------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let test2() =
  let comment = ref [] in
  try
    let obtenu = 
      let temp = app#prochain_arret (RetourVers "Pointe-de-Sainte-Foy (Ouest)")
	  "800" 2009 ~date:(date_a_nbjours "20160330") 
          ~heure:(heure_a_nbsecs "12:10:00") in 
      (temp#get_voyage_id, secs_a_heure temp#get_arrivee) in
    let attendu = ("47307292-20161011multiint-1111100", "12:20:00") in 
    if (assert_equal "prochain_arret@resultat_correct1" obtenu
	  attendu) 
    then () else comment := (!comment) @ [ "resultat incorrect" ];

    let obtenu = let temp = app#prochain_arret (AllerVers "Beauport (Est)") 
		     "800" 2009 ~date:(date_a_nbjours "20160330") 
                     ~heure:(heure_a_nbsecs "12:10:00") in 
      (temp#get_voyage_id, secs_a_heure temp#get_arrivee) in
    let attendu = ("47307181-20161011multiint-1111100", "12:15:00") in 
    if (assert_equal "prochain_arret@resultat_correct1" obtenu
	  attendu) 
    then () else comment := (!comment) @ [ "resultat incorrect" ];

    let obtenu = 
      let temp = app#prochain_arret (RetourVers "Pointe-de-Sainte-Foy (Ouest)") 
	  "11" 1905 ~date:(date_a_nbjours "20160330") 
          ~heure:(heure_a_nbsecs "12:10:00") in 
      (temp#get_voyage_id, secs_a_heure temp#get_arrivee) in
    let attendu = ("47370969-20161011multiint-1111100", "13:31:00") in 
    if (assert_equal "prochain_arret@resultat_correct1" obtenu
	  attendu) 
    then () else comment := (!comment) @ [ "resultat incorrect" ];
    begin
      if (assert_throw_exception "prochain_arret@date_invalide" 
  	    (fun () -> app#prochain_arret (RetourVers "Pointe-de-Sainte-Foy (Ouest)") 
		"11" 1905 ~date:(date_a_nbjours "20110330") 
                ~heure:(heure_a_nbsecs "12:10:00") ))
      then () else comment := (!comment) @ [ "precondition date"];

      if (assert_throw_exception "prochain_arret@ligne_invalide" 
  	    (fun () -> app#prochain_arret (RetourVers "Pointe-de-Sainte-Foy (Ouest)") 
		"1001" 1905 ~date:(date_a_nbjours "20160330") 
                ~heure:(heure_a_nbsecs "12:10:00") )) 
      then () else comment := (!comment) @ [ "precondition ligne"];

      if (assert_throw_exception "prochain_arret@heure_invalide" 
  	    (fun () -> app#prochain_arret (RetourVers "Pointe-de-Sainte-Foy (Ouest)") 
		"11" 1905 ~date:(date_a_nbjours "20160330") 
                ~heure:(-4) )) 
      then () else comment := (!comment) @ [ "precondition heure"];

      if (assert_throw_exception "prochain_arret@station_invalide" 
  	    (fun () -> app#prochain_arret (RetourVers "Pointe-de-Sainte-Foy (Ouest)") 
		"11" 19005 ~date:(date_a_nbjours "20160330") 
                ~heure:(heure_a_nbsecs "12:10:00") )) 
      then () else comment := (!comment) @ [ "precondition station"];

      if (assert_throw_exception "prochain_arret@direction_invalide" 
  	    (fun () -> app#prochain_arret (AllerVers "Pointe-de-Sainte-Foy (Ouest)") 
		"11" 1905 ~date:(date_a_nbjours "20160330") 
                ~heure:(heure_a_nbsecs "12:10:00") )) 
      then () else comment := (!comment) @ [ "precondition direction"];

      if (assert_throw_exception "prochain_arret@Plus d'arrets" 
  	    (fun () -> app#prochain_arret (RetourVers "Pointe-de-Sainte-Foy (Ouest)") 
		"11" 1905 ~date:(date_a_nbjours "20160330") 
                ~heure:(heure_a_nbsecs "24:20:00") )) 
      then () else comment := (!comment) @ [ "plus de station"];

      !comment
    end

  with 
  | Non_Implante _ -> comment := (!comment) @ [ "Fonction non_implantee" ]; 
    !comment
  | _ -> comment := (!comment) @ [ "Test non complete" ]; !comment;;


(* -- TEST ------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let test3() =

  let comment = ref [] in
  try
    let obtenu = let temp = app#prochaines_lignes_entre 
		     ~date:(date_a_nbjours "20160330") ~heure:(heure_a_nbsecs "12:10:00") 
		     1445 1807 in 
      L.map (fun (a, b, c) -> a, (secs_a_heure b), (secs_a_heure c)) temp in
    let attendu = 
      [("331", "16:08:40", "16:08:59"); ("391", "15:51:40", "15:51:59");
       ("801", "12:11:45", "12:12:00"); ("800", "12:16:45", "12:17:00");
       ("354", "16:09:40", "16:09:59")]in 
    if (assert_equal "prochaines_lignes_entre@longueur_correct1" 
	  (List.length obtenu) (List.length attendu)) 
    then () else comment := (!comment) @ [ "longueur incorrecte" ];
    if (assert_equal_list "prochaines_lignes_entre@contenu_correct1"  
	  obtenu attendu) 
    then () else comment := (!comment) @ [ "contenu incorrect" ];

    let obtenu = 
      let temp = app#prochaines_lignes_entre ~date:(date_a_nbjours "20160330") 
          ~heure:(heure_a_nbsecs "18:10:00") 1445 1807 in 
      L.map (fun (a, b, c) -> a, (secs_a_heure b), (secs_a_heure c)) temp in
    let attendu = [("801", "18:18:45", "18:19:00"); 
		   ("800", "18:12:45", "18:13:00")] in 
    if (assert_equal "prochaines_lignes_entre@longueur_correct1" 
	  (List.length obtenu) (List.length attendu)) 
    then () else comment := (!comment) @ [ "longueur incorrecte" ];
    if (assert_equal_list "prochaines_lignes_entre@contenu_correct1"  
	  obtenu attendu) 
    then () else comment := (!comment) @ [ "contenu incorrect" ];

    if (assert_throw_exception "prochain_arret@date_invalide" 
	  (fun () -> app#prochaines_lignes_entre ~date:(date_a_nbjours "20180330") 
              ~heure:(heure_a_nbsecs "12:10:00") 1445 1807))
    then () else comment := (!comment) @ [ "precondition date"];

    if (assert_throw_exception "prochain_arret@heure_invalide" 
          (fun () -> app#prochaines_lignes_entre ~date:(date_a_nbjours "20160330") 
              ~heure:(-4) 1445 1807)) 
    then () else comment := (!comment) @ [ "precondition heure"];

    if (assert_throw_exception "prochain_arret@station1_invalide" 
          (fun () -> app#prochaines_lignes_entre ~date:(date_a_nbjours "20160330") 
              ~heure:(heure_a_nbsecs "12:10:00") 18445 1807)) 
    then () else comment := (!comment) @ [ "precondition station1"];

    if (assert_throw_exception "prochain_arret@station2_invalide" 
          (fun () -> app#prochaines_lignes_entre ~date:(date_a_nbjours "20160330") 
              ~heure:(heure_a_nbsecs "12:10:00") 1445 18007)) 
    then () else comment := (!comment) @ [ "precondition station2"];		
    !comment

  with 
  | Non_Implante _ -> comment := (!comment) @ [ "Fonction non_implantee" ]; 
    !comment
  | _ -> comment := (!comment) @ [ "Test non complete" ]; !comment;;


(* -- TEST ------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let test4() =

  let comment = ref [] in
  try
    app#maj_etiquette_arete ~date:(date_a_nbjours "20160330")
      ~heure:(heure_a_nbsecs "12:10:00") 1445 1807;
    let n1 = H.find app#get_noeuds 1445 and n2 = H.find app#get_noeuds 1807 in
    let obtenu = G.E.label (G.find_edge app#get_graphe n1 n2) in
    let attendu = 19 in
    if (assert_equal "maj_etiquette_arete@resultat_correct" obtenu attendu)
    then () else comment := (!comment) @ [ "longueur incorrecte" ];
    if (assert_equal "maj_etiquette_arete@nb_arete_correct1"  (
	G.nb_edges app#get_graphe) 5521)
    then () else comment := (!comment) @ [ "contenu incorrect" ];

    begin
      if (assert_throw_exception "maj_etiquette_arete@date_invalide"
  	    (fun () -> app#maj_etiquette_arete ~date:(date_a_nbjours "20120330")
                ~heure:(heure_a_nbsecs "12:10:00") 1445 1807))
      then () else comment := (!comment) @ [ "precondition date"];

      if (assert_throw_exception "maj_etiquette_arete@heure_invalide"
  	    (fun () -> app#maj_etiquette_arete ~date:(date_a_nbjours "20160330")
                ~heure:(-4) 1445 1807))
      then () else comment := (!comment) @ [ "precondition heure"];

      if (assert_throw_exception "maj_etiquette_arete@station1_invalide"
  	    (fun () -> app#maj_etiquette_arete ~date:(date_a_nbjours "20160330")
                ~heure:(heure_a_nbsecs "12:10:00") 0 1807))
      then () else comment := (!comment) @ [ "precondition station1"];

      if (assert_throw_exception "maj_etiquette_arete@station2_invalide"
  	    (fun () -> app#maj_etiquette_arete ~date:(date_a_nbjours "20160330")
                ~heure:(heure_a_nbsecs "12:10:00") 1445 180007))
      then () else comment := (!comment) @ [ "precondition station2"];

      if (assert_throw_exception "maj_etiquette_arete@station2_invalide"
  	    (fun () -> app#maj_etiquette_arete ~date:(date_a_nbjours "20160330")
                ~heure:(heure_a_nbsecs "12:10:00") 1445 2001))
      then () else comment := (!comment) @ [ "precondition station2"];
      !comment
    end

  with
  | Non_Implante _ -> comment := (!comment) @ [ "Fonction non_implantee" ]; 
    !comment
  | _ -> comment := (!comment) @ [ "Test non complete" ]; !comment;;


(* -- TEST ------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let test5() =

  let comment = ref [] in

  try 
    app#maj_toutes_aretes_reseau  ~date:(date_a_nbjours "20160330")
      ~heure:(heure_a_nbsecs "12:10:00") ();
    let obtenu = app#paires_stations_possibles desjardins chez_nous 
	~rmax:0.25 in 
    let attendu = [(1561, 3099, 0.0670203347070013472, 0.179909546084110278);
                   (1561, 1380, 0.0670203347070013472, 0.154517201193211856);
                   (1559, 3099, 0.209829057030505972, 0.179909546084110278);
                   (1559, 1380, 0.209829057030505972, 0.154517201193211856);
                   (1999, 1806, 0.24702817980565947, 0.0318930938209307721);
                   (1999, 3099, 0.24702817980565947, 0.179909546084110278);
                   (1999, 1380, 0.24702817980565947, 0.154517201193211856);
                   (1783, 1806, 0.200345313296168781, 0.0318930938209307721);
                   (1515, 1806, 0.0838530217212952456, 0.0318930938209307721);
                   (1783, 3099, 0.200345313296168781, 0.179909546084110278);
                   (1515, 3099, 0.0838530217212952456, 0.179909546084110278);
                   (1783, 1380, 0.200345313296168781, 0.154517201193211856);
                   (1515, 1380, 0.0838530217212952456, 0.154517201193211856);
                   (1561, 1807, 0.0670203347070013472, 0.18210297135972292);
                   (1999, 1807, 0.24702817980565947, 0.18210297135972292);
                   (1561, 1808, 0.0670203347070013472, 0.0491256844424609387);
                   (1999, 1808, 0.24702817980565947, 0.0491256844424609387);
                   (1561, 1806, 0.0670203347070013472, 0.0318930938209307721);
                   (1559, 1807, 0.209829057030505972, 0.18210297135972292);
                   (1783, 1807, 0.200345313296168781, 0.18210297135972292)] in

    begin
      if (assert_equal
            "paires_stations_possibles@longueur_correct" (List.length obtenu)
            (List.length attendu)) 
      then () else comment := (!comment) @ [ "longueur incorrecte" ];

      if (assert_equal_list "paires_stations_possibles@contenu_correct" obtenu
  	    attendu) 
      then () else comment := (!comment) @ [ "contenu incorrect" ];

      let fake_pos = (new coordonnees (-46.778826)(-71.275169)) in 

      if (assert_throw_exception
            "paires_stations_possibless@rayon_invalide" 
            (fun () -> app#paires_stations_possibles desjardins chez_nous 
		~rmax:(-0.25))) 
      then () else comment := (!comment) @ [ "precondition rayon"];

      if (assert_throw_exception
            "paires_stations_possibles@position1_invalide" 
            (fun () -> app#paires_stations_possibles fake_pos chez_nous 
		~rmax:0.25 ))
      then () else comment := (!comment) @ [ "precondition position1"];

      if (assert_throw_exception
            "paires_stations_possibles@position2_invalide" 
            (fun () -> app#paires_stations_possibles desjardins fake_pos 
		~rmax:0.25 ))
      then () else comment := (!comment) @ [ "precondition position2"];

      !comment
    end

  with 
  | Non_Implante _ -> comment := (!comment) @ [ "Fonction non_implantee" ]; 
    !comment
  | _ -> comment := (!comment) @ [ "Test non complete" ]; !comment;;


(* -- TEST ------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let test6() =

  let comment = ref [] in
  try
    app#maj_toutes_aretes_reseau  ~date:(date_a_nbjours "20160330")
      ~heure:(heure_a_nbsecs "12:10:00") ();
    let obtenu = app#plus_court_chemin 1515 3099 in
    let attendu = 
      [1515; 1787; 1776; 1790; 1791; 2000; 2001; 2003; 2006; 1434; 2007; 4144;
       3099] in
    begin
      if (assert_equal "plus_court_chemin @longueur_correct1" 
	    (List.length obtenu) (List.length attendu)) 
      then () else comment := (!comment) @ [ "longueur incorrecte" ];

      if (assert_equal_list "plus_court_chemin @contenu_correct1" obtenu
  	    attendu) 
      then () else comment := (!comment) @ [ "contenu incorrect" ];

      if (assert_equal "plus_court_chemin @contenu_en ordre1" obtenu
  	    attendu) 
      then () else comment := (!comment) @ [ "contenu_en ordre" ];
    end;

    let obtenu = app#plus_court_chemin 1999 1806  in
    let attendu = 
      [1999; 1787; 1776; 1790; 1791; 1793; 1794; 1795; 1796; 1798; 1799; 1801;
       1802; 1804; 1803; 1805; 1806] in
    if (assert_equal "plus_court_chemin @longueur_correct2" 
          (List.length obtenu) (List.length attendu)) 
    then () else comment := (!comment) @ [ "longueur incorrecte" ];

    if (assert_equal_list "plus_court_chemin @contenu_correct2" obtenu
	  attendu) 
    then () else comment := (!comment) @ [ "contenu incorrect" ];

    if (assert_equal "plus_court_chemin @contenu_en ordre2" obtenu
	  attendu) 
    then () else comment := (!comment) @ [ "contenu_en ordre" ];

    begin      
      if (assert_throw_exception "plus_court_chemin @station_invalide" 
  	    (fun () -> app#plus_court_chemin 0 3099 )) 
      then () else comment := (!comment) @ [ "precondition station1"];

      if (assert_throw_exception "plus_court_chemin @station_invalide" 
  	    (fun () -> app#plus_court_chemin 3099 0 )) 
      then () else comment := (!comment) @ [ "precondition station2"];
      !comment
    end

  with 
  | Non_Implante _ -> comment := (!comment) @ [ "Fonction non_implantee" ]; 
    !comment
  | _ -> comment := (!comment) @ [ "Test non complete" ]; !comment;;


(* -- TEST en plus ----------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let test_plus() =
  let comment = ref [] in
  try
    app#maj_toutes_aretes_reseau  ~date:(date_a_nbjours "20160330")
      ~heure:(heure_a_nbsecs "12:10:00") ();
    let obtenu = 
      let ch = 
	[1999; 1787; 1776; 1790; 1791; 1793; 1794; 1795; 1796; 1798; 1799; 1801;
         1802; 1804; 1803; 1805; 1806] in 
      app#plans_possibles_pour_chemin ~date:(date_a_nbjours "20160330")
        ~heure:(heure_a_nbsecs "12:10:00") ch  in
    let attendu = 
      [[("111", 62520, 63480,
         [1999; 1787; 1776; 1790; 1791; 1793; 1794; 1795; 1796; 1798; 1799; 1801;
          1802]);
        ("11", 64088, 64200, [1802; 1804; 1803; 1805; 1806])];
       [("11", 45000, 45960,
         [1999; 1787; 1776; 1790; 1791; 1793; 1794; 1795; 1796; 1798; 1799; 1801;
          1802; 1804; 1803; 1805; 1806])]] in
    begin
      if (assert_equal "plans_possibles_pour_chemin @longueur_correct1" 
	    (List.length obtenu) (List.length attendu)) 
      then () else comment := (!comment) @ [ "longueur incorrecte" ];
      
      if (assert_equal_list "plans_possibles_pour_chemin @contenu_correct1" 
	    obtenu attendu) 
      then () else comment := (!comment) @ [ "contenu incorrect" ];
    end;
    
    app#maj_toutes_aretes_reseau  ~date:(date_a_nbjours "20160402")
      ~heure:(heure_a_nbsecs "12:10:00") ();
    let obtenu = 
      let ch = [1515; 1787; 1776; 1790; 1791; 2000; 2001; 2003; 2006; 1434; 
		2007; 4144; 3099] in 
      app#plans_possibles_pour_chemin ~date:(date_a_nbjours "20160402")
        ~heure:(heure_a_nbsecs "12:10:00") ch  in
    let attendu = 
      [[("87", 43800, 44437, [1515; 1787; 1776; 1790; 1791; 2000]);
        ("801", 44820, 45420, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("87", 43800, 44437, [1515; 1787; 1776; 1790; 1791; 2000]);
        ("800", 45240, 45840, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("87", 43800, 44437, [1515; 1787; 1776; 1790; 1791; 2000]);
        ("915", 92880, 93351, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("801", 44340, 45420,
         [1515; 1787; 1776; 1790; 1791; 2000; 2001; 2003; 2006; 1434; 2007; 4144;
          3099])];
       [("800", 43860, 44940,
         [1515; 1787; 1776; 1790; 1791; 2000; 2001; 2003; 2006; 1434; 2007; 4144;
          3099])];
       [("13", 44520, 44858, [1515; 1787; 1776; 1790; 1791]);
        ("87", 46096, 46237, [1791; 2000]);
        ("801", 46680, 47280, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("13", 44520, 44858, [1515; 1787; 1776; 1790; 1791]);
        ("87", 46096, 46237, [1791; 2000]);
        ("800", 47100, 47700, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("13", 44520, 44858, [1515; 1787; 1776; 1790; 1791]);
        ("87", 46096, 46237, [1791; 2000]);
        ("915", 92880, 93351, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("13", 44520, 44858, [1515; 1787; 1776; 1790; 1791]);
        ("801", 45608, 46320,
         [1791; 2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("13", 44520, 44858, [1515; 1787; 1776; 1790; 1791]);
        ("800", 45128, 45840,
         [1791; 2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("13", 44520, 44858, [1515; 1787; 1776; 1790; 1791]);
        ("915", 92807, 93351,
         [1791; 2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("13", 44520, 44858, [1515; 1787; 1776; 1790; 1791]);
        ("16", 46257, 46362, [1791; 2000]);
        ("801", 46680, 47280, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("13", 44520, 44858, [1515; 1787; 1776; 1790; 1791]);
        ("16", 46257, 46362, [1791; 2000]);
        ("800", 47100, 47700, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("13", 44520, 44858, [1515; 1787; 1776; 1790; 1791]);
        ("16", 46257, 46362, [1791; 2000]);
        ("915", 92880, 93351, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("915", 92460, 93351,
         [1515; 1787; 1776; 1790; 1791; 2000; 2001; 2003; 2006; 1434; 2007; 4144;
          3099])];
       [("16", 44220, 44750, [1515; 1787; 1776; 1790; 1791; 2000]);
        ("801", 44820, 45420, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("16", 44220, 44750, [1515; 1787; 1776; 1790; 1791; 2000]);
        ("800", 45240, 45840, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])];
       [("16", 44220, 44750, [1515; 1787; 1776; 1790; 1791; 2000]);
        ("915", 92880, 93351, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])]] in
    if (assert_equal "plans_possibles_pour_chemin @longueur_correct1" 
	  (List.length obtenu) (List.length attendu)) 
    then () else comment := (!comment) @ [ "longueur incorrecte" ];
    
    if (assert_equal_list "plans_possibles_pour_chemin @contenu_correct1" 
	  obtenu attendu) 
    then () else comment := (!comment) @ [ "contenu incorrect" ];
    
    !comment
      
  with 
  | Non_Implante _ -> comment := (!comment) @ [ "Fonction non_implantee" ]; 
    !comment
  | _ -> comment := (!comment) @ [ "Test non complete" ]; !comment;;


(* -- TEST ------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let test7() =
  let comment = ref [] in
  try
    app#maj_toutes_aretes_reseau  ~date:(date_a_nbjours "20160330")
      ~heure:(heure_a_nbsecs "12:10:00") ();
    let obtenu = app#trouver_trajet_optimal ~date:(date_a_nbjours "20160428")
        ~heure:(heure_a_nbsecs "12:10:00") desjardins chez_nous  ~rmax:0.25 in
    let attendu = [
      (0.0838530217212952456, 0.179909546084110278,
       [("801", 43860, 45060,
         [1515; 1787; 1776; 1790; 1791; 2000; 2001; 2003; 2006; 1434; 2007;
          4144; 3099])]);
      (0.0838530217212952456, 0.179909546084110278,
       [("800", 44160, 45360,
         [1515; 1787; 1776; 1790; 1791; 2000; 2001; 2003; 2006; 1434; 2007;
          4144; 3099])]);
      (0.200345313296168781, 0.0318930938209307721,
       [("11", 44938, 45960,
         [1783; 1999; 1787; 1776; 1790; 1791; 1793; 1794; 1795; 1796; 1798;
          1799; 1801; 1802; 1804; 1803; 1805; 1806])]);
      (0.24702817980565947, 0.0318930938209307721,
       [("11", 45000, 45960,
         [1999; 1787; 1776; 1790; 1791; 1793; 1794; 1795; 1796; 1798; 1799;
          1801; 1802; 1804; 1803; 1805; 1806])]);
      (0.0838530217212952456, 0.179909546084110278,
       [("87", 43860, 44557, [1515; 1787; 1776; 1790; 1791; 2000]);
        ("800", 44700, 45360, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])]);
      (0.0838530217212952456, 0.179909546084110278,
       [("87", 43860, 44557, [1515; 1787; 1776; 1790; 1791; 2000]);
        ("801", 45000, 45660, [2000; 2001; 2003; 2006; 1434; 2007; 4144; 3099])]);
      (0.0838530217212952456, 0.0318930938209307721,
       [("87", 43860, 44416, [1515; 1787; 1776; 1790; 1791]);
        ("11", 45351, 45960,
         [1791; 1793; 1794; 1795; 1796; 1798; 1799; 1801; 1802; 1804; 1803;
          1805; 1806])]);
      (0.0838530217212952456, 0.0318930938209307721,
       [("801", 43860, 44288, [1515; 1787; 1776; 1790; 1791]);
        ("11", 45351, 45960,
         [1791; 1793; 1794; 1795; 1796; 1798; 1799; 1801; 1802; 1804; 1803;
          1805; 1806])]);
      (0.0838530217212952456, 0.154517201193211856,
       [("801", 43860, 44965,
         [1515; 1787; 1776; 1790; 1791; 2000; 2001; 2003; 2006; 1434; 2007;
          4144]);
        ("13", 46152, 46320, [4144; 1380])]);
      (0.0838530217212952456, 0.0318930938209307721,
       [("800", 44160, 44588, [1515; 1787; 1776; 1790; 1791]);
        ("11", 45351, 45960,
         [1791; 1793; 1794; 1795; 1796; 1798; 1799; 1801; 1802; 1804; 1803;
          1805; 1806])])]
    in
    if (assert_equal "trouver_trajet_optimal@longueur_correct1" 
	  (List.length obtenu) (List.length attendu)) 
    then () else comment := (!comment) @ [ "longueur incorrecte" ];

    if (assert_equal_list "trouver_trajet_optimal@contenu_correct1" 
	  obtenu attendu) 
    then () else comment := (!comment) @ [ "contenu incorrect" ];

    if (assert_equal "trouver_trajet_optimal@ordre_correct1" 
	  obtenu  attendu) 
    then () else comment := (!comment) @ [ "ordre incorrecte" ];

    let fake_pos = (new coordonnees (-46.778826)(-71.275169)) in 
    if (assert_throw_exception
          "trouver_trajet_optimal@position1_invalide" 
          (fun () -> app#trouver_trajet_optimal ~date:(date_a_nbjours "20160428")
              ~heure:(heure_a_nbsecs "12:10:00") desjardins fake_pos  ~rmax:0.25))
    then () else comment := (!comment) @ [ "precondition position1"];

    if (assert_throw_exception
          "trouver_trajet_optimal@position2_invalide" 
          (fun () -> app#trouver_trajet_optimal ~date:(date_a_nbjours "20160428")
              ~heure:(heure_a_nbsecs "12:10:00") fake_pos chez_nous  ~rmax:0.25))
    then () else comment := (!comment) @ [ "precondition position2"];

    if (assert_throw_exception
          "trouver_trajet_optimal@prayon_invalide" 
          (fun () -> app#trouver_trajet_optimal ~date:(date_a_nbjours "20160428")
              ~heure:(heure_a_nbsecs "12:10:00") desjardins chez_nous  ~rmax:(-25.)))
    then () else comment := (!comment) @ [ "precondition rayon"];

    if (assert_throw_exception
          "trouver_trajet_optimal@position_invalide" 
          (fun () -> app#trouver_trajet_optimal ~date:(date_a_nbjours "20120428")
              ~heure:(heure_a_nbsecs "12:10:00") desjardins chez_nous  ~rmax:0.25))
    then () else comment := (!comment) @ [ "precondition date"];

    if (assert_throw_exception
          "trouver_trajet_optimal@position_invalide" 
          (fun () -> app#trouver_trajet_optimal ~date:(date_a_nbjours "20160428")
              ~heure:(-4) desjardins chez_nous  ~rmax:0.25))
    then () else comment := (!comment) @ [ "precondition heure"];
    !comment

  with 
  | Non_Implante _ -> comment := (!comment) @ [ "Fonction non_implantee" ]; 
    !comment
  | _ -> comment := (!comment) @ [ "Test non complete" ]; !comment;;


let test() =
  let all_tests = [ "lister_stations_sur_itineraire", test1;
                    "prochain_arret", test2;
                    "prochaines_lignes_entre", test3;
                    "maj_etiquette_arete", test4;
                    "paires_stations_possibles", test5;
                    "plus_court_chemin", test6;
                    "plans_possibles_pour_chemin", test_plus;
                    "trouver_trajet_optimal", test7;
                  ] in
  List.fold_left
    (fun l_res (nom_f, t) ->
       let comments = t () in  (l_res @ [(nom_f, comments)])
    ) [] all_tests;;

(*
Résultats attendus:

test();;

- : (string * string list) list =
[("lister_stations_sur_itineraire", []); ("prochain_arret", []);
 ("prochaines_lignes_entre", []); ("maj_etiquette_arete", []);
 ("paires_stations_possibles", []); ("plus_court_chemin", []);
 ("plans_possibles_pour_chemin", []); ("trouver_trajet_optimal", [])]

*)
