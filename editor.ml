open Graphics;;

let tileTaille = 32;;
let nbLigne = 16 and nbColonne = 22;;
let widthFen = nbColonne*tileTaille + 296;;
let heightFen = 850;;
let nbImages = 160;;

Graphics.open_graph (" "^(string_of_int widthFen)^"x"^(string_of_int heightFen));; (* Attention : il faut un espace au début de la chaine *)

let getColor : string -> color = fun line ->
let r = int_of_string ("0x"^(String.sub line 0 2)) in
let g = int_of_string ("0x"^(String.sub line 2 2)) in
let b = int_of_string ("0x"^(String.sub line 4 2)) in
Graphics.rgb r g b;;

let rec construireArray : in_channel -> string -> int -> color array = fun c line n -> match n with
| a when a = (tileTaille-1) -> [|(getColor line)|]
| a -> Array.append [|(getColor line)|] (construireArray c (input_line c) (n+1));;

let rec construireArrayArray : in_channel -> int -> color array array = fun c m -> match m with
| a when a = (tileTaille-1) -> [|(construireArray c (input_line c) 0)|]
| a -> Array.append [|(construireArray c (input_line c) 0)|] (construireArrayArray c (m+1));;

let rec chargerImages : int -> image list = fun e -> match e with
| a when a = (nbImages+1) -> []
| _ -> let channel = open_in ("./img/img"^(string_of_int e)^".txt") in
       let caa = construireArrayArray channel 0 in
       (Graphics.make_image caa) :: chargerImages (e+1) ;;

let images = chargerImages 1;;

let rec convert : string -> int list = fun chaine -> if (String.length chaine) = 0 then []
else let posEspace = String.index chaine ' ' in int_of_string (String.sub chaine 0 posEspace) :: convert (String.sub chaine (posEspace+1) ((String.length chaine)-posEspace-1));;

let rec getLines : in_channel -> int list list = fun channel -> try let line = input_line channel in [(convert line)] @ (getLines channel) with End_of_file -> [[]];;

let lireFichier : int -> int list list = fun numMap -> 
let fichier = "./map/map"^(string_of_int numMap)^".txt" in
let channel = open_in fichier in
let lines = getLines channel in
close_in channel;lines;;

let rec ligneToString : int list -> int -> string = fun t i -> match i with
| a when a = nbColonne -> "\n"
| v -> string_of_int (List.nth t v) ^ " " ^ ligneToString t (i+1);;

let rec tableauToString : int list list -> int -> string = fun t compteur -> match compteur with
| b when b = nbLigne -> ""
| w -> (ligneToString (List.nth t w) 0) ^ tableauToString t (w+1);;

let sauvegarde = fun t numMap -> let channel = open_out ("./map/map"^(string_of_int numMap)^".txt") in (* Sauvegarde du tableau t dans le fichier map^numMap^.txt *)
Printf.fprintf channel "%s\n" (tableauToString t 0) ; close_out channel;;
 
let dessinerTile : int -> int -> int -> unit = fun num numLigne numColonne -> match num with
| 0 -> Graphics.set_color black ; Graphics.fill_rect (numColonne *tileTaille) ((heightFen-tileTaille) - (numLigne *tileTaille)) tileTaille tileTaille
| _ -> Graphics.draw_image (List.nth images (num-1)) (numColonne *tileTaille) ((heightFen-tileTaille) - (numLigne *tileTaille));;

let rec dessinerCarte = fun t depLigne depColonne -> 
match (depLigne,depColonne) with
| (a,b) when a = nbLigne -> ()
| (a,b) when b = nbColonne -> dessinerCarte t (a+1) 0
| (a,b) -> dessinerTile (List.nth (List.nth t a) b) a b;dessinerCarte t a (b+1);;

let rec dessinerToutesTiles = fun n -> match n with 
| a when a = nbImages -> dessinerTile n (n mod 25) ((n/25)+24)
| _ -> dessinerTile n (n mod 25) ((n/25)+24) ; dessinerToutesTiles (n+1)
;;

let dessinerChoixTile = fun numMap tileCourante ->
			    dessinerToutesTiles 0;
			    
			    dessinerTile tileCourante 18 10; (* Affiche la couleur actuellement sélectionnée*)
			    Graphics.moveto 10 340 ; Graphics.draw_string "Actuellement selectionnee --> " ;
			    
			    dessinerTile 0 18 15;
			    dessinerTile 0 18 17; (* Pour changer la map à éditer *)
			    Graphics.moveto 10 300 ; Graphics.draw_string "Cliquez a gauche pour la map precedente, et a droite pour la map suivante : " ;
			    Graphics.moveto 10 280 ; Graphics.draw_string ("Vous etes sur la map " ^ (string_of_int numMap));;

let attendreClic () = let dep = wait_next_event [Button_down] in (dep.mouse_x,dep.mouse_y);;

let getLigneColonne : (int*int) -> (int*int) = fun (posX,posY) -> let colonne = posX / tileTaille and ligne = (850-posY) / tileTaille in (ligne,colonne);;   

let rec sousListeModifiee : int list -> int -> int -> int -> int list = fun t b compteurColonne tileCourante -> match compteurColonne with
| v when v = b -> tileCourante :: (sousListeModifiee t b (compteurColonne + 1) tileCourante)
| v when v = nbColonne -> []
| v -> (List.nth t v) :: (sousListeModifiee t b (compteurColonne + 1) tileCourante);;

let rec listeModifiee : int list list -> int -> int -> int -> int -> int list list= fun t a b compteurLigne tileCourante -> match compteurLigne with
| v when v = a -> (sousListeModifiee (List.nth t v) b 0 tileCourante) :: (listeModifiee t a b (compteurLigne+1) tileCourante)
| v when v = nbLigne -> []
| v -> (List.nth t v) :: (listeModifiee t a b (compteurLigne+1) tileCourante);;

let gererClic : int -> int -> int list list * int * int = fun numMap tileCourante -> match (getLigneColonne (attendreClic ())) with
| (a,b) when a < nbLigne && b < nbColonne -> ((listeModifiee (lireFichier numMap) a b 0 tileCourante),numMap,tileCourante)

| (a,b) when a <= 24 && b = 24 -> let nouvelleTile = a+((b-24)*25) in (lireFichier numMap,numMap,nouvelleTile) (* Renvoie le tableau inchangé *)
| (a,b) when a <= 24 && b = 25 -> let nouvelleTile = a+((b-24)*25) in (lireFichier numMap,numMap,nouvelleTile) (* Renvoie le tableau inchangé *)
| (a,b) when a <= 24 && b = 26 -> let nouvelleTile = a+((b-24)*25) in (lireFichier numMap,numMap,nouvelleTile) (* Renvoie le tableau inchangé *)
| (a,b) when a <= 24 && b = 27 -> let nouvelleTile = a+((b-24)*25) in (lireFichier numMap,numMap,nouvelleTile) (* Renvoie le tableau inchangé *)
| (a,b) when a <= 24 && b = 28 -> let nouvelleTile = a+((b-24)*25) in (lireFichier numMap,numMap,nouvelleTile) (* Renvoie le tableau inchangé *)
| (a,b) when a <= 24 && b = 29 -> let nouvelleTile = a+((b-24)*25) in (lireFichier numMap,numMap,nouvelleTile) (* Renvoie le tableau inchangé *)
| (a,b) when a <= 10 && b = 30 -> let nouvelleTile = a+((b-24)*25) in (lireFichier numMap,numMap,nouvelleTile) (* Renvoie le tableau inchangé *)

| (a,b) when a = 18 && b = 15 && numMap <> 1 -> let nouveauNumMap = numMap - 1 in (lireFichier nouveauNumMap , nouveauNumMap,tileCourante) (* Renvoie la carte précédente *)
| (a,b) when a = 18 && b = 17 && numMap <> 6 -> let nouveauNumMap = numMap + 1 in (lireFichier nouveauNumMap , nouveauNumMap,tileCourante) (* Renvoie la carte suivante *)
| _ -> (lireFichier numMap,numMap , tileCourante);; (* Renvoie le tableau et le numéro courant de la map inchangés *)

let rec loop numMap tileCourante = let (tableau,nouveauNumMap,nouvelleTile) = gererClic numMap tileCourante in sauvegarde tableau nouveauNumMap ; dessinerCarte tableau 0 0 ; dessinerChoixTile nouveauNumMap nouvelleTile ; loop nouveauNumMap nouvelleTile;;

dessinerCarte (lireFichier 5) 0 0;;
dessinerChoixTile 5 1;;
loop 5 1;; (* numMap tileCourante *)


