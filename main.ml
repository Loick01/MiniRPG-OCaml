open Graphics;;
open Unix;;

let tileTaille = 32;;
let caseCadreTaille = 47;;
let nbLigne = 16;;
let nbColonne = 22;;
let widthFen = nbColonne*tileTaille;;
let heightFen = nbLigne*tileTaille;;
let nbMapLigneCarte = 3;;
let tmpEntreDep = 0.25;;
let listeTileCollision = [3;4;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;43;56;57;58;59;60;61;62;63;93;92;123;124;125;126;127;128;129;130;131;132;133;134;135;
                          136;137;138;139;140;141;142;143;144;145;146;147;148;149;150;151;152;153;154;155;156;157;158;159;160];; (* Numéro des tiles qui font des collisions avec le personnage *)
let nbImages = 160;;

Graphics.open_graph (" "^(string_of_int widthFen)^"x"^(string_of_int heightFen));; (* Attention : il faut un espace au début de la chaine *)

(** getColor l renvoie une valeur de type color (du module Graphics) correspondant a la chaine l (composee de 6 caracteres, ou les 2 premiers sont la valeur hexadecimale pour la couleur rouge, les 2 suivants pour la couleur verte, et les 2 derniers pour la couleur bleu)*)
let getColor : string -> color = fun line ->
  let r = int_of_string ("0x"^(String.sub line 0 2)) in
  let g = int_of_string ("0x"^(String.sub line 2 2)) in
  let b = int_of_string ("0x"^(String.sub line 4 2)) in
  Graphics.rgb r g b
;;

(** construireArray c l n x renvoie une color array de taille x correspondant a une ligne de pixel d'une image ayant servi a ouvrir le channel c, et n sert de compteur*)
let rec construireArray : in_channel -> string -> int -> int -> color array = fun c line n lenX -> 
  match n with (* On est obligé d'utiliser des arrays pour les images voir documentation Graphics *)
    | a when a = (lenX-1) -> [|(getColor line)|]
    | a -> Array.append [|(getColor line)|] (construireArray c (input_line c) (n+1) lenX)
;;
    
(** construireArrayArray c m x y renvoie une color array array correspondant a une image ayant servi a ouvrir le channel c, de longueur x et de hauteur y, et m sert de compteur*)
let rec construireArrayArray : in_channel -> int -> int -> int -> color array array = fun c m lenX lenY -> 
  match m with
    | a when a = (lenY-1) -> [|(construireArray c (input_line c) 0 lenX)|]
    | a -> Array.append [|(construireArray c (input_line c) 0 lenX)|] (construireArrayArray c (m+1) lenX lenY)
;;

(** chargerImagesCadre n renvoie une liste de n valeur de type image (du module Graphics), chacune correspondant respectivement au fichier ./img/imgCadren.txt*)
let rec chargerImagesCadre : int -> image list = fun e -> match e with
| 10 -> [] (* Il y a 9 images pour le cadre *)
| _ -> let channel = open_in ("./img/imgCadre"^(string_of_int e)^".txt") in
       let caa = construireArrayArray channel 0 caseCadreTaille caseCadreTaille in 
       (Graphics.make_image caa) :: chargerImagesCadre (e+1)
;;
       
(** chargerImagesTile n renvoie une liste de n valeur de type image (du module Graphics), chacune correspondant respectivement au fichier ./img/imgn.txt*)
let rec chargerImagesTile : int -> image list = fun e -> match e with
| a when a = (nbImages+1) -> []
| _ -> let channel = open_in ("./img/img"^(string_of_int e)^".txt") in
       let caa = construireArrayArray channel 0 tileTaille tileTaille in 
       (Graphics.make_image caa) :: chargerImagesTile (e+1)
;;

(** chargerImagesMonstres () renvoie une liste de 3 valeur de type image (du module Graphics), chacune correspondant respectivement au fichier ./img/imgMonstren.txt*)       
let rec chargerImagesMonstres : unit -> image list = fun () ->
  let channel1 = open_in "./img/imgMonstre1.txt" and
      channel2 = open_in "./img/imgMonstre2.txt" and
      channel3 = open_in "./img/imgMonstre3.txt" in
  
  let caa1 = construireArrayArray channel1 0 290 250 and 
      caa2 = construireArrayArray channel2 0 201 250 and
      caa3 = construireArrayArray channel3 0 346 270 in
  
  (Graphics.make_image caa1) :: (Graphics.make_image caa2) :: (Graphics.make_image caa3) :: [] 
;;

let images = chargerImagesTile 1;; (* Liste des images des tiles *)
let monstres = chargerImagesMonstres ();; (* Liste des images des monstres *)
let imagesCadre = chargerImagesCadre 1;; (* Liste des images pour le cadre *)

(** dessinerLigneSupCadre e va utiliser les images de la liste imagesCadre pour dessiner la ligne superieure du cadre, et e sert de compteur*)
let rec dessinerLigneSupCadre : int -> unit = fun e -> match e with
| 0 -> (Graphics.draw_image (List.nth imagesCadre 7) 0 141) ; dessinerLigneSupCadre (e+1)
| 14 -> (Graphics.draw_image (List.nth imagesCadre 8) (e*47) 141)
| _ -> (Graphics.draw_image (List.nth imagesCadre 4) (e*47) 141) ; dessinerLigneSupCadre (e+1)

(** dessinerLigneInfCadre e va utiliser les images de la liste imagesCadre pour dessiner la ligne inferieur du cadre, et e sert de compteur*)
let rec dessinerLigneInfCadre : int -> unit = fun e -> match e with
| 0 -> (Graphics.draw_image (List.nth imagesCadre 5) 0 0) ; dessinerLigneInfCadre (e+1)
| 14 -> (Graphics.draw_image (List.nth imagesCadre 6) (e*47) 0)
| _ -> (Graphics.draw_image (List.nth imagesCadre 2) (e*47) 0) ; dessinerLigneInfCadre (e+1)

(** dessinerLigneMedCadre e numLigne va utiliser les images de la liste imagesCadre pour dessiner les lignes entre la ligne superieur et la ligne inferieur du cadre, numLigne servant a determiner la position où dessiner la ligne, et e sert de compteur*)
let rec dessinerLigneMedCadre : int -> int -> unit = fun e numLigne -> match e with
| 0 -> (Graphics.draw_image (List.nth imagesCadre 3) 0 (188-((numLigne+1)*47))) ; dessinerLigneMedCadre (e+1) numLigne
| 14 -> (Graphics.draw_image (List.nth imagesCadre 1) (e*47) (188-((numLigne+1)*47)))
| _ -> (Graphics.draw_image (List.nth imagesCadre 0) (e*47) (188-((numLigne+1)*47))) ; dessinerLigneMedCadre (e+1) numLigne

(** construireCadre e va dessiner a l'ecran un cadre de 4 ligne, et e sert de compteur. Ce cadre sera ensuite recuperer (grâce a Graphics.get_image) puis reutiliser*)
let rec construireCadre : int -> unit = fun e -> match e with
| 0 -> dessinerLigneSupCadre 0 ; construireCadre (e+1)
| 3 -> dessinerLigneInfCadre 0
| n -> dessinerLigneMedCadre 0 n ; construireCadre (e+1)
;;

let cadre = construireCadre 0 ; Graphics.get_image 0 0 704 188;; (* Erreur Stack overflow quand on essaye de construire le cadre en une seule array comme pour les autres images, du coup on divise le cadre en 9 images (beaucoup de ces images se répète dans le cadre), puis on le reconstitue, et on le récupère depuis la fenêtre, et on le sauvegarde au type image *) 

(** dessinerChaine l x y dessine sur la fenetre la chaine l en (x,y)*)
let dessinerChaine : string -> int -> int -> unit = fun chaine posX posY ->
  (try
    Graphics.set_font "-*-fixed-medium-r-semicondensed--23-*-*-*-*-*-iso8859-1" (* Si la police n'est pas reconnu, le programme va gérer l'erreur *)
  with
    _ -> ()) ;
  Graphics.moveto posX posY ;
  Graphics.set_color white ;
  Graphics.draw_string chaine ;
;;

Random.self_init();;

(* TYPE POUR LE PERSONNAGE *)
type classe = Archer | Guerrier | Magicien;;
type genre = Homme | Femme;;
type sac = { pieces : int ; poulet : int ; eponges : int };;
type personnage = { nom : string ; classe : classe ; genre : genre ; pv : float ; exp : float ; niveau : int ; items : sac };;

(* TYPE POUR LES MONSTRES *)
type race = Golem | Moustiques of int | Serpent;;
type drop = Piece of int | Poulet of int | Eponge of int | None;; (* Les monstres drop une certaines quantités en fonction de leur nombre de pv *)
type monstre = { style : race ; item : drop ; pv : int};; 

(** contenuSac s retourne une chaine decrivant le contenu du sac s*)
let contenuSac : sac -> string = fun sac -> (string_of_int sac.eponges)^" eponges, "^(string_of_int sac.poulet)^" poulets, "^(string_of_int sac.pieces)^" pieces";;

(** getClasse p retourne une chaine correspondant a la classe du personnage p, en d'adaptant a son genre*)
let getClasse : personnage -> string = fun p -> match p.classe with
  | Archer -> if p.genre = Homme then "Archer" else "Archere"
  | Guerrier -> if p.genre = Homme then "Guerrier" else "Guerriere"
  | Magicien -> if p.genre = Homme then "Magicien" else "Magicienne"
;;

(** afficherPersonnage p affiche sur la fenetre les informations du personnage p, a des positions determines*)
let afficherPersonnage : personnage -> unit = fun p -> 
  Graphics.draw_image cadre 0 0 ; 
  dessinerChaine "Voici l'etat de votre pitoyable personnage : " 20 143 ;
  dessinerChaine (p.nom^" | "^(getClasse p)^" niveau "^(string_of_int p.niveau)) 35 103 ;
  dessinerChaine ("Points d'experience : " ^ (string_of_float p.exp)^" | Points de vie : " ^ (string_of_float p.pv)) 35 63 ;
  dessinerChaine ("Contenu de votre sac : " ^ (contenuSac p.items)) 35 23
;;

(** afficherApparitionMonstre m vide la fenetre, affiche sur celle-ci l'image correspondant au monstre m, et affiche une phrase introduisant le monstre et donnant son nombre de pv*)
let afficherApparitionMonstre : monstre -> unit = fun m -> 
  clear_graph () ;
  Graphics.draw_image cadre 0 0 ; 
  match m.style with
    | Moustiques (n) when n = 1 -> dessinerChaine "Un moustique en quete de vengeance vous attaque !" 25 143 ; 
                                   dessinerChaine "Vous etes a jour dans vos vaccins ? On sait jamais... " 25 103 ;
                                   dessinerChaine ("Il a "^(string_of_int m.pv)^" points de vie") 25 63 ;
                                   (Graphics.draw_image (List.nth monstres 1) 240 200) 
    | Moustiques (n) -> dessinerChaine ("Une nuee de "^(string_of_int n)^" moustiques volent autour de vous !") 25 143 ;
                        dessinerChaine "Il est peut-etre l'heure de se doucher non ?" 25 103 ;
                        dessinerChaine ("Les moustiques ont "^(string_of_int m.pv)^" points de vie") 25 63 ;
                        (Graphics.draw_image (List.nth monstres 1) 240 200) 
    | Golem -> dessinerChaine "Un immense golem surgit et veut en decoudre !" 25 143 ;
               dessinerChaine ("Il a "^(string_of_int m.pv)^" points de vie") 25 103 ;
               (Graphics.draw_image (List.nth monstres 0) 205 200)
    | Serpent -> dessinerChaine "Un serpent rampe vers vous !" 25 143 ;
                 dessinerChaine ("Il a "^(string_of_int m.pv)^" points de vie") 25 103 ;
                 (Graphics.draw_image (List.nth monstres 2) 180 200)
;;

(** genererItem n genere aleatoirement un item, en une certaine quantite determinee selon n. Le dernier cas du filtrage n'arrive jamais, c'est juste pour enlever le warning a la compilation *)
let genererItem : int -> drop = fun nbPv -> 
  match (Random.int 3) with (*Plus le monstre a de pv, plus il drop l'item *)
    | 0 -> Piece (nbPv)
    | 1 -> Poulet (nbPv/2)
    | 2 -> Eponge (nbPv/4)
    | _ -> None
;;

(** spawnMonstre () renvoie un monstre de style aleatoire, avec un nombre de point de vie fixe plus une valeur aleatoire. Le dernier cas du filtrage n'arrive jamais, c'est juste pour enlever le warning a la compilation *)
let rec spawnMonstre : unit -> monstre = fun () -> 
  match (Random.int 3) with
    | 0 -> let nbPv = (25+(Random.int 6)+1) in { style=Golem ; item=genererItem nbPv ; pv=nbPv }
    | 1 -> let nbMoustique = 1+(Random.int 10) in let nbPv = 2 + nbMoustique in {style=Moustiques (nbMoustique);item=None;pv=nbPv }
    | 2 -> let nbPv = (10+(Random.int 4)+1) in { style=Serpent;item=genererItem nbPv;pv=nbPv}
    | _ -> spawnMonstre ()
;;


(** frapper p affiche sur la fenetre le deroulement des actions, et renvoie un int correspondant au points de degats fait par le personnage p, selon sa classe, avec un certain pourcentage de reussite dependant de sa classe et de son niveau *)
let frapper : personnage -> int = fun p -> 
  match p.classe with
    | Archer -> if (Random.int 100) < 70+(5*(p.niveau-1)) then (dessinerChaine "Votre fleche atteint sa cible !" 25 143 ; 4) 
                else (dessinerChaine "Vous loupez votre cible, comment avez-vous pu echouer ?" 25 143 ; 0)
    | Guerrier -> if (Random.int 100) < 30+(5*(p.niveau-1)) then (dessinerChaine "Votre lame atteint le monstre !" 25 143 ; 10) 
                  else (dessinerChaine "Vous etes si lent que vous n'atteignez pas votre cible !" 25 143 ; 0)
    | Magicien -> if (Random.int 100) < 50+(5*(p.niveau-1)) then (dessinerChaine "Vous touchez l'ennemi avec votre sort !" 25 143 ; 5)
                  else (dessinerChaine "Votre sort ricoche sur le monstre, vous ne voudriez pas changer de classe par hasard ?" 25 143 ; 0)
;;

(** monstre_frapper m affiche sur la fenetre le deroulement des actions, et renvoie un float (pour pouvoir être soustrait au pv du joueur qui sont des float) correspondant au points de degats fait par le monstre m, selon son style*)
let monstre_frapper : monstre -> float = fun p -> 
  match p.style with
    | Golem -> dessinerChaine "Le golem frappe et vous ecrase !" 25 143 ; 4.
    | Moustiques (n) when n = 1 -> dessinerChaine "Le moustique aspire une goutte de votre sang !" 25 143 ; 0.5 (* Vérifiez sur le sujet pour les dégats d'un seul moustique *)
    | Moustiques (n) -> dessinerChaine "Vous attirez la nuee de moustiques qui preleve votre sang !" 25 143 ; 0.5 *. (Float.of_int n)
    | Serpent -> dessinerChaine "Le serpent attaque et vous mords !" 25 143 ; 2.
;;

(** manger p affiche sur la fenetre le deroulement des actions, et renvoie un couple dont la première valeur est un booleen indiquant si le personnage p a reussi a manger, et la seconde etant le nouvel etat de p *)
let manger : personnage -> bool * personnage = fun p -> 
  Graphics.draw_image cadre 0 0 ; 
  dessinerChaine "Appuyez une sur une touche pour sortir" 25 25;
  match p.items.poulet with
    | 0 -> dessinerChaine "Vous n'avez pas de poulet dans votre sac !" 25 143; (false , p)
    | n when p.pv=20. -> dessinerChaine "Vous avez deja tout vos pv" 25 143 ; dessinerChaine "Manger ce poulet pourrait avoir de graves consequences !" 25 103; (false , p) 
    | n when p.pv=19. -> dessinerChaine "Vous recuperez 1 pv et etes au bord de l'indigestion !" 25 143; (true , {p with pv=20. ; items = {p.items with poulet=(n-1)}}) 
    | n -> dessinerChaine "Vous mangez un delicieux poulet et recuperez 2 pv !" 25 143 ; (true , {p with pv=p.pv+.2. ; items = {p.items with poulet=(n-1)}})
;;

type entitee = Personnage of personnage | Monstre of monstre;;

(** dormir p affiche sur la fenetre le deroulement des actions, et renvoie une aleatoirement une entitee : soit un monstre qui tue le joueur pendant son sommeil, soit un personnage ayant le nouvel etat du personnage p*)
let dormir : personnage -> entitee = fun p -> 
  Graphics.draw_image cadre 0 0 ; 
  match (Random.int 100) with
    | n when n < 5 -> Monstre (spawnMonstre ())
    | n when p.pv>16. -> dessinerChaine "Vous vous endormez... Vous vous reveillez en sursaut !" 25 143 ; 
                         dessinerChaine ("Vous avez recupere "^(string_of_float (20.-.p.pv))^" pv !") 25 103 ; 
                         Personnage {p with pv=20.}
    | n -> dessinerChaine "Vous vous endormez... Vous vous reveillez enfin !" 25 143 ; 
           dessinerChaine "Vous avez recupere 4 pv !" 25 103 ; 
           Personnage {p with pv=p.pv+.4.;}
;;

(** getInput () renvoie le char correspondant a la touche du clavier pressee par le joueur, les test servant a ne pas renvoyer ce char tant qu'une touche est disponible dans la file des événement (voir documentation Graphics)*)
let getInput : unit -> char = fun () -> let rec aux : char -> char = fun key -> if (key_pressed ()) then aux (read_key ()) else (Unix.sleepf tmpEntreDep ; key) in aux (read_key ());;

(** attendreInput () renvoie un type unit. Cette fonction permet de faire defiler des messages sur la fenetre, lorsque le joueur appuie sur n'importe quel touche du clavier*)
let attendreInput : unit -> unit = fun () -> let rec aux : char -> unit = fun key -> if (key_pressed ()) then aux (read_key ()) else () in aux (read_key ());;

(** attendreClic () renvoie un couple de int correspondant aux coordonnees du clic de la souris du joueur*)
let attendreClic : unit -> (int*int) = fun () -> let dep = wait_next_event [Button_down] in (dep.mouse_x,dep.mouse_y);;

(** dansZoneClic (x,y) (infX,infY) (supX,supY) renvoie true si les cooordonnees (x,y) sont a l'interieur du rectangle dont le coin inferieur gauche est en (infX,infY) et le coin superieur droit est en (supX,supY), false sinon*)
let dansZoneClic : int*int -> int*int -> int*int -> bool = fun (x,y) (infX,infY) (supX,supY) -> (x >= infX && x <= supX) && (y >= infY && y <= supY);;

(** mortDuJoueur p affiche sur la fenetre le deroulement des actions, en s'adaptant au genre du personnage p, et attend l'appui d'une touche du clavier par le joueur avant de sortir de cette fonction en renvoyant le type unit*)
let mortDuJoueur : personnage -> unit = fun p -> 
  Graphics.draw_image cadre 0 0 ; 
  let mot = (if p.genre = Homme then "mort !" else "morte !") in 
  dessinerChaine ("Vous etes "^mot) 25 143 ;
  attendreInput () ; afficherPersonnage p ; attendreInput ()
;;

exception PersonnageMort of personnage;;
exception MonstreVaincu of personnage;;

(** ajouterDropMonstre p m affiche sur la fenetre le deroulement des actions, et renvoie le nouvel etat du personnage p apres lui avoir donne le drop qui appartenait au monstre m*)
let ajouterDropMonstre : personnage -> monstre -> personnage  = fun p m -> 
  Graphics.draw_image cadre 0 0 ;
  match m.item with
    | Piece (n) -> dessinerChaine ("Vous recuperez "^(string_of_int n)^" pieces d'or sur le monstre !") 25 143 ; {p with items = {p.items with pieces=p.items.pieces+n}}
    | Poulet (n) -> dessinerChaine ("Vous recuperez "^(string_of_int n)^" poulets sur le monstre !") 25 143; {p with items = {p.items with poulet=p.items.poulet+n}}
    | Eponge (n) -> dessinerChaine ("Vous recuperez "^(string_of_int n)^" eponges sur le monstre !") 25 143 ; {p with items = {p.items with eponges=p.items.eponges+n}}
    | None -> dessinerChaine "Vous ne recuperez rien sur le monstre !" 25 143 ; p
;;

(** ajouterExpMonstre p m affiche sur la fenetre le deroulement des actions, et renvoie le nouvel etat du personnage p apres lui avoir donne un certain nombre de points d'experiences selon le style du monstre m*)
let ajouterExpMonstre : personnage -> monstre -> personnage = fun p -> fun m -> 
  match m.style with
    | Golem -> dessinerChaine "Vous gagnez 8 points d'experiences !" 25 103 ; 
               if p.exp+.8. >= (2.**(Float.of_int(p.niveau+1))) *. 10. then (dessinerChaine ("Vous passez au niveau "^(string_of_int (p.niveau+1))) 25 63 ; {p with exp=p.exp+.8. ; niveau=p.niveau+1})
               else {p with exp=p.exp+.8.}
    | Moustiques (_) -> dessinerChaine "Vous gagnez 2 points d'experiences !" 25 103 ;
               if p.exp+.2. >= (2.**(Float.of_int(p.niveau+1))) *. 10. then (dessinerChaine ("Vous passez au niveau "^(string_of_int (p.niveau+1))) 25 63 ; {p with exp=p.exp+.2. ; niveau=p.niveau+1})
               else {p with exp=p.exp+.2.}
    | Serpent -> dessinerChaine "Vous gagnez 4 points d'experiences !" 25 103 ;
               if p.exp+.4. >= (2.**(Float.of_int(p.niveau+1))) *. 10. then (dessinerChaine ("Vous passez au niveau "^(string_of_int (p.niveau+1))) 25 63 ; {p with exp=p.exp+.4.; niveau=p.niveau+1})
               else {p with exp=p.exp+.4.}
;;

(** finCombat p m retourne le nouvel etat du personnage p, apres lui avoir donne un drop et un certain nombre de points d'experiences selon le monstre m*)
let finCombat : personnage -> monstre -> personnage = fun p m -> let j = ajouterDropMonstre p m in attendreInput () ; ajouterExpMonstre j m;;

(** tourCombat p m affiche sur la fenetre le deroulement des actions, peut lever des  exceptions MonstreVaincu ou PersonnageVaincu selon, respectivement, le nombre de pv du personnage p et du monstre m, ou retourne un couple dont le premier element est le nouvel etat de p, et le second element est le nouvel etat de m, les nouvels etats etant determines pour les 2 elements apres s'etre infliges mutuelllement des degats*)
let tourCombat : personnage -> monstre -> personnage * monstre = fun p m ->
  Graphics.draw_image cadre 0 0 ;
  let joueurDegat = frapper p in let newM = {m with pv=(m.pv-joueurDegat)} in 
  	if newM.pv <= 0 then raise (MonstreVaincu (p)) 
  	else dessinerChaine ("Vous avez retire "^(string_of_int joueurDegat)^" pv au monstre !") 25 103 ; dessinerChaine ("Il lui en reste "^(string_of_int newM.pv)) 25 63 ;
  	     attendreInput () ; 
  	     Graphics.draw_image cadre 0 0 ;
  	     let monstreDegat = monstre_frapper m in let newP = {p with pv=(p.pv-.monstreDegat)} 
  	     in if newP.pv <= 0. then (mortDuJoueur newP ; raise (PersonnageMort (newP))) 
  	        else (dessinerChaine ("Le monstre vous a retire "^(string_of_float monstreDegat)^" pv !") 25 103 ; 
  	             dessinerChaine ("Il vous en reste "^(string_of_float (newP.pv))) 25 63 ; 
  	             attendreInput () ; (newP,newM))
;;

(** combattre p m affiche sur la fenetre le deroulement des actions, et renvoie le nouvel etat de p, selon son choix d'action (attaquer, manger, visualiser son personnage, ou fuire) ou selon si l'exception MonstreVaincu est geree dans cette fonction, auquel cas finCombat p m est appele*)
let rec combattre : personnage -> monstre -> personnage = fun p m -> 
  Graphics.draw_image cadre 0 0 ; 
  dessinerChaine "Cliquez sur l'action de votre choix" 25 143 ;
  dessinerChaine "Attaquer" 200 35; dessinerChaine "Manger" 450 35 ; dessinerChaine "Visualiser" 200 90 ; dessinerChaine "Fuir" 450 90 ;
  try
    match attendreClic () with
      | att when dansZoneClic att (200,35) (292,58) -> let personnage,monstre = tourCombat p m in (combattre personnage monstre)
      | mgr when dansZoneClic mgr (450,35) (522,58) -> let (_,personnage) = manger p in attendreInput () ; combattre personnage m
      | visual when dansZoneClic visual (200,90) (320,113) -> afficherPersonnage p ; attendreInput () ; (combattre p m)
      | fuit when dansZoneClic fuit (450,90) (498,113) -> Graphics.draw_image cadre 0 0 ; 
                                                          dessinerChaine "Vous courez aussi vite que vous pouvez !" 25 143 ;
                                                          dessinerChaine "Vous reussissez a vous echapper..." 25 103 ;
                                                          p
      | _ -> combattre p m 
  with MonstreVaincu p -> Graphics.draw_image cadre 0 0 ; dessinerChaine "Vous avez vaincu votre ennemi !" 25 143 ; attendreInput () ; finCombat p m
;;
    
(** malheureuse_rencontre p renvoie le nouvele etat du personnage p, apres que celui-ci est fait un combat contre un monstre determine aleatoirement*)
let malheureuse_rencontre : personnage -> personnage= fun p -> 
  let m = spawnMonstre() in afficherApparitionMonstre m ; Unix.sleepf 3. ; attendreInput () ; (combattre p m)
;;

(** getGenre () affiche sur la fenetre le deroulement des actions, et retourne le genre sur lequelle a clique le joueur sur la fenetre*)
let rec getGenre : unit -> genre = fun () ->
  Graphics.draw_image cadre 0 0 ; 
  dessinerChaine "Cliquez le genre de votre personnage" 25 143 ; 
  dessinerChaine "Homme" 200 80 ; dessinerChaine "Femme" 450 80 ;
  match attendreClic () with
    | h when dansZoneClic h (200,80) (260,103) -> Homme
    | f when dansZoneClic f (450,80) (510,103) -> Femme
    | _ -> getGenre ()
;;

(** getClasse g affiche sur la fenetre le deroulement des actions selon le genre g, et retourne la classe sur laquelle a clique le joueur sur la fenetre*)
let rec getClasse : genre -> classe = fun genrePerso ->  
  Graphics.draw_image cadre 0 0 ; 
  dessinerChaine "Cliquez sur la classe de votre personnage" 25 143 ;
  let listeClasse = (if genrePerso = Homme then ["Archer";"Guerrier";"Magicien"] else ["Archere";"Guerriere";"Magicienne"]) in
  dessinerChaine (List.nth listeClasse 0) 140 80 ; dessinerChaine (List.nth listeClasse 1) 290 80 ; dessinerChaine (List.nth listeClasse 2) 450 80 ;
  match attendreClic () with
    | a when dansZoneClic a (140,80) (220,103) -> Archer
    | g when dansZoneClic g (290,80) (400,103) -> Guerrier
    | m when dansZoneClic m (450,80) (565,103) -> Magicien
    | _ -> getClasse genrePerso 
;;

(** getNom () affiche sur la fenetre le deroulement des actions, et retourne la string saisi dans le terminal, qui sera ensuite utilise comme nom du personnage*)
let getNom : unit -> string = fun () ->
  dessinerChaine "Saisissez votre nom dans le terminal" 20 143 ; read_line ()
;; 

(** creationPersonnage () renvoie un personnage après que le joueur ait rentre les informations de celui-ci (nom,genre,classe)*)
let creationPersonnage : unit -> personnage = fun () -> 
  let n = getNom () in
  let g = getGenre () in 
  let c = getClasse g in
    {nom = n ;
    genre = g ;
    classe= c ;
    pv=20. ;
    exp=0. ;
    niveau=1 ;
    items={pieces = 0 ; poulet = 0 ; eponges = 0 }}
;;

(** convert c renvoie une int list ou les elements sont ajoutes successivement en parcourant la string c (un element n'etant rajoute que lors un espace est rencontre lors du parcours de la string)*)
let rec convert : string -> int list = fun chaine -> 
  if (String.length chaine) = 0 then []
  else let posEspace = String.index chaine ' ' in int_of_string (String.sub chaine 0 posEspace) :: convert (String.sub chaine (posEspace+1) ((String.length chaine)-posEspace-1))
;;

(** getLines c renvoie une int list list correspondant a une map, recupere grace a un channel ouvert avec un fichier ./map/mapn.txt ou n est un certain numero*)
let rec getLines : in_channel -> int list list = fun channel -> try let line = input_line channel in [(convert line)] @ (getLines channel) with End_of_file -> [[]];;

(** lireFichier n ouvre un channel avec le fichier ./map/mapn.txt et retourne une int list list correspondant a ce fichier*)
let lireFichier : int -> int list list = fun numMap -> 
  let fichier = "./map/map"^(string_of_int numMap)^".txt" in
  let channel = open_in fichier in
  let lines = getLines channel in
  close_in channel;lines
;;

(** dessinerTile n nL nC dessine sur la fenetre l'image recupere dans images (liste de toutes les tiles possible) a une position determinee grace a nL et nC*)
let dessinerTile : int -> int -> int -> unit = fun num numLigne numColonne -> 
  match num with
  | 0 -> Graphics.set_color black ; Graphics.fill_rect (numColonne *tileTaille) ((heightFen-tileTaille) - (numLigne *tileTaille)) tileTaille tileTaille
  | _ -> Graphics.draw_image (List.nth images (num-1)) (numColonne *tileTaille) ((heightFen-tileTaille) - (numLigne *tileTaille))
;;

(** dessinerCarte t dL dC dessine sur la fenetre des tiles afin de reconstituer la carte correspondant a t (qui est une int list list). dL et dC servent de compteur*)
let rec dessinerCarte : int list list -> int -> int -> unit = fun t depLigne depColonne -> 
match (depLigne,depColonne) with
| (a,b) when a = nbLigne -> ()
| (a,b) when b = nbColonne -> dessinerCarte t (a+1) 0
| (a,b) -> dessinerTile (List.nth (List.nth t a) b) a b;dessinerCarte t a (b+1);;

(** dessinerPerso x y dessine un carre noir de dimension 32x32 en une position determinee grace a x et y*)
let dessinerPerso : int -> int -> unit = fun posX posY ->
  Graphics.set_color black ; Graphics.fill_rect (posX * tileTaille) ((heightFen-tileTaille) - (posY *tileTaille)) tileTaille tileTaille
;;

exception QuitterJeu of personnage;;

(** quitterJeu p affiche sur la fenetre le deroulement des actions. Cette fonction est appele dans le cas ou le joueur quitte le jeu, dans le cas ou son personnage meurt, et dans le cas ou le joueur ferme la fenetre. C'est la fonction qui est appele dans tous les cas a la fin du l'execution du programme*)
let quitterJeu : personnage -> unit = fun p -> 
  Graphics.draw_image cadre 0 0 ; 
  (match p.niveau with
    | 10 -> dessinerChaine "Felicitations ! Vous avez atteint le niveau 10 lors de" 25 143 ; dessinerChaine "votre partie !" 25 103 ; 
    | _ -> dessinerChaine "Coup dur, vous n'avez pas atteint le niveau 10..." 25 143 ; dessinerChaine "Mais vous pouvez toujours reessayer !" 25 103 ;
  ) ; 
  attendreInput () ; dessinerChaine "Au revoir !" 25 25 ; Unix.sleepf 2.
;;

(** testDormir p affiche sur la fenetre le deroulement des actions, et renvoie le nouvel etat du personnage p. Peut aussi lever une exception PersonnageMort*)
let testDormir : personnage -> personnage = fun p -> 
  match (dormir p) with
    | Personnage (j) -> attendreInput () ; j 
    | Monstre (m) -> let phraseMonstre = 
  	(if m.style = Golem then "Un golem trebuche sur vous !" 
  	else (if m.style = Serpent then "Un serpent vous etrangle !" 
  	else (if m.style = Moustiques (1) then "Un moustique rentre dans votre gorge !" 
  	else "Une nuee de moustique vous attaque"))) 
        in dessinerChaine "Pendant votre repos" 25 143 ; dessinerChaine phraseMonstre 25 103 ;
        let mot = (if p.genre = Homme then "mort !" else "morte !") in 
        dessinerChaine ("Vous etes " ^ mot) 25 63 ;
        attendreInput () ; raise (PersonnageMort ({p with pv=(0.)}))
;;

(** lancementCombatAleatoire p renvoie le nouvel etat du personnage p apres un combat, si l'aleatoire en a decide ainsi, sinon renvoie simplement p*)
let lancementCombatAleatoire : personnage -> personnage = fun p -> 
 match Random.int 20 with
   | n when n = 1 -> let newP = malheureuse_rencontre p in newP (* On changera la fréquence d'apparition plus tard *)
   | n -> p 
;;

(** actionJoueur t (a,b) touche numMap perso va agir selon touche pour faire l'une des actions possibles proposees par le jeu*)
let actionJoueur : int list list -> (int*int) -> char -> int -> personnage -> (int*int*int list list*int*personnage) = fun t (a,b) touche numMap perso -> 
  match touche with 
    | 'q' when a = 0 -> let nouveauNumMap = numMap -1 in ((a+nbColonne-1),b,lireFichier nouveauNumMap,nouveauNumMap,perso) 
    | 'z' when b = 0 -> let nouveauNumMap = numMap - nbMapLigneCarte in (a,(b+nbLigne-1),lireFichier nouveauNumMap,nouveauNumMap,perso)
    | 'd' when a = (nbColonne -1) -> let nouveauNumMap = numMap +1 in ((a-nbColonne+1),b,lireFichier nouveauNumMap,nouveauNumMap,perso)
    | 's' when b = (nbLigne-1) -> let nouveauNumMap = numMap + nbMapLigneCarte in (a,(b-nbLigne+1),lireFichier nouveauNumMap,nouveauNumMap,perso)
    (* Lorsqu'on change de carte, le perso est projeté à l'opposé de la nouvelle carte *)
    
    | 'q' when not (List.mem (List.nth (List.nth t b) (a-1)) listeTileCollision) -> ((a-1),b,t,numMap,perso) (*Deplacement à gauche*)
    | 'z' when not (List.mem (List.nth (List.nth t (b-1)) a) listeTileCollision) -> (a,(b-1),t,numMap,perso) (*Deplacement en haut*)  
    | 'd' when not (List.mem (List.nth (List.nth t b) (a+1)) listeTileCollision) -> ((a+1),b,t,numMap,perso) (*Deplacement à droite*)
    | 's' when not (List.mem (List.nth (List.nth t (b+1)) a) listeTileCollision) -> (a,(b+1),t,numMap,perso) (*Deplacement en bas*)

    | 'i' -> afficherPersonnage perso ; attendreInput () ; (a,b,t,numMap,perso) (* Visualiser le personnage *)
    | 'm' -> let (_,newP) = manger perso in attendreInput () ; (a,b,t,numMap,newP) (* Manger un poulet si c'est possible *)
    | 'n' -> raise (QuitterJeu (perso)) (* Quitter le jeu *)
    | 'o' -> let newP = testDormir perso in (a,b,t,numMap,newP) (* Dormir *)
    | _ -> (a,b,t,numMap,perso)
;;

(** loop (nl,nC) numMap p est la boucle principale du jeu, qui va agir selon l'action du joueur a chaque appel recursif. Dans le cas ou le joueur se deplace, lancementCombatAleatoire est appele*)
let rec loop (nL,nC) numMap perso = 
  let tableau = lireFichier numMap in 
  let (posX,posY,nouveauTableau,nouveauNumMap,tempP) = actionJoueur tableau (nL,nC) (getInput ()) numMap perso in 
  dessinerCarte nouveauTableau 0 0 ; dessinerPerso posX posY ; let newP = (if posX != nL || posY != nC then lancementCombatAleatoire tempP else tempP) in loop (posX,posY) nouveauNumMap newP;;

(* DEROULEMENT DU JEU *)

let perso = creationPersonnage ();;
dessinerCarte (lireFichier 1) 0 0;; (* 5 est le numéro de la map de départ du perso *)
dessinerPerso 15 10;; (* Coordonnées de départ du perso *)

try
loop (15,10) 1 perso
with
| QuitterJeu perso -> quitterJeu perso
| PersonnageMort perso -> quitterJeu perso
| _ -> print_string "Vous avez fermé la fenêtre sans prévenir, ne faites plus jamais ça !\n"
;;


