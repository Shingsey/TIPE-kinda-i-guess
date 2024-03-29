type permutation = int array;;
type braid = int * permutation List.t;;

(*Display functions*)

let display_perm x =
  let n = Array.length x in
  print_string "\n[|";
  for i = 1 to n do
    print_int i;
    print_string "\t|";
  done;
  print_string "]\n[|";
  for i = 0 to n-1 do
    print_int x.(i);
    print_string "\t|";
  done;
  print_string "]\n";;

(*Travail sur les facteurs canoniques (tresses positives divisant delta_n)*)

(*
Renvoie la table de décomposition en cycles descendants associée à une permutation, ne fonctionne que si la permutation est composée de cycles descendants à supports disjoints
*)
let perm_to_dec (a : permutation) : permutation =
  let n = Array.length a in
  let x = Array.make n 0 in
  for i = (n-1) downto 0 do
    if x.(i) = 0 then x.(i) <- i+1;
    if a.(i) < i+1 then x.(a.(i) - 1) <- x.(i)
    done;
  x;;

(*
Renvoie la permutation associée à la table de décomposition en cycles descendants
*)
let dec_to_perm (x : permutation) : permutation =
  let n = Array.length x in
  let a,z = Array.make n 0, Array.make n 0 in
  for i = 0 to (n-1) do
    if z.(x.(i)-1) = 0 then a.(i) <- x.(i) else a.(i) <- z.(x.(i)-1);
    z.(x.(i)-1) <- i+1;
  done;
  a;;

(*
  Renvoie l'inverse d'un facteur canonique
*)
let inverse_can (x : permutation) : permutation =
  let n = Array.length x in
  let res = Array.make n 0 in
  for i = 0 to n-1 do
    res.(x.(i) - 1) <- (i+1);
  done;
  res;;
(*
   Renvoie le PGCD à gauche de deux tables de permutation A et B 
*)
let left_pgcd (a : permutation) (b : permutation) : permutation =
  let n = Array.length a in
  let res = Array.init n (fun x -> x+1) in
  let u,v,w = Array.make n 0,Array.make n 0,Array.make n 0 in
  let rec sort c a b head tail =
    if tail > head then
      begin
        let mean = (head+tail)/2 in
        (*print_string "head,mean,tail : ";print_int head;print_int mean;print_int tail;print_string "\n";*)
        sort c a b head mean;
        sort c a b (mean+1) tail;
        u.(mean) <- a.(c.(mean) - 1);
        v.(mean) <- b.(c.(mean) - 1);
        if head < mean then
          begin
            for i = mean - 1 downto head do 
              u.(i) <- min a.(c.(i) - 1) u.(i+1);
              v.(i) <- min b.(c.(i) - 1) v.(i+1);
            done;
          end;
        u.(mean+1) <- a.(c.(mean+1) - 1);
        v.(mean+1) <- b.(c.(mean+1) - 1);
        if tail > mean + 1 then
          begin
            for i = mean + 2 to tail do
              u.(i) <- max a.(c.(i) - 1) u.(i-1);
              v.(i) <- max b.(c.(i) - 1) v.(i-1);
            done;
          end;
          let l,r = ref head, ref (mean+1) in
          for i = head to tail do
            (*print_string "\n l,r : ";print_int !l;print_int !r;print_string "\n";*)
            if(!l > mean) || ((!r <= tail) && (u.(!l) > u.(!r)) && (v.(!l) > v.(!r))) then
              begin w.(i) <- c.(!r); incr r;end
            else begin w.(i) <- c.(!l);incr l;end
          done;
        for i = head to tail do c.(i) <- w.(i); done;
      end;
  in
  sort res a b 0 (n-1);
  inverse_can res;;


let equal_can a b = (a = b);;


(*Random braids*)

Random.self_init ();;

let random_canonical n : permutation =
  let res = Array.init n (fun x -> x+1) in
  for i = 0 to n-1 do
    let j = (Random.int (n-i)) + i in
    let temp = res.(j) in
    res.(j) <- res.(i);
    res.(i) <- temp;
  done;
  res;;

let rec split (l1 : 'a list) (l2 : 'a list) (l : 'a list) : ('a list) * ('a list) = match l with
  |[] -> l1,l2;
  |h::t ->  if Random.int 2 = 0 then split (h::l1) l2 t else split l1 (h::l2) t;;

let rec part_unif (l : 'a list) : 'a list list = match l with
  |[] -> [];
  |h::t -> let (a : 'a list),(b : 'a list) = split [] [] t in
     (h::a)::(part_unif b);;



let test k =
  f

let a = [|3;1;2;7;5;4;6|];;
let b = [|1;3;2;7;5;4;6|];;
let c = [|7;6;5;4;3;2;1|];;
let x = left_pgcd a b;;
display_perm (random_canonical 6);;




