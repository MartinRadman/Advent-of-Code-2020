#load "str.cma"
let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

module List = struct
  include List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let lines = String.split_on_char '\n'
end

module type Solver = sig
  val naloga1 : string -> string

  val naloga2 : string -> string -> string
end

module Solver0 : Solver = struct
  let cost_fun x = (x / 3) - 2

  let rec full_cost x =
    let c_cost = cost_fun x in
    if c_cost <= 0 then 0 else c_cost + full_cost c_cost

  let naloga1 data =
    let lines = List.lines data in
    lines |> List.int_list
    |> List.fold_left (fun s x -> s + cost_fun x) 0
    |> string_of_int

  let naloga2 data _part1 =
    data |> List.lines |> List.int_list |> List.map full_cost |> List.sum
    |> string_of_int
end

(* Tukaj re-definirajte funkcijo naloga1 in naloga2 *)
module Solver1 : Solver = struct
  let rec sestej_in_preveri_ali_je_2020 x = function
    | [] -> None
    | a :: rep -> if x + a = 2020 then Some (x * a) else sestej_in_preveri_ali_je_2020 x rep

  let rec preveri_vse_vsote = function
    | [] -> None
    | x :: xs -> let vrednost = sestej_in_preveri_ali_je_2020 x xs
      in
      if vrednost <> None then
      vrednost
      else
      preveri_vse_vsote xs
    
  let izlusci_vrednost = function
    | Some vrednost -> vrednost
    | _ -> failwith "Vnesel si narobno obliko tipa."

  let naloga1 podatki =
    let vrstice = List.lines podatki in
    vrstice |> List.int_list
    |> preveri_vse_vsote
    |> izlusci_vrednost
    |> string_of_int

  let rec sestej_in_preveri_ali_je_splosno splosno x = function
    | [] -> None
    | a :: rep -> if x + a = (2020 - splosno) then Some (x * a) else sestej_in_preveri_ali_je_splosno splosno x rep


  let rec preveri_vse_vsote_splosno splosno = function
    | [] -> None
    | x :: xs -> let vrednost = sestej_in_preveri_ali_je_splosno splosno x xs
      in
      if vrednost <> None then
      vrednost
      else
      preveri_vse_vsote_splosno splosno xs

  let rec preveri_vse_trojne_vsote = function
    | [] -> None
    | x :: [] -> None
    | x1 :: xs -> let vrednost = preveri_vse_vsote_splosno x1 xs
    in
    if vrednost <> None then
    Some (x1 * izlusci_vrednost vrednost)
    else
    preveri_vse_trojne_vsote xs

  let naloga2 podatki _part1 = 
    let vrstice = List.lines podatki in
    vrstice |> List.int_list
    |> preveri_vse_trojne_vsote
    |> izlusci_vrednost
    |> string_of_int 
end

module Solver2 : Solver = struct
  let loci_pogoj_in_geslo niz =
    match (String.split_on_char ':' niz) with
    | [pogoj; geslo] -> pogoj, String.trim geslo
    | _ -> failwith "Napačna oblika niza."

  let rec loci_pogoje_in_gesla_seznama pogoji gesla = function
    | [] -> pogoji, gesla
    | x :: xs -> let pogoj, geslo = loci_pogoj_in_geslo x in
      loci_pogoje_in_gesla_seznama (pogoj :: pogoji) (geslo :: gesla) xs

  let razdeli_pogoj pogoj =
    match String.split_on_char '-' pogoj with
    | [] -> failwith "Napačna oblika niza."
    | x1 :: [] -> failwith "Napačna oblika niza."
    | x1 :: x2 :: x3 :: xs -> failwith "Napačna oblika niza."
    | [n1; ostalo] ->
      match String.split_on_char ' ' ostalo with
      | [n2; crka] -> n1, n2, crka
      | _ -> failwith "Napačna oblika niza."
  
  (* Pobrano iz https://rosettacode.org/wiki/Substring/Top_and_tail#OCaml *)
  let strip_first_char str =
  if str = "" then "" else
  String.sub str 1 ((String.length str) - 1)

  let rec preveri_pojavitev_crke crka pojavitev = function
    | "" -> pojavitev
    | niz -> if crka = niz.[0] then preveri_pojavitev_crke crka (pojavitev + 1) (strip_first_char niz) 
      else preveri_pojavitev_crke crka (pojavitev) (strip_first_char niz)

  let preveri_pravilnost pogoj geslo =
    let n1, n2, crka = razdeli_pogoj pogoj in
    let pojavitev = preveri_pojavitev_crke crka.[0] 0 geslo in
    if pojavitev >= (int_of_string n1) && pojavitev <= (int_of_string n2) then true else false

  let rec prestej_pravilne pravilni = function
    | [], [] -> pravilni
    | pogoj :: p, geslo :: g -> if preveri_pravilnost pogoj geslo then prestej_pravilne (pravilni + 1) (p, g) else prestej_pravilne pravilni (p, g)
    | _ -> failwith "Naroben vnos."

  let naloga1 podatki =
    let vrstice = List.lines podatki in
    vrstice |> loci_pogoje_in_gesla_seznama [] []
    |> prestej_pravilne 0
    |> string_of_int


  let preveri_dolzino d niz = if String.length niz >= d then true else false

  let preveri_ta_pravo_pravilnost pogoj geslo =
    let n1, n2, crka = razdeli_pogoj pogoj in
    let i1, i2 = (int_of_string n1), (int_of_string n2) in
    if preveri_dolzino i1 geslo then
      if preveri_dolzino i2 geslo then
        (geslo.[i1 - 1] = crka.[0] && geslo.[i2 - 1] <> crka.[0]) || (geslo.[i1 - 1] <> crka.[0] && geslo.[i2 - 1] = crka.[0])
      else
        geslo.[i1 - 1] = crka.[0]
    else
      false


  let rec prestej_ta_prave_pravilne pravilni = function
    | [], [] -> pravilni
    | pogoj :: p, geslo :: g -> if preveri_ta_pravo_pravilnost pogoj geslo then prestej_ta_prave_pravilne (pravilni + 1) (p, g) else prestej_ta_prave_pravilne pravilni (p, g)
    | _ -> failwith "Naroben vnos."

  let naloga2 podatki _part1 = 
    let vrstice = List.lines podatki in
    vrstice |> loci_pogoje_in_gesla_seznama [] []
    |> prestej_ta_prave_pravilne 0
    |> string_of_int
end

module Solver3 : Solver = struct
  let preveri_zadetek mesto niz = if niz.[mesto] = '#' then true else false

  let rec preveri_stezo zadetki mesto naklon = function
    | [] -> zadetki
    | vrstica :: rep -> if preveri_zadetek mesto vrstica then preveri_stezo (zadetki + 1) ((mesto + naklon) mod 31) naklon rep else preveri_stezo zadetki ((mesto + naklon) mod 31) naklon rep
  
  
  let naloga1 podatki = 
    let vrstice = List.lines podatki in
    vrstice |> preveri_stezo 0 0 3
    |> string_of_int

  let rec izloci_sode lihi stikalo = function
  | [] -> List.rev lihi
  | x :: xs -> if stikalo then izloci_sode (x :: lihi) (not stikalo) xs else izloci_sode lihi (not stikalo) xs
  
  let naloga2 podatki _part1 = 
    let vrstice = List.lines podatki in
    let preverjanje = preveri_stezo 0 0 in
    let f = fun x y -> x * y in
    let lihi = (izloci_sode [] true vrstice) in
    let stevilo = List.fold_left f 1 [preverjanje 1 vrstice; preverjanje 3 vrstice; preverjanje 5 vrstice; preverjanje 7 vrstice; preverjanje 1 lihi] in
    string_of_int stevilo
  
end

module Solver4 : Solver = struct
  let razdeli_blok blok =
    let vrstice = String.split_on_char '\n' blok in
    let raz_vrstice = List.map (String.split_on_char ' ') vrstice in
    List.concat raz_vrstice

  let rec prestej_in_zabelezi_podatke presteti zabelezeni vrednosti = function
    | [] -> presteti, zabelezeni, vrednosti
    | x :: xs -> match String.split_on_char ':' x with
      | [tip; vrednost] -> prestej_in_zabelezi_podatke (presteti + 1) (tip :: zabelezeni) (vrednost :: vrednosti) xs
      | _ -> failwith "Napačen tip niza."
       

  let obravnavaj_pravilnost st_polj zabelezena_polja =
    st_polj = 8 || (st_polj = 7 && not (List.mem "cid" zabelezena_polja))

  let rec preveri_pravilnost_podatkov pravilni = function
    | [] -> pravilni
    | x :: xs -> let razdeljen = razdeli_blok x in
      let presteti, zabelezeni, _ = prestej_in_zabelezi_podatke 0 [] [] razdeljen in
      if obravnavaj_pravilnost presteti zabelezeni then
        preveri_pravilnost_podatkov (pravilni + 1) xs
      else
        preveri_pravilnost_podatkov pravilni xs

  let naloga1 podatki = 
    let bloki = Str.split (Str.regexp "\n\n+") podatki in
    bloki |> preveri_pravilnost_podatkov 0
    |> string_of_int


  (* Dobljeno iz https://reasonml.chat/t/iterate-over-a-string-pattern-match-on-a-string/1317/2 *) 
  let explode input =
    let rec aux idx lst =
      if idx<0 then lst else aux (idx-1) (input.[idx] :: lst)
    in aux (String.length input - 1) []

  (* Dobljeno iz https://stackoverflow.com/questions/49184057/does-ocaml-have-a-module-that-is-like-isdigit-and-isalpha-in-c-c *)
  let is_alpha = function 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false

  (* Pobrano iz https://rosettacode.org/wiki/Substring/Top_and_tail#OCaml *)
  let strip_first_char str =
  if str = "" then "" else
  String.sub str 1 ((String.length str) - 1)

  let preveri_visino visina =
    if String.index_opt visina 'c' <> None then
      match String.split_on_char 'c' visina with
      | [stevilo_str; _] -> let stevilo = int_of_string stevilo_str in stevilo >= 150 && stevilo <= 193
      | _ -> failwith "Napačen zapis višine."
    else
      if String.index_opt visina 'i' <> None then
        match String.split_on_char 'i' visina with
        | [stevilo_str; _] -> let stevilo = int_of_string stevilo_str in stevilo >= 59 && stevilo <= 76
        | _ -> failwith "Napačen zapis višine."
    else false

  let preveri_lase barva =
    let vrednost = strip_first_char barva in
    let char_sez = explode vrednost in
      let rec preveri_barvo_aux mesto = function
        | [] -> mesto = 6
        | x :: xs -> if is_digit x || is_alpha x then preveri_barvo_aux (mesto + 1) xs else false
    in
    preveri_barvo_aux 0 char_sez

  let preveri_ali_stevilo stevilo_str =
    let stevila = explode stevilo_str in
      let rec preveri_aux mesto = function
        | [] -> mesto = 9
        | x :: xs -> if is_digit x then preveri_aux (mesto + 1) xs else false
    in
    preveri_aux 0 stevila

  let preveri_tip tip vrednost =
    match tip with
    | "byr" -> let stevilo = int_of_string vrednost in
      stevilo >= 1920 && stevilo <= 2002
    | "iyr" -> let stevilo = int_of_string vrednost in
      stevilo >= 2010 && stevilo <= 2020
    | "eyr" -> let stevilo = int_of_string vrednost in
      stevilo >= 2020 && stevilo <= 2030
    | "hgt" -> preveri_visino vrednost
    | "hcl" -> preveri_lase vrednost
    | "ecl" -> List.mem vrednost ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
    | "pid" -> preveri_ali_stevilo vrednost
    | "cid" -> true
    | _ -> failwith "Napačen tip."

  let rec obravnavaj_strozjo_pravilnost zabelezeni vrednosti =
    match zabelezeni, vrednosti with
    | [], [] -> true
    | z :: zs, v :: vs -> if preveri_tip z v then obravnavaj_strozjo_pravilnost zs vs else false
    | _ -> failwith "Napačna oblika podatkov."   

  let rec preveri_strozjo_pravilnost pravilni = function
    | [] -> pravilni
    | x :: xs -> let razdeljen = razdeli_blok x in 
      let presteti, zabelezeni, vrednosti = prestej_in_zabelezi_podatke 0 [] [] razdeljen in
        if obravnavaj_pravilnost presteti zabelezeni && obravnavaj_strozjo_pravilnost zabelezeni vrednosti then
          preveri_strozjo_pravilnost (pravilni + 1) xs
        else
          preveri_strozjo_pravilnost pravilni xs

  let naloga2 podatki _part1 = 
    let bloki = Str.split (Str.regexp "\n\n+") podatki in
    bloki |> preveri_strozjo_pravilnost 0
    |> string_of_int

end

module Solver5 : Solver = struct
  (* Dobljeno iz https://reasonml.chat/t/iterate-over-a-string-pattern-match-on-a-string/1317/2 *) 
  let explode input =
    let rec aux idx lst =
      if idx<0 then lst else aux (idx-1) (input.[idx] :: lst)
    in aux (String.length input - 1) []

  let najdi_sedez niz =
     let sez = explode niz in
       let rec najdi_sedez_aux (v1, v2) (s1, s2) = function
         | [] -> v1, s1
         | 'F' :: xs -> najdi_sedez_aux (v1, ((v1 - 1) + v2) / 2) (s1, s2) xs
         | 'B' :: xs -> najdi_sedez_aux (((v1 - 1) + v2) / 2 + 1, v2) (s1, s2) xs
         | 'L' :: xs -> najdi_sedez_aux (v1, v2) (s1, ((s1 - 1) + s2) / 2) xs
         | 'R' :: xs -> najdi_sedez_aux (v1, v2) (((s1 - 1) + s2) / 2 + 1, s2) xs
         | _ -> failwith "Napačna oblika niza."
        in
      najdi_sedez_aux (1, 128) (1, 8) sez

  let rec preveri_vse_sedeze vrednosti = function
    | [] -> vrednosti
    | x :: xs -> let v1, s1 = najdi_sedez x in preveri_vse_sedeze ((8 * (v1 - 1) + s1 - 1) :: vrednosti) xs

  let naloga1 podatki =
    let vrstice = List.lines podatki in
    vrstice |> preveri_vse_sedeze []
    |> List.fold_left max 0 
    |> string_of_int


  let rec najdi_moj_sedez = function
    | x1 :: x2 :: xs -> if x1 = x2 - 2 then x1 + 1 else najdi_moj_sedez (x2 :: xs)
    | _ -> failwith "Na napačnem letalu si."

  let naloga2 podatki _part1 = 
    let vrstice = List.lines podatki in
    vrstice |> preveri_vse_sedeze []
    |> List.sort compare
    |> najdi_moj_sedez
    |> string_of_int

end


(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
  | "4" -> (module Solver4)
  | "5" -> (module Solver5)
  | _ -> failwith "Ni še rešeno"

let main () =
  let day = Sys.argv.(1) in
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data = preberi_datoteko ("data/day_" ^ day ^ ".in") in
  let p1_start = Sys.time () in
  let part1 = Solver.naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
  izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()