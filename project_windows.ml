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

module Solver6 : Solver = struct
  (* Dobljeno iz https://reasonml.chat/t/iterate-over-a-string-pattern-match-on-a-string/1317/2 *) 
  let explode input =
    let rec aux idx lst =
      if idx<0 then lst else aux (idx-1) (input.[idx] :: lst)
    in aux (String.length input - 1) []

  let prestej_razlicne_pojavitve blok =
    let znaki = explode blok in
      let rec prestej_razlicne_pojavitve_aux zabelezeni presteti = function
        | [] -> presteti
        | '\n' :: xs -> prestej_razlicne_pojavitve_aux zabelezeni presteti xs
        | x :: xs -> if List.mem x zabelezeni then 
            prestej_razlicne_pojavitve_aux zabelezeni presteti xs
          else 
            prestej_razlicne_pojavitve_aux (x :: zabelezeni) (presteti + 1) xs
      in
    prestej_razlicne_pojavitve_aux [] 0 znaki

  let rec preveri_vse_bloke vrednosti = function
    | [] -> List.fold_left (+) 0 vrednosti
    | x :: xs -> preveri_vse_bloke (prestej_razlicne_pojavitve x :: vrednosti) xs

  let naloga1 podatki =
   let bloki = Str.split (Str.regexp "\n\n") podatki in
   bloki |> preveri_vse_bloke []
   |> string_of_int


  let presek sez1 sez2 =
    let rec presek_aux sez skupni = function
    	| [] -> skupni
      | x :: xs -> if List.mem x sez then presek_aux sez (x :: skupni) xs else presek_aux sez skupni xs
    in
    presek_aux sez1 [] sez2

  let prestej_skupne blok =
    let vrstice = String.split_on_char '\n' blok in
    let kandidati = explode (List.hd vrstice) in
      let rec prestej_skupne_aux skupni = function
        | [] -> List.length skupni
        | x :: xs -> let crke = explode x in prestej_skupne_aux (presek skupni crke) xs
      in
    prestej_skupne_aux kandidati (List.tl vrstice)

  let rec preveri_vse_bloke2 vsota = function
    | [] -> vsota
    | x :: xs -> preveri_vse_bloke2 (prestej_skupne x + vsota) xs

  let naloga2 podatki _part1 = 
    let bloki = Str.split (Str.regexp "\n\n") podatki in
    bloki |> preveri_vse_bloke2 0
    |> string_of_int

end

module Solver7 : Solver = struct
  let naloga1 podatki =
    ""


  let naloga2 podatki _part1 = 
    ""

end

module Solver8 : Solver = struct
  let loci_podatke vrstica =
    match Str.split (Str.regexp " [+-]") vrstica with
    | [ukaz; vrednost] -> if String.contains vrstica '-' then
        ukaz, (int_of_string vrednost) * (-1)
      else
        ukaz, int_of_string vrednost
    | _ -> failwith "Napačna oblika vrstice"

  let rec zgradi_zaporedje_ukazov zap = function
    | [] -> List.rev zap
    | x :: xs -> zgradi_zaporedje_ukazov (loci_podatke x :: zap) xs

  let rec izvajaj_ukaze zgo_mest mesto acc ukazi =
    if List.mem mesto zgo_mest then acc else
    let ukaz, vrednost = List.nth ukazi mesto in
    match ukaz with
    | "acc" -> izvajaj_ukaze (mesto :: zgo_mest) (mesto + 1) (acc + vrednost) ukazi
    | "nop" -> izvajaj_ukaze (mesto :: zgo_mest) (mesto + 1) acc ukazi
    | "jmp" -> izvajaj_ukaze (mesto :: zgo_mest) (mesto + vrednost) acc ukazi
    | _ -> failwith "Tak ukaz ne obstaja."

  let naloga1 podatki =
    let vrstice = List.lines podatki in
    vrstice |> zgradi_zaporedje_ukazov []
    |> izvajaj_ukaze [] 0 0
    |> string_of_int


  let rec izvajaj_ukaze2 originalno_stikalo stikalo zgo_mest mesto acc ukazi =
    if mesto = List.length ukazi then acc else
    if List.mem mesto zgo_mest then izvajaj_ukaze2 (originalno_stikalo + 1) (originalno_stikalo + 1) [] 0 0 ukazi else
    let ukaz, vrednost = List.nth ukazi mesto in
    match ukaz with
    | "acc" -> izvajaj_ukaze2 originalno_stikalo stikalo (mesto :: zgo_mest) (mesto + 1) (acc + vrednost) ukazi
    | "nop" -> if stikalo = 0 then
        izvajaj_ukaze2 originalno_stikalo (stikalo - 1) (mesto :: zgo_mest) (mesto + vrednost) acc ukazi
      else
        izvajaj_ukaze2 originalno_stikalo (stikalo - 1) (mesto :: zgo_mest) (mesto + 1) acc ukazi
    | "jmp" -> if stikalo = 0 then
        izvajaj_ukaze2 originalno_stikalo (stikalo - 1) (mesto :: zgo_mest) (mesto + 1) acc ukazi
      else
        izvajaj_ukaze2 originalno_stikalo (stikalo - 1) (mesto :: zgo_mest) (mesto + vrednost) acc ukazi
    | _ -> failwith "Tak ukaz ne obstaja."

  let naloga2 podatki _part1 = 
    let vrstice = List.lines podatki in
    vrstice |> zgradi_zaporedje_ukazov []
    |> izvajaj_ukaze2 0 0 [] 0 0
    |> string_of_int

end

module Solver9 : Solver = struct
  let rec se_da_sesteti2 x xs kandidat =
    match xs with
    | [] -> false
    | y :: ys -> if y + x = kandidat then true else se_da_sesteti2 x ys kandidat

  let rec se_da_sesteti1 opazovani kandidat =
    match opazovani with
    | [] -> false
    | x :: xs -> if se_da_sesteti2 x xs kandidat then true else se_da_sesteti1 xs kandidat

  let rec preveri_stevilke opazovani = function
    | [] -> failwith "Seznam nima ustreznega števila."
    | x :: xs -> if List.length opazovani <> 25 then
        preveri_stevilke (opazovani @ [x]) xs
      else
        if se_da_sesteti1 opazovani x then
          preveri_stevilke (List.tl opazovani @ [x]) xs
        else
          x

  let naloga1 podatki =
    let vrstice = List.lines podatki in
    vrstice |> List.map int_of_string
    |> preveri_stevilke []
    |> string_of_int


  let sum sez = List.fold_left (+) 0 sez

  let sestej_min_max sez =
    let min = List.fold_left (fun x y -> if x < y then x else y) 177777905 sez in
    let max = List.fold_left (fun x y -> if x > y then x else y) 0 sez in
    min + max

  let rec skrajsaj iskano = function
    | [] -> []
    | x :: xs -> if sum xs > iskano then skrajsaj iskano xs else xs

  let rec preveri_vsote_zaporednih opazovani iskano = function
    | [] -> failwith "Ne obstaja."
    | x :: xs -> if List.length opazovani < 2 then
        preveri_vsote_zaporednih (opazovani @ [x]) iskano xs
      else
        let vsota = sum opazovani in
        if vsota > iskano then 
          let nov_sez = skrajsaj iskano opazovani in
          if sum nov_sez = iskano && List.length nov_sez >= 2 then
            nov_sez
          else
            preveri_vsote_zaporednih (nov_sez @ [x]) iskano xs
        else
          if vsota = iskano then
            opazovani
          else
            preveri_vsote_zaporednih (opazovani @ [x]) iskano xs


  let naloga2 podatki _part1 = 
    let vrstice = List.lines podatki in
    vrstice |> List.map int_of_string
    |> preveri_vsote_zaporednih [] 177777905
    |> sestej_min_max
    |> string_of_int

end

module Solver10 : Solver = struct
  let rec preveri_razlike_v_verigi razlike = function
    | [] | _ :: [] -> razlike
    | x1 :: x2 :: xs -> let razlika = x2 - x1 in
      if razlika > 3 then failwith "Nemogoče zgraditi verigo." else preveri_razlike_v_verigi (razlika :: razlike) (x2 :: xs)

  let rec prestej_enke_in_trojke_in_zmnozi (e, t) = function
    | [] -> e * t
    | x :: xs ->
      match x with
      | 1 -> prestej_enke_in_trojke_in_zmnozi (e + 1, t) xs
      | 3 -> prestej_enke_in_trojke_in_zmnozi (e, t + 1) xs
      | _ -> prestej_enke_in_trojke_in_zmnozi (e, t) xs

  let naloga1 podatki =
    let vrstice = List.map int_of_string (List.lines podatki) in
    vrstice |> List.cons 0 
    |> List.cons (List.fold_left max 0 vrstice + 3)
    |> List.sort compare
    |> preveri_razlike_v_verigi []
    |> prestej_enke_in_trojke_in_zmnozi (0, 0)
    |> string_of_int


  let sum sez = List.fold_left (+) 0 sez

  let rec preveri_naslednje_moznosti x st = function
    | [] -> st
    | y :: ys -> if y - x <= 3 then preveri_naslednje_moznosti x (st + 1) ys else st

  let rec sestavi_mozne_naslednje st novi = function
    | [] -> novi
    | x :: xs -> if st > 0 then sestavi_mozne_naslednje (st - 1) ((x :: xs) :: novi) xs else novi

  (* Oblika kode pobrana iz https://www.cs.cornell.edu/courses/cs3110/2020fa/textbook/adv/memoization.html *)
  let preveri_vse_moznosti sez =
  let memo: int option array = Array.make (List.length sez + 1) None in
  let rec f_mem sez =
  let d = List.length sez in
    match memo.(d) with
    Some result -> result            (* computed already! *)
      | None ->
      let result = if d = 0 then 
        0 
      else
        if d = 1 then
          1
        else
          let x, xs = List.hd sez, List.tl sez in
          let naslednji_korak_st = preveri_naslednje_moznosti x 0 xs in
          let naslednji_koraki = sestavi_mozne_naslednje naslednji_korak_st [] xs in
          sum (List.map f_mem naslednji_koraki)  
      in
        memo.(d) <- (Some result);   (* record in table *)
        result
  in
    f_mem(sez)

  let naloga2 podatki _part1 = 
    let vrstice = List.map int_of_string (List.lines podatki) in
    vrstice |> List.cons 0 
    |> List.cons (List.fold_left max 0 vrstice + 3)
    |> List.sort compare
    |> preveri_vse_moznosti
    |> string_of_int

end

module Solver11 : Solver = struct
  (* Dobljeno iz https://reasonml.chat/t/iterate-over-a-string-pattern-match-on-a-string/1317/2 *) 
  let explode input =
    let rec aux idx lst =
      if idx<0 then lst else aux (idx-1) (input.[idx] :: lst)
    in aux (String.length input - 1) []

  let pripravi_array sezsez =
    sezsez |> List.map Array.of_list |> Array.of_list

  let sosede_v_seznam array y x =
    match y, x with
    | 0, x ->
      (match x with
      | 0 -> [array.(1).(0); array.(0).(1); array.(1).(1)]
      | 91 -> [array.(0).(90); array.(1).(0); array.(1).(91)]
      | x -> [array.(0).(x - 1); array.(0).(x + 1); array.(1).(x - 1); array.(1).(x); array.(1).(x + 1)])
    | y, 0 ->
      (match y with
      | 93 -> [array.(93).(1); array.(92).(0); array.(92).(1)]
      | y -> [array.(y - 1).(0); array.(y - 1).(1); array.(y).(1); array.(y + 1).(0); array.(y + 1).(1)])
    | 93, x ->
      (match x with
      | 91 -> [array.(92).(91); array.(92).(90); array.(93).(91)]
      | x -> [array.(93).(x - 1); array.(93).(x + 1); array.(92).(x - 1); array.(92).(x); array.(92).(x + 1)])
    | y, 91 -> [array.(y - 1).(91); array.(y + 1).(91); array.(y - 1).(90); array.(y).(90); array.(y + 1).(90)]
    | y, x -> [array.(y - 1).(x); array.(y + 1).(x); array.(y - 1).(x - 1); array.(y).(x - 1); array.(y + 1).(x - 1); array.(y - 1).(x + 1); array.(y).(x + 1); array.(y + 1).(x + 1)]

  let rec prestej_lojtre st = function
    | [] -> st
    | x :: xs -> if x = '#' then prestej_lojtre (st + 1) xs else prestej_lojtre st xs

  let index_naslednjega y x =
    if y <> 93 then 
      y + 1, x
    else
      if x <> 91 then
        0, x + 1
      else
        -1, -1

  let rec izvedi_korak stari_array array y x =
    if (y, x) = (-1, -1) then array else
    let st_lojter = (sosede_v_seznam stari_array y x) |> prestej_lojtre 0 in
    let yn, xn = index_naslednjega y x in
    if stari_array.(y).(x) = '#' && st_lojter >= 4 then
      (array.(y).(x) <- 'L';
      izvedi_korak stari_array array yn xn)
    else
      if stari_array.(y).(x) = 'L' && st_lojter = 0 then
        (array.(y).(x) <- '#';
        izvedi_korak stari_array array yn xn)
      else
        izvedi_korak stari_array array yn xn

  let rec izvajaj_korake_dokler_so_spremembe array =
    let stari = array |> Array.copy |> Array.map Array.copy in
    let nov_array = izvedi_korak stari array 0 0 in
    if stari = nov_array then nov_array else izvajaj_korake_dokler_so_spremembe nov_array

  let rec prestej_zasedene y x st array =
    if (y, x) = (-1, -1) then st else
    let yn, xn = index_naslednjega y x in
    match array.(y).(x) with
    | '#' -> prestej_zasedene yn xn (st + 1) array
    | _ -> prestej_zasedene yn xn st array
    

  let naloga1 podatki =
    let vrstice = List.lines podatki in
    vrstice |> List.map explode
    |> pripravi_array
    |> izvajaj_korake_dokler_so_spremembe
    |> prestej_zasedene 0 0 0
    |> string_of_int


  let naloga2 podatki _part1 = 
    ""

end

module Solver15 : Solver = struct
  let najdi_index x sez =
    let rec najdi_index_aux x st = function 
      | [] -> None
      | y :: ys -> if y = x then Some st else najdi_index_aux x (st + 1) ys
    in
    najdi_index_aux x 0 sez

  let novi = function
    | [] -> failwith "Napačen vnos."
    | x :: xs -> let st = najdi_index x xs in
      match st with
        | None -> 0
        | Some stint -> 1 + stint

  let rec izvajaj_do_2020 st sez =
    if st = 2021 then List.hd sez else
    let naslednji = novi sez in
    izvajaj_do_2020 (st + 1) (naslednji :: sez)

  let naloga1 podatki =
    let sez = String.split_on_char ',' podatki in
    sez |> List.rev
    |> List.map int_of_string
    |> izvajaj_do_2020 7
    |> string_of_int

  let naloga2 podatki _part1 = 
    ""

end

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
  | "4" -> (module Solver4)
  | "5" -> (module Solver5)
  | "6" -> (module Solver6)
  | "7" -> (module Solver7)
  | "8" -> (module Solver8)
  | "9" -> (module Solver9)
  | "10" -> (module Solver10)
  | "11" -> (module Solver11)
  | "15" -> (module Solver15)
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