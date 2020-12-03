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
let naloga1 podatki = 
  ""

let naloga2 podatki _part1 = 
  ""

end


(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
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