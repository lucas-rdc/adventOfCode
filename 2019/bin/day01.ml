let fuel_of_mass (x: int): int =
  (x/3)-2

let total_fuel (x: int): int =
  let rec aux acc x =
    let fuel = fuel_of_mass(x) in
    if fuel <= 0 then 
      acc 
    else 
      aux (acc+fuel) fuel
  in
  aux 0 x

let () =
  let input_filename = "./data/day01.txt" in
  let lines = Utils.read_file_lines input_filename in
  let _ = lines
    |> List.to_seq
    |> Seq.map int_of_string
    |> Seq.map fuel_of_mass
    |> Seq.fold_left (+) 0
    |> Printf.printf "part1: %d\n" 
  in
  lines
  |> List.to_seq
  |> Seq.map int_of_string
  |> Seq.map total_fuel
  |> Seq.fold_left (+) 0
  |> Printf.printf "part2: %d\n"