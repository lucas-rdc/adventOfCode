let tee (f : 'a -> unit) (x : 'a) : 'a =
  f x;
  x

let binop (op: int->int->int) (pc: int) (program: int array) =
  let l = program.(pc+1) in
  let r = program.(pc+2) in
  let dest = program.(pc+3) in
  let lv = program.(l) in
  let rv = program.(r) in
  program.(dest) <- op lv rv

let rec run_program (pc: int) (program: int array): int =
  match program.(pc) with
  | 1  -> 
    binop ( + ) pc program;
    run_program (pc+4) program
  | 2  ->
    binop ( * ) pc program;
    run_program (pc+4) program
  | 99 ->
    program.(0)
  | x  -> failwith (Printf.sprintf "invalid opcode: %d\n" x)

let set_input (noun: int) (verb: int) (program: int array): unit =
  program.(1) <- noun;
  program.(2) <- verb

let restore_state (program: int array): unit =
  set_input 12 2 program

let brute_force_part2 (program: int array): (int * int) =
  let rec aux i j =
    let prog = Array.copy program in
    set_input i j prog;
    match (run_program 0 prog), i, j with
    | 19690720, _, _ -> (i, j)
    | _, i, 99 when i < 99 -> aux (i+1) 0
    | _, _, _  when i > 99 -> failwith "No solution found for noun and verb 0..99"
    | _, _, _ -> aux i (j+1)
  in
  aux 0 0

let () =
  let input_filename = "./data/day02.txt" in
  let lines = Utils.read_file_lines input_filename in
  let line = try List.hd lines with _ -> failwith "Can't parse input" in
  let program = line 
  |> String.split_on_char ','
  |> List.map int_of_string
  |> Array.of_list
  in
  program
  |> Array.copy
  |> tee restore_state
  |> run_program 0
  |> (Printf.printf "part1: %d\n");
  program
  |> brute_force_part2
  |> (fun (a, b) -> 100*a+b)
  |> (Printf.printf "part2: %d\n");