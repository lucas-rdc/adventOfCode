let read_file_lines (file: string): string list = 
  try
    In_channel.with_open_bin file In_channel.input_lines
  with _ -> failwith (Printf.sprintf "Cannot read input file %s" file)