open Common;

let mode_string_to_type = mode_s =>
  switch mode_s {
  | "ENC" => Some(Encrypt)
  | "DEC" => Some(Decrypt)
  | _ => None
  };

type cipher =
  | Caesar
  | Playfair
  | Rail_Fence
  | Row_Transpose
  | Vigenere;

let cipher_name_to_type = name_s =>
  switch name_s {
  | "CES" => Some(Caesar)
  | "PLF" => Some(Playfair)
  | "RFC" => Some(Rail_Fence)
  | "RTS" => Some(Row_Transpose)
  | "VIG" => Some(Vigenere)
  | _ => None
  };

type args_status =
  | Fine(cipher, operation_mode, string)
  | Problems(string);

let check_args = (~cipher_type, ~mode, ~text) =>
  switch (cipher_type, mode, text) {
  | (Some(cipher_type), Some(mode), Some(text)) =>
    Fine(cipher_type, mode, text)
  | _ =>
    let report_a =
      switch text {
      | None => "Could not read input file\n"
      | _ => ""
      };
    let report_b =
      switch (cipher_type, mode) {
      | (None, None) => "Incorrect values for <CIPHER NAME> and <CIPHER MODE>\n"
      | (_, None) => "Incorrect value for <CIPHER NAME>\n"
      | (None, _) => "Incorrect value for <CIPHER MODE>\n"
      | _ => ""
      };
    Problems(report_a ++ report_b);
  };

type cipher_result =
  | Success(string, string)
  | Failure(string);

let handle_result = res =>
  switch res {
  | Failure(problem_string) => print_endline(problem_string)
  | Success(out_file_name, out_text) =>
    let out_file = CCIO.File.make(out_file_name);
    let final_out_text = out_text ++ "\n";
    CCIO.File.write_exn(out_file, final_out_text);
    print_endline("Success!");
  };

let main = () => {
  let (arg_count, arg_values) = CLI.init();
  /* TODO: process input strings to ensure:
          1. lowercase
          2. No spaces
          3. No punctuation
     */
  /* TODO Split this up into functions */
  (
    switch arg_count {
    | x when x < 6 => Failure("Not enough arguments")
    | x when x > 6 => Failure("Too many arguments")
    | _ =>
      let cipher_type = List.nth(arg_values, 1) |> cipher_name_to_type;
      let key = List.nth(arg_values, 2);
      let mode = List.nth(arg_values, 3) |> mode_string_to_type;
      let input_file_name = List.nth(arg_values, 4);
      let output_file_name = List.nth(arg_values, 5);
      let input_text = CCIO.(with_in(input_file_name, read_line));
      switch (check_args(~cipher_type, ~mode, ~text=input_text)) {
      | Problems(p_str) => Failure(p_str)
      | Fine(cipher_type, mode, text) =>
        let out_text =
          switch cipher_type {
          | Caesar =>
            /* TODO: Handle case where parse fails */
            let by = int_of_string(key);
            Caesar.shift_letters(text, ~by, ~mode);
          | Playfair => Playfair.substitute(text, ~key, ~mode)
          | Rail_Fence =>
            let depth = int_of_string(key);
            Rail_Fence.transpose(text, ~depth, ~mode);
          | Row_Transpose => Row_Transpose.process(text, ~key, ~mode)
          | Vigenere => Vigenere.process(text, ~key, ~mode)
          };
        Success(output_file_name, out_text);
      };
    }
  )
  |> handle_result;
};

let () = main();