open Containers;

module Cipher = {

  type t =
    | Caesar(int)
    | Playfair
    | Vigenre;

  module Caesar = {
    let shift_letter = (letter, ~by as shift_amount) => {
      let last_letter_code  = 122; /* ASCII code for 'z' */
      let first_letter_code = 97; /* ASCII code for 'a' */
      let code_ceiling      = last_letter_code + 1;
      let input_code = letter |> Char.lowercase_ascii |> Char.code;

      let shifted_val_temp =
        (input_code + shift_amount) mod code_ceiling;
      let shifted_val =
        (shifted_val_temp < first_letter_code) ?
          (shifted_val_temp + first_letter_code) : shifted_val_temp;

      Char.chr(shifted_val)
    };

    let shift_letters = (input_string, ~by) => 
      String.map(shift_letter(~by), input_string);
  };

  let encrypt = (input_string, ~method) =>
    switch method {
    | Caesar(shift_amount) =>
      Caesar.shift_letters(input_string, ~by=shift_amount)
    | _ => "wow"
    };

  let decrypt = (input_string, ~method) =>
    switch method {
    | Caesar(shift_amount) =>
      Caesar.shift_letters(input_string, ~by=Int.neg(shift_amount))
    | _ => "wow"
    };

};

let () = "wowee" |> Cipher.encrypt(~method=Caesar(1)) |> print_endline;
