open Containers;

open Common;

let shift_letter = (letter, ~by as shift_amount) => {
  let last_letter = 122; /* ASCII code for 'z' */
  let first_letter = 97; /* ASCII code for 'a' */
  let code_ceiling = last_letter + 1;
  let input_code = letter |> Char.lowercase_ascii |> Char.code;
  let shifted_temp = (input_code + shift_amount) mod code_ceiling;
  let shifted =
    shifted_temp < first_letter ? shifted_temp + first_letter : shifted_temp;
  Char.chr(shifted);
};

let shift_letters = (input_string, ~by, ~mode) => {
  let by =
    switch mode {
    | Encrypt => by
    | Decrypt => Int.neg(by)
    };
  String.map(shift_letter(~by), input_string);
};