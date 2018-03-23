open Containers;

open Common;

let ascii_to_alpha = code => CCInt.rem(code - 97, 26);

let alpha_to_ascii = value => value + 97;

let shift_letter = (letter, ~by as shift_amount) => {
  let input_code =
    letter |> Char.lowercase_ascii |> Char.code |> ascii_to_alpha;
  let shifted_temp = input_code + shift_amount;
  CCInt.rem(shifted_temp, 26) |> alpha_to_ascii |> Char.chr;
};

let shift_letters = (input_string, ~by, ~mode) => {
  let by =
    switch mode {
    | Encrypt => by
    | Decrypt => Int.neg(by)
    };
  String.map(shift_letter(~by), input_string);
};