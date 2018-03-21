open Containers;

open Common;

let letter_to_index = letter => Char.code(letter);

let fake_vigenere_matrix = (a, b, mode) =>
  switch mode {
  | Encrypt =>
    let out = a + b - 97;
    out > 122 ? out - 26 : out;
  | Decrypt =>
    let out = a - b + 97;
    out < 97 ? out + 26 : out;
  };

let substitute = (text_letter, key_letter, ~mode) => {
  let text_index = letter_to_index(text_letter);
  let key_index = letter_to_index(key_letter);
  fake_vigenere_matrix(text_index, key_index, mode) |> Char.chr;
};

let process = (text, ~key, ~mode) => {
  let rec repeat_key = (key, repetitions) =>
    repetitions > 1 ? key ++ repeat_key(key, repetitions - 1) : key;
  let text_length = String.length(text);
  let key_length = String.length(key);
  let repetitions = text_length / key_length;
  let intermediate_key = repeat_key(key, repetitions);
  let cap_key = (key, remainder) => key ++ String.sub(key, 0, remainder);
  let remainder = text_length mod key_length;
  let final_key =
    String.length(intermediate_key) > text_length ?
      String.sub(intermediate_key, 0, text_length) :
      cap_key(intermediate_key, remainder);
  String.map2(substitute(~mode), text, final_key);
};