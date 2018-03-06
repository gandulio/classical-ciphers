open Containers;

open Containers.List.Infix;

type operation_mode =
  | Encrypt
  | Decrypt;

type matrix_location = {
  row: int,
  column: int
};

module Map = Core_kernel.Map;

module Caesar = {
  let shift_letter = (letter, ~by as shift_amount) => {
    let last_letter_code = 122; /* ASCII code for 'z' */
    let first_letter_code = 97; /* ASCII code for 'a' */
    let code_ceiling = last_letter_code + 1;
    let input_code = letter |> Char.lowercase_ascii |> Char.code;
    let shifted_val_temp = (input_code + shift_amount) mod code_ceiling;
    let shifted_val =
      (shifted_val_temp < first_letter_code) ?
        (shifted_val_temp + first_letter_code) : shifted_val_temp;
    Char.chr(shifted_val);
  };
  let shift_letters = (input_string, ~by, ~mode) => {
    let by =
      switch mode {
      | Encrypt => by
      | Decrypt => Int.neg(by)
      };
    String.map(shift_letter(~by), input_string);
  };
};

module Playfair = {
  module Char_Map = Core_kernel.Char.Map;
  type meta_matrix = {
    locations: Char_Map.t(matrix_location),
    indices: Char_Map.t(int),
    array: CCImmutArray.t(char)
  };
  type matrix_alignment =
    | Column
    | Row
    | None;
  let strip_j = input_string =>
    String.replace(~which=`All, ~sub="j", ~by="i", input_string);
  let build_list = key => {
    module Char_Set = Set.Make(Char);
    let alphabet =
      "abcdefghiklmnopqrstuvwxyz" |> String.to_list |> Char_Set.of_list;
    let key_l = key |> strip_j |> String.to_list;
    let key_s = Char_Set.of_list(key_l);
    let filtered_alphabet = Char_Set.diff(alphabet, key_s) |> Char_Set.to_list;
    List.append(key_l, filtered_alphabet);
  };
  let increment_location = location =>
    switch location.column {
    | 4 => {row: location.row + 1, column: 0}
    | _ => {...location, column: location.column + 1}
    };
  let calculate_locations = () => {
    let rec calculate = (cells_remaining, ~previous=?, ()) =>
      switch cells_remaining {
      | 0 => []
      | _ =>
        let location =
          switch previous {
          | Some(location) => increment_location(location)
          | None => {row: 0, column: 0} /* This case only occurs for 1st element */
          };
        [location, ...calculate(cells_remaining - 1, ~previous=location, ())];
      };
    let cell_count = 25;
    calculate(cell_count, ());
  };
  let build_map = letters => {
    let locations = calculate_locations();
    List.combine(letters, locations) |> Char_Map.of_alist_exn;
  };
  let ensure_even_length = text => {
    let text_l = String.length(text);
    switch (text_l mod 2) {
    | 0 => text
    | _ => text ++ "x"
    };
  };
  let rec insert_needed_filler = word_l =>
    switch word_l {
    | [first, second, ...rest] =>
      Char.equal(first, second) ?
        [first, 'x', ...insert_needed_filler([second, ...rest])] :
        [first, second, ...insert_needed_filler(rest)]
    | [first] => [first]
    | [] => []
    };
  let get_alignment = (pair, locations) => {
    let a = List.nth(pair, 0);
    let b = List.nth(pair, 1);
    let location_a = Map.find_exn(locations, a);
    let location_b = Map.find_exn(locations, b);
    (location_a.column == location_b.column) ?
      Column : (location_a.row == location_b.row) ? Row : None;
  };
  let circular_row_shift = (letter, ~letters, ~mode) => {
    let index = Map.find_exn(letters.indices, letter);
    let new_index =
      switch mode {
      | Encrypt =>
        switch ((index + 1) mod 5) {
        | 0 => index - 4
        | _ => index + 1
        }
      | Decrypt =>
        switch (index mod 5) {
        | 0 => index + 4
        | _ => index - 1
        }
      };
    CCImmutArray.get(letters.array, new_index);
  };
  let circular_column_shift = (letter, ~letters, ~mode) => {
    let index = Map.find_exn(letters.indices, letter);
    let new_index =
      switch mode {
      | Encrypt => index > 19 ? index - 20 : index + 5
      | Decrypt => index < 5 ? index + 20 : index - 5
      };
    CCImmutArray.get(letters.array, new_index);
  };
  let location_to_index = location => location.column + location.row * 5;
  let alternate_replace = (pair, ~letters) => {
    let a = List.nth(pair, 0);
    let b = List.nth(pair, 1);
    let a_loc = Map.find_exn(letters.locations, a);
    let b_loc = Map.find_exn(letters.locations, b);
    let new_a_loc = {row: a_loc.row, column: b_loc.column};
    let new_b_loc = {row: b_loc.row, column: a_loc.column};
    let new_a =
      new_a_loc |> location_to_index |> CCImmutArray.get(letters.array);
    let new_b =
      new_b_loc |> location_to_index |> CCImmutArray.get(letters.array);
    [new_a, new_b];
  };
  let substitute_pair = (pair, ~letters, ~mode) =>
    switch (get_alignment(pair, letters.locations)) {
    | Column => [
        circular_column_shift(List.nth(pair, 0), ~letters, ~mode),
        circular_column_shift(List.nth(pair, 1), ~letters, ~mode)
      ]
    | Row => [
        circular_row_shift(List.nth(pair, 0), ~letters, ~mode),
        circular_row_shift(List.nth(pair, 1), ~letters, ~mode)
      ]
    | None => alternate_replace(pair, ~letters)
    };
  let substitute = (text, ~key, ~mode) => {
    /* TODO: handle removal of duplicates from provided key */
    let proto_matrix = build_list(key);
    let m_matrix = {
      locations: build_map(proto_matrix),
      indices: List.combine(proto_matrix, 0 -- 24) |> Char_Map.of_alist_exn,
      array: CCImmutArray.of_list(proto_matrix)
    };
    let final_text_l =
      switch mode {
      | Encrypt =>
        text
        |> strip_j
        |> String.to_list
        |> insert_needed_filler
        |> String.of_list
        |> ensure_even_length
        |> String.to_list
      | Decrypt => String.to_list(text)
      };
    let pairs = List.sublists_of_len(2, final_text_l);
    List.map(substitute_pair(~letters=m_matrix, ~mode), pairs)
    |> List.flatten
    |> String.of_list;
  };
};

module Rail_Fence = {
  type real_position_designation =
    | Last_Real_Position
    | Penultimate_Real_Postition;
  type safe_position_designation =
    | Full_Column
    | Last_Full_Column(real_position_designation);
  let designate_real = (~row, ~remainder) =>
    (row < remainder) ? Penultimate_Real_Postition : Last_Real_Position;
  let designate_col = (~row, ~column, ~last_full_col, ~remainder) =>
    (column == last_full_col) ?
      Last_Full_Column(designate_real(~row, ~remainder)) : Full_Column;
  let location_to_index = (~row, ~column, ~depth) => column * depth + row;
  let traverse = (text_a, ~text_len, ~depth) => {
    let rec traverse_r =
            (text_a, ~row, ~column, ~last_full_col, ~remainder, ~depth) => {
      let col_designation =
        designate_col(~row, ~column, ~last_full_col, ~remainder);
      switch col_designation {
      | Full_Column =>
        let current_i = location_to_index(~row, ~column, ~depth);
        let current_letter = CCImmutArray.get(text_a, current_i);
        let column = column + 1;
        let next_letters =
          traverse_r(
            text_a,
            ~row,
            ~column,
            ~last_full_col,
            ~remainder,
            ~depth
          );
        [current_letter, ...next_letters];
      | Last_Full_Column(Penultimate_Real_Postition) =>
        let current_i = location_to_index(~row, ~column, ~depth);
        let current_letter = CCImmutArray.get(text_a, current_i);
        let column = column + 1;
        let final_row_i = location_to_index(~row, ~column, ~depth);
        let final_row_letter = CCImmutArray.get(text_a, final_row_i);
        let row = row + 1;
        let next_letters =
          traverse_r(
            text_a,
            ~row,
            ~column=0,
            ~last_full_col,
            ~remainder,
            ~depth
          );
        [current_letter, final_row_letter, ...next_letters];
      | Last_Full_Column(Last_Real_Position) =>
        let current_i = location_to_index(~row, ~column, ~depth);
        (row < depth - 1) ?
          {
            let current_letter = CCImmutArray.get(text_a, current_i);
            let row = row + 1;
            let next_letters =
              traverse_r(
                text_a,
                ~row,
                ~column=0,
                ~last_full_col,
                ~remainder,
                ~depth
              );
            [current_letter, ...next_letters];
          } :
          [CCImmutArray.get(text_a, current_i)];
      };
    };
    let full_columns = text_len / depth;
    let last_full_col = full_columns - 1;
    let remainder = text_len - full_columns * depth;
    traverse_r(text_a, ~row=0, ~column=0, ~last_full_col, ~remainder, ~depth);
  };
  /* TODO: Handle decrypt */
  let transpose = (text, ~depth, ~mode) => {
    let _mode = mode;
    let text_len = String.length(text);
    let text_a = text |> String.to_array |> CCImmutArray.of_array_unsafe;
    traverse(text_a, ~text_len, ~depth) |> String.of_list;
  };
};

module Row_Transpose = {
  module Int_MMap = CCMultiMap.MakeBidir(Int, Int);
  module Loc_Comp = {
    module T = {
      type t = matrix_location;
      let compare = (a, b) =>
        (a.row < b.row) ?
          (-1) :
          (a.row > b.row) ?
            1 : (a.column < b.column) ? (-1) : (a.column > b.column) ? 1 : 0;
      let sexp_of_t = entry =>
        Core_kernel.Sexp.List([
          Core_kernel.Sexp.Atom(Int.to_string(entry.row)),
          Core_kernel.Sexp.Atom(Int.to_string(entry.column))
        ]);
      let t_of_sexp = _e => {row: 0, column: 0};
    };
    include T;
    include Core_kernel.Comparable.Make(T);
  };
  let increment_location = (location, column_count) =>
    (location.column == column_count - 1) ?
      {row: location.row + 1, column: 0} :
      {...location, column: location.column + 1};
  let calculate_locations = (~cell_count, ~column_count) => {
    let rec calculate = (cells_remaining, ~previous=?, ()) =>
      switch cells_remaining {
      | 0 => []
      | _ =>
        let location =
          switch previous {
          | Some(location) => increment_location(location, column_count)
          | None => {row: 0, column: 0} /* This case only occurs for 1st element */
          };
        [location, ...calculate(cells_remaining - 1, ~previous=location, ())];
      };
    calculate(cell_count, ());
  };
  let increment_location_dec = (location, row_count) =>
    (location.row == row_count - 1) ?
      {row: 0, column: location.column + 1} :
      {...location, row: location.row + 1};
  let calculate_locations_dec = (~cell_count, ~row_count) => {
    let rec calculate = (cells_remaining, ~previous=?, ()) =>
      switch cells_remaining {
      | 0 => []
      | _ =>
        let location =
          switch previous {
          | Some(location) => increment_location_dec(location, row_count)
          | None => {row: 0, column: 0} /* This case only occurs for 1st element */
          };
        [location, ...calculate(cells_remaining - 1, ~previous=location, ())];
      };
    calculate(cell_count, ());
  };
  let make_multi = (column_list) => {
    let rec make = (columns, index, map) => {
      switch columns {
      | [] => map
      | [first, ...rest] => Int_MMap.add(make(rest, index + 1, map), index, first)
      };
    };
    let map = Int_MMap.empty;
    make(column_list, 0, map)
  };
  let process_en = (text, ~key) => {
    let key_l_temp = String.to_list(key);
    let key_l =
      List.map(
        value => {
          let value_s = String.make(1, value);
          int_of_string(value_s) - 1;
        },
        key_l_temp
      )
      |> Array.of_list;
    let text_len = String.length(text);
    let columns = String.length(key);
    let rows_temp = text_len / columns;
    let rows =
      switch (text_len mod columns) {
      | 0 => rows_temp
      | _ => rows_temp + 1
      };
    let final_text =
      switch (text_len mod columns) {
      | 0 => text
      | _ => String.pad(~side=`Right, ~c='x', rows * columns, text)
      };
    let final_text_l = String.to_list(final_text);
    let locations =
      calculate_locations(~cell_count=rows * columns, ~column_count=columns);
    let proto_map = List.combine(locations, final_text_l);
    let map = Core_kernel.Map.of_alist_exn((module Loc_Comp), proto_map);
    let rec traverse_r = (map, row, column, rows, columns, column_list) =>
      (row < rows - 1) ?
        {
          let current =
            Core_kernel.Map.find_exn(map, {row, column: column_list[column]});
          let next =
            traverse_r(map, row + 1, column, rows, columns, column_list);
          [current, ...next];
        } :
        (column < columns - 1) ?
          {
            let current =
              Core_kernel.Map.find_exn(
                map,
                {row, column: column_list[column]}
              );
            let next =
              traverse_r(map, 0, column + 1, rows, columns, column_list);
            [current, ...next];
          } :
          [Core_kernel.Map.find_exn(map, {row, column: column_list[column]})];
    let output_l = traverse_r(map, 0, 0, rows, columns, key_l);
    String.of_list(output_l);
  };
  let map_get = (bimap, to_find) => {
    switch (Int_MMap.find1_right(bimap, to_find)) {
    | None => 0
    | Some(num) => num
    }
  };
  let process_dec = (text, ~key) => {
    let key_l_temp = String.to_list(key);
    let key_l =
      List.map(
        value => {
          let value_s = String.make(1, value);
          int_of_string(value_s) - 1;
        },
        key_l_temp
      );
    let bidir_map = make_multi(key_l);
    let text_len = String.length(text);
    let columns = String.length(key);
    let rows = text_len / columns;
    let locations =
      calculate_locations_dec(~cell_count=rows * columns, ~row_count=rows);
    let text_l = String.to_list(text);
    let proto_map = List.combine(locations, text_l);
    let letter_map = Core_kernel.Map.of_alist_exn((module Loc_Comp), proto_map);
    let rec traverse_r = (map, row, column, rows, columns, column_map) =>
      (column < columns - 1) ?
        {
          let current = 
            Core_kernel.Map.find_exn(
              map,
              {row, column: map_get(column_map, column)}
            );
          let next =
            traverse_r(map, row, column + 1, rows, columns, column_map);
          [current, ...next];
        } :
        (row < rows - 1) ?
        {
          let current =
            Core_kernel.Map.find_exn(map, {row, column: map_get(column_map, column)});
          let next =
            traverse_r(map, row + 1, 0, rows, columns, column_map);
          [current, ...next];
        } :
        [Core_kernel.Map.find_exn(map, {row, column: map_get(column_map, column)})];
    let output_l = traverse_r(letter_map, 0, 0, rows, columns, bidir_map);
    String.of_list(output_l);
  };
  let process = (text, ~key, ~mode) => {
    switch (mode) {
    | Encrypt => process_en(text, ~key)
    | Decrypt => process_dec(text, ~key)
    }
  };
};

module Vigenere = {
  let substitute = (text_letter, key_letter) => {
    let text_index = letter_to_index(text_letter);
    let key_index = letter_to_index(key_letter);
    fake_vigenere_matrix(text_index, key_index) |> Char.chr;
  };
  let process = (text, ~key, ~mode) => {
    let text_length = String.length(text);
    let rec repeat_key = (key, repetitions) =>
      (repetitions > 1) ? (key ++ repeat_key(key, repetitions - 1)) : key;
    let key_length = String.length(key);
    let repetitions = text_length / key_length;
    let intermediate_key = repeat_key(key, repetitions);
    let cap_key = (key, remainder) => key ++ String.sub(key, 0, remainder);
    let remainder = text_length mod key_length;
    let final_key = cap_key(intermediate_key, remainder);
    String.map2(substitute, text, final_key);
  };
};

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
    Problems(
      (
        switch text {
        | None => "Could not read input file\n"
        | _ => ""
        }
      )
      ++ 
      (
        switch (cipher_type, mode) {
        | (None, None) => "Incorrect values for <CIPHER NAME> and <CIPHER MODE>\n"
        | (_, None) => "Incorrect value for <CIPHER NAME>\n"
        | (None, _) => "Incorrect value for <CIPHER MODE>\n"
        | _ => ""
        }
      )
    )
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
    | x when x < 6 => "Not enough arguments"
    | x when x > 6 => "Too many arguments"
    | _ =>
      let cipher_type = List.nth(arg_values, 1) |> cipher_name_to_type;
      let key = List.nth(arg_values, 2);
      let mode = List.nth(arg_values, 3) |> mode_string_to_type;
      let input_file_name = List.nth(arg_values, 4);
      let _output_file_name = List.nth(arg_values, 5);
      let text = CCIO.(with_in(input_file_name, read_line));
      switch (check_args(~cipher_type, ~mode, ~text)) {
      | Problems(p_str) => p_str
      | Fine(cipher_type, mode, text) =>
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
        }
      }
    }
  )
  |> print_endline;
};

let () = main();