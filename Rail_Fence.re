open Containers;

open Common;

type real_position_designation =
  | Last_Real_Position
  | Penultimate_Real_Postition;

type safe_position_designation =
  | Full_Column
  | Last_Full_Column(real_position_designation);

let designate_real = (~row, ~remainder) =>
  row < remainder ? Penultimate_Real_Postition : Last_Real_Position;

let designate_col = (~row, ~column, ~last_full_col, ~remainder) =>
  column == last_full_col ?
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
        traverse_r(text_a, ~row, ~column, ~last_full_col, ~remainder, ~depth);
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
      row < depth - 1 ?
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

let letters_pass = (start_ind, depth, base_columns, remainder, text_len, text) => {
  let rec grab = (at, depth, base_columns, offsets_left, text_len, text) => {
    let current = CCImmutArray.get(text, at);
    let index_increment = offsets_left > 0 ? base_columns + 1 : base_columns;
    let next_index = at + index_increment;
    let next =
      next_index < text_len ?
        grab(
          next_index,
          depth,
          base_columns,
          offsets_left - 1,
          text_len,
          text
        ) :
        [];
    [current, ...next];
  };
  grab(start_ind, depth, base_columns, remainder, text_len, text);
};

let extra_pass = (start_ind, depth, remainder, text) => {
  let rec grab = (at, depth, grab_count, text) =>
    grab_count > 0 ?
      {
        let current = CCImmutArray.get(text, at);
        let next = grab(at + (depth + 1), depth, grab_count - 1, text);
        [current, ...next];
      } :
      [];
  grab(start_ind, depth, remainder, text);
};

let do_passes = (depth, text_len, text) => {
  let base_limit = text_len / depth;
  let remainder = text_len mod depth;
  let rec handle = (current_pass, base_limit, depth, remainder, text_len, text) =>
    current_pass < base_limit ?
      List.append(
        letters_pass(
          current_pass,
          depth,
          base_limit,
          remainder,
          text_len,
          text
        ),
        handle(current_pass + 1, base_limit, depth, remainder, text_len, text)
      ) :
      remainder > 0 ? extra_pass(current_pass, depth, remainder, text) : [];
  handle(0, base_limit, depth, remainder, text_len, text);
};

let transpose = (text, ~depth, ~mode) => {
  let text_len = String.length(text);
  let text_a = text |> String.to_array |> CCImmutArray.of_array_unsafe;
  switch mode {
  | Encrypt => traverse(text_a, ~text_len, ~depth) |> String.of_list
  | Decrypt => do_passes(depth, text_len, text_a) |> String.of_list
  };
};