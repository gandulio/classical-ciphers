open Containers;

open Common;

module Int_MMap = CCMultiMap.MakeBidir(Int, Int);

module Loc_Comp = {
  module T = {
    type t = matrix_location;
    let compare = (a, b) =>
      a.row < b.row ?
        (-1) :
        a.row > b.row ?
          1 : a.column < b.column ? (-1) : a.column > b.column ? 1 : 0;
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
  location.column == column_count - 1 ?
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
  location.row == row_count - 1 ?
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

let make_multi = column_list => {
  let rec make = (columns, index, map) =>
    switch columns {
    | [] => map
    | [first, ...rest] =>
      Int_MMap.add(make(rest, index + 1, map), index, first)
    };
  let map = Int_MMap.empty;
  make(column_list, 0, map);
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
    row < rows - 1 ?
      {
        let current =
          Core_kernel.Map.find_exn(map, {row, column: column_list[column]});
        let next =
          traverse_r(map, row + 1, column, rows, columns, column_list);
        [current, ...next];
      } :
      column < columns - 1 ?
        {
          let current =
            Core_kernel.Map.find_exn(map, {row, column: column_list[column]});
          let next =
            traverse_r(map, 0, column + 1, rows, columns, column_list);
          [current, ...next];
        } :
        [Core_kernel.Map.find_exn(map, {row, column: column_list[column]})];
  let output_l = traverse_r(map, 0, 0, rows, columns, key_l);
  String.of_list(output_l);
};

let map_get = (bimap, to_find) =>
  switch (Int_MMap.find1_right(bimap, to_find)) {
  | None => 0
  | Some(num) => num
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
    column < columns - 1 ?
      {
        let current =
          Core_kernel.Map.find_exn(
            map,
            {row, column: map_get(column_map, column)}
          );
        let next = traverse_r(map, row, column + 1, rows, columns, column_map);
        [current, ...next];
      } :
      row < rows - 1 ?
        {
          let current =
            Core_kernel.Map.find_exn(
              map,
              {row, column: map_get(column_map, column)}
            );
          let next = traverse_r(map, row + 1, 0, rows, columns, column_map);
          [current, ...next];
        } :
        [
          Core_kernel.Map.find_exn(
            map,
            {row, column: map_get(column_map, column)}
          )
        ];
  let output_l = traverse_r(letter_map, 0, 0, rows, columns, bidir_map);
  String.of_list(output_l);
};

let process = (text, ~key, ~mode) =>
  switch mode {
  | Encrypt => process_en(text, ~key)
  | Decrypt => process_dec(text, ~key)
  };