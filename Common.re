type operation_mode =
  | Encrypt
  | Decrypt;

type matrix_location = {
  row: int,
  column: int
};

module Map = Core_kernel.Map;