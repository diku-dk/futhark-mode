-- Disable auto-indentation for the '->' lines here.

module type mt1 = {
  type calibration_result = i32

  val least_squares [num_vars]
      :  (objective: [num_vars]real -> real)
      -> (max_global: i32) -> (np: i32)
      -> [num_vars]optimization_variable
      -> (num_observed: i32)
      -> calibration_result [num_vars]
}

module type mt2 = {
  val least_squares [num_vars]
      :  (objective: [num_vars]real -> real)
      -> (max_global: i32) -> (np: i32)
      -> [num_vars]optimization_variable
      -> (num_observed: i32)
      -> calibration_result [num_vars]
}
