module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
 type env = {
    function_index : int StringMap.t; (* Index for each function *)
    local_index    : int StringMap.t; (* FP offset for args, locals *)
  }