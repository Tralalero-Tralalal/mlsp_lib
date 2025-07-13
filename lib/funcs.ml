open Rpc_lib.Basic
open Yojson.Basic
open Response.Error.Code

let initialize process_id : Response.t =
  let open Types.Initialize in
  if not !initialized then begin
    initialized := true;
    Response.construct_response (`Int 7) (Ok (from_string "{}"))
  end
  else
    Response.construct_response (`Int 7) 
    (Error (Response.Error.construct_error ServerAlreadyInitialized "Server was already initialized bozo!" (from_string "{}")))
