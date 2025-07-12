open Rpc_lib.Basic
open Yojson.Basic

let initialized = ref false;;

module Initialize = struct
  
  type client_info = {
    name: string;
    version: string option 
  }

  type params = {
    process_id: int;
    client_info: client_info option;
    locale: string option;
      rootpath: string option (*| null*)
  };;

let initialize (params : Structured.t) : Response.t =
  let open Response.Error.Code in
  match params with
  | `Null -> 
  if not !initialized then begin
    initialized := true;
    Response.construct_response (`Int 7) (Ok (from_string "{}"))
  end
  else
    Response.construct_response (`Int 7) 
    (Error (Response.Error.construct_error ServerAlreadyInitialized "Server was already initialized bozo!" (from_string "{}")))
  | _ -> Response.construct_response (`Int 7) 
    (Error (Response.Error.construct_error InvalidParams  "There are supposed to be no parameters bozo!" (from_string "{}")))

end
