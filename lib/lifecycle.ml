open Yojson.Basic
open Yojson.Basic.Util
open Rpc_lib.Basic

module Initialize = struct
let initialized = ref false;;

  type client_info = {
    name: string;
    version: string option 
  }

  type t = {
    process_id: int;
  };;

  let yojson_to_t json = 
    let p_id = json |> member "process_id" |> to_int in 
    {process_id = p_id};;


  let initialize process_id : Response.t =
    if not !initialized then begin
    initialized := true;
    Response.construct_response (`Int 7) (Ok (from_string (Printf.sprintf"{\"process_id\": %d}" process_id)))
  end
  else
    Response.construct_response (`Int 7) 
    (Error (Response.Error.construct_error ServerAlreadyInitialized "Server was already initialized bozo!" (from_string "{}")))

  let full_initialize = fun params -> let fields = yojson_to_t params in initialize fields.process_id
end
