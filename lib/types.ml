open Yojson.Basic.Util
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
    {process_id = p_id}
end
