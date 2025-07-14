open Yojson.Basic.Util
open Rpc_lib.Basic

module Initialize = struct

 let initialized = ref false;;

    type p_id = 
      | Null
      | Int of int;;

    let json_to_p_id json : p_id =
      match json with
      | `Null -> Null
      | `Int x -> Int x
      | _ -> raise (Type_error ("process_id is of wrong type", json))
   
    type client_info = {
      name: string;
      version: string option 
    };;
   
    type request = {
      process_id: p_id;
    };;
 
    let request_of_yojson json = 
      let p_id = json |> member "process_id" |> json_to_p_id in 
      {process_id = p_id};;

    type result = {
      capabilities: bool;
    };;


    type error = {
      retry: bool;
    }

    type response = (result, error) Result.t;;
    
    let yojson_of_result res : Yojson.Basic.t = 
      `Assoc ["capabilities", `Bool res.capabilities];;
    
    let yojson_of_error err : Yojson.Basic.t  = 
      `Assoc ["retry", `Bool err.retry];;

    let yojson_of_response (resp : response) =
      match resp with
      | Ok res -> yojson_of_result res
      | Error err -> yojson_of_error err

    let initialize process_id : response =
      try
        assert (!initialized = false);
        initialized := true; 
        match process_id with
        | Null -> Ok {capabilities = true}
        | Int _ -> Ok {capabilities = true}
      with _ -> Error {retry = false}
        ;;

    let choose_between id resp = 
      let open Response in
      let open Error in
      match resp with
      | Ok res -> yojson_of_result res |> (fun res -> Ok res) |> construct_response id |> Response.yojson_of_t
      | Error err -> yojson_of_error err |> (fun err -> Error (construct_error Code.ServerNotInitialized "Server was already initialized bozo" err))  |>
          Response.construct_response id |> Response.yojson_of_t;;

    let respond params : Yojson.Basic.t =
        let id = Id.t_of_yojson (`Int 7) in
        let fields = request_of_yojson params in 
          initialize fields.process_id |> choose_between id 
end
