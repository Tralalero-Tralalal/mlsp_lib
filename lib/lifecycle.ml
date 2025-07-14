open Yojson.Basic.Util

module Initialize = struct

 let initialized = ref false;;
  module Request = struct
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
 
    let yojson_to_request json = 
      let p_id = json |> member "process_id" |> json_to_p_id in 
      {process_id = p_id};;
  end

  module Response = struct
    open Request
    
    type t = {
      capabilities: bool;
    };;
    
    let yojson_of_t t = 
      `Assoc ["capabilities", `Bool t.capabilities];;

    let initialize process_id : t =
      if not !initialized then begin
      initialized := true;
      match process_id with
      | Null -> {capabilities = false}
      | Int _ -> {capabilities = true}
      end
      else
        {capabilities = true}

    let full_initialize = fun params -> let fields = Request.yojson_to_request params in initialize fields.process_id
  end
end
