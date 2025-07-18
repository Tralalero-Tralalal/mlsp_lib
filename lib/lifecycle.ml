open Yojson.Basic.Util
open Yojson.Basic
open Rpc_lib.Basic

type uri = string;;

let to_string_opt = function
  | None -> None
  | Some str -> Some (to_string str);;
  
module Initialize = struct
 let initialized = ref false;;
   
  module Req = struct
    module Types = struct
    type p_id = 
      | Null
      | Int of int;;

    type client_info = {
      name: string;
      version: string option 
    };;
    
    type rootpath = 
    | Null
    | String of string;;
    
    type rooturi = 
    | Null
    | DocUri of string;;
    
    type workspaceFoldersType = 
    | Null
    | WorkspaceFolders of workspaceFolder list

    and workspaceFolder = {
    uri: uri;
    name: string
  };;
    
    type lspAny = 
    | Null
    | String of string
    | LspObject of lspObject
    | LspArray of lspArray
    | Int of int
    | UInt of int
    | Decimal of float
    | Bool of bool

    and lspObject = (string * lspAny) list

    and lspArray = lspAny list;;
      
    type traceValue = 
    | Off
    | Messages
    | Verbose;;

    type request = {
      processId: p_id;
      clientInfo: client_info option;
      locale: string option;
      rootPath: rootpath option;
      rootUri: rooturi;
      initializationsOption: lspAny option;
      trace: traceValue option;
      workspaceFolders:  workspaceFoldersType option;
    };; 

    end
    module Conversions = struct
      open Types

    let json_to_p_id : t -> p_id = function
    | `Null -> Null
    | `Int x -> Int x
    | json -> raise (Type_error ("process_id is of wrong type", json))

  let json_to_client_info = function
  | `Assoc _ as json -> {
      name = (json |> get_req_mem "name" |> to_string);
      version = (json |> get_opt_mem "version" |> to_string_opt);
    }
  | json -> raise (Type_error ("client_info is of wrong type", json))

  let opt_to_client_info : t option -> client_info option = function
    | None -> None
    | Some cli_Info -> Some (json_to_client_info cli_Info);;
    
  let json_to_root_path : t -> rootpath = function  
    | `Null -> Null
    | `String x -> String x
    | json -> raise (Type_error ("rootpath is of wrong type", json))

  let opt_to_root_path = function
    | None -> None
    | Some cli_Info -> Some (json_to_root_path cli_Info);;
  
  let json_to_root_uri :t -> rooturi = function 
    | `Null -> Null
    | `String x -> DocUri x
    | json -> raise (Type_error ("rootpath is of wrong type", json));;
 

  let rec json_to_lsp_any = function
  | `Null -> Null
  | `String str -> String str
  | `Assoc ass -> LspObject (List.map (fun obj -> let (str, json) = obj in (str, json_to_lsp_any json)) ass);
  | `List ls -> LspArray (List.map (fun ele -> json_to_lsp_any ele) ls)     
  | `Int i -> if i >= 0 then Int i else UInt i  
  | `Float f -> Decimal f 
  | `Bool b -> Bool b;;  

  let opt_to_lsp_any = function
    | None -> None
    | Some lspAny -> Some (json_to_lsp_any lspAny);;
 
  let str_to_trace = function 
    | "off" -> Off
    | "messages" -> Messages
    | "verbose" -> Verbose
    | x -> raise (Type_error ("trace value does not match any known string", `String x));;

  let json_to_trace = function 
  | `String str -> str_to_trace str
  | json -> raise (Type_error ("trace value is of wrong type", json));;
  
  let opt_to_trace = function
    | None -> None
    | Some trace -> Some (json_to_trace trace);;
  
  let json_workspace_folder : t -> workspaceFolder = function
  | `Assoc _ as json -> {
      uri = (json |> get_req_mem "uri" |> to_string);
      name = (json |> get_req_mem "name" |> to_string);
    }
  | json -> raise (Type_error ("workspaceFolder is of wrong type", json));;

  let json_to_workspace_folders : t -> workspaceFoldersType = function
  | `Null ->  Null
  | `List ls -> WorkspaceFolders (List.map (fun wf -> json_workspace_folder wf) ls)
  | json -> raise (Type_error ("WorkspaceFolders is of wrong type", json))
  
  let opt_to_workspace_folders : t option -> workspaceFoldersType option = function
  | None -> None
  | Some wf -> Some (json_to_workspace_folders wf);;
 (*Json to Type*)


  let request_of_yojson json = 
      let p_id = json |> get_req_mem "process_id" |> json_to_p_id in 
      let client_info = json |> get_opt_mem "client_info" |> opt_to_client_info in 
    {
    processId = p_id;
    clientInfo = client_info;
    locale = json |> get_opt_mem "locale" |> to_string_opt;
    rootPath = json |> get_opt_mem "rootPath" |> opt_to_root_path;
    rootUri = json |> get_req_mem "rootUri" |> json_to_root_uri;
    initializationsOption = json |> get_opt_mem  "initializationsOption" |> opt_to_lsp_any;
    trace = json |> get_opt_mem  "trace" |> opt_to_trace;
    workspaceFolders = json |> get_opt_mem "workspaceFolders" |> opt_to_workspace_folders
    };;
  end
  end

  open Req.Types
  open Req.Conversions

  (*Opt to not*)
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

    let yojson_of_response : response -> t = function
      | Ok res -> yojson_of_result res
      | Error err -> yojson_of_error err

    let initialize (process_id : p_id) : response =
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
      let open Rpc_lib.Basic.Response.Error in
    try
        let id = Id.t_of_yojson (`Int 7) in
        let fields = request_of_yojson params in 
          initialize fields.processId |> choose_between id 
    with
    | Missing_Member str -> yojson_of_error {retry = false} |> 
      (fun err -> Error (construct_error Code.InvalidRequest str err))  |>
          Response.construct_response (`Int 0) |> Response.yojson_of_t
    | str ->  yojson_of_error {retry = false} |> 
      (fun err -> Error (construct_error Code.InternalError (Printexc.to_string str) err))  |>
          Response.construct_response (`Int 0) |> Response.yojson_of_t

end
