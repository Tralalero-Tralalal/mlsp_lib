let initialized = ref false;;

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

