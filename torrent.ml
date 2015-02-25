type file = {
    path : string option;
    length : int option;
}


type t = {
    announce : string option;
    name : string option;
    piece_length : int option;
    files : file list;
    pieces : string option;
}


let rec match_field decoded field =
    match decoded with
    | Bencode.Dict list ->
          ( match list with
            | [] -> None
            | (key,value)::tl ->
                    if (String.compare key field) == 0 then
                            Some value
                    else
                        let v = match_field value field in
                        match v with
                        | None -> match_field (Bencode.Dict tl) field
                        | x -> x )

    | Bencode.List list ->
          ( match list with
            | [] -> None
            | hd::tl ->
                let v = match_field hd field in
                match v with
                | None -> match_field (Bencode.List tl) field
                | x -> x )

    | Bencode.Integer i -> None

    | Bencode.String s -> None


let apply f opt  =
    match opt with
    | None -> None
    | Some x -> f x

let none_to o def =
    match o with
    | None -> def
    | Some x -> x

let rec files_from_list list files =
    match list with
    | [] -> files
    | hd::tl ->
            let path = apply (Bencode.as_string) (match_field hd "path")
            and length = apply (Bencode.as_int) (match_field hd "length") in
            let file = { path; length } in
            files_from_list tl files@[file]



let create_from_file filepath =
    let d = Bencode.decode (`File_path  filepath ) in
    Printf.printf "%s\n" (Bencode.pretty_print d);
    let announce = apply (Bencode.as_string) (match_field d "announce")
    and name = apply (Bencode.as_string) (match_field d "name")
    and piece_length = apply (Bencode.as_int) (match_field d "piece length")
    and pieces = apply (Bencode.as_string) (match_field d "pieces") 
    and decoded_files = apply (Bencode.as_list) (match_field d "files") in
    let files = 
        files_from_list (none_to decoded_files []) [] in
    { announce; name;
      piece_length; files;  pieces }


let name torrent =
    match torrent.name with
    | None -> ""
    | Some str -> str


let print t = 
    Printf.printf "%s\n" (none_to t.announce "");
    Printf.printf "%s\n" (none_to t.name "");
    Printf.printf "%d\n" (none_to t.piece_length 0);
