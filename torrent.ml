type t = {
    announce : string option;
    name : string option;
    piece_length : int option;
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


let apply f opt =
    match opt with
    | None -> None
    | Some x -> f x

let create_from_file filepath =
    let d = Bencode.decode (`File_path  filepath ) in
    Printf.printf "%s\n" (Bencode.pretty_print d);
    let announce = apply (Bencode.as_string) (match_field d "announce")
    and name = apply (Bencode.as_string) (match_field d "name")
    and piece_length = apply (Bencode.as_int) (match_field d "piece length")
    and pieces = apply (Bencode.as_string) (match_field d "pieces") in
    { announce; name;
      piece_length; pieces }


let name torrent =
    match torrent.name with
    | None -> ""
    | Some str -> str
