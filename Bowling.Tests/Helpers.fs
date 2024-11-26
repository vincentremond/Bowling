namespace Bowling

[<RequireQualifiedAccess>]
module List =

    let windowedStep size step list =
        let rec loop acc =
            function
            | [] -> List.rev acc
            | list when List.length list < size -> List.rev acc
            | list ->
                let window = List.take size list
                let rest = List.skip step list
                loop (window :: acc) rest

        loop [] list
