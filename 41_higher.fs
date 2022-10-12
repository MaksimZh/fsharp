// 41.4.1
let list_filter f xs = List.foldBack (fun head tail -> if (f head) then head :: tail else tail) xs []

// 41.4.2
let sum (p, xs) = List.fold (fun acc item -> if (p item) then item + acc else acc) 0 xs

// 41.4.3
let revrev = List.fold (fun acc item -> (List.rev item) :: acc) []
