// 39.1
let rec rmodd = function
  | [] -> []
  | [x] -> []
  | first :: (second :: tail) -> second :: (rmodd tail)

// 39.2
let rec del_even = function
  | [] -> []
  | head :: tail when (head % 2 = 0) -> (del_even tail)
  | head :: tail -> head :: (del_even tail)

// 39.3
let rec multiplicity x xs = match xs with
  | [] -> 0
  | head :: tail when (head = x) -> 1 + (multiplicity x tail)
  | _ :: tail -> multiplicity x tail

// 39.4
let rec split = function
  | [] -> ([], [])
  | [x] -> ([x], [])
  | first :: (second :: tail) ->
        let (a, b) = split tail
        (first :: a, second :: b)

// 39.5
let rec zip = function
  | ([], []) -> []
  | (h1 :: t1, h2 :: t2) -> (h1, h2) :: zip(t1, t2)
  | _ -> failwith "different lengths"
