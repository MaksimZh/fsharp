// 34.1
let upto n =
    let rec fn = function
      | (0, acc) -> acc
      | (n, acc) -> fn(n - 1, n :: acc) 
    fn(n, [])

// 34.2
let rec dnto = function
  | 0 -> []
  | n -> n :: dnto(n - 1)

// 34.3
let evenn n =
    let rec fn = function
      | (0, acc) -> acc
      | (n, acc) -> fn(n - 1, (n - 1) * 2 :: acc) 
    fn(n, [])
