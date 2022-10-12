// 40.1
let sum (p, xs) =
    let rec iter (s, a) = match s with
        | [] -> a
        | head :: tail -> iter (tail, if (p head) then a + head else a)
    iter (xs, 0)

// 40.2.1
let count (xs, n) =
    let rec iter (s, a) = match s with
        | [] -> a
        | head :: tail when (head > n) -> a
        | head :: tail -> iter(tail, if head = n then a + 1 else a)
    iter (xs, 0)

// 40.2.2
let insert (xs, n) =
    let rec iter (s, a) = match s with
        | [] -> a @ [n]
        | head :: tail when (head >= n) -> a @ (n :: s)
        | head :: tail -> iter(tail, a @ [head])
    iter (xs, [])

// 40.2.3
let intersect (xs1, xs2) =
    let rec iter (s1, s2, a) = match (s1, s2) with
        | ([], _) | (_, []) -> a
        | (h1 :: t1, h2 :: t2) -> match (compare h1 h2) with
            | 0 -> iter(t1, t2, a @ [h1])
            | c when c > 0 -> iter(s1, t2, a)
            | _ -> iter(t1, s2, a)
    iter (xs1, xs2, [])

// 40.2.4
let plus (xs1, xs2) =
    let rec iter (s1, s2, a) = match (s1, s2) with
        | ([], _) -> a @ s2
        | (_, []) -> a @ s1
        | (h1 :: t1, h2 :: t2) -> match (compare h1 h2) with
            | 0 -> iter(t1, t2, a @ [h1; h2])
            | c when c > 0 -> iter(s1, t2, a @ [h2])
            | _ -> iter(t1, s2, a @ [h1])
    iter (xs1, xs2, [])

// 40.2.5
let minus (xs1, xs2) =
    let rec iter (s1, s2, a) = match (s1, s2) with
        | ([], _) -> a
        | (_, []) -> a @ s1
        | (h1 :: t1, h2 :: t2) -> match (compare h1 h2) with
            | 0 -> iter(t1, t2, a)
            | c when c > 0 -> iter(s1, t2, a)
            | _ -> iter(t1, s2, a @ [h1])
    iter (xs1, xs2, [])

// 40.3.1
let smallest xs =
    let rec iter = function
        | ([], a) -> a
        | (head :: tail, None) -> iter(tail, Some(head))
        | (head :: tail, Some(v)) -> iter(tail, Some(if v > head then head else v))
    iter (xs, None)

// 40.3.2
let delete (n, xs) =
    let rec iter = function
        | ([], a) -> a
        | (head :: tail, a) when (head = n) -> a @ tail
        | (head :: tail, a) -> iter(tail, a @ [head])
    iter (xs, [])

// 40.3.3
let sort xs =
    let rec iter (s, a) = match (smallest s) with
        | None -> a
        | Some(v) -> iter(delete(v, s), a @ [v])
    iter(xs, [])

// 40.4
let revrev xs =
    let rec iter = function
        | ([], a) -> a
        | (head :: tail, a) -> iter(tail, (List.rev head) :: a)
    iter(xs, [])