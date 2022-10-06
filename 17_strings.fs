// 17.1
let rec pow = function
 | (s, 0) -> ""
 | (s, n) -> s + pow(s, n - 1)

// 17.2
let rec isIthChar = function
 | (s, i, c) when String.length(s) <= i -> false
 | (s, i, c) -> s.[i] = c

// 17.3
let rec occFromIth = function
 | (s, n, c) when n >= String.length(s) -> 0
 | (s, n, c) when isIthChar(s, n, c) -> occFromIth(s, n + 1, c) + 1
 | (s, n, c) -> occFromIth(s, n + 1, c)
