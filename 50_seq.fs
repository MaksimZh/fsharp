// 50.2.1
let fac_seq = seq {
    let mutable f = 1
    let mutable n = 1
    while true do
        yield f
        f <- f * n
        n <- n + 1
}

// 50.2.2
let seq_seq = seq {
    yield 0
    let mutable i = 1
    while true do
        yield -i
        yield i
        i <- i + 1
}
