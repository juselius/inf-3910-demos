module Units

[<Measure>] type cm
[<Measure>] type cl = cm^3
[<Measure>] type cm2 = cm^2

let a = 2.0<cm>
let b = 4.0<cl>
let c  : float<cm2> = b/a
let c' = b/a
// let d = a + b