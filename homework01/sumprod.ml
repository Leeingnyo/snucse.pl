let rec prod (m, n, k) = if (k <= 0) then 1.  else prod (m, n, k - 1) *. m (n, k)
let rec sumprod (m, n, k) = if (n <= 0) then 0. else prod(m, n, k) +. sumprod(m, n - 1, k)
