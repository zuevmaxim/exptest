max.int.exp.test <- function(x, nrepl=2000)
{
    DNAME <- deparse(substitute(x))
    k <- 0
    n <- length(x)
    x <- sort(x)
    t <- 0
    for (i in 1 : n) {
        t = max(t, x[i] - x[i - 1])
    }
    t <- t / sum(x)
    for (i in 1 : nrepl)
    {
        z <- rexp(n)
        z <- sort(z)
        T <- 0
        for (i in 1 : n)
        {
            T = max(T, z[i] - z[i - 1])
        }
        T <- T / sum(z)
        if (T < t)k = k + 1
    }
    p.value <- k / nrepl

    RVAL <- list(statistic = c(L = t), p.value = p.value, method = "Maximum interval test for exponentiality", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}