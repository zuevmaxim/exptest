bartlett.exp.test <- function(x, simulate.p.value=FALSE, nrepl=2000)
{
    calcB <- function(z)
    {
        n <- length(x)
        L <- log(sum(x) / n)
        S <- 0
        for (i in 1 : n)
        {
            S = S + log(x[i])
        }
        S = S / n
        B <- 12 * n ^ 2 / (7 * n + 1) * (L + S)
        return(B)
    }

    DNAME <- deparse(substitute(x))
    n <- length(x)

    for (i in 1 : n)
    {
        if (x[i] < 0) {
            RVAL <- list(p.value = 0, method = "Test for exponentiality based on Bartlet-Moran's characterization", data.name = DNAME)
            class(RVAL) <- "htest"
            return(RVAL)
        }
    }

    B = calcB(x)
    chi <- qchisq(.95, n - 1)
    if (simulate.p.value)
    {
        l <- 0
        for (m in 1 : nrepl)
        {
            z <- rexp(n)
            if (calcB(z) < chi)
            {
                l <- l + 1
            }
        }
        p.value <- l / nrepl
    }
    else
    {
        p.value <- if (B < chi) 1 else 0
    }

    RVAL <- list(statistic = c(In = B), p.value = p.value, method = "Test for exponentiality based on Bartlet-Moran's characterization", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

res = bartlett.exp.test(rexp(1000))
print(res)