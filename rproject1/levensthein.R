lev <- function(s1, s2) {
    if (nchar(s1) == 0)
        return(nchar(s2)) 
    if (nchar(s2) == 0)
        return(nchar(s1))

    if (substr(s1, 1, 1) == substr(s2, 1, 1)) {
        lev(substr(s1, 2, nchar(s1)), substr(s2, 2, nchar(s2)))
    }
    else {
        min(1 + lev(substr(s1, 2, nchar(s1)), s2),
        1 + lev(s1, substr(s2, 2, nchar(s2))),
        1 + lev(substr(s1, 2, nchar(s1)), substr(s2, 2, nchar(s2)))
        )
    }
}

system.time(lev("lasse", "liten"))
system.time(levmemo("lasse", "liten"))