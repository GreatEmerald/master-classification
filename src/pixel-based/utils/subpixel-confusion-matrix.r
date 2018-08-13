# Sub-pixel confusion–uncertainty matrix
# Implementation following https://doi.org/10.1016/j.rse.2007.07.017

# Glossary
# Values should have a range of 0-1
observed = Data.df[,GetLargeClassNames(Data.df)]/100
N = nrow(observed)
K = length(GetLargeClassNames(Data.df))
n = 1:N
k = 1:K
l = 1:K
s_nk = as.numeric(predicted[nrow(predicted),k]) # Just one pixel
names(s_nk) = colnames(predicted)
r_nl = as.numeric(observed[nrow(predicted),l])
names(r_nl) = colnames(observed)
s_pk = sum(s_nk)
r_pl = sum(r_nl)
sp_nk = s_nk - r_nl; sp_nk[sp_nk<0] = 0
rp_nl = r_nl - s_nk; rp_nl[rp_nl<0] = 0
p_nkl = Comparator(s_nk, r_nl) # colSums == r_nl
p_min = Comparator(s_nk, r_nl, D=MIN_D) # colSums != r_nl (pessimistic, values higher)
p_least= Comparator(s_nk, r_nl, D=LEAST_D) # colSums != r_nl (optimistic, values lower)
corrplot::corrplot(p_nkl) # The part of [row] that was predicted should actually have gone to [column]
image(p_nkl)
lattice::levelplot(p_nkl, ylab="Observed", xlab="Predicted")
lattice::levelplot(p_min, ylab="Observed", xlab="Predicted")
lattice::levelplot(p_least, ylab="Observed", xlab="Predicted")
P_kl = sum(p_nkl)
P_kp = rowSums(p_nkl)
P_pl = colSums(p_nkl)
P_pp = sum(P_kl) # Same as P_kl?..

# Sanity checks
stopifnot(all(r_nl >= 0))
stopifnot(all(r_nl <= 1))
stopifnot(all(s_nk >= 0))
stopifnot(all(s_nk <= 1))
# There may be a loss in float precision, so round
stopifnot(all(round(rowSums(s_nk), 10) == 1))
stopifnot(all(round(rowSums(r_nl), 10) == 1))

# Comparator function
Comparator = function(s_nk, r_nl, A=MIN, D=PROD_D)
{
    A = match.fun(A)
    D = match.fun(D)
    
    stopifnot(is.numeric(s_nk))
    stopifnot(is.numeric(r_nl))
    stopifnot(is.numeric(sp_nk))
    stopifnot(is.numeric(rp_nl))

    Result = matrix(NA, K, K) #p_nkl
    for (k in 1:K)
    {
        for (l in 1:K)
        {
            if (k == l) {
                Result[k,l] = A(s_nk[k], r_nl[l])
            } else {
                temp = D(sp_nk, rp_nl, k, l)
                Result[k,l] = temp
            }
        }
    }
    rownames(Result) = names(s_nk)
    colnames(Result) = names(r_nl)
    return(Result)
}

VectorComparator = function(s_nk, r_nl, A=MIN, D=MIN) # Same but vectorised
{
    A = match.fun(A)
    D = match.fun(D)

    ResultA = A(s_nk, r_nl)
    Result = D(sp_nk, rp_nl)
    diag(Result) = ResultA
    
    return(Result)
}

# Agreement/disagreement operators
MIN = function(s_nk, r_nl)
{
    min(s_nk, r_nl) # This is actually literally min, so it's not really worth implementing
}

SI = function(s_nk, r_nl)
{
    1 - abs(s_nk - r_nl)/(s_nk + r_nl)
}

PROD = function(s_nk, r_nl)
{
    s_nk * r_nl # This is literally "*"
}

LEAST = function(s_nk, r_nl)
{
    max(s_nk + r_nl - 1, 0)
}

# l is "should be attributed to...", k is "but was guessed as"
PROD_D = function(sp_nk, rp_nl, k, l)
{
    sp_nk[k] * rp_nl[l] / sum(rp_nl)
}

MIN_D = function(sp_nk, rp_nl, k, l)
{
    min(sp_nk[k], rp_nl[l])
}

LEAST_D = function(sp_nk, rp_nl, k, l)
{
    max(sp_nk[k]+rp_nl[l]-sum(rp_nl), 0)
}
