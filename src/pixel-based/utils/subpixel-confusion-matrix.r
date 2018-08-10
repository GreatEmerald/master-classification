# Sub-pixel confusion–uncertainty matrix
# Implementation following https://doi.org/10.1016/j.rse.2007.07.017

# Glossary
# Values should have a range of 0-1
observed = Data.df[,GetLargeClassNames(Data.df)]/100
N = nrow(observed)
K = length(Classes)
n = 1:N
k = 1:K
l = 1:K
s_nk = predicted[n,k]
r_nl = observed[n,l]
s_pk = colSums(s_nk)
r_pl = colSums(r_nl)
sp_nk = s_nk - r_nl; sp_nk[sp_nk<0] = 0
rp_nl = r_nl - s_nk; rp_nl[rp_nl<0] = 0

# Sanity checks
stopifnot(all(r_nl >= 0))
stopifnot(all(r_nl <= 1))
stopifnot(all(s_nk >= 0))
stopifnot(all(s_nk <= 1))
# There may be a loss in float precision, so round
stopifnot(all(round(rowSums(s_nk), 14) == 1))
stopifnot(all(round(rowSums(r_nl), 2) == 1))
which(round(rowSums(r_nl), 2) != 1)
