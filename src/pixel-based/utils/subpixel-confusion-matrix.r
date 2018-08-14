# Sub-pixel confusion–uncertainty matrix
# Implementation following https://doi.org/10.1016/j.rse.2007.07.017

# Glossary
# Values should have a range of 0-1
#observed = Data.df[,GetLargeClassNames(Data.df)]/100
observed = c(X1=0.4, X2=0.3, X3=0.2, X4=0.1)
predictedA = c(X1=0.2, X2=0.3, X3=0.4, X4=0.1)
predictedB = c(X1=0.3, X2=0.4, X3=0.1, X4=0.2)
#N = nrow(observed)
#K = length(GetLargeClassNames(Data.df))
K = length(observed)
#n = 1:N
k = 1:K
l = 1:K
#s_nk = as.numeric(predicted[nrow(predicted),k]) # Just one pixel
#names(s_nk) = colnames(predicted)
s_nk = predictedA
#r_nl = as.numeric(observed[nrow(predicted),l])
#names(r_nl) = colnames(observed)
r_nl = observed
s_pk = sum(s_nk)
r_pl = sum(r_nl)
sp_nk = s_nk - r_nl; sp_nk[sp_nk<0] = 0
rp_nl = r_nl - s_nk; rp_nl[rp_nl<0] = 0

# Sanity checks
stopifnot(all(r_nl >= 0))
stopifnot(all(r_nl <= 1))
stopifnot(all(s_nk >= 0))
stopifnot(all(s_nk <= 1))
# There may be a loss in float precision, so round
stopifnot(all(round(sum(s_nk), 10) == 1))
stopifnot(all(round(sum(r_nl), 10) == 1))

p_nkl = Comparator(s_nk, r_nl) # colSums == r_nl
p_min = Comparator(s_nk, r_nl, D=MIN_D) # colSums != r_nl (pessimistic, values higher)
p_least= Comparator(s_nk, r_nl, D=LEAST_D) # colSums != r_nl (optimistic, values lower)
corrplot::corrplot(p_nkl) # The part of [row] that was predicted should actually have gone to [column]
image(p_nkl)
lattice::levelplot(p_nkl, ylab="Observed", xlab="Predicted")
lattice::levelplot(p_min, ylab="Observed", xlab="Predicted")
lattice::levelplot(p_least, ylab="Observed", xlab="Predicted")
P_kl = (p_min + p_least)/2
lattice::levelplot(P_kl, ylab="Observed", xlab="Predicted")
P_kp = rowSums(P_kl) # These classes were overestimated (similar to, but less accurate than, s_nk)
P_pl = colSums(P_kl) # These classes were underestimated (similar to, but less accurate than, r_nl)
P_pp = sum(P_kl) # Similar to, but less acurrate than, 1 (fractions summing to 1)

# Uncertainties associated with the estimates P
U_kl = (p_min - p_least)/2
lattice::levelplot(U_kl, ylab="Observed", xlab="Predicted")
U_kp = rowSums(U_kl)
U_pl = colSums(U_kl)
U_pp = sum(U_kl)

# Add total rows/columns: with uncertainty
P_FullMatrix = cbind(P_kl, total=P_kp)
P_FullMatrix = rbind(P_FullMatrix, total=c(P_pl, P_pp))

U_FullMatrix = cbind(U_kl, total=U_kp)
U_FullMatrix = rbind(U_FullMatrix, total=c(U_pl, U_pp))
# Without uncertainty we'd use r_nl and s_nk

# Accuracy indices
# P_kk means the diagonal!
P_OA_s = (P_pp*sum(diag(P_kl))) / (P_pp^2 - U_pp^2)
U_OA_s = (U_pp*sum(diag(P_kl))) / (P_pp^2 - U_pp^2)

P_UA_s = (diag(P_kl)*P_kp) / (P_kp^2 - U_kp^2) # Problems when we have uncertainty == prediction, division by 0
U_UA_s = (diag(P_kl)*U_kp) / (P_kp^2 - U_kp^2) # In which case the diagonals are 0, so it's 0 by definition
P_UA_s[is.nan(P_UA_s)] = 0
U_UA_s[is.nan(U_UA_s)] = 0

P_PA_s = (diag(P_kl)*P_pl) / (P_pl^2 - U_pl^2)
U_PA_s = (diag(P_kl)*U_pl) / (P_pl^2 - U_pl^2)
P_PA_s[is.nan(P_PA_s)] = 0
U_PA_s[is.nan(U_PA_s)] = 0

# Kappa coefficient
# Expected proportion of agreement using the monkey method
# Loop
#P_e = 0
#for (k in 1:K)
#{
#    P_e = P_e + ((P_pp^2 + U_pp^2)*(P_pl[k]*P_kp[k] + U_pl[k]*U_kp[k]) - 2 * P_pp*U_pp*(U_pl[k]*P_kp[k] + P_pl[k]*U_kp[k])) / (P_pp^2-U_pp^2)^2
#}
# Vectorised
P_e = sum(((P_pp^2 + U_pp^2)*(P_pl*P_kp + U_pl*U_kp) - 2 * P_pp*U_pp*(U_pl*P_kp + P_pl*U_kp)) / (P_pp^2-U_pp^2)^2)
U_e = sum((2 * P_pp*U_pp*(P_pl*P_kp + U_pl*U_kp) - (P_pp^2 + U_pp^2)*(U_pl*P_kp + P_pl*U_kp)) / (P_pp^2-U_pp^2)^2)

Sign = (1-P_OA_s-U_OA_s)*(1-P_e-U_e)
P_Kappa_s = ((P_OA_s-P_e) * (1-P_e) - (sign(Sign)*U_OA_s+U_e) * U_e)/((1-P_e)^2-U_e^2)
U_Kappa_s = ((sign(Sign)*(1-P_OA_s)*U_e + (1-P_e)*U_OA_s)/((1-P_e)^2-U_e^2))

# Comparator function
Comparator = function(s_nk, r_nl, A=MIN, D=PROD_D)
{
    A = match.fun(A)
    D = match.fun(D)
    
    stopifnot(is.numeric(s_nk))
    stopifnot(is.numeric(r_nl))
    stopifnot(length(s_nk) == length(r_nl))
    K = length(s_nk)
    
    # Overestimation and underestimation
    sp_nk = s_nk - r_nl; sp_nk[sp_nk<0] = 0
    rp_nl = r_nl - s_nk; rp_nl[rp_nl<0] = 0
    
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

# Function for calculating a confusion matrix
SCM = function(predicted, observed, agreement=MIN, disagreement="SCM", accuracy=FALSE, totals=FALSE)
{
    # Aliases; TODO: remove/rename
    s_nk = predicted
    r_nl = observed
    
    # Sanity checks: the input must be 0-1 range and add up to 100%
    stopifnot(all(r_nl >= 0))
    stopifnot(all(r_nl <= 1))
    stopifnot(all(s_nk >= 0))
    stopifnot(all(s_nk <= 1))
    stopifnot(all(round(sum(s_nk), 10) == 1))
    stopifnot(all(round(sum(r_nl), 10) == 1))
    
    if (is.character(disagreement) && disagreement == "SCM") # Mean of MIN_D and LEAST_D + confusion
    {
        p_min = Comparator(s_nk, r_nl, A=agreement, D=MIN_D)
        #print(p_min)
        p_least = Comparator(s_nk, r_nl, A=agreement, D=LEAST_D)
        #print(p_least)
        P_kl = (p_min + p_least)/2
        U_kl = (p_min - p_least)/2
    } else {
        P_kl = Comparator(s_nk, r_nl, A=agreement, D=disagreement)
        U_kl = matrix(0, nrow=nrow(P_kl), ncol=ncol(P_kl), dimnames=dimnames(P_kl)) # No uncertainties for other types; U is a zero matrix
    }
    scm = structure(list(P=P_kl, U=U_kl, agreement=as.character(substitute(agreement)), disagreement=as.character(substitute(disagreement))), class="scm")
    
    if (accuracy || totals)
        scm = accuracy.scm(scm)
    
    if (totals)
        scm = totals.scm(scm)

    return(scm)
}

# Function for adding "total" columns/rows to the SCM
totals.scm = function(scm)
{
    stopifnot(class(scm) == "scm")

    if (is.null(scm[["P_row_totals"]]))
    {
        warning("Passed an SCM without calculating accuracy metrics. This will be done for you.")
        scm = accuracy.scm(scm)
    }
    
    P_kp = rowSums(scm$P)
    P_pl = colSums(scm$P)
    P_pp = sum(scm$P)

    U_kp = rowSums(scm$U)
    U_pl = colSums(scm$U)
    U_pp = sum(scm$U)

    # Add total rows/columns: with uncertainty
    P_FullMatrix = cbind(scm$P, total=scm$P_row_totals)
    P_FullMatrix = rbind(P_FullMatrix, total=c(scm$P_col_totals, scm$P_total))

    U_FullMatrix = cbind(scm$U, total=scm$U_row_totals)
    U_FullMatrix = rbind(U_FullMatrix, total=c(scm$U_col_totals, scm$U_total))
    
    # Add user/producer accuracy
    P_FullMatrix = cbind(P_FullMatrix, user.acc=c(scm$P_user_accuracy, NA))
    P_FullMatrix = rbind(P_FullMatrix, prod.acc=c(scm$P_producer_accuracy, NA, scm$P_overall_accuracy))

    U_FullMatrix = cbind(U_FullMatrix, user.acc=c(scm$U_user_accuracy, NA))
    U_FullMatrix = rbind(U_FullMatrix, prod.acc=c(scm$U_producer_accuracy, NA, scm$U_overall_accuracy))
    
    scm$P = P_FullMatrix
    scm$U = U_FullMatrix
    return(scm)
}

# Add accuracy metrics to an scm object
accuracy.scm = function(scm)
{
    stopifnot(class(scm) == "scm")
    
    P_kp = rowSums(scm$P)
    P_pl = colSums(scm$P)
    P_pp = sum(scm$P)

    U_kp = rowSums(scm$U)
    U_pl = colSums(scm$U)
    U_pp = sum(scm$U)

    P_OA_s = (P_pp*sum(diag(scm$P))) / (P_pp^2 - U_pp^2)
    U_OA_s = (U_pp*sum(diag(scm$P))) / (P_pp^2 - U_pp^2)

    P_UA_s = (diag(scm$P)*P_kp) / (P_kp^2 - U_kp^2) # Problems when we have uncertainty == prediction, division by 0
    U_UA_s = (diag(scm$P)*U_kp) / (P_kp^2 - U_kp^2) # In which case the diagonals are 0, so it's 0 by definition
    P_UA_s[is.nan(P_UA_s)] = 0
    U_UA_s[is.nan(U_UA_s)] = 0

    P_PA_s = (diag(scm$P)*P_pl) / (P_pl^2 - U_pl^2)
    U_PA_s = (diag(scm$P)*U_pl) / (P_pl^2 - U_pl^2)
    P_PA_s[is.nan(P_PA_s)] = 0
    U_PA_s[is.nan(U_PA_s)] = 0

    # Kappa coefficient
    # Expected proportion of agreement (i.e. when using the monkey method)
    P_e = sum(((P_pp^2 + U_pp^2)*(P_pl*P_kp + U_pl*U_kp) - 2 * P_pp*U_pp*(U_pl*P_kp + P_pl*U_kp)) / (P_pp^2-U_pp^2)^2)
    U_e = sum((2 * P_pp*U_pp*(P_pl*P_kp + U_pl*U_kp) - (P_pp^2 + U_pp^2)*(U_pl*P_kp + P_pl*U_kp)) / (P_pp^2-U_pp^2)^2)

    Sign = (1-P_OA_s-U_OA_s)*(1-P_e-U_e)
    P_Kappa_s = ((P_OA_s-P_e) * (1-P_e) - (sign(Sign)*U_OA_s+U_e) * U_e)/((1-P_e)^2-U_e^2)
    U_Kappa_s = ((sign(Sign)*(1-P_OA_s)*U_e + (1-P_e)*U_OA_s)/((1-P_e)^2-U_e^2))
    
    scm[["P_row_totals"]] = P_kp
    scm[["P_col_totals"]] = P_pl
    scm[["P_total"]] = P_pp
    scm[["U_row_totals"]] = U_kp
    scm[["U_col_totals"]] = U_pl
    scm[["U_total"]] = U_pp
    scm[["P_overall_accuracy"]] = P_OA_s
    scm[["U_overall_accuracy"]] = U_OA_s
    scm[["P_user_accuracy"]] = P_UA_s
    scm[["U_user_accuracy"]] = U_UA_s
    scm[["P_producer_accuracy"]] = P_PA_s
    scm[["U_producer_accuracy"]] = U_PA_s
    scm[["P_user_accuracy"]] = P_UA_s
    scm[["P_kappa"]] = P_Kappa_s
    scm[["U_kappa"]] = U_Kappa_s
    
    return(scm)
}

# Tests
observed = c(X1=0.4, X2=0.3, X3=0.2, X4=0.1)
predictedA = c(X1=0.2, X2=0.3, X3=0.4, X4=0.1)
predictedB = c(X1=0.3, X2=0.4, X3=0.1, X4=0.2)

MIN_PROD_A = accuracy.scm(SCM(predictedA, observed, disagreement=PROD_D))
MIN_PROD_B = accuracy.scm(SCM(predictedB, observed, disagreement=PROD_D))

MIN_PROD_A[["P_overall_accuracy"]] == 0.8
MIN_PROD_B[["P_overall_accuracy"]] == 0.8

round(MIN_PROD_A[["P_kappa"]], 4) == 0.7297
round(MIN_PROD_B[["P_kappa"]], 4) == 0.7222

SCM_A = accuracy.scm(SCM(predictedA, observed))
SCM_A[["P_overall_accuracy"]] == 0.8
SCM_A[["U_overall_accuracy"]] == 0
round(SCM_A$P_kappa, 4) == 0.7297
SCM_A$U_kappa == 0

SCM_B = accuracy.scm(SCM(predictedB, observed))
round(SCM_B[["P_overall_accuracy"]], 4) == 0.8333
round(SCM_B[["U_overall_accuracy"]], 4) == 0.1667
round(SCM_B$P_kappa, 4) == 0.7778
round(SCM_B$U_kappa, 4) == 0.2222

# From calc example
observed = c(dec.trees=32, evg.trees=32, agriculture=0, grass=20, water=0, urban=3, bare=10, shrubs=3) / 100
reference = c(dec.trees=30, evg.trees=40, agriculture=0, grass=20, water=0, urban=0, bare=10, shrubs=0) / 100
model1 = c(dec.trees=30, evg.trees=25, agriculture=2, grass=23, water=2, urban=3, bare=12, shrubs=3) / 100
control = c(dec.trees=12.5, evg.trees=12.5, agriculture=12.5, grass=12.5, water=12.5, urban=12.5, bare=12.5, shrubs=12.5) / 100

totals.scm(accuracy.scm(SCM(reference, observed, disagreement=PROD_D)))
totals.scm(accuracy.scm(SCM(reference, observed)))

totals.scm(accuracy.scm(SCM(model1, observed, disagreement=PROD_D)))
totals.scm(accuracy.scm(SCM(model1, observed)))

totals.scm(accuracy.scm(SCM(control, observed, disagreement=PROD_D)))
totals.scm(accuracy.scm(SCM(control, observed)))
