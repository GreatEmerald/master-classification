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

# Example at the start of the paper; these are agreement matrices only
observed = c(Class_1=0.5, Class_2=0.375, Class_3=0.125)
predicted = c(Class_1=0.625, Class_2=0.25, Class_3=0.125)
SCM(predicted, observed, agreement="*", disagreement=NULL, totals=TRUE) # (b)
SCM(predicted, observed, agreement=LEAST, disagreement=NULL, totals=TRUE) # (c)
SCM(predicted, observed, agreement=min, disagreement=NULL, totals=TRUE) # (d)
SCM(predicted, observed, totals=TRUE, plot=TRUE) # What an SCM would look like of that example
SCM(predicted, observed, disagreement=PROD_D, totals=TRUE) # Equivalent to MIN-PROD

# From calc example
observed = c(dec.trees=32, evg.trees=32, agriculture=0, grass=20, water=0, urban=3, bare=10, shrubs=3) / 100
reference = c(dec.trees=30, evg.trees=40, agriculture=0, grass=20, water=0, urban=0, bare=10, shrubs=0) / 100
model1 = c(dec.trees=30, evg.trees=25, agriculture=2, grass=23, water=2, urban=3, bare=12, shrubs=3) / 100
control = c(dec.trees=12.5, evg.trees=12.5, agriculture=12.5, grass=12.5, water=12.5, urban=12.5, bare=12.5, shrubs=12.5) / 100

totals.scm(accuracy.scm(SCM(reference, observed, disagreement=PROD_D)))
totals.scm(accuracy.scm(SCM(reference, observed)))
scmref = SCM(reference, observed)
plot(scmref)
totals.scm(accuracy.scm(scmref))

totals.scm(accuracy.scm(SCM(model1, observed, disagreement=PROD_D, plot=TRUE)))
totals.scm(accuracy.scm(SCM(model1, observed, plot=TRUE)))

totals.scm(accuracy.scm(SCM(control, observed, disagreement=PROD_D, plot=TRUE)))
totals.scm(accuracy.scm(SCM(control, observed, plot=TRUE)))

# What if we have more than one pixel?
observed3 = rbind(observed, observed, observed)
predicted = rbind(reference, model1, control)

SCM(predicted, observed3, totals=TRUE, plot=TRUE) # 79%
SCM(predicted, observed3, totals=TRUE, scale=TRUE) # Scaling does not affect the accuracy statistics
SCM(predicted, observed3, totals=TRUE, plot=TRUE, disagreement=PROD_D) # 79%

# Compared to individual ones
SCM(reference, observed, totals=TRUE, plot=TRUE) # 92%
SCM(model1, observed, totals=TRUE, plot=TRUE) # 92%
SCM(control, observed, totals=TRUE, plot=TRUE) # 65%
