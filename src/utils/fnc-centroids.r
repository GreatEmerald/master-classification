# Fuzzy nearest prototype helper functions: calculate centroids based on actual weighted mean

# data is a data.frame containing all observations
# formula is the format dominant_lc~c+o+v+a+r+s, and dominant_lc is a factor whose levels match column names of data
GetClassMeans = function(formula, data)
{
    DominantFactor = formula.tools::lhs.vars(formula)
    Classes = levels(data[,DominantFactor])
    Covars = formula.tools::rhs.vars(formula)
    
    combos = expand.grid(Classes=Classes, Covars=Covars)
    ClassMeans = function(x)
    {
        Hmisc::wtd.mean(data[,x["Covars"]], data[,x["Classes"]])
    }
    cm = apply(combos, 1, ClassMeans)
    c.means = matrix(cm, nrow=length(Classes), dimnames=list(Classes, Covars))
    return(c.means)
}

GetClassSDs = function(formula, data)
{
    DominantFactor = formula.tools::lhs.vars(formula)
    Classes = levels(data[,DominantFactor])
    Covars = formula.tools::rhs.vars(formula)

    combos = expand.grid(Classes=Classes, Covars=Covars)
    ClassSDs = function(x)
    {
        sqrt(Hmisc::wtd.var(data[,x["Covars"]], data[,x["Classes"]], normwt = TRUE))
    }
    csd = apply(combos, 1, ClassSDs)
    c.sds = matrix(csd, nrow=length(Classes), dimnames=list(Classes, Covars))
    return(c.sds)
}
