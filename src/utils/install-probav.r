# Independent script for installing the PROBA-V package.
# Only needs to run once, but no-op if run multiple times

# Get PROBA-V processing package by JE&JD
if (!("probaV" %in% installed.packages()[,"Package"]))
{
    if (!("devtools" %in% installed.packages()[,"Package"]))
    {
        install.packages("devtools")
    }
    library(devtools)
    options(unzip = 'internal')
    if (!("lubridate" %in% installed.packages()[,"Package"]))
    {
        install.packages("lubridate")
    }
    install_github("JornDallinga/probaV")
}
