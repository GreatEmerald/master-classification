# Util functions for getting soil information from LandGIS/SoilGrids
library(RCurl)
library(rjson)

# Given a data.frame that includes fields 'x' and 'y', return a URL at which we can find soil data from LandGIS
LGURL = function(points)
{
    return(paste0("https://landgisapi.opengeohub.org/query/point?lon=", points$x, "&lat=", points$y, "&coll=predicted250m&regex=sol_[^t].*_m_250m_b"))
}

# Same for SoilGrids
SGURL = function(points)
{
    return(paste0("https://rest.soilgrids.org/query?lon=", points$x, "&lat=", points$y))
}

# Downloads a JSON file from a given URL, and returns it as a numeric matrix.
# ulproperty is the field in the response to unlist, "properties" for SG and "response" for LG
GetSGMatrix = function(url, ulproperty="properties")
{
    SGData = NULL
    while (is.null(SGData))
    {
        print(paste("Downloading data from URL:", url))
        SGData = try(rjson::fromJSON(RCurl::getURL(url)))
        if (class(SGData) == "try-error")
        {
            print("Could not download the data, retrying in 30s.")
            SGData = NULL
            Sys.sleep(30)
        }
    }
    QR = unlist(SGData[[ulproperty]])
    RRow = as.numeric(QR)
    return(matrix(RRow, ncol=length(RRow), dimnames=list(NULL, names(QR))))
}
