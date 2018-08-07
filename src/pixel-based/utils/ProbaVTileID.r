# Function that returns the Proba-V tile number when given a set of coordinates
ProbaVTileID = function(point)
{
    # X00Y00 is -180.0005, -170.0005,  65.0005,  75.0005  (xmin, xmax, ymin, ymax)
    # X12Y13 is  -60.0005,  -50.0005, -64.9995, -54.9995
    # X17Y03 is  -10.0005,   -0.0005,  35.0005,  45.0005
    # X18Y04 is   -0.0005,    9.9995,  25.0005,  35.0005
    # X18Y06 is   -0.0005,    9.9995,   5.0005,  15.0005
    # X18Y07 is   -0.0005,    9.9995,  -4.9995,   5.0005
    # X35Y12 is  169.9995,  179.9995, -54.9995, -44.9995
    # Point 0,0 is at X18Y07
    
    # xmin: -180.0005 + 10 * X
    # xmax: xmin + 10
    # ymin:   65.0005 - 10 * Y
    # ymax: ymin + 10
    
    # Inversion: (180.0005 + xmin)/10 = X
    XID = as.integer((180.0005 + point$x)/10)
    # Inversion:  (75.0005 - ymin)/10 = Y
    YID = as.integer((75.0005 - point$y)/10)
    
    return(sprintf("X%02dY%02d", XID, YID))
}

# Africa is from X16Y03 to X23Y10
