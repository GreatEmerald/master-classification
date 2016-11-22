# A user-friendly function to get QC values for PROBA-V

# True means must have, False means must not have, NA means don't care
GetProbaVQCMask = function(bluegood = NA, redgood = NA, nirgood = NA, swirgood = NA, land = NA, ice = NA, cloud = NA, shadow = NA)
{
  bluegoodbit = bitwShiftL(1, 7)
  redgoodbit = bitwShiftL(1, 6)
  nirgoodbit = bitwShiftL(1, 5)
  swirgoodbit = bitwShiftL(1, 4)
  landbit = bitwShiftL(1, 3)
  icebit = bitwShiftL(1, 2)
  cloudbit = bitwShiftL(1, 1)
  shadowbit = bitwShiftL(1, 0)
  
  # If True, add the bit, if False don't, if NA keep original and also add it
  BitIterator = function(input, state, bit)
  {
    if(is.na(state))
      input = c(input, input + bit)
    else if(state)
      input = input + bit
    return(input)
  }
  
  result = 0L
  result = BitIterator(result, bluegood, bluegoodbit)
  result = BitIterator(result, redgood, redgoodbit)
  result = BitIterator(result, nirgood, nirgoodbit)
  result = BitIterator(result, swirgood, swirgoodbit)
  result = BitIterator(result, land, landbit)
  result = BitIterator(result, ice, icebit)
  result = BitIterator(result, cloud, cloudbit)
  result = BitIterator(result, shadow, shadowbit)
  
  return(result)
}
