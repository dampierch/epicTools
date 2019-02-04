#####
##### Test code for proposed diet order
#####

#to do: 1) summarize diet order 2) fix input so only integers accepted
### goals of this workflow

  # every patient gets oral diet order option
  # only patients with enteral access get enteral diet order option
  # only patients with oral diet order != regular get clinical condition
  # only patients with oral diet == "nothing w/ exceptions" get exceptions option
  # only patients with enteral diet order option !=NA and !="nothing at all" get proximal limit option


### set options menus

oral_diet_options <- c("regular", "therapeutic", "nothing at all", "nothing with exceptions")
enteral_diet_options <- c("tube feeds", "meds", "flushes", "nothing at all")
clinical_condition_options <- c("awaiting surgery/procedure", "aspiration risk/awaiting speech eval", "dysphagia", "ileus/obstruction", "ischemia/inflammation", "other")
oral_diet_exceptions <- c("meds", "sips with meds", "ice chips", "gum")
enteral_access_options <- c("gastric", "post-pyloric", "jejunal")


### set EMR data

possible_ldas <- c("NGT", "OGT", "PEG" ,"PEG-J", "J", "DHT", "CVL", "art", "PIV", "ETT")
enteral_ldas <- c("NGT", "OGT", "PEG" ,"PEG-J", "J", "DHT")


### set variables

odi <- NULL
edi <- NULL
cci <- NULL
nei <- NULL
eai <- NULL


### define functions

#get oral diet preference
oral_diet_idx <- function()
{
  print.noquote("What can the patient swallow by mouth?")
  print.noquote("Oral diet order options:")
  for (i in 1:length(oral_diet_options)) {
    print.noquote(paste(i, oral_diet_options[i], sep = " - "))
  }
  inp <- readline(prompt="Enter oral diet order as integer: ")
  return(as.integer(inp))
}

#get enteral diet preference
enteral_diet_idx <- function()
{
  print.noquote("What can the patient be given by tube?")
  print.noquote("Enteral diet order options:")
  for (i in 1:length(enteral_diet_options)) {
    print.noquote(paste(i, enteral_diet_options[i], sep = " - "))
  }
  inp <- NA
  while (sum(grepl("done", inp)) < 1) {
    inp <- append(inp, readline(prompt="Enter enteral diet order as integer (type done when finished): "))
  }
  inp <- inp[!is.na(inp)]
  inp <- inp[c(1:length(inp)-1)]
  return(as.vector(inp))
}

#get active ldas (should not require user input)
active_ldas_idx <- function()
{
  print.noquote("This is for development purposes; a CER rule can check LDAs.")
  print.noquote("Possible LDAs:")
  for (i in 1:length(possible_ldas)) {
    print.noquote(paste(i, possible_ldas[i], sep = " - "))
  }
  inp <- NA
  while (sum(grepl("done", inp)) < 1) {
    inp <- append(inp, readline(prompt="Enter active LDAs as integer (type done when finished): "))
  }
  inp <- inp[!is.na(inp)]
  inp <- inp[c(1:length(inp)-1)]
  return(as.vector(inp))
}

#get clinical condition
clinical_condition_idx <- function()
{
  print.noquote("What is the indication for your diet order?")
  print.noquote("Clinical condition options:")
  for (i in 1:length(clinical_condition_options)) {
    print.noquote(paste(i, clinical_condition_options[i], sep = " - "))
  }
  inp <- readline(prompt="Enter clinical condition as integer: ")
  return(as.integer(inp))
}

#get NPO exceptions
npo_exceptions_idx <- function()
{
  print.noquote("What items can your patient swallow by mouth?")
  print.noquote("Oral diet 'nothing with exceptions' options:")
  for (i in 1:length(oral_diet_exceptions)) {
    print.noquote(paste(i, oral_diet_exceptions[i], sep = " - "))
  }
  inp <- NA
  while (sum(grepl("done", inp)) < 1) {
    inp <- append(inp, readline(prompt="Enter NPO exceptions as integer (type done when finished): "))
  }
  inp <- inp[!is.na(inp)]
  inp <- inp[c(1:length(inp)-1)]
  return(as.vector(inp))
}

#get proximal limit of enteral access
enteral_access_idx <- function()
{
  print.noquote("How far proximally can the patient's enteral tract be used?")
  print.noquote("Enteral access options:")
  for (i in 1:length(enteral_access_options)) {
    print.noquote(paste(i, enteral_access_options[i], sep = " - "))
  }
  inp <- readline(prompt="Enter proximal limit of enteral access as integer: ")
  return(as.integer(inp))
}


### first line diet order to address food type/oropharyngeal utilization preference

odi <- oral_diet_idx()


### second line diet order to address enteral utilization preference

## test for presence of enteral access; offer enteral diet options
## only if patient has enteral access

active_ldas <- possible_ldas[as.integer(active_ldas_idx())]
ldas <- NULL
for (i in 1:length(active_ldas)) {
  ldas <- append(ldas, sum(grepl(active_ldas[i], enteral_ldas)))
}
if (sum(ldas) >= 1) {

  #patient has active enteral access recorded
  edi <- enteral_diet_idx()

} else {

  #patient does not have active enteral access recorded
  print.noquote("Patient does not have active enteral access recorded.")
  print.noquote("There are no enteral diet order preferences.")

}


### third line diet order to assess clinical condition

## test oral diet order; require clinical condition only if
## oral diet is != "regular"

if (odi != 1) {

  #patient has oral diet order different from regular
  cci <- clinical_condition_idx()

} else {

  #patient has regular diet order
  print.noquote("Patient does not need clinical condition to explain regular oral diet order.")

}

## test of clinical condition and oral diet order to clarify that
## most patients made NPO for surgery or procedure can have exceptions

if ( !is.null(cci) ) {
    if (cci == 1 & odi != 4 ) {
      print.noquote("Most patients awaiting surgery should have an oral diet order of nothing with exceptions, with the exceptions being medications and sips with medications.")
    }
}


### fourth line diet order to address exceptions in the case of a "nothing w/ exceptions" value in oral diet order

## test oral diet order; offer exceptions options only if
## oral diet is "nothing with exceptions"

if (odi == 4) {

  #patient has nothing with exceptions oral diet order
  nei <- npo_exceptions_idx()

} else {

  #patient does not have nothing with exceptions oral diet order
  print.noquote("Patient is strictly NPO, without exception.")

}


### fifth line diet order to address proximal extent of safe enteral access

## test enteral diet order; offer access options only if
## enteral diet is not empty or "nothing at all"

if ( !is.null(edi) ) {

  if ( sum( grepl(4, edi) ) > 0 ) {

    #patient has enteral diet set to "nothing at all"
    print.noquote("The patient's enteric tract is not safe to use.")

  } else {

    #patient has an enteral diet order that is not set to "nothing at all"
    eai <- enteral_access_idx()

  }

}


### report diet order

#aggregate options and show final diet order

#if (!is.null(odi)) {

#}
