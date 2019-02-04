#####
##### Proof of concept code for proposed diet order
#####


### goals of this workflow

  # every inpatient gets oral diet order option
  # only patients with oral diet order != regular get clinical condition
  # only patients with oral diet == "nothing w/ exceptions" get exceptions option
  # only patients with enteral access get enteral diet order option
  # only patients with enteral diet order option !=NA and !="nothing at all" get proximal limit option


### set options menus

oral_diet_options <- c("regular", "therapeutic", "nothing at all", "nothing with exceptions")
clinical_condition_options <- c("awaiting surgery/procedure", "aspiration risk/awaiting speech eval", "dysphagia", "ileus/obstruction", "ischemia/inflammation", "other")
npo_exceptions_options <- c("meds", "sips with meds", "ice chips", "gum")
enteral_diet_options <- c("tube feeds", "meds", "flushes", "nothing at all")
enteral_access_options <- c("gastric", "post-pyloric", "jejunal")


### set EMR data

possible_ldas <- c("NGT", "OGT", "PEG" ,"PEG-J", "J", "DHT", "CVL", "art", "PIV", "ETT")
enteral_ldas <- c("NGT", "OGT", "PEG" ,"PEG-J", "J", "DHT", "NJT")
jejunal_ldas <- c("PEG-J", "J", "NJT")
gastric_ldas <- c("NGT", "OGT", "PEG", "DHT")


### set variables

odi <- NULL
cci <- NULL
nei <- NULL
edi <- NULL
eai <- NULL


### define functions

#get active ldas (should not require user input)
active_ldas_idx <- function()
{
  print.noquote("This is for development purposes; a CER rule can check LDAs.")
  print.noquote("Possible LDAs:")
  for (i in 1:length(possible_ldas)) {
    print.noquote(paste(i, possible_ldas[i], sep = " - "))
  }
  inp <- NA
  while (sum(grepl(0, inp)) < 1) {
    new <- readline(prompt="Enter active LDAs as integer (type 0 when done): ")
    new <- as.integer(new)
    if (is.na(new)) {
      print.noquote("*** Entry must be INTEGER ***")
      new <- active_ldas_idx()
    }
    inp <- append(inp, new)
  }
  inp <- inp[!is.na(inp)]
  inp <- inp[c(1:length(inp)-1)]
  return(as.vector(inp))
}

#get oral diet preference (every inpatient)
oral_diet_idx <- function()
{
  print.noquote("What can the patient be given by mouth?")
  print.noquote("Oral diet order options:")
  for (i in 1:length(oral_diet_options)) {
    print.noquote(paste(i, oral_diet_options[i], sep = " - "))
  }
  inp <- readline(prompt="Enter oral diet order as integer: ")
  inp <- as.integer(inp)
  if (is.na(inp)) {
    print.noquote("*** Entry must be INTEGER ***")
    inp <- oral_diet_idx()
  }
  return(inp)
}

#get clinical condition (if oral diet not regular)
clinical_condition_idx <- function()
{
  print.noquote("What is the indication for your diet order?")
  print.noquote("Clinical condition options:")
  for (i in 1:length(clinical_condition_options)) {
    print.noquote(paste(i, clinical_condition_options[i], sep = " - "))
  }
  inp <- readline(prompt="Enter clinical condition as integer: ")
  inp <- as.integer(inp)
  if (is.na(inp)) {
    print.noquote("*** Entry must be INTEGER ***")
    inp <- clinical_condition_idx()
  }
  return(inp)
}

#get NPO exceptions (if oral diet == npo with exceptions)
npo_exceptions_idx <- function()
{
  print.noquote("Patient NPO with exceptions. What can the patient be given by mouth?")
  print.noquote("Oral diet 'nothing with exceptions' options:")
  for (i in 1:length(npo_exceptions_options)) {
    print.noquote(paste(i, npo_exceptions_options[i], sep = " - "))
  }
  inp <- NA
  while (sum(grepl(0, inp)) < 1) {
    new <- readline(prompt="Enter NPO exceptions as integer (type 0 when done): ")
    new <- as.integer(new)
    if (is.na(new)) {
      print.noquote("*** Entry must be INTEGER ***")
      new <- npo_exceptions_idx()
    }
    inp <- append(inp, new)
  }
  inp <- inp[!is.na(inp)]
  inp <- inp[c(1:length(inp)-1)]
  return(as.vector(inp))
}

#test for enteral ldas
enteral_ldas_idx <- function()
{
  active_ldas <- possible_ldas[as.integer(active_ldas_idx())]
  eli <- NULL
  for (i in 1:length(active_ldas)) {
    eli <- append(eli, which(enteral_ldas == active_ldas[i]))   #which for exact match
  }
  return(eli)
}

#get enteral diet preference (if patient has enteral lda)
enteral_diet_idx <- function()
{
  print.noquote("What can the patient be given by tube?")
  print.noquote("Enteral diet order options:")
  for (i in 1:length(enteral_diet_options)) {
    print.noquote(paste(i, enteral_diet_options[i], sep = " - "))
  }
  inp <- NA
  while (sum(grepl(0, inp)) < 1) {
    new <- readline(prompt="Enter enteral diet order as integer (type 0 when done): ")
    new <- as.integer(new)
    if (is.na(new)) {
      print.noquote("*** Entry must be INTEGER ***")
      new <- enteral_diet_idx()
    }
    inp <- append(inp, new)
  }
  inp <- inp[!is.na(inp)]
  inp <- inp[c(1:length(inp)-1)]
  return(as.vector(inp))
}

#get proximal limit of enteral access (if patient has enteral lda) {eventually this should be limited by enteral LDAs}
enteral_access_idx <- function()
{
  jlda <- grep("J", enteral_ldas[eldas])
  glda <- NULL
  for (i in 1:length(gastric_ldas)) {
    glda <- append(glda, grep(gastric_ldas[i], enteral_ldas[eldas]))    #grep to catch PEG and PEG-J
  }
  if ( length(glda) > 0 ) {
    if ( length(jlda) > 0 ) {
      print.noquote("Patient has tubes at multiple enteric levels (e.g. gastric, jejunal).")
      print.noquote("How far proximally can the patient's enteral tract be used?")
      print.noquote("Enteral access options:")
      for (i in 1:length(enteral_access_options)) {
        print.noquote(paste(i, enteral_access_options[i], sep = " - "))
      }
      inp <- readline(prompt="Enter proximal limit of enteral access as integer: ")
      inp <- as.integer(inp)
      if (is.na(inp)) {
        print.noquote("*** Entry must be INTEGER ***")
        inp <- enteral_access_idx()
      }
    } else {
      print.noquote("Patient has a gastric tube.")
      inp <- 1
    }
  } else if ( length(jlda) >0 ) {
    print.noquote("Patient has a jejunal tube.")
    inp <- 3
  } else {
    inp <- NULL
  }
  inp <- inp[!is.na(inp)]
  return(inp)
}

#get aggregate diet order
sum_diet <- function()
{
  sum_diet <- append("Oral diet:", oral_diet_options[odi])
  if (!is.null(cci)) {
    cc <- append("Clinical condition:", clinical_condition_options[cci])
    if (!is.null(nei)) {
      ne <- append("NPO exceptions:", npo_exceptions_options[nei])
      if (!is.null(edi)) {
        ed <- append("Enteral diet:", enteral_diet_options[edi])
        if ( length(eai) > 0 ) {
          ea <- append("Enteral access:", enteral_access_options[eai])
          sum_diet <- append(sum_diet, c(cc, ne, ed, ea))
        } else {
          sum_diet <- append(sum_diet, c(cc, ne, ed))
        }
      } else {
        sum_diet <- append(sum_diet, c(cc, ne))
      }
    } else if (!is.null(edi)) {
      ed <- append("Enteral diet:", enteral_diet_options[edi])
      if ( length(eai) >0 ) {
        ea <- append("Enteral access:", enteral_access_options[eai])
        sum_diet <- append(sum_diet, c(cc, ed, ea))
      } else {
        sum_diet <- append(sum_diet, c(cc, ed))
      }
    } else {
      sum_diet <- append(sum_diet, cc)
    }
  } else if (!is.null(edi)) {
    ed <- append("Enteral diet:", enteral_diet_options[edi])
    if ( length(eai) > 0 ) {
      ea <- append("Enteral access:", enteral_access_options[eai])
      sum_diet <- append(sum_diet, c(ed, ea))
    } else {
      sum_diet <- append(sum_diet, ed)
    }
  } else {
    sum_diet <- sum_diet
  }
  return(sum_diet)
}


### first line diet order to address food type/oropharyngeal utilization preference

odi <- oral_diet_idx()

if (is.null(odi) | odi == 0) {
  print.noquote("The patient must have an oral diet order. Please enter oral diet order as integer.")
  odi <- oral_diet_idx()
}


### second line diet order to assess clinical condition

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


### third line diet order to address exceptions in the case of a "nothing w/ exceptions" value in oral diet order

## test oral diet order; offer exceptions options only if
## oral diet is "nothing with exceptions"

if (odi == 4) {

  #patient has nothing with exceptions oral diet order
  nei <- npo_exceptions_idx()

} else {

  #patient does not have nothing with exceptions oral diet order
  print.noquote("No need to choose NPO exceptions.")

}


### fourth line diet order to address enteral utilization preference

## test for presence of enteral access; offer enteral diet options
## only if patient has enteral access

eldas <- enteral_ldas_idx()

if (length(eldas) > 0) {

  #patient has active enteral access recorded
  edi <- enteral_diet_idx()

} else {

  #patient does not have active enteral access recorded
  print.noquote("Patient does not have active enteral access recorded.")
  print.noquote("There are no enteral diet order preferences.")

}


### fifth line diet order to address proximal extent of safe enteral access

## test enteral diet order; offer access options only if
## enteral diet is not empty or "nothing at all"

if ( !is.null(edi) ) {

  if ( sum( grepl(4, edi) ) > 0 ) {

    #patient has enteral diet set to "nothing at all"
    print.noquote("The patient's tube should not be used to administer meds/feeds.")

  } else {

    #patient has an enteral diet order that is not set to "nothing at all"
    eai <- enteral_access_idx()

  }

}


### report aggregate diet order

sum_diet <- sum_diet()
print.noquote("Final diet order:")
print.noquote(sum_diet)
