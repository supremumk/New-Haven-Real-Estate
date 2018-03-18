#### This is revised version of data scrape for NH real estate data.
#### Meng Kuang. Sep,19,2016
#### For debug: try single html x <- scan( file= "newdata2016/1.html", what="", sep="\n" )


##### Create a "full" dataframe
pid <- 1:27307
variables <- data.frame(matrix(NA, 27307, 40))
myresult <- data.frame(pid, variables, stringsAsFactors=FALSE)
ownerhistory <- paste(rep(c("buyer",  "date", "price"), 5), rep(1:5, each=3), sep="")
names(myresult) <- c("pid", "multibuilding", "location", "totval", "address", 
                     ownerhistory, "yearbuilt", "sqft", "replcost", 
                     "pctgood", "style", "model", "grade", "occupancy", "actype", 
                     "bedrooms", "nineplus", "bathrooms", "halfbaths", "bathstyle", 
                     "kstyle", "exval", "acres", "zone", "neighborhood", "landval", 
                     "garagesqft")
getdata <- function (x) {
  # multiple building indicator
  build_line <- grep("MainContent_ctl.*_lblHeading", x) 
  build_indicator <- length(build_line) > 1
  
  # location
  location_line <- grep("MainContent_lblLocation\"", x) 
  location <- x[location_line]
  location <- gsub("<[^<>]*>|^\\s+", "", location)
  
  # totval
  totval_line <- grep ("MainContent_lblGenAppraisal\"", x)
  totval <- x[totval_line]
  totval <- as.numeric(gsub("\\D", "", totval))
  
  # owner add
  add <- x[grep("MainContent_lblAddr1", x)]
  add <- gsub("<br> <br>", ", ", add) 
  add <- gsub("<br>", ", ", add)
  add <- gsub("<[^<>]*>", "", add)
  add <- gsub("\t\t\tAddress", "", add)
  #add <- stringr::str_trim(add, side = "both") don't use package!
  add <- gsub("^\\s+|\\s+$", "", add)
  # owner history
  start <- grep("Ownership History", x)[1] + 1
  temp <- grep("</table>", x)
  end <- min(temp[temp > start])
  temp <- grep("$", x[start:end], fixed=TRUE, value=TRUE) 
  temp <- strsplit(temp, "</td>", fixed=TRUE)  
  colnum <- length(temp[[1]])
  temp <- matrix(unlist(temp), ncol=colnum, byrow=TRUE) 
  temp <- gsub("<[^<>]*>|\t|\\$|,", "", temp)[1:min(5,nrow(temp)), c(1,colnum,2)]
  tempmat <- matrix(temp, ncol=3,byrow=TRUE)
  temp <- c(as.vector(t(temp)), rep(NA, (5-nrow(tempmat))*3))
  z <- temp    
  
  # year built
  year_line <- grep("MainContent_ctl01_lblYearBuilt", x)
  year <- x[year_line]
  year <- gsub("<[^<>]*>","",year)
  year <- gsub("^\\s+", "", year)
  year <- as.numeric(year)
  
  # get sqft from x
  sqft_line <- grep("MainContent_ctl01_lblBldArea", x)
  sqft <- x[sqft_line]
  sqft <- gsub("<[^<>]*>", "", sqft)
  sqft <- gsub("\\D", "", sqft)
  sqft <- as.numeric(sqft)
  
  #get replcost from x
  repl_line <- grep("MainContent_ctl01_lblRcn", x)
  repl_line <- repl_line[1]
  repl <- x[repl_line]
  repl <- gsub("<[^<>]*>", "", repl)
  repl <- gsub("\\D", "", repl)
  repl <- as.numeric(repl)
  
  #get pctgood from x
  pct_line <- grep("MainContent_ctl01_lblPctGood", x)
  pct <- x[pct_line]
  pct <- gsub("<[^<>]*>", "", pct)
  pct <- gsub("\\D", "", pct)
  pct <- as.numeric(pct)
  
  #get style from x
  sty <- x[grep("Building Attributes", x) + 4]
  sty <- strsplit(sty, "</td>")[[1]][2]
  sty <- gsub("<[^<>]*>", "", sty)
  #sty <- stringr::str_trim(sty, side = "both")
  sty <- gsub("^\\s+|\\s+$", "", sty)
  if (sty=="") { sty <- NA }
  
  #get model from x
  mod_line <- grep("Building Attributes", x) + 6
  mod <- x[mod_line]
  mod <- strsplit(mod, "</td>")[[1]][2]
  mod <- gsub("<[^<>]*>", "", mod)
  #mod <- stringr::str_trim(mod, side = "both")
  mod <- gsub("^\\s+|\\s+$", "", mod)
  if (mod==""){mod <- NA}
  
  #get grade from x
  grade_line <- c(grep("Grade", x), grep("GRADE", x))
  if (length(grade_line)==0) {
    grade <- NA
  } else {
    grade <- x[grade_line]
    grade <- strsplit(grade, "</td>")[[1]][2]
    grade <- gsub("<[^<>]*>", "", grade)
    #grade <- stringr::str_trim(grade, side = "both")
    grade <- gsub("^\\s+|\\s+$", "", grade)
    if (grade==""){grade <- NA}
  }
  grade <- gsub("GRADE", "", grade)
  grade <- gsub("_", "", grade)
  
  #get occupancy from x
  occu_line <- grep("Building Attributes", x)
  occu_line <- occu_line
  occu_line <- occu_line+12
  occu <- x[occu_line]
  occu <- strsplit(occu, "</td>")
  occu <- occu[[1]][2]
  occu <- gsub("<[^<>]*>", "", occu)
  occu <- as.numeric(occu)
  
  # ac type
  ac_line <- grep("AC Type", x)
  if (length(ac_line)==0){
    ac <- NA
  } else {
    ac_line <- (ac_line)[1]
    ac <- x[ac_line]
    ac <- strsplit(ac, "</td>")[[1]][2]
    ac <- gsub("<[^<>]*>", "", ac)
    # ac <- stringr::str_trim(ac, side = "both") 
    ac <- gsub("^\\s+|\\s+$", "", ac)
    if (ac==""){ac <- NA}
  }
  
  # bedrooms
  bed_line <- c(grep("Ttl Bedrms", x), grep("Total Bedrooms", x), 
                grep("Total Bedrms",x))
  if (length(bed_line)==0){
    bed <- NA
  } else {
    bed_line <- min(bed_line)
    bed <- x[bed_line]
    bed <- gsub("<[^<>]*>","",bed) # building 1
  }
  bed_line <- (bed_line)[1]
  bed <- x[bed_line]
  bed <- gsub("<[^<>]*>","",bed) # building 1
  b <- length(grep("9+", bed)) >= 1
  bed <- as.numeric(gsub("\\D", "", bed))
  
  # bathrooms
  bath_line <- c(grep("Ttl Bathrms", x),grep("Total Bthrms", x),grep("Total Baths",x))
  if (length(bath_line)==0) {
    bath <- NA
  } else {
    bath_line <- min(bath_line)
    bath <- x[bath_line]
    bath <- strsplit(bath, "</td>")
    bath <- bath[[1]][2]
    bath <- gsub("<[^<>]*>", "", bath)
    if (bath=="1/2") {
      bath <- 0.5 # 1/2 (e.g.7426)
    }else{
      bath <- gsub("[a-z]", "", bath)
      bath <- as.numeric(gsub("[A-Z]", "", bath)) # "2 Full" (e.g.228) "4.5" (e.g.5207)
    }
  }
  
  # halfbath
  half_line <- c(grep("Ttl Half Bths", x),grep("Total Half Baths", x))
  if (length(half_line)==0) {
    half <- NA
  } else {
    half_line <- (half_line)[1]
    half <- x[half_line]
    half <- as.numeric(gsub("\\D", "", half))
  }
  
  # bathstyle
  bstyle_line <- grep("Bath Style", x)
  if (length(bstyle_line)==0){
    bstyle <- NA
  } else {
    bstyle_line <- (bstyle_line)[1]
    bstyle <- x[bstyle_line]
    bstyle <- gsub("<[^<>]*>", "", bstyle)
    bstyle <- gsub("\t\t\t\t\tBath Style:", "", bstyle)
    # bstyle <- stringr::str_trim(bstyle, side = "both") 
    bstyle <- gsub("^\\s+|\\s+$", "", bstyle)
    if (bstyle==""){bstyle <- NA}
  }
  
  # kstyle
  kstyle_line <- grep("Kitchen Style", x)
  if (length(kstyle_line)==0){
    kstyle <- NA
  } else {
    kstyle_line <- (kstyle_line)[1]
    kstyle <- x[kstyle_line]
    kstyle <- gsub("<[^<>]*>", "", kstyle)
    kstyle <- gsub("\t\t\t\t\tKitchen Style:", "", kstyle)
    #kstyle <- stringr::str_trim(kstyle, side = "both") 
    kstyle <- gsub("^\\s+|\\s+$", "", kstyle)
    if (kstyle==""){kstyle <- NA}
  }
  
  # extra value
  exval <- c()
  exval_line_start <- grep("tabs-5", x)
  exval_line_end <- grep("tabs-6", x)
  if (exval_line_end - exval_line_start == 20){
    exval <- NA
  }else{
    number <- (exval_line_end - exval_line_start - 17)/2
    for(e in 1:number){
      t <- x[exval_line_start+11+2*e]
      t <- strsplit(t, "</td>")
      t <- t[[1]][4]
      t <- gsub("\\D", "", t) 
      t <- as.numeric(t)
      exval <- c(exval, t)
      exval <- sum(exval, na.rm=TRUE)
    }
  }
  
  
  # land size
  line_acre <- grep("MainContent_lblLndAcres",x)
  acre <- x[line_acre]
  acre <- stringr::str_trim(acre, side = "both")
  acre <- gsub("<[^<>]*>","",acre)
  acre <- gsub("[^.0-9]","",acre) # only keep the figures!
  
  # zone
  line_zone <- grep("MainContent_lblZone", x)
  zone <- x[line_zone]
  zone <- stringr::str_trim(zone, side = "both")
  zone <- gsub("Zone","",zone)
  zone <- gsub("<[^<>]*>","",zone)
  if (zone==""){zone <- NA}
  
  # neighborhood
  line_nb <- grep("MainContent_lblNbhd", x)
  nb <- x[line_nb]
  nb <- stringr::str_trim(nb, side = "both")
  nb <- gsub("Neighborhood","",nb)
  nb <- gsub("<[^<>]*>","",nb)
  if (nb==""){nb <- NA}
  
  # landval
  val_line <- grep("MainContent_lblLndAppr",x)
  val <- x[val_line]
  val <- stringr::str_trim(val, side = "both")
  val <- gsub("[^.0-9]","",val) 
  
  # garage
  # http://apps.coj.net/pao_propertySearch/Codes.aspx?Code=SAT
  # 4 types of garage code: FDG FGR UDG UGR # 27012, 15194
  Gar_line <- c(grep("<td>FDG</td>", x),grep("<td>FGR</td>", x), 
                grep("<td>UDG</td>",x), grep("<td>UGR</td>",x))
  if (length(Gar_line)>0){
    garage1 <- x[Gar_line+1]
    garage1 <- gsub("\\D", "", garage1)
    garage1 <- as.numeric(garage1)
    garage1 <- sum(garage1)
  } else {
    garage1 <- NA
  }
  GAR_line <- c(grep("<td>FDG1</td>", x),grep("<td>FGR1</td>", x), 
                grep("<td>UDG1</td>",x), grep("<td>UGR1</td>",x))
  if (length(GAR_line)>0){
    garage2 <- x[GAR_line]
    garage2 <- strsplit(garage2, "</td>")
    garage2 <- t(as.data.frame(garage2))
    garage2 <- garage2[,5]
    garage2 <- sum(as.numeric(gsub("\\D", "", garage2)))
  } else {
    garage2 <- NA
  }
  if (is.na(garage1) & is.na(garage2)){
    garage <- NA
  } else {
    garage <- sum(garage1, garage2, na.rm=TRUE)
  }
  
  return (list(build_indicator, location, totval, add, z[1], z[2], z[3], z[4],
               z[5], z[6], z[7], z[8], z[9], z[10], z[11], z[12], z[13], z[14], 
               z[15], year, sqft, repl, pct, sty, mod, grade, occu, ac, bed, 
               b, bath, half, bstyle, kstyle, exval, acre, zone, nb, val, garage))
}

bad <- dget(file ="bad.txt")

for ( i in (1:27307)){
  if ( !(i %in% bad) ){
    # Set file path
    filename <- try(file.path('newdata2016', paste(i, '.html', sep="")))
    cat(i)
    x <- try(scan( file = filename, what="", sep="\n" ))
    data <- try(getdata(x))
    for ( j in (1:40)){
      try(myresult[i, j+1] <- data[[j]])
    }
  }
}

for (j in 2:41) {
  myresult[,j] <- gsub("&nbsp;", " ", myresult[,j])
  myresult[,j] <- gsub("&lt;", "<", myresult[,j])
  myresult[,j] <- gsub("&gt;", ">", myresult[,j])
  myresult[,j] <- gsub("&amp;", "&", myresult[,j])
  myresult[,j] <- gsub("&#39;", "'", myresult[,j])
  myresult[,j][which(myresult[,j]==" " | is.na(myresult[,j]))] <- NA
  }
write.csv(myresult, file = "625_gl395_xy224_mk2297.csv", row.names=FALSE)
