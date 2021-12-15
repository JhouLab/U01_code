#
# Reads Pedigree docs from Wake Forest
#
# Generates allPedigree table and allPedigree.csv file.
# 
# This table/file lists all parent/child relationships from
# the Wake Forest breeders. Each row has a breeder id, mother, father, child.
# (There is a final JhouID column that is not currently used)
#
# These do NOT include the actual shipped animals - those are in the allShip table and file.
#
# Someday those two could probably be merged into a single table/file, but since
# they come from different sources, and are updated at different times, I have not
# bothered.
#

library(readxl)
library(stringr) # Need this for str_extract
# library(data.table) # 

# Find Dropbox folder
computerName = Sys.info()["nodename"]


PedigreeFolder = paste0(".")  # /JhouLab's shared workspace/U01 folder/Shipping documents/Pedigree")

pedigreeFiles = list.files(PedigreeFolder, full.names = FALSE, recursive = FALSE)

# Create empty array to hold file names
fileList = data.frame(matrix(ncol=2,nrow=0))
# Create headers
colnames(fileList) = c("GenerationNum", "FilePath") # NULL

# Build list of file names to read
for (f in pedigreeFiles){
  if (!startsWith(f, "Generation")) { next }
  
  fullpath = paste0(PedigreeFolder, "/", f)

  # Extract case number from filename
  generationNum = str_extract(f, "(?<=Gen )\\d+") # Match digits after word "Gen "

  # Make table with two columns, case # and file path
  fileList = rbind(fileList, c(as.integer(generationNum), fullpath))
}

colnames(fileList) = c("GenerationNum", "FilePath") # NULL

# Sort by generation number
fileList = fileList[order(nchar(fileList$GenerationNum),fileList$GenerationNum),]

allPedigree = data.frame(matrix(ncol=3,nrow=0))
colnames(allPedigree) = c("Dame", "Sire", "Child")

# Read each pedigree document
for (index in 1:nrow(fileList)){
  f = fileList[index,]
  sheetnames = excel_sheets(path = f$FilePath)
  sheets = sheetnames[startsWith(toupper(sheetnames), "BAC")]
  
  for (x in length(sheets)) {
  
    sheet1 = sheets[[x]]
    # Number of spreadsheet rows to skip
    skip = 1
    if (f$GenerationNum == 81) {
      skip = 2
    } else if (f$GenerationNum == 86) {
      if (x == 2)
        skip = 0
    } else if (f$GenerationNum == 87) {
      skip = 0
    }
    pedInfo = read_excel(f$FilePath, sheet = sheet1, skip = skip)
  
    cols = colnames(pedInfo)
    # Get key column names
    col_pair = cols[startsWith(toupper(cols), "PAR")][1]
    col_sire =cols[startsWith(toupper(cols), "SIR")][1]
    col_dam =cols[startsWith(toupper(cols), "DAM")][1]
    col_child =cols[startsWith(toupper(cols), "ACCESS")][1]
    
    children = pedInfo[,col_child]
  
    for (row in 1:nrow(children)) {
      pedRow = pedInfo[row,]
      allPedigree = rbind(allPedigree, data.frame(PairID=pedRow[col_pair][[1]], Dame=pedRow[col_dam][[1]], Sire=pedRow[col_sire][[1]], Child=pedRow[col_child][[1]]))
    }  
  }
}

cat(paste0("Finished reading ", nrow(fileList), " pedigree files. Now adding JhouID (this currently doesn't do anything)\n"))

allPedigree$JhouID = NA

#
# This is in both GWAS2a_make_grm.R and GWAS1_read_shipping_docs.R Consider putting in common source file?
#
# Obtain Case ID that was read by GWAS1_read_all_behaviors.R, which uses Excel Master sheet.
GetCaseIDfromRFID = function(rfid) {
  index = match(rfid, allDataBySubject$RFID)
  allDataRow = allDataBySubject[index,]
  return (as.numeric(sub(".","", allDataRow$CaseID)))
}

# Add Jhou lab ID (since breeders were not shipped, this won't pick up any IDs)
# Someday when we add the shipped animals to this list, this may beccome useful
for (index in 1:nrow(allPedigree)) {

  allPedRow = allPedigree[index,]
  
  child = allPedRow$Child
  if (child %in% allShip$`Access ID`) {
    RFID = allShipRow[allShipRow$`Access ID` == child,]$`Transponder ID`
    ExcelID = GetCaseIDfromRFID(RFID)
    allShip[index,]$JhouID = ExcelID
  }
}

write.csv(allPedigree, "allPedigree.csv", row.names = FALSE)
cat("Wrote file 'allPedigree.csv'.\n")
