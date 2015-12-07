require(RMongo)
source("~/Documents/MiscColloidFunctions.R")

mdb <- mongoDbConnect('MarshallDB')
#f <- dbGetDistinct(mdb, "dipole", "InteractionForce", '{"ParticleCount": 5000}')

Sweeps <- c(500000, 1000000, 1500000, 2000000, 2500000, 3000000, 3500000, 4000000, 4500000, 5000000, 5500000)

corrCount <- 20
pptCount <- 5000
corrs <- rep(0, corrCount)
counts <- rep(1, corrCount)

SystemSize <- 1.215185968820742*ptCount*2
maxCorrLength <- 30
corrLength <- seq(0, maxCorrLength, length = corrCount)

for (sw in Sweeps){
  print(sw)
  
  q <- dbGetQueryForKeys(mdb, "dipole",
                         paste(c('{',
                                 '"ParticleCount":' , 5000,
                                 ', "InteractionForce":' , sprintf("%.53f", f[findNearestIndex(1, f)]),
                                 #', "Sweeps": {"$mod": [500000, 0]}',
                                 ', "Sweeps":', sprintf("%.53f", sw),
                                 '}'), collapse = " "),
                         paste(c('{', 
                               '"ParticleCoordinates":', 1, 
                                 ', "ParticleRotationZ":', 1,
                               '}'), collapse = " "),
                         skip = 0,
                         limit = 100000)
  
  
  for(ind in 1:length(q$ParticleCoordinates)){
    
    rotations <- q$ParticleRotationZ[ind]
    rotations <- gsub("\\[", "", rotations)
    rotations <- gsub("\\]", "", rotations)
    rotations <- as.numeric(unlist(strsplit(rotations, split=",")))
    
    coords <- q$ParticleCoordinates[ind]
    coords <- gsub("\\]", "", coords)
    coords <- gsub("\\[", "", coords)
    coords <- as.numeric(unlist(strsplit(coords, split=",")))
    
    ptCount <- length(coords)
    for (i in seq_along(coords)){
      ptCoordinate <- coords[i]
      ptRotation <- rotations[i]
      
      for (j in seq_along(coords)){
        ptNextCoordinate <- coords[getNext(j, ptCount)]
        ptNextRotation <- rotations[getNext(j, ptCount)]
        
        dst <- getDistanceRight(ptCoordinate, ptNextCoordinate, SystemSize)
        
        if(dst > maxCorrLength){
          break
        }
        nearest <- findNearesIndex(dst, corrLength)
        corrs[nearest] = corrs[nearest] + ptRotation*ptNextRotation
        counts[nearest] = counts[nearest] + 1
      }
    }
  }
}
corrs <- corrs/counts