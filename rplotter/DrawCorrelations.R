require(RMongo)
source("~/Documents/MiscColloidFunctions.R")

try(dyn.unload("/home/mpolovyi/.CLion12/system/cmake/generated/781f67f/781f67f/Release/libDecodeBinary.so"))
dyn.load("/home/mpolovyi/.CLion12/system/cmake/generated/781f67f/781f67f/Release/libDecodeBinary.so")

ptCount <- 500
rho <- 1

collectionName <- "dipole_upd"
mdb <- mongoDbConnect('MarshallDB')
f <- dbGetDistinct(mdb, paste(c(collectionName, "simulation_stats"), collapse = "_"), "InteractionForce",
                   paste(c('{',
                           '"Density":', sprintf("%.56f", rho),
                           ', "ParticleCount":' , sprintf("%.56f", ptCount),
                           '}'), collapse = " "))

Sweeps <- dbGetDistinct(mdb, paste(c(collectionName, "simulation_stats"), collapse = "_"), "Sweeps", 
                        paste(c('{',
                                '"Density":', sprintf("%.56f", rho),
                                ', "ParticleCount":' , sprintf("%.56f", ptCount),
                                '}'), collapse = " "))

Sweeps <- c(4500000, 5000000, 5500000)

corrCount <- 150
corrs <- rep(0, corrCount)
counts <- rep(1, corrCount)

SystemSize <- 0.607593*ptCount/rho
maxCorrLength <- 100
corrLength <- seq(0, maxCorrLength, length = corrCount)

for (sw in Sweeps){
  q <- dbGetQueryForKeys(mdb, "dipole_upd",
                         paste(c('{',
                                 '"ParticleCount":' , ptCount,
                                 ', "InteractionForce":' , sprintf("%.53f", f[findNearestIndex(1, f)]),
                                 #', "Sweeps": {"$mod": [500000, 0]}',
                                 ', "Sweeps":', sprintf("%.53f", sw),
                                 ', "Density":', rho,
                                 '}'), collapse = " "),
                         paste(c('{', 
                               '"ParticleCoordinates":', 1, 
                                 ', "ParticleRotationZ":', 1,
                               '}'), collapse = " "),
                         skip = 0,
                         limit = 100000)
  
  print(sw)
  
  for(ind in 1:5){ #:length(q$ParticleCoordinates)){
    rotations <- rep(0, ptCount)
    .C("Function_UnwrapBinary",
       as.character(q$ParticleRotationZ[ind]),
       as.integer(ptCount),
       as.double(rotations))[[3]] -> rotations
    
    coords <- rep(0, ptCount)
    .C("Function_UnwrapBinary",
       as.character(q$ParticleCoordinates[ind]),
       as.integer(ptCount),
       as.double(coords))[[3]] -> coords
    
    print(ind)
    
    for (i in seq_along(coords)){
      
      ptCoordinate <- coords[i]
      ptRotation <- rotations[i]
      
      j <- i
      corrs[1] <- corrs[1] + ptRotation^2
      counts[1] <- counts[1] + 1
      dst <- 0
      while (dst < maxCorrLength) {
        j <- getNext(j, ptCount)
        ptNextCoordinate <- coords[j]
        ptNextRotation <- rotations[j]
        
        dst <- getDistanceRight(ptCoordinate, ptNextCoordinate, SystemSize)
        
        nearest <- findNearestIndex(dst, corrLength)
        corrs[nearest] = corrs[nearest] + ptRotation*ptNextRotation
        counts[nearest] = counts[nearest] + 1
      }
    }
  }
}
corrs <- corrs/counts
plot(corrLength, corrs, lwd = 2, pch = 1, col = 1)
# legend("topright", legend = c("0.5", "1", "1.5"), col = c(1, 2, 3), pch = c(1, 2, 3), lty = 0, lwd = 2)
