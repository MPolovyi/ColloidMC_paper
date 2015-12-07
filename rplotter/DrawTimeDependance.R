mdb <- mongoDbConnect('MarshallDB')

ptCount <- dbGetDistinct(mdb, "dipole", "ParticleCount")
ptCount <- ptCount[ptCount != 1000]

op <- c()
sw <- c()
used_forces <- c()
intf <- list()

for (pc in ptCount){
  intf[[toString(pc)]] <- c(0)
  force <- c(intf[[toString(pc)]],
                            dbGetDistinct(mdb, "dipole", "InteractionForce", 
                                          paste(c('{"ParticleCount":' , pc, '}'), collapse = " ")))
  
  f <- force[which.min(abs(force - 2))]
  used_forces <- c(used_forces, f)
  sweeps <- dbGetDistinct(mdb, "dipole", "Sweeps", 
                          paste(c('{"ParticleCount":' , pc,
                                  ', "InteractionForce":' , sprintf("%.52f", f),
                                '}'),
                          collapse = " "))
  
  for (s in sweeps){
    
      q <- dbGetQueryForKeys(mdb,
                             "dipole",
                             paste(c('{"ParticleCount":' , pc ,
                                     ', "InteractionForce":' , sprintf("%.52f", f),
                                   ', "Sweeps":' , s, '}'),
                                   collapse = " "), 
                             '{"OrderParameterFromAngle":1}', skip= 0, limit = 10000)
    
      if(length(q$OrderParameterFromAngle) > 0) {
        print(paste(c('{"ParticleCount":' , pc ,
                      ', "InteractionForce":' , sprintf("%.52f", f),
                      ', "Sweeps":' , s),
                    collapse = " "))
          op <- c(op, mean(q$OrderParameterFromAngle))
          sw <- c(sw, s)
    }
  }
}