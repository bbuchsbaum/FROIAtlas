
connect_atlas_db <- function(dbpath=NULL) {
  if (is.null(dbpath)) {
    dbpath <- system.file("data/FROIAtlas.sqlite", package = "FROIAtlas")
  } 
  
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = dbpath)
}

roi_names <- function(conn) {
  x <- execute(Select(conn, from="Foci"))
  unique(x$FROI)
}



clusterCoords <- function(coords, method=c("pdf", "clues", "pam")) {
  if (method[1] == "clues") {
    clues(as.matrix(coords), n0=3, strengthMethod="CH")  
  } else if (method[1]=="pdf") {
    pdfCluster(as.matrix(coords))
  } else if (method[1]=="pam") {
    cg <- clusGap(coords, pam,5)
    K <- which.max(cg$Tab[,"gap"])
    pam(coords, K)
  } else {
    stop(paste("illegal method: ", method))  
  }
}





changeLabel <- function(conn, oldName, newName) {
  u1 = Update(conn, "Foci", list(FROI=newName), where=Equals("FROI", oldName))
  u2 = Update(conn, "Study", list(FROI=newName), where=Equals("FROI", oldName))
  execute(u1)
  execute(u2)
}

tal2mni <- function(coord) {
  MTT <- solve(matrix(c(.9357, .0029, -.0072, -1.0423,
                  -.0065, .9396, -.0726, -1.3940,
                  .0103,  .0752, .8967, 3.6475,
                  0, 0, 0, 1), 4,4, byrow=TRUE))
}

get_roi_foci <- function(conn, froi, hemi=NULL) {
  sel <- Select(conn, from="Foci", where=Equals("FROI",froi))
  foci <- execute(sel)
  if (!is.null(hemi)) {
    if (toupper(hemi) == "LEFT") {
      keep <- foci[,2]  <= 0
      foci <- foci[keep,]
    } else if (toupper(hemi) == "RIGHT") {
      keep <- foci[,2]  > 0
      foci <- foci[keep,]
    } else if (toupper(hemi) == "BOTH") {
      foci
    } else {
      stop(paste("illegal value for hemi argument:", hemi))
    }
  }
  
  foci
  
  
}

outliers <- function(coords, qcrit=.999, plot=TRUE) {
  res <- sign1(coords, qcrit=qcrit)
  if (plot) {
    boxplot(res$x.dist)
  }
  
  res
}

check_outliers <- function(conn, roiname, hemi="left") {
  foci <- get_roi_foci(conn, roiname, hemi) 
  coords <- foci[,2:4]
  
  cvals <- c(.95, .99, .999, .9999)
  outmat <- do.call(cbind, lapply(cvals, function(crit) outliers(coords, crit, plot=FALSE)$wfinal01))
  outscore <- apply(outmat,1, function(vals) {
    o <- which(vals == 0)
    if (length(o) > 0) {
      cvals[o[length(o)]]
    } else {
      0
    }
  })
  
  foci$dscore <- outliers(coords, .95, plot=FALSE)$x.dist  
  foci$prob <- outscore
  foci
  
}

blur_coord <- function(coord, template, kernel, weight=1) {
  ## convert coordinate from MNI space to voxel space
  grid.loc <- coordToGrid(template, coord)   
  
  ## shift kernel so that it is centered around 'grid.loc'
  voxmat <- floor(voxels(kernel, centerVoxel=grid.loc))
  indices <- gridToIndex(template, voxmat)  
  neuroim:::SparseBrainVolume(kernel@weights * weight, template, indices=indices)
}

blur_foci <- function(coords, template, kerndim=c(15,15,15)) {
  kernel <- Kernel(kerndim, spacing(template), dnorm, mean=0, sd=5)
  centroid <- apply(coords, 2, function(vals) median(vals))
  Dcent <- apply(coords, 1, function(coord) {
    sqrt(sum((coord - centroid)^2))
  })
  
  res <- mclapply(1:nrow(coords), function(i) {
    blur_coord(coords[i,,drop=FALSE], kernel)    
  })
  
  res <- Reduce("+", res)  
}

boot_foci <- function(coords, N=50, template=NULL, kernel=NULL, centroidWeighted=FALSE) {
  if (is.null(template)) {
    template = readRDS(system.file("data/MNI_SPACE.RDS", package="FROIAtlas"))
  }
  if (is.null(kernel)) {
    kernel = Kernel(c(15,15,15), spacing(template), dnorm, mean=0, sd=5)
  }
  
  if (ncol(coords) != 3) {
    stop("coords must be matrix with 3 columns (X, Y, Z)")
  }
  
    
  centroid <- apply(coords, 2, function(vals) median(vals))
  Dcent <- apply(coords, 1, function(coord) {
    sqrt(sum((coord - centroid)^2))
  })
  
  Dweights <- 1/(Dcent)
  Dweights <- Dweights/sum(Dweights)
  res <- lapply(1:N, function(i) {
    print(i)
    boot.sam <- if (centroidWeighted) {
      sample(1:nrow(coords), replace=TRUE, prob=Dweights)
    } else {
      sample(1:nrow(coords), replace=TRUE)
    }
    
    C <- t(as.matrix(apply(coords[boot.sam,], 2, function(vals) mean(vals))))
    blur_coord(C, template, kernel)
  })
 
  res <- Reduce("+", res)
  R <- range(res)
  ovals <- (res@data - R[1])/diff(R)
  BrainVolume(as.vector(ovals), space(template))
}


#.fixHemi <- function(conn) {
#  dbBeginTransaction(conn)
#  foci <- dbReadTable(conn, "Foci")
#  up1 <- Update(conn, "Foci", list(Hemisphere="Left"), where=Equals("Hemisphere", "L"))
#  up2 <- Update(conn, "Foci", list(Hemisphere="Right"), where=Equals("Hemisphere", "R"))
#  execute(up1)
#  execute(up2)
#  dbCommit(conn)
#}

#.fillROI <- function(conn) {
#  dbBeginTransaction(conn)
#  study <- dbReadTable(conn, "Study")
#  for (i in 1:nrow(study)) {
#    print(i)
#    id <- study$PMID[i]
#    sel <- Select(conn, from="Foci", where=Equals("PMID",id))
#    res <- as.list(execute(sel))
#    uset <- list("FROI"=study$FROI[i])
    
#    up <- Update(conn, "Foci", uset, where=Equals("PMID", id))
#    print(up)
#    execute(up)
#  }
#  dbCommit(conn) 
#}