
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

get_roi_foci <- function(conn, froi) {
  sel <- Select(conn, from="Foci", where=Equals("FROI",froi))
  res <- execute(sel)
}

blur_coord <- function(coord, template, kernel, weight=1) {
  ## convert coordinate from MNI space to voxel space
  grid.loc <- coordToGrid(template, coord)   
  
  ## shift kernel so that it is centered around 'grid.loc'
  voxmat <- floor(voxels(kernel, centerVoxel=grid.loc))
  indices <- gridToIndex(space(template), voxmat)  
  neuroim:::SparseBrainVolume(kernel@weights * weight, space(template), indices=indices)
}

blur_foci <- function(coords, template) {
  kernel <- Kernel(c(15,15,15), spacing(template), dnorm, mean=0, sd=5)
  centroid <- apply(coords, 2, function(vals) median(vals))
  Dcent <- apply(coords, 1, function(coord) {
    sqrt(sum((coord - centroid)^2))
  })
  
  res <- mclapply(1:nrow(coords), function(i) {
    blur_coord(coords[i,,drop=FALSE], kernel)    
  })
  
  res <- Reduce("+", res)  
}

boot_foci <- function(coords, N=50, template=NULL, kernel=NULL) {
  if (is.null(template)) {
    template = loadVolume("data/MNI_152_1mm.nii")
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
  res <- mclapply(1:N, function(i) {
    print(i)
    boot.sam <- sample(1:nrow(coords), replace=TRUE, prob=Dweights)
    C <- t(as.matrix(apply(coords[boot.sam,], 2, function(vals) mean(vals))))
    print(C)
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