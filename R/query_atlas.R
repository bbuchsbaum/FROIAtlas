
#' create a database connection to FROIAtlas.sqlite db
#' @export
connect_atlas_db <- function(dbpath=NULL) {
  if (is.null(dbpath)) {
    dbpath <- system.file("data/FROIAtlas.sqlite", package = "FROIAtlas")
  } 
  
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = dbpath)
}

#' get all names of fROIs in database
#' @export
roi_names <- function(conn) {
  x <- execute(Select(conn, from="Foci"))
  unique(x$FROI)
}

#' dump atlas to tab separated ascii file
#' @export
dumpAtlas <- function(db) {
  rois <- roi_names(db)
  res <- lapply(rois, function(roi) {
    print(roi)
    foci.left <- get_roi_foci(db, roi, "left")
    foci.right <- get_roi_foci(db, roi, "right")
    
    c1 <- as.matrix(foci.left[,2:4])
    c2 <- as.matrix(foci.right[,2:4])
    
    if (nrow(c1) > 0) {
      o1 <- outliers(as.matrix(c1) + rnorm(length(c1))/10000, .95)$wfinal01 == 0
      foci.left$out95 <- o1
    } else {
      foci.left$out95 <- numeric(0)
    }
    
    if (nrow(c2) > 0) {    
      o2 <- outliers(c2 + rnorm(length(c2))/10000, .95)$wfinal01 == 0
      foci.right$out95 <- o2
    } else {
      foci.right$out95 <- numeric(0)
    }
    
    rbind(foci.left, foci.right)
       
  })
  
  out <- do.call(rbind, res)
}

#' @export
clusterCoords <- function(coords, method=c("pdf", "pam")) {
  method <- match.arg(method)
  if (method=="pdf") {
    pdfCluster(as.matrix(coords))
  } else if (method=="pam") {
    cg <- clusGap(coords, pam,5)
    K <- which.max(cg$Tab[,"gap"])
    pam(coords, K)
  } else {
    stop(paste("illegal method: ", method))  
  }
}

#' @export
changeLabel <- function(conn, oldName, newName) {
  u1 = Update(conn, "Foci", list(FROI=newName), where=Equals("FROI", oldName))
  u2 = Update(conn, "Study", list(FROI=newName), where=Equals("FROI", oldName))
  execute(u1)
  execute(u2)
}


#' @export
tal2mni <- function(coord) {
  MTT <- solve(matrix(c(.9357, .0029, -.0072, -1.0423,
                  -.0065, .9396, -.0726, -1.3940,
                  .0103,  .0752, .8967, 3.6475,
                  0, 0, 0, 1), 4,4, byrow=TRUE))
}

# Function to convert weights
convert_weight <- function(weight) {
  if (grepl("/", weight)) {
    parts <- strsplit(weight, "/")[[1]]
    return(as.numeric(parts[1]) / as.numeric(parts[2]))
  } else {
    return(as.numeric(weight))
  }
}



#' get table of information for supplied region of interest
#' @export
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
  
  # Apply the conversion to the Weight column
  foci$Weight <- sapply(foci$Weight, convert_weight)
  
  
  
  foci
  
  
}

#' get outlying coordinates based on multivariate test from \code{mvoutlier::sign1}
#' @export
outliers <- function(coords, qcrit=.999, plot=TRUE) {
  res <- sign1(coords, qcrit=qcrit)
  if (plot) {
    boxplot(res$x.dist)
  }
  
  res
}

#' @export
check_outliers <- function(conn, roiname, hemi="left") {
  foci <- get_roi_foci(conn, roiname, hemi) 
  coords <- as.matrix(foci[,2:4])
  
  ## add jitter
  coords <- coords + rnorm(length(coords))/100000
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


# coord_density <- function(coords, template) {
#   blurred <- blur_foci(coords, template, kerndim=c(15,15,15), sd=6)
#   idx <- which(blurred != 0)
#   cGrid <- neuroim2::index_to_coord(template, idx)
#   kres <- kepdf(coords, cGrid, bwtype="adaptive")@estimate
#   kres <- kres * 1/max(kres)
#   out <- neuroim2::NeuroVol(kres, space(template), indices=idx)
# }

#' @export
blur_coord <- function(coord, template, kernel, weight=1) {
  ## convert coordinate from MNI space to voxel space
  grid.loc <- neuroim2::coord_to_grid(template, coord)   
  
  ## shift kernel so that it is centered around 'grid.loc'
  voxmat <- floor(neuroim2::voxels(kernel, center_voxel=grid.loc))
  indices <- neuroim2::grid_to_index(template, voxmat)  
  neuroim2:::SparseNeuroVol(kernel@weights * weight, neuroim2::space(template), indices=indices)
}

#' @export
blur_foci <- function(coords, template, kerndim=c(15,15,15), sd=5) {
  kernel <- neuroim2::Kernel(kerndim, neuroim2::spacing(template), dnorm, mean=0, sd=sd)
  res <- lapply(1:nrow(coords), function(i) {
    vol <- blur_coord(as.numeric(coords[i,,drop=FALSE]), template, kernel)    
    vol@data * 1/max(vol)
  })
  
  res <- Reduce("+", res)  
  neuroim2::SparseNeuroVol(res@x, neuroim2::space(template), indices=res@i)
}


boot_foci <- function(coords, N=50, template=NULL, kernel=NULL, centroidWeighted=FALSE, trim=.1) {
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
    
    C <- t(as.matrix(apply(coords[boot.sam,], 2, function(vals) mean(vals, trim=trim))))
    blur_coord(C, template, kernel)
  })
 
  res <- Reduce("+", res)
  R <- range(res)
  ovals <- (res@data - R[1])/diff(R)
  NeuroVol(ovals@x, template, indices=ovals@i)
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