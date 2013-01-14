
connect_atlas_db <- function(dbpath=NULL) {
  if (is.null(dbpath)) {
    dbpath <- system.file("data/FROIAtlas.sqlite", package = "FROIAtlas")
  } 
  
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = dbpath)
}

get_roi_foci <- function(conn, froi) {
  sel <- Select(conn, from="Foci", where=Equals("FROI",froi))
  res <- execute(sel)
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