
setGeneric("%and%",
		function(x,y)
			standardGeneric("%and%")
)

setGeneric("%or%",
		function(x,y)
			standardGeneric("%or%")
)

setGeneric("execute",
		function(x,...)
			standardGeneric("execute")
)


					  

setClass("WhereExp", contains="VIRTUAL")
setClass("ArithmeticExp",representation=representation(column="character"), contains="WhereExp")
setClass("Equals", contains="ArithmeticExp")
setClass("EqualsString",representation=representation(value="character"), contains="Equals")
setClass("EqualsInt",representation=representation(value="integer"), contains="Equals")

setClass("NotEquals", contains="ArithmeticExp")
setClass("NotEqualsString",representation=representation(value="character"), contains="NotEquals")
setClass("NotEqualsInt",representation=representation(value="integer"), contains="NotEquals")

setClass("Like", representation=representation(value="character"), contains="ArithmeticExp")

setClass("LessThan", representation=representation(value="numeric"), contains="ArithmeticExp")
setClass("GreaterThan", representation=representation(value="numeric"), contains="ArithmeticExp")

setClass("And", representation=representation(x="WhereExp", y="WhereExp"), contains="WhereExp")
setClass("Or", representation=representation(x="WhereExp", y="WhereExp"), contains="WhereExp")
setClass("Empty", contains="WhereExp")
setClass("Group", contains="WhereExp", representation=representation(x="WhereExp"))


Empty <- new("Empty")
	
Group <- function(exp) {
	new("Group", x=exp)		
}
LessThan <- function(column, value) {
	new("LessThan", column=column, value=value)			
}

GreaterThan <- function(column, value) {
	new("GreaterThan", column=column, value=value)			
}

Like <- function(column, value) {
	new("Like", column=column, value=value)			
}

NotEquals <- function(column, value) {
	if (is.numeric(value)) {
		new("NotEqualsInt", column=column, value=as.integer(value))
	} else if (is.character(value)) {
		new("NotEqualsString", column=column, value=value)		
	} else {
		stop()
	}
}

Equals <- function(column, value) {
	if (is.numeric(value)) {
		new("EqualsInt", column=column, value=as.integer(value))
	} else if (is.character(value)) {
		new("EqualsString", column=column, value=value)		
	} else {
		stop()
	}
}

setClass("Query", 
		representation=representation(
				conn="DBIConnection"), contains="VIRTUAL")

setClass("Select", 
		representation=representation(
				columns="character",
				from="character",
				where="WhereExp",
				orderBy="character"),
		contains="Query")

setClass("Update", 
		representation=representation(
				table="character",
				set="list",
				where="WhereExp"),
		contains="Query")

setClass("Insert", 
		representation=representation(
				table="character",
				values="list"),
		contains="Query")


#UPDATE table_name
#SET column1=value, column2=value2,...
#WHERE some_column=some_value



Select <- function(conn, columns="*", from, where=Empty, orderBy="") {
	new("Select", conn=conn, columns=columns, from=from, where=where, orderBy=orderBy)
}

Update <- function(conn, table, set, where) {
	new("Update", conn=conn, table=table, set=set, where=where)
}

Insert <- function(conn, table, values) {
	new("Insert", conn=conn, table=table, values=values)
}


setMethod("print", signature=signature(x="Select"),
		def=function(x) {
			paste("SELECT", 
			paste(x@columns, collapse=","),
			paste("FROM", x@from),
			ifelse(!inherits(x@where, "Empty"), paste("WHERE", print(x@where)), ""),
			ifelse(x@orderBy != "", paste("ORDER BY", x@orderBy), ""))
		
		})

assignList <- function(alist) {
  res <- lapply(1:length(alist), function(i) {
    N <- names(alist)[i]
    V <- alist[[i]]
    if (is.character(V)) {
      paste(N, "=", "'", V, "'", sep="")
    } else {
      paste(N, "=", as.character(V), sep="")
    }
  })
  paste(res, collapse=", ")
}

setMethod("print", signature=signature(x="Update"),
		def=function(x) {
			paste("UPDATE", 
					paste(x@table),
					paste("SET", assignList(x@set)),
					paste("WHERE", print(x@where)))
					
			
		})


commaList <- function(values) {
	res <- lapply(as.list(values), function(x) {
				if (is.character(x)) { 
					paste("'", x, "'", sep="")
				} else {
					as.character(x)
				}})
  
	paste("(", paste(res, collapse=", "), ")", sep="")
				
}

setMethod("print", signature=signature(x="Insert"),
		def=function(x) {
			paste("INSERT INTO", 
					paste(x@table),
					commaList(names(x@values)),
					paste("VALUES"),
					commaList(x@values))
										
		})

setMethod("execute", signature=signature(x="Select"),
		def=function(x) {
			statement <- print(x)
			qres <- dbSendQuery(x@conn,statement) 	
			ret <- fetch(qres,-1)
			dbClearResult(qres)
			ret
				
		})

setMethod("execute", signature=signature(x="Update"),
		def=function(x) {
			statement <- print(x)
			dbSendQuery(x@conn,statement) 				
		})

setMethod("execute", signature=signature(x="Insert"),
		def=function(x, conn=NULL) {
			statement <- print(x)
                        if (!is.null(conn)) {
                          dbSendQuery(conn,statement)
                        } else {
                          dbSendQuery(x@conn,statement)
                        }
                          
			
		})


setMethod("%and%", signature=signature(x="WhereExp", y="WhereExp"),
		def=function(x, y) { new("And", x=x, y=y) })

setMethod("%or%", signature=signature(x="WhereExp", y="WhereExp"),
		def=function(x, y) { new("Or", x=x, y=y) })

setMethod("print", signature=signature(x="Group"),
		def=function(x) {
			paste("(", print(x@x), ")")
		})

setMethod("print", signature=signature(x="And"),
		def=function(x) {
			paste(print(x@x), "AND", print(x@y), sep=" ")
		})

setMethod("print", signature=signature(x="Or"),
		def=function(x) {
			paste(print(x@x), "OR", print(x@y), sep=" ")
		})

setMethod("print", signature=signature(x="EqualsInt"),
		def=function(x) {
			paste(x@column, "=", x@value, sep=" ")
		})

setMethod("print", signature=signature(x="EqualsString"),
		def=function(x) {
			paste(x@column, "=", paste("\"",  x@value, "\"", sep=""), sep="")
		})

setMethod("print", signature=signature(x="NotEqualsInt"),
		def=function(x) {
			paste(x@column, "<>", x@value, sep=" ")
		})

setMethod("print", signature=signature(x="NotEqualsString"),
		def=function(x) {
			paste(x@column, "<>", paste("'",  x@value, "'", sep=""), sep="")
		})

setMethod("print", signature=signature(x="LessThan"),
		def=function(x) {
			paste(x@column, "<", x@value, sep=" ")
		})

setMethod("print", signature=signature(x="GreaterThan"),
		def=function(x) {
			paste(x@column, ">", x@value, sep=" ")
		})

setMethod("print", signature=signature(x="Like"),
		def=function(x) {
			paste(x@column, "LIKE", paste("'", x@value, "'", sep=""), sep=" ")
		})

