###############################################################################
# req: require and install
###############################################################################

req <- function(...){
    invisible(sapply(list(...), function(lib){
        if(!(lib %in% .packages(all=T)))
            install.packages(lib)
        require(lib, character.only=T)
    }))
}

###############################################################################
# benchmark: benchmark a set of functions running them a number of times in
#   random order
###############################################################################

benchmark <- function(..., n=1, averages=T){
	fns <- c(...)
	ordr <- rep(1:length(fns), each=n)
	ordr <- sample(ordr, length(ordr))
	sums <- rep(0, length(fns))
	for(i in ordr){
		tick <- proc.time()
		fns[[i]]()
		tock <- proc.time() - tick
		sums[i] <- sums[i] + tock[3]
	}
	if(averages)
	    sums/n
	else
	    sums
}

###############################################################################
# cache: cache stuff
###############################################################################

req('digest')

# TODO if label changes, but hash exists, load previous label into new label

cache <- function(x, label, dir=NULL, pre='cache', ext="Rdata", hash="crc32", dateFormat="%Y%m%d%H%M%S", deleteOld=FALSE, justHash=FALSE){
	if(is.null(dir)){
		if(is.null(o<-getOption('jrsCacheDir'))){
			dir<-'.'
		}else{
			dir<-o
		}
	}

    if(is.function(x)){
        strRep <- deparse(body(x))
    }else{	    
	    strRep <- deparse(substitute(x))
	}
    
    # algo=c("md5", "sha1", "crc32", "sha256")
    hash <- digest(paste(strRep, collapse=""), algo=hash, serialize=F)
	if(justHash)
		return(hash)
	
    filenamesNoDate <- Sys.glob(file.path(dir, paste(pre, label, "*", hash, ext, sep=".")))
    filenamesNoDateOrHash <- Sys.glob(file.path(dir, paste(pre, label, "*", "*", ext, sep=".")))
	if(!length(filenamesNoDate)){
	    if(is.function(x))
		    temp <- x()
		else
		    temp <- eval(substitute(x))
		
		date <- format(Sys.time(), dateFormat)
    	filename <- file.path(dir, paste(pre, label, date, hash, ext, sep="."))
		save(temp, file=filename)
	}else{
	    filename <- filenamesNoDate
		load(filename)
	}
	if(deleteOld)
	    file.remove(filenamesNoDateOrHash[filenamesNoDateOrHash!=filename])
	
	return(temp)
}

memoize <- function(x, label=NULL){
    return(function(...){    
        strRep <- substitute(...)
        argsHash <- digest(paste(strRep, collapse=""), algo="crc32", serialize=F)
        cache({x(...)}, label=argsHash)
    })
}

# a <- function(x){x + 93}
# b <- memo(a)
# b(4)

###############################################################################
# cache: cache stuff
###############################################################################

docs <- function(x){
	description <- grep('###',bodytxt<-sub("\t", '', capture.output(x)), perl=T)
	#description <- sub('^[[:blank:]]*###[[:blank:]]*', '', description, perl=T)
    
	return(description)
	if(length(description) > 0){
		descritption <- bodytxt[description[1]:description[2]]
		return(gsub("#",'', descritption, perl=T))
	}
}

man <- docs

# Tests:
#   file.remove(Sys.glob("cache.test1.*.Rdata")) # Start with a clean directory
#   file.remove(Sys.glob("cache.test2.*.Rdata"))
#   cache(function(){1+1}, 'test1', deleteOld=F) # One file
#   cache(function(){1+1}, 'test1', deleteOld=F) # Nothing changes
#   cache(function(){1+1}, 'test1', deleteOld=T) # Nothing changes
#   cache(function(){2+1}, 'test1', deleteOld=F) # Two files
#   cache(function(){2+1}, 'test1', deleteOld=F) # Nothing changes
#   cache(function(){2+1}, 'test1', deleteOld=T) # First one deleted
#   cache(function(){3+1}, 'test1', deleteOld=T) # Second one deleted
#   cache(function(){3+1}, 'test2', deleteOld=T) # Another file created