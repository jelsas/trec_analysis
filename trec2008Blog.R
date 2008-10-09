# trec2008Blog.R
# an R script defining a few functions for processing the TREC blog track
# results files and generating some pretty pictures.
#
# Author: Jonathan Elsas (jelsas@cs.cmu.edu)
#
# USAGE: 
# this script should be source'd from within an R session, then the 
# functions can be called.
#
# EXAMPLE:
# to generate a grid of "scaled system performance" plots:
# > source("/path/to/trec2008Blog.R")
# > plotAll("/path/to/data/", plotFn=plotOurPerf.scale, xaxt="n")
# produces an plot shown at the bottom of the following post:
# http://windowoffice.tumblr.com/post/53677875/visualizing-retrieval-performance
# 
# NOTE:
# All this code assumes the queries in the results files are exactly the 
# same as the queries in the summary file.  There is not checking to ensure
# this is actually the case.  You have been warned.

# reads in a trec results file, returning a reasonably formatted
# data frame
readTrecResults <- function(filename, 
			measures=c("map", "P10", "R-prec"), 
			remove.all=TRUE) {
	# read in the (key, qid, val) triples
	raw.data = read.table(filename, sep="", header=FALSE,
		col.names=c("measure", "qid", "value"), as.is=TRUE)
	results = data.frame(qid=sort(unique(raw.data$qid)))
	for( m in measures) {
		this.measure = raw.data[which(raw.data$measure == m),]
		# ensure we're sorting by qid
		this.measure = this.measure[order(this.measure$qid),]
		results = cbind(results, this.measure$value)
		}
	names(results) = c("qid", make.names(measures))
	if(remove.all) { 
		results = results[which(results$qid != "all"),] }
	return(results)
	}
	
# reads in the summary results file, provided with the blog track results
readTrecSummary <- function(filename) {
	raw.data = read.table(filename, sep="", header=FALSE,
		col.names=c("qid", "nrel", "map.best", "map.median", 
			"map.worst", "rprec.best", "rprec.median", 
			"rprec.worst", "p10.best", "p10.median", "p10.worst"),
		skip=3)
	# convert p10 values to floats
	raw.data$p10.best = raw.data$p10.best / 10
	raw.data$p10.median = raw.data$p10.median / 10
	raw.data$p10.worst = raw.data$p10.worst / 10
	# again, ensure sorting by qid
	raw.data = raw.data[order(raw.data$qid),]
	return(raw.data)
	}
	
# plots the absolute performance, with max, median and min performance
plotOurPerf.abs <- function(ourPerf, bestPerf, medianPerf, 
		worstPerf, ylab=deparse(substitute(ourPerf)), 
		xlab="", ...){
	# assume everything's sorted by the same key... not so smart,
	# but it works
	our.order = order(ourPerf, decreasing=TRUE)
	ylim=c(min(worstPerf), max(bestPerf))
	plot( worstPerf[our.order], ylim=ylim, type="l", col="red",
		xlab=xlab, ylab=ylab, frame.plot=FALSE,
		...)
	lines( medianPerf[our.order], type="l", col="blue")
	lines( bestPerf[our.order], type="l", col="green")
	lines( ourPerf[our.order], type="l", col="purple", lwd=2)
	}

# plots the relative performance, with max, median performance
plotOurPerf.rel <- function(ourPerf, bestPerf, medianPerf, 
		worstPerf, ylab=paste("relative", deparse(substitute(ourPerf))), 
		xlab="", ...){
	# skip the ones with a median perf of zero
	nonzero = which(medianPerf > 0)
	ourPerf = ourPerf[nonzero]
	bestPerf = bestPerf[nonzero]
	medianPerf = medianPerf[nonzero]
	our.pct.over.median = 100* (ourPerf - medianPerf) / medianPerf
	best.pct.over.median = 100* (bestPerf - medianPerf) / medianPerf
	our.order = order(our.pct.over.median, decreasing=TRUE)
	ylim=c(min(our.pct.over.median, na.rm=TRUE),
		max(best.pct.over.median, na.rm=TRUE))
	
	plot( our.pct.over.median[our.order], ylim=ylim, type="l", col="purple",
		xlab=xlab, ylab=ylab, frame.plot=FALSE, lwd=2, ...)
	lines( best.pct.over.median[our.order], type="l", col="green")
	abline(h=0, col="gray")
	}
	
# plots the scaled performance
plotOurPerf.scale <- function(ourPerf, bestPerf, medianPerf, 
		worstPerf, ylab=paste("scale", deparse(substitute(ourPerf))), 
		xlab="", ...){
	ours.scaled = (ourPerf - medianPerf) / (bestPerf -medianPerf)
	our.order = order(ours.scaled, decreasing=TRUE)
	
	plot( ours.scaled[our.order], type="l", col="purple",
		yaxt="n", xlab=xlab, ylab=ylab, frame.plot=FALSE, lwd=2, ...)
	axis(side=2, at=c(0,1), labels=c("median", "best"))
	abline(h=0, col="gray")
	}
	
# make some pretty charts, in a grid
# dataDir should point to a directory containing your .blogdist
# results files (output from trec_eval -q) and the 
# _trec_trec17_tables_blog.feed file
#
# this probably won't work on non-linux/mac systems.  sorry!
#
plotAll <- function(dataDir, plotFn=plotOurPerf.abs, ...)  {
	# find the .blogdist files in dataDir
	blogdist.files = list.files(dataDir, pattern=glob2rx("*.blogdist"))
	all.summ = readTrecSummary(paste(dataDir, "_trec_trec17_tables_blog.feed", sep="/"))
	op = par(no.readonly = TRUE)
	par(mfrow=c(length(blogdist.files),3), mar=c(2, 4, 2, 0) + 0.1)

	for(i in 1:length(blogdist.files)) {
		f = blogdist.files[i]
		name = unlist(strsplit(f, "\\."))[1]
		results = readTrecResults(paste(dataDir, f, sep="/"))
		plotFn(results$map, all.summ$map.best, all.summ$map.median, all.summ$map.worst, 
			ylab=name, main=if(i==1){"AP"}else{""}, ...)
		plotFn(results$R.prec, all.summ$rprec.best, all.summ$rprec.median, 
			all.summ$rprec.worst, ylab="", main=if(i==1){"R-Prec"}else{""}, ...)
		plotFn(results$P10, all.summ$p10.best, all.summ$p10.median, all.summ$p10.worst,
			ylab="", main=if(i==1){"P10"}else{""}, ...)
		}	
	par(op)
	}