
#' NETWORK GRAPHING
#' 
#' Stu's function for plotting networks
#' 
#' Details
#' 
#' @param dat value
#' @param arrowcolors R colors. Length = 2
#' @param legend Logical. Should a legend be added to plot?
#' @param ... Additional argumnets passed to the S3 plot method for
#' the class returned by graph.adjacency. Typically a newtwork graph object.
#' @return A network graph
#' @author Stu Field 
#' @seealso \code{\link{igraph}} 
#' @references %% ~put references to the literature/web site here~ 
#' @examples 
#' 
#' nodes <- c("SEED",
#'            expression(SD[1]),
#'            expression(SD[2]),
#'            "SP",
#'            "YA",
#'            "MA",
#'            expression(iSD[1]),
#'            expression(iSD[2]),
#'            "iSP",
#'            "iYA",
#'            "iMA"
#'            )
#'
#' L <- length(nodes)
#' Network <- matrix(0, nrow=L, ncol=L , dimnames=list(nodes, nodes))
#' one <- cbind(c(1:5, 7:10), c(2:6, 8:11))
#' two <- cbind(c(2:6, 2:5),c(2:6+5, 2:5+6))
#' three <- cbind(c(5, 6, 10, 11), rep(1, 4))
#' Network[one]   <- 1
#' Network[two]   <- 0.66
#' Network[three] <- 0.01
#' diag(Network)  <- 0.9
#' Network[1, 1]  <- 0
#' Network
#' 
#' ###################################
#' #    arrows for self nodes        #
#' #    in radians                   #
#' #    Output of function           #
#' #    design by hand               #
#' ###################################
#' loopvec <- NA                       # for edge.loop.angle argument
#' loopvec[c(2,6,29,32)] = 0           # horizontal 3 O'Clock
#' loopvec[c(15,20,22,24,26)] = 3      # horizontal 9 O'Clock
#' loopvec[10] = 6                     # 2 O'Clock
#' 
#' dotted <- rep(1,32)
#' dotted[c(14,19,28,31)] = 4          # enter into edge.lty if desired
#' dotted[1] = 3                       # enter into edge.lty if desired
#' 
#' ##########################
#' #     Graph Newtork      #
#' #     vertex = nodes     #
#' #     edge = arrows      #
#' ##########################
#' par(bg="thistle", mar=c(3,2,2,0))
#' CreateNetGraph(dat=Network,
#'          vertex.label=nodes, 
#'          vertex.label.dist=0,
#'          vertex.size=35,
#'          vertex.shape="circle",
#'          vertex.color=c(rep("white", 6), rep("black", 5)),
#'          vertex.label.color=c(rep("black", 6), rep("white", 5)),
#'          edge.arrow.mode=">",
#'          edge.curved=0.01,
#'          edge.loop.angle=loopvec, # looping angles of selfing nodes in radians
#'          edge.arrow.size=0.75,
#'          edge.width=2,
#'          edge.lty=dotted,
#'          arrowcolors=c("white", "black"),
#'          legend=TRUE)
#' 
#' 
#' # These may be more examples #
#' 
#' #adjmat <- matrix(sample(0:1,25*25, replace=TRUE), ncol=25, dimnames=list(LETTERS[1:25], LETTERS[1:25]))

#' #plot(graph.adjacency(adjmat), vertex.color="navy", vertex.label=LETTERS[1:25], edge.arrow.size=.3, edge.color=1, vertex.label.color="white")

#' #plot(graph.adjacency(adjmat), vertex.color="navy", vertex.label=LETTERS[1:25], edge.arrow.size=.3, edge.color=1, vertex.label.color="white")

#' #tkplot(graph.adjacency(adjmat), vertex.color="navy", vertex.label=LETTERS[1:25], edge.arrow.size=.3, edge.color=1, vertex.label.color="white")

#' #rglplot(graph.adjacency(adjmat), vertex.color="navy", vertex.label=LETTERS[1:25], edge.arrow.size=.3, edge.color=1, vertex.label.color="white")
#' 
#' 
#' @import igraph
#' @export CreateNetGraph
CreateNetGraph <- function(dat, arrowcolors=c("green", "red"), legend=FALSE, ...) {

   # Create an igraph object from the matrix of weights
   grph <- graph.adjacency(dat, mode="directed", weighted=TRUE, diag=TRUE)

   # Create index names to improve readability
   fwd <- 1
   rev <- 2
   cwts <- matrix(0, nrow=length(E(grph)), ncol=2)
 
   for (i in 2:length(E(grph))) {
      # Get node names for each edge to index weights
      node1 <- V(grph)[(get.edge(grph, i-1)[1])]$name
      node2 <- V(grph)[(get.edge(grph, i-1)[2])]$name
      cwts[i,fwd] <- dat[node1,node2]
      cwts[i,rev] <- dat[node2,node1]
   }

   # Create color palette ranging from col1 (lowest val) to col2
   br.palette <- colorRampPalette(arrowcolors)
   palette(br.palette(512))

   # Rescale weights to 0:512 then apply weights as colors
   f <- 512 / max(cwts)
   cwts <- round(cwts * f)
   E(grph)$color <- cwts[, 1] # note that zeros for color index = invisible
   label.angles <- seq(0, -2*pi, length=nrow(dat))

   #    get node loops: edges that self     #
   e <- get.edgelist(grph)
   node.loops <- which(e[, 1]==e[, 2])
   cat("Number of selfing edges (diagonal):", length(node.loops), "\n")
   cat("Number of total edges:", nrow(e), "\n")
   if (length(node.loops) > 0) {
      cat("Edges by Loop number:", node.loops, "\n")
   } else {
      cat("Edges by Loop number: 0", "\n")
   }

   plot(grph, layout=layout.circle, vertex.label.degree=label.angles, ...)

   if ( legend ) {
      legend(x=-1.75, y=-1, col=c("black", "gray50", "white"),
             lty= 1, lwd=3, title="Arrows:", bg="lavender", box.lty=0, cex=0.8,
             legend=c("Survival & transition", "Infection","Fecundity"))
      legend(x=1, y=-1.0, col=c("white","black"), pch= 19, title="Nodes:",
             legend=c("Susceptible", "Infected"), bg="lavender", box.lty=0, cex=0.9)
  }
}


