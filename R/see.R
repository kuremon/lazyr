#' @title See heatmap representation of data frame
#' @description 
#' Display data frame in the plot panel with value of the data frame being represented as color.  
#' @param data data to view. Can be a matrix or a data frame.
#' @param use.rownames specify if the row names of the data frame should be displayed on the y axis.
#' @param tilt.colnames specify if the column names should be tiltled on the x axis.
#' @param col.as.factor specify if the values of the data frame should be considered as factor. By default it is the case if
#' all columns of the data frame are categorical.
#' @return a ggplot2 object
#' @export
see=function(data,use.rownames=TRUE,tilt.colnames=TRUE,col.as.factor=FALSE,with.text=FALSE,color="black",format,...){
  data=as.data.frame(data)
  .rownames=row.names(data)
  data=transform(data,.row=ordered(seq(nrow(data)),rev(seq(nrow(data)))))
  data.molten=melt(data,id=".row")
  
  are.all.col.categorical=all(as.logical(colwise(is.categorical)(data)))
  if(col.as.factor||are.all.col.categorical){
    data=colwise(as.factor)(data)
    all.levels=unlist(llply(as.list(data),levels))#this additional lines allow to keep potential existing order.
    all.levels=all.levels[!duplicated(all.levels)]
    data.molten[["value"]]=factor(data.molten[["value"]],levels=all.levels)
  }
  
  if(!missing(format)&&with.text) data.molten$value.formatted=format(data.molten$value,...)
  
  g=ggplot(data.molten,aes(x=variable,y=.row,fill=value))+geom_raster()+labs(x="",y="")
  if(use.rownames){
    g=g+scale_y_discrete(breaks=data$.row,labels=.rownames)
  }
#   else{# change the first tick into 1 instead of zero
#     message("rownames are not used.")
#     y.labels=ggplot_build(g)$panel$ranges[[1]]$y.labels;
#     y.labels[1]="1";
#     g=g+scale_y_discrete(breaks=y.labels,trans=scales::reverse_trans());
#   }
  if(tilt.colnames) g=g+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1)) 
  if(with.text){
    if(missing(format)){
      g=g+geom_text(aes(label=value),colour=color)
    }else{
      g=g+geom_text(aes(label=value.formatted),colour=color)
    }
  }
  g
}