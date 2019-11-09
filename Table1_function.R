
table1.fun<-function(data=data,
                     outcome=outcome)
  
{
  
  library(gmodels)
  
  # Moving outcome variable to column position one
  col_idx <- grep("df2.copd1", names(tab1.df))
  data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]
  names(data)
  
  #making blank table
  table1<-data.frame(matrix(nrow = 0, ncol = 8))
  colnames(table1) <- c("rownames","yes1", "yes2","no1","no2","positive","negative","pvalue")
  
  for (i in colnames(data))
  {
    check.bin <- function(v, naVal = NULL)
    {
      if( !is.numeric(v) ) stop("Only numeric vectors are accepted.")
      v2 <- na.omit(v)  # Dropping missing values
      v_unique <- unique(v2) # Keeping unique values only
      v_unique2 <- v_unique[! v_unique %in% naVal]  # checking that unique values are not NAs
      if ( length(unique(v_unique2)) > 2L ||        # If binary unique value would be only 0-1 or 1-2
           any(as.integer(v_unique2) != v_unique2) ) "con" else "bin"
    }
    
    table1[i,1]<-i
    
    if (check.bin(data[,i])=="bin")
    {
      table1[i,2]<- format(CrossTable(data[,1],data[,i])$t[2,2],trim=T)
      table1[i,4]<- format(CrossTable(data[,1],data[,i])$t[1,2],trim=T)
      
      table1[i,3]<- round(CrossTable(data[,1],data[,i])$prop.row[2,2]*100,1)
      table1[i,5]<- round(CrossTable(data[,1],data[,i])$prop.row[1,2]*100,1)
      
      table1[i,6]<- paste(table1[i,2], " (", table1[i,3], "%)", sep="")
      table1[i,7]<- paste(table1[i,4], " (", table1[i,5], "%)", sep="")
      
      table1[i,8]<-format((chisq.test(data[,1], data[,i]))$p.value, scientific=FALSE, digits = 3)
      
    }
    else 
    {
      table1[i,2]<-round(aggregate(data[,i], by=list(data[,1]), FUN=mean, na.rm=T)[2,2],1)
      table1[i,4]<-round(aggregate(data[,i], by=list(data[,1]), FUN=mean, na.rm=T)[1,2],1)
      
      table1[i,3]<-round(aggregate(data[,i], by=list(data[,1]), FUN=sd, na.rm=T)[2,2],1)
      table1[i,5]<-round(aggregate(data[,i], by=list(data[,1]), FUN=sd, na.rm=T)[1,2],1)
      
      table1[i,6]<- paste(table1[i,2], " ± ", table1[i,3], sep="")
      table1[i,7]<- paste(table1[i,4], " ± ", table1[i,5], sep="")
      
      aov.fit<-(aov(data[,i] ~ data[,1], data=data))
      aov.test<-unlist(summary(aov.fit))
      table1[i,8]<-format(aov.test["Pr(>F)1"], scientific=FALSE, digits = 3, trim=T)
      
    }
    
  }
  table1<-table1[-c(1), ]
  rownames(table1)<-NULL
  print(table1)
  write.csv(table1,"table1.csv")
}

table1.fun(tab1.df,df2.copd1)
