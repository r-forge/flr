getClassMethods<- function(class, where){
     v<- showMethods(class=class, where=where, printTo=FALSE)->v
     unlist(lapply(strsplit(unlist(lapply(strsplit(v[substr(v, 1, 5) %in% "Funct"], " \\("), "[", 1)), ": "), "[", 2))}
