
rcpp_hello_world <- function(){
	.Call( "rcpp_hello_world", PACKAGE = "FLcpp" )
}

