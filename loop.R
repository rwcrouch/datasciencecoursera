loop<-function(x){

for (i in seq_len(ncol(x))){

	for (
j in seq_len(nrow(x))){
		print(x[j,i])

			}
		}
}