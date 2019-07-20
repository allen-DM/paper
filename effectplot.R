
effect_plot=function(effect_x){
x_ncol=ncol(effect_x)
x_colnames=colnames(effect_x)
x_var=NULL
x_mean=NULL
par(mfrow=c(1,x_ncol-1))
	for(i in 1:(x_ncol-1)){

		RMSE=tapply(effect_x[,x_ncol],effect_x[,i],mean)
	if(class(effect_x[1,i])=='numeric'|class(effect_x[1,i])=='integer')
		{
		x_vars=as.numeric(names(table(effect_x[,i])))
		
		
		plot(x_vars,y=RMSE,
				xlab=paste0(x_colnames[i]),ylab=paste0(x_colnames[x_ncol]),
				col=2, lty = 3, main="Effect Plot for Taguchi Design",
				type="l")
		}
		else{plot(RMSE,
				xlab=paste0(x_colnames[i]),ylab=paste0(x_colnames[x_ncol]),
				col=2, lty = 3, main="Effect Plot for Taguchi Design",
				type="l")
			lenx=length(table(effect_x[,3]))
			tmp_name=names(table(effect_x[,i]))
			for(j in 1:lenx)
			text(j,RMSE[[j]],paste0(tmp_name[j]))		
			}	

					}}



