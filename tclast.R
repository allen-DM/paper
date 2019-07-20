#田口法實驗設計用於CARET應用
q_trgrid=function(x){
  require(qualityTools)
  nfactor=ncol(x) #計算有多少調整變數
  
  lev_tmp=c()	#用於儲存每個調整巷內的水準數
  #根據nfactor，計算每因子有多少水準
  for(i in 1:nfactor){
    lev_tmp[[i]]=length(names(table(x[,i])))
  }
  
  ##判斷因子中有多少變動水準(不包含水準為一的因子)
  #水準數需小於三
  #qualityTools套件內僅可設定兩變數兩種不同水準
  if( length(table(lev_tmp[which(lev_tmp>1)]))<3 ){
    ch_lev=lev_tmp[which(lev_tmp>1)] #變動因子的水準數
    
    #判斷水準個數是否不為一
    
    if(length(table(ch_lev))!=1){ #有兩水準的作法
      lev1=names(table(ch_lev))[1]
      lev2=names(table(ch_lev))[2]
      fat1=table(ch_lev)[1]
      fat2=table(ch_lev)[2]
      m_taguchi=taguchiChoose(factors1=fat1, level1=lev1,
                              factors2=fat2, level2=lev2)
      if(is.na(m_taguchi)){stop("無法找到符合的田口直交法，可利用taguchiChoose()決定實驗因子及水準")}
      ta_mat=taguchiDesign(m_taguchi)@design
      #ta_mat=ta_mat[,1:nfactor]
      ta_name=colnames(ta_mat)
    }
    if(length(table(ch_lev))==1){ #一水準做法
      lev1=names(table(ch_lev))[1]
      fat1=table(ch_lev)[1]
      m_taguchi=taguchiChoose(factors1=fat1, level1=lev1)
      if(is.na(m_taguchi)){stop("無法找到符合的田口直交法，可利用taguchiChoose()決定實驗因子及水準")}
      ta_mat=taguchiDesign(m_taguchi)@design
      ta_mat2= ta_mat

      ta_name=colnames(ta_mat)
	nta=nrow(ta_mat)
    }
    
      #根據水準數分別寫入田口直交表
      for(i in 1:length(ch_lev)){ #用來抓調教矩陣的行列
        c_index=which(colnames(ta_mat)==ta_name)

        for(j in c_index){	#抓田口矩陣要換取的行
          tmp=0 #設定跳出迴圈開關
			

          if( length(table(x[,i])) == length(table(ta_mat[,j])) ){
			
		for(z in 1:length(table(x[,i])) ){ 	#選擇行內要變換的元素
              
              ta_mat[which(ta_mat2[,j]==z),j] = names(table(x[,i]))[z]
              if(class(x[,i])=="integer"){ta_mat[,j]=as.integer(ta_mat[,j])}
		  if(class(x[,i])=="numeric"){ta_mat[,j]=as.numeric(ta_mat[,j])}

              ind=j	
              tmp=1	#因有改變內部變數，故將跳出迴圈開關打開			
              
            }
          }
          if(tmp!=0) break 
        }
        colnames(ta_mat)[ind] = colnames(x)[i] #每一次變換矩陣後，即更改變數名稱
      }	
    new_var=ta_mat[,-which(colnames(ta_mat)==ta_name)]
    
    if(ncol(new_var)!=ncol(x)){#新增一水準因子
    dif_x=which(!colnames(x)%in%colnames(new_var))
      for(index in dif_x){
      new_var=cbind(new_var,a=x[,index])
      colnames(new_var)[which(colnames(new_var)=="a")]=colnames(x)[index]
    }
    }
    
    #檢查最後矩陣數值部分類別是否為numeric
    
    
    
  }else{stop("因子數需小於二，可利用taguchiChoose()決定實驗因子及水準")}
  return(new_var)
}

