#�Фf�k����]�p�Ω�CARET����
q_trgrid=function(x){
  require(qualityTools)
  nfactor=ncol(x) #�p�⦳�h�ֽվ��ܼ�
  
  lev_tmp=c()	#�Ω��x�s�C�ӽվ�Ѥ������Ǽ�
  #�ھ�nfactor�A�p��C�]�l���h�֤���
  for(i in 1:nfactor){
    lev_tmp[[i]]=length(names(table(x[,i])))
  }
  
  ##�P�_�]�l�����h���ܰʤ���(���]�t���Ǭ��@���]�l)
  #���Ǽƻݤp��T
  #qualityTools�M�󤺶ȥi�]�w���ܼƨ�ؤ��P����
  if( length(table(lev_tmp[which(lev_tmp>1)]))<3 ){
    ch_lev=lev_tmp[which(lev_tmp>1)] #�ܰʦ]�l�����Ǽ�
    
    #�P�_���ǭӼƬO�_�����@
    
    if(length(table(ch_lev))!=1){ #������Ǫ��@�k
      lev1=names(table(ch_lev))[1]
      lev2=names(table(ch_lev))[2]
      fat1=table(ch_lev)[1]
      fat2=table(ch_lev)[2]
      m_taguchi=taguchiChoose(factors1=fat1, level1=lev1,
                              factors2=fat2, level2=lev2)
      if(is.na(m_taguchi)){stop("�L�k���ŦX���Фf����k�A�i�Q��taguchiChoose()�M�w����]�l�Τ���")}
      ta_mat=taguchiDesign(m_taguchi)@design
      #ta_mat=ta_mat[,1:nfactor]
      ta_name=colnames(ta_mat)
    }
    if(length(table(ch_lev))==1){ #�@���ǰ��k
      lev1=names(table(ch_lev))[1]
      fat1=table(ch_lev)[1]
      m_taguchi=taguchiChoose(factors1=fat1, level1=lev1)
      if(is.na(m_taguchi)){stop("�L�k���ŦX���Фf����k�A�i�Q��taguchiChoose()�M�w����]�l�Τ���")}
      ta_mat=taguchiDesign(m_taguchi)@design
      ta_mat2= ta_mat

      ta_name=colnames(ta_mat)
	nta=nrow(ta_mat)
    }
    
      #�ھڤ��ǼƤ��O�g�J�Фf�����
      for(i in 1:length(ch_lev)){ #�Ψӧ�ձЯx�}����C
        c_index=which(colnames(ta_mat)==ta_name)

        for(j in c_index){	#��Фf�x�}�n��������
          tmp=0 #�]�w���X�j��}��
			

          if( length(table(x[,i])) == length(table(ta_mat[,j])) ){
			
		for(z in 1:length(table(x[,i])) ){ 	#��ܦ椺�n�ܴ�������
              
              ta_mat[which(ta_mat2[,j]==z),j] = names(table(x[,i]))[z]
              if(class(x[,i])=="integer"){ta_mat[,j]=as.integer(ta_mat[,j])}
		  if(class(x[,i])=="numeric"){ta_mat[,j]=as.numeric(ta_mat[,j])}

              ind=j	
              tmp=1	#�]�����ܤ����ܼơA�G�N���X�j��}�����}			
              
            }
          }
          if(tmp!=0) break 
        }
        colnames(ta_mat)[ind] = colnames(x)[i] #�C�@���ܴ��x�}��A�Y����ܼƦW��
      }	
    new_var=ta_mat[,-which(colnames(ta_mat)==ta_name)]
    
    if(ncol(new_var)!=ncol(x)){#�s�W�@���Ǧ]�l
    dif_x=which(!colnames(x)%in%colnames(new_var))
      for(index in dif_x){
      new_var=cbind(new_var,a=x[,index])
      colnames(new_var)[which(colnames(new_var)=="a")]=colnames(x)[index]
    }
    }
    
    #�ˬd�̫�x�}�ƭȳ������O�O�_��numeric
    
    
    
  }else{stop("�]�l�ƻݤp��G�A�i�Q��taguchiChoose()�M�w����]�l�Τ���")}
  return(new_var)
}
