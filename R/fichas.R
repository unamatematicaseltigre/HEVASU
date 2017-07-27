#Funciones para generar las fichas académicas
#Fecha : 25/03/2017
#Autor : José L. Romero
#requiere los siguientes paquetes: rJava, gWidgets, RGtk2
#usa también LaTeX/texlive
#-----------------------------------------------------------------------

generar_fichas <- function(paginaweb) {
	#función para generar fichas
	#1) se supone que los archivos de fichas (.REP)
	#están en el directorio de trabajo.
	#2) se supone que el usuario ha especificado el cl/ua
	#en el selector del menú principal.
	#3) las fichas se elaboran para el lapso académico
	#seleccionado en el menú principal.
	#Las fichas se elaboran para el cl/ua y el lapso seleccionado.
	mostrar_progreso <- function(progreso) {
	#Función para mostrar la barra de progreso
		mensaje <- paste("Generando fichas académicas: [",
			paste(rep("█",progreso),collapse=""),
			paste(rep("░",25-progreso),collapse=""),"]",sep="")
		svalue(barra_status) <- mensaje; Sys.sleep(0.001)
	}
	
	svalue(barra_status) <- 
		"Extrayendo data de fichas en archivos .REP..."
	Sys.sleep(0.001)
	#Consolidar los reportes en un solo archivo y convertir a UTF-8
	system("cat *.REP > FICHAS.REP")
	system("iconv -f ISO-8859-14 -t UTF-8 FICHAS.REP -o fichas.txt")
	#eliminar lineas sobrantes. Si no hay lineas, retornar con error.
	archivo <- file("fichas.txt","r")
	lineas <- readLines(archivo)
	close(archivo)
	if (length(lineas)==0) return(1) #error: no hay archivos de fichas
	lineas_s <- NULL
	for (i in lineas) {
		if (!i=="")
			if (substr(i,1,1)=="-"|substr(i,1,1)=="|")
				lineas_s <- c(lineas_s,i) 
	}
	archivo <- file("fichas.txt","w")
	writeLines(lineas_s,archivo)
	close(archivo)
	lineas <- lineas_s
	#recorre las líneas en memoria en primera pasada para obtener
	#la cédula del estudiante, el nombre, y los números de línea
	#(en el archivo "lineas") donde comienza y termina su ficha.
	indices <- data.frame(ci=NULL,nombre=NULL,i=NULL)
	for (i in 1:length(lineas)) {
		if (substr(lineas[i],1,8)=="| ALUMNO") {
			ci <- gsub("^\\| ALUMNO  \\| .{2}([0-9]*).+","\\1",lineas[i])
			nombre <- gsub(paste0("^\\| ALUMNO  \\|"," +(V|E)?\\-([0-9]*)","+([A-Za-zÑÁÉÍÓÚÀÈÌÒÙáéíóúàèìùò ,.]*)"," {2,100}FECHA.+")
			,"\\1-\\2 \\3",lineas[i])
			nombre <- gsub("^\\s+|\\s+$","",nombre)
			nombre <- gsub("¥","Ñ",nombre)
			nombre <- gsub("^\\-","",nombre)
			indices <- rbind(indices,data.frame(ci=as.numeric(ci),
				nombre=as.character(nombre),i=as.numeric(i-1)))
		}
	}
	if (nrow(indices)==0) return(2) #error: archivos de fichas no válidos
	indices <- cbind(indices,c(indices[2:nrow(indices),3]-1,
		length(lineas)))
	#en una segunda pasada de las líneas, se obtienen los otros
	#datos: la carrera, los créditos logrados, el índice académico y
	#por supuesto la data de cada materia cursada, registrada en campos
	#separados por "&". hay columnas hasta para 8 materias cursadas
	#por semestre.
	supra_materias <- NULL
	carreras <- NULL
	clog <- NULL
	iac <- NULL
	for (i in 1:nrow(indices)) {
		carrera <- gsub("^\\| CARRERA \\| +([0-9]{3}) +([A-Z ()]+).+$",
			"\\1 \\2",lineas[(indices[i,3]+2)])
		carrera <- gsub("^\\s+|\\s+$","",carrera)
		carreras <- c(carreras,carrera)
		creditos_logrados <- as.numeric(gsub(
			"\\| En Periodo   Creditos Logrados: +([0-9]+).+",
			"\\1",lineas[(indices[i,4]-1)]))
		clog <- c(clog,creditos_logrados)
		indice.academico <- as.numeric(gsub(
			"^.+Indice Academico:  ([0-9.]+) +\\|$","\\1",
			lineas[indices[i,4]-1]))  
		iac <- c(iac,indice.academico)
		a <- indices[i,3]+8; b <- indices[i,4]-3
		materias <- NULL
		if (a<=b) {
			for (j in a:b) {
				renglonj<-gsub("^\\| (.+)\\s+\\|$","\\1",lineas[j])
				renglonj <- gsub("^\\s+|\\s+$","",renglonj)
				renglonj <- gsub("([A-Z.]) {2,}([A-Z])","\\1 \\2",renglonj)
				renglonj <- gsub("NP R","NP  R",renglonj)
				renglonj <- gsub(" {2,}"," & ",renglonj)
				renglonj <- gsub("¥","Ñ",renglonj)
				materias <- c(materias,renglonj)
			}
		}
		while (length(materias)<8) materias <- c(materias,NA)
		supra_materias <- rbind(supra_materias,materias)
	}
	iac[is.na(iac)]<-0
	indices <- cbind(indices[,1:2],carreras,clog,iac,supra_materias)
	colnames(indices) <- c("ci","nombre","carrera","creditos.logrados",
		"indice.academico","M1","M2","M3","M4","M5","M6","M7","M8")
	write.table(indices,"data_fichas.csv",row.names=FALSE,sep="\t",
		quote=FALSE)
	#Aquí se comienza a generar las fichas con pdflatex
	mostrar_progreso(0)
	#obtener la información del centro local y unidad de apoyo
	cloa <- svalue(selector_oficina)
	#centrolocal es una variable global
	centro_local <- paste0("(",substr(cloa,1,2),") ",centrolocal)
	#leer el nombre de la oficina de apoyo
	ciudades <- read.table(file.path(dir_programa,"certificados",
		"ciudades.csv"),header=TRUE,row.names=1,sep="\t",as.is=T)
	cloaf <- gsub(" - ","",cloa)
	cloaf <- gsub("^0","",cloaf)
	oficina_de_apoyo <- paste0("(",gsub(".*([0-9]{2})$","\\1",cloaf),") ",
		toupper(ciudades[cloaf,]))
	#lee el archivo de "data_fichas.csv"
	archivo_tex <- file(file.path(dir_programa,"certificados",
		"ficha.tex"), open="r")
	latex_original <- readLines(archivo_tex)
	close(archivo_tex)
	lapsof <- gsub("-","",svalue(selector_lapso))
	#para cada ficha en el archivo...
	for (i in 1:nrow(indices)) {
		#copia el archiv tex
		lineas <- latex_original[1:62]
		#agrega data especifica
		lineas[30] <- gsub("nombre",indices[i,2],lineas[30])
		lineas[31] <- gsub("lacarrera",indices[i,3],lineas[31])
		lineas[32] <- gsub("id",indices[i,1],lineas[32])
		lineas[33] <- gsub("creditos",indices[i,4],lineas[33])
		lineas[34] <- gsub("indice_academico",indices[i,5],lineas[34])
		lineas[35] <- gsub("lapso_sin_guion",lapsof,lineas[35])
		lineas[36] <- gsub("centro_local",centro_local,lineas[36])
		lineas[37] <- gsub("oficina_de_apoyo",oficina_de_apoyo,lineas[37])
		lineas[38] <- gsub("pagina_web",paginaweb,lineas[38])
		#agregar data de materias
		for (j in 1:8) {
			materia <- paste0("M",j)
			if (!is.na(indices[i,materia])) 
				lineas <- c(lineas,paste0("\t\t",indices[i,materia],"\\\\"))
		}
		#agregar resto
		lineas <- c(lineas,latex_original[63:75])
		#escribe el archivo tex
		nombre_archivo <- paste(sprintf("%08d",indices[i,1]),".tex",sep="")
		archivo_tex <- file(nombre_archivo,open="w")
		writeLines(lineas,con=archivo_tex)
		close(archivo_tex)
		system(paste("pdflatex",nombre_archivo))
		#borra los archivos .tex .aux y .log de esa ficha
		unlink(nombre_archivo)
		unlink(gsub("tex$","aux",nombre_archivo))
		unlink(gsub("tex$","log",nombre_archivo))
		unlink(gsub("tex$","out",nombre_archivo))
		mostrar_progreso(round(i/nrow(indices)*25))
	}
	return(0)
}#generar_fichas

#-----------------------------------------------------------------------

generar_html_fichas <- function(archivo_tsv) {
	svalue(barra_status) <- paste("Generando html para",
		"publicación de fichas...")
	Sys.sleep(0.001)
	#guarda el directorio de trabajo
	#utilizará como directorio de trabajo el directorio del archivo tsv.
	#ahí se creará el archivo html.
	backup_dir <- getwd()
	setwd(dirname(archivo_tsv))
	#comienza la generación del html
	lineas <- "<!doctype html>"
	lineas <- c(lineas,"<html lang='es'>")
	lineas <- c(lineas,"	<meta charset='utf-8'>")
	lineas <- c(lineas,paste0("<div align='justify'>",
		"Por favor indica a continuación el número de cédula ",
		"para descargar la ficha académica:<br /><br /></div>"))
	lineas <- c(lineas,"<!-- more -->")
	lineas <- c(lineas,"<form method='POST' name='datospersonales'>")
	lineas <- c(lineas,"<table>")
	lineas <- c(lineas,"<tbody>")
	lineas <- c(lineas,"<tr>")
	lineas <- c(lineas,"	<td bgcolor='#ADD8E6' width='120'>Cédula</td>")
	lineas <- c(lineas,paste0("	<td width='100'><input id='cedula' ",
		"maxlength='8' size='12' type='text' /></td>"))
	lineas <- c(lineas,"</tr>")
	lineas <- c(lineas,"<tr>")
	lineas <- c(lineas,"	<td align='center' colspan='2'>")
	lineas <- c(lineas,paste0("		<input name='B1' onclick='Consultar()' ",
		"type='button' value='Descargar ficha académica' />"))
	lineas <- c(lineas,"	</td>")
	lineas <- c(lineas,"</tr>")
	lineas <- c(lineas,"</tbody></table>")
	lineas <- c(lineas,"</form>")
	lineas <- c(lineas,"")
	lineas <- c(lineas,"<script>")
	lineas <- c(lineas,"	function Consultar() {")
	lineas <- c(lineas,paste0("		var cedula=document.getElementById",
		"('cedula').value;"))
	lineas <- c(lineas,"		// verifica si cedula tiene ficha")
	lineas <- c(lineas,"		j=0")
	lineas <- c(lineas,paste0("		while (j<fichas.length && ",
		"!(fichas[j][0]==cedula)) {"))
	lineas <- c(lineas,"			j+=1")
	lineas <- c(lineas,"		}")
	lineas <- c(lineas,"		if (j==fichas.length) {")
	lineas <- c(lineas,paste0("			alert('La cédula '+cedula+' ",
		"no tiene ficha académica asociada. Por favor ingrese otro ",
		"número de cédula.');"))
	lineas <- c(lineas,paste0("			document.getElementById('cedula').",
		"innerHTML='';"))
	lineas <- c(lineas,"		} else {")
	lineas <- c(lineas,"			// recupera url de la ficha")
	lineas <- c(lineas,"			urlficha=fichas[j][1];")
	lineas <- c(lineas,"			window.open(urlficha,'_blank');")
	lineas <- c(lineas,"		}")
	lineas <- c(lineas,"	}")
	lineas <- c(lineas,"")
	lineas <- c(lineas,"	var fichas=new Array(0);")
	lineas <- c(lineas,"	fichas=[")
	data_fichas <- read.table(archivo_tsv,sep="\t",
		header=TRUE,as.is=TRUE)
	for (i in 1:(nrow(data_fichas)-1)) {
		lineas <- c(lineas,paste0("		[",data_fichas[i,1],
			",'",data_fichas[i,2],"'],"))
	}
	i <- nrow(data_fichas)
	lineas <- c(lineas,paste0("		[",data_fichas[i,1],
		",'",data_fichas[i,2],"']"))
	lineas <- c(lineas,"	]")
	lineas <- c(lineas,"</script>")
	lineas <- c(lineas,"</html>")
	writeLines(lineas,"fichas.html",sep="\r\n")
	#fin de generación html. Restaura el directorio de trabajo anterior
	setwd(backup_dir)
	svalue(barra_status) <- ""; Sys.sleep(0.001)
	
}
