#Funciones para generar los certificados del Curso Introductorio
#Fecha : 05/01/2014
#Autor : José L. Romero
#requiere los siguientes paquetes: rJava, gWidgets, RGtk2
#usa también LaTeX/texlive
#-----------------------------------------------------------------------

generar_certificados <- function(archivo_Excel) {
	
	mostrar_progreso <- function(progreso) {
	#Función para mostrar la barra de progreso
		mensaje <- paste("Generando certificados de aprobación: [",
			paste(rep("█",progreso),collapse=""),
			paste(rep("░",25-progreso),collapse=""),"]",sep="")
		svalue(barra_status) <- mensaje; Sys.sleep(0.001)
	}
	
	mostrar_progreso(0)
	#archivo_Excel es por ejemplo "OL_nancybello_2013-2.xls"
	#lee la data del introductorio (archivo csv)
	archivo_csv <- file.path(dirname(archivo_Excel),
		gsub("\\.xls$|\\.XLS$",".csv",basename(archivo_Excel)) )
	acsv <- file(archivo_csv,open="r")
	csv_lineas <- readLines(con=acsv,n=4)
	close(acsv)
	nomina <- read.table(file=archivo_csv,header=TRUE,skip=4,sep="\t")
	#filtra la nomina - solo los aprobados
	nomina <- subset(nomina,subset=aprobado,select=c("cedula","nombre"))
	#obtener la información de la ciudad de esa cl-ua
	codigo_clua <- paste(strsplit(csv_lineas[3],split=" - ")[[1]],
		collapse="")
	ciudades <- read.table(file.path(dir_programa,"certificados",
		"ciudades.csv"), sep="\t",header=TRUE)
	ciudades$cloa <- sapply(ciudades$cloa,function(i) 
		sprintf("%04d",i) )
	nombre_ciudad <- as.character(ciudades$ciudad)[ciudades$cloa==
		codigo_clua]
	#lee el archivo tex original
	archivo_tex <- file(file.path(dir_programa,"certificados",
		"certificado.tex"), open="r")
	original <- readLines(archivo_tex)
	close(archivo_tex)
	#cambia los datos que son iguales para este lote de certificados
	#centrolocal
	original[25] <- paste0("\\newcommand{\\centrolocal}{",centrolocal,
		"}")
	#clua
	original[26] <- paste0("\\newcommand{\\clua}{",codigo_clua,"}")
	#lapsoguion
	original[29] <- paste0("\\newcommand{\\lapsoguion}{",csv_lineas[4],
		"}")
	#lapso
	original[30] <- paste0("\\newcommand{\\lapso}{",gsub("-","",
		csv_lineas[4]), "}")
	#ciudad donde está ubicado el CL/unidad de apoyo
	original[31] <- paste0("\\newcommand{\\ciudad}{",nombre_ciudad,"}")
	#dia
	original[32] <- paste0("\\newcommand{\\dia}{",format(Sys.Date(),
		"%d"),"}")
	#mes
	meses <- c("enero","febrero","marzo","abril","mayo","junio","julio",
		"agosto", "septiembre","octubre","novimebre","diciembre")
	original[33] <- paste0("\\newcommand{\\mes}{",meses[as.numeric(
		format(Sys.Date(), "%m"))],"}")
	#año
	original[34] <- paste0("\\newcommand{\\anho}{",format(Sys.Date(),
		"%Y"),"}")
	#orientador
	original[35] <- paste0("\\newcommand{\\orientador}{",csv_lineas[2],
		"}")
	#tituloorientador
	original[36] <- paste0("\\newcommand{\\tituloorientador}{",
		csv_lineas[1],"}")
	#copia los archivos makebarcode.sty y saman.jpg al directorio donde
	#se van a generar los certificados
	archivo_sty <- file.path(dir_programa,"certificados",
		"makebarcode.sty")
	archivo_saman <- file.path(dir_programa,"certificados","saman.jpg")
	system(paste("cp",archivo_saman,dirname(archivo_Excel)))
	system(paste("cp",archivo_sty,dirname(archivo_Excel)))
	#guarda el directorio de trabajo
	#utilizará como directorio de trabajo el directorio del archivo Excel.
	#ahí se crearán todos los certificados.
	backup_dir <- getwd()
	setwd(dirname(archivo_Excel))
	filas <- nrow(nomina)
	for (i in 1:filas) {
		#prepara una copia del archivo tex original
		lineas <- original
		#cambia los datos para cada estudiante
		#nombre del aspirante
		lineas[27] <- paste0("\\newcommand{\\aspirante}{",
			nomina$nombre[i],"}")
		#cédula del aspirante
		lineas[28] <- paste0("\\newcommand{\\cedula}{",nomina$cedula[i],
			"}")
		#copia el archivo tex
		nombre_archivo <- file.path(dirname(archivo_Excel),
			paste(sprintf("%08d",nomina$cedula[i]),".tex",sep=""))
		archivo_tex <- file(nombre_archivo,open="w")
		writeLines(lineas,con=archivo_tex)
		close(archivo_tex)
		system(paste("pdflatex",nombre_archivo))
		#borra los archivos .tex .aux y .log de ese estudiante
		unlink(nombre_archivo)
		unlink(gsub("tex$","aux",nombre_archivo))
		unlink(gsub("tex$","log",nombre_archivo))
		mostrar_progreso(round(i/filas*25))
	}
	#borra los archivos saman.jpg y makebarcode.sty
	unlink("saman.jpg")
	unlink("makebarcode.sty")
	#vuelve al directorio de trabajo anterior
	setwd(backup_dir)
	svalue(barra_status) <- ""; Sys.sleep(0.001)
} #generar_certificados

#-----------------------------------------------------------------------

publicar_certificados <- function(archivo_csv,archivo_tsv) {
	#función para generar el código HTML para la publicación
	#de los certificados de aprobación del curso introductorio.
	#el atchivo HTML se generará en el mismo directorio que
	#archivo_csv.
	datos_CI <- read.table(archivo_csv,skip=4,
		as.is=TRUE,header=TRUE,sep="\t")
	nombre <- paste(sprintf(datos_CI$cedula,fmt="%08d"),"-",
		datos_CI$nombre)
	datos_CI <- data.frame(cedula=datos_CI$cedula,nombre=nombre)
	lista_archivos <- read.table(archivo_tsv,sep="\t",
		header=TRUE)
	mezclado <- merge(lista_archivos,datos_CI,by="cedula")
	#comienza la generación del html
	lineas <- "<!doctype html>"
	lineas <- c(lineas,"<html lang='es'>")
	lineas <- c(lineas,"	<meta charset='utf-8'>")
	lineas <- c(lineas,paste0("<div align='justify'>",
		"Desde el formulario a continuación podrá descargar el ",
		"certificado de aprobación del Curso Introductorio, lapso ",
		"xxxx-x. Recuerde imprimir su certificado en opalina blanca ",
		"tamaño carta, y pasar por la sede de la Universidad para ",
		"la firma y el sello, de lo contrario no tiene validez.",
		"<br /><br /></div>"))
	lineas <- c(lineas,"<!-- more -->")
	lineas <- c(lineas,"<script type='text/javascript'>")
	lineas <- c(lineas,"function cargacertificado() {")
	lineas <- c(lineas,"	var textstring = '';")
	lineas <- c(lineas,"	var box = document.forms['FormCedulaCertificado'].elements[0];")
	lineas <- c(lineas,"	if (!box.value) {")
  lineas <- c(lineas,"		alert('No ha indicado la cédula');")
	lineas <- c(lineas,"		box.focus();")
	lineas <- c(lineas,"		return;")
	lineas <- c(lineas,"	}")
	lineas <- c(lineas,"	var win=window.open(box.value,'_blank');")
	lineas <- c(lineas,"	win.focus();")
	lineas <- c(lineas,"}")
	lineas <- c(lineas,"</script>")
	lineas <- c(lineas,"<form action='#' name='FormCedulaCertificado' onsubmit='cargacertificado(); return false'>")
	lineas <- c(lineas,"	<table align='center' style='width: 400px;'>")
	lineas <- c(lineas,"  <tbody>")
	lineas <- c(lineas,"	<tr>")
	lineas <- c(lineas,"  <td><select id='Cedula'>")
	lineas <- c(lineas,paste0("      <option value='",
		as.character(mezclado$url),"'>",as.character(mezclado$nombre),
		"</option>"))
	lineas <- c(lineas,"    </select></td>")
	lineas <- c(lineas,"  </tr>")
	lineas <- c(lineas,paste("  <tr><td><input type='submit'",
		"value='Ver certificado' /></td></tr>"))
	lineas <- c(lineas,"  </tbody></table>")
	lineas <- c(lineas,"</form>")
	lineas <- c(lineas,"</html>")
	writeLines(lineas,file.path(dirname(archivo_csv),"certificados.html"),
		sep="\r\n")
} #publicar_certificados()
