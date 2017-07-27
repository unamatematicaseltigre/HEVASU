#Funciones relacionadas con la publicación de objetivos logrados
#Fecha : 01/04/2017
#Autor : José L. Romero
#-----------------------------------------------------------------------

nombre_archivo_datos_googleforms <- function(archivo_xls) {
	#genera el nombre del archivo con los datos para google forms
	#a partir del archivo Excel de OL del asesor
	archivo_dat <- gsub("(OL_[a-z]+).+\\.xls","\\1.dat",
		basename(archivo_xls))
	archivo_dat <- file.path(dirname(archivo_xls),archivo_dat)
	return(archivo_dat)
} #nombre_archivo_datos_googleforms()

#-----------------------------------------------------------------------

lee_archivo_datos_googleforms <- function(archivo_dat) {
	#lee el archivo de datos de googleforms y retorna una lista
	#con esos datos
	lineas <- readLines(archivo_dat)
	url <- gsub('^[ \t]*url: \"(.+)\",','\\1',lineas[1])
	lineas[2] <- gsub('^[ \t]*data: [{](.*?)[}],','\\1',lineas[2])
	cedula <- gsub(paste0('\"([a-zA-Z0-9.]*?)\": ',
		'cedula,\"([a-zA-Z0-9.-]*?)\": asignatura'),'\\1',lineas[2])
	asignatura <- gsub(paste0('\"([a-zA-Z0-9.]*?)\": ',
		'cedula,\"([a-zA-Z0-9.-]*?)\": asignatura'),'\\2',lineas[2])
	return(list(url=url,cedula=cedula,asignatura=asignatura))
} #lee_archivo_datos_googleforms()

#-----------------------------------------------------------------------

escribe_archivo_datos_googleforms <- function(lista_dgf,archivo_dat) {
	l1 <- paste0('\t\t\t\t\turl: \"',lista_dgf$url,'\",')
	l2 <- paste0('\t\t\t\t\tdata: {\"',lista_dgf$cedula,
		'\": cedula,\"',lista_dgf$asignatura,'\": asignatura},')
	lineas <- c(l1,l2)
	writeLines(lineas,archivo_dat)
} #escribe_archivo_datos_googleforms()

#-----------------------------------------------------------------------

crear_html_objetivos_logrados <- function(archivo_Excel,
	con_googleforms) {
	#el archivo_Excel es el archivo con los OL del asesor
	#con_googleforms es un booleano que indica si se va a registrar
	#la consulta.
	archivo_dat <- nombre_archivo_datos_googleforms(archivo_Excel)
	base_archivos <- file.path(dirname(archivo_Excel),
		gsub("^OL_","",basename(archivo_Excel)))
	base_archivos <- gsub("\\.xls$",".csv",base_archivos)
	archivo_ASIG <- file.path(dirname(base_archivos),
		paste0("ASIG_",basename(base_archivos)))
	archivo_EV <- file.path(dirname(base_archivos),
		paste0("EV_",basename(base_archivos)))
	#leer data asignaturas y matricula
	asignaturas <- read.table(archivo_ASIG,sep="\t",header=TRUE)
	matricula <- read.table(archivo_EV,sep="\t",header=TRUE)
	if (con_googleforms) googleforms <- readLines(archivo_dat)
	#comienza la generación del html
	lineas <- "<!doctype html>"
	lineas <- c(lineas,"<html lang='es'>")
	lineas <- c(lineas,"	<meta charset='utf-8'>")
	if (con_googleforms) {
		lineas <- c(lineas,paste0("<script src='https://ajax.googleapis.",
			"com/ajax/libs/jquery/1.11.3/jquery.min.js'>"))
		lineas <- c(lineas,"</script>")
	}
	lineas <- c(lineas,paste("<div align='justify'>Por favor indica",
		"tu número de cédula y el código de la asignatura cuyos",
		"resultados de evaluación quieres consultar:",
		"<br /><br /></div>"))
	lineas <- c(lineas,"<script>")
	lineas <- c(lineas,"	var matricula=new Array(0);")
	lineas <- c(lineas,"	var asignaturas=new Array(0);")
	lineas <- c(lineas,"")
	lineas <- c(lineas,"	function Consultar() {")
	lineas <- c(lineas,paste0("		var cedula=document.",
		"getElementById('cedula').value;"))
	lineas <- c(lineas,paste0("		var asignatura=document.",
		"getElementById('asignatura').value;"))
	lineas <- c(lineas,"		// verifica si es asignatura asesorada")
	lineas <- c(lineas,"		i=0")
	lineas <- c(lineas,paste("		while (i<asignaturas.length",
		"&& asignaturas[i][0]!=asignatura) {"))
	lineas <- c(lineas,"			i+=1")
	lineas <- c(lineas,"		}")
	lineas <- c(lineas,"		if (i==asignaturas.length) {")
	lineas <- c(lineas,paste("			alert('Asignatura no asesorada.",
		"Por favor ingrese otro código de asignatura.');"))
	lineas <- c(lineas,paste0("		document.getElementById",
		"('asignatura').innerHTML='';"))
	lineas <- c(lineas,"		} else {")
	lineas <- c(lineas,"			j=0")
	lineas <- c(lineas,paste("			while (j<matricula.length &&",
		"!(matricula[j][0]==cedula && matricula[j][4]==asignatura)) {"))
	lineas <- c(lineas,"				j+=1")
	lineas <- c(lineas,"			}")
	lineas <- c(lineas,"			if (j==matricula.length) {")
	lineas <- c(lineas,paste("				alert('La cédula '+cedula+'",
		"no aparece en la nómina para la asignatura indicada. Por",
		"favor ingrese otro código de asignatura o número de cédula.');"))
	lineas <- c(lineas,paste0("				document.getElementById",
		"('asignatura').innerHTML='';"))
	lineas <- c(lineas,paste0("				document.getElementById",
		"('cedula').innerHTML='';"))
	lineas <- c(lineas,"			} else {")
	lineas <- c(lineas,paste("				// recupera información de",
		"objetivos logrados en un arreglo"))
	lineas <- c(lineas,"				ol=matricula[j][5].split('');")
	lineas <- c(lineas,paste("			// recupera evaluaciones desde",
		"la data de la asignatura"))
	lineas <- c(lineas,paste0("				evaluaciones=asignaturas",
		"[i][1].split(',');"))
	lineas <- c(lineas,paste0("				presentadas=matricula[j]",
		"[6].split('');"))
	lineas <- c(lineas,"				// imprime la información de evaluación")
	lineas <- c(lineas,paste("				cadena='<table><tr><td",
		"width=\"120\" bgcolor=\"#ADD8E6\">';"))
	lineas <- c(lineas,paste0("				cadena+='Estudiante</td>",
		"<td width=\"360\" colspan=\"';"))
	lineas <- c(lineas,paste0("				cadena+=ol.length+'\">'+",
		"matricula[j][1];"))
	lineas <- c(lineas,paste0("				cadena+='</td></tr><tr>",
		"<td bgcolor=\"#ADD8E6\">Asignatura</td>';"))
	lineas <- c(lineas,paste0("				cadena+='<td colspan=\"'+",
		"(ol.length)+'\">'+asignatura+'</td></tr>';"))
	lineas <- c(lineas,paste0("				cadena+='<tr><td bgcolor",
		"=\"#ADD8E6\">Evaluaciones presentadas / corregidas</td>';"))
	lineas <- c(lineas,paste0("				cadena+='<td colspan=\"'+",
		"(ol.length)+'\">';"))
	lineas <- c(lineas,"				for (k=0; k<presentadas.length; k++) {")
	lineas <- c(lineas,paste("					if (presentadas[k]=='1')",
		"cadena+=evaluaciones[k]+' ';"))
	lineas <- c(lineas,"				}")
	lineas <- c(lineas,"				cadena+='</td></tr>';")
	lineas <- c(lineas,paste("				cadena+='<tr><td rowspan",
		"=\"2\" bgcolor=\"#ADD8E6\">Objetivos Logrados</td>';"))
	lineas <- c(lineas,"				for (k=1; k<ol.length+1; k++) {")
	lineas <- c(lineas,paste0("					cadena+='<td align=",
		"\"center\" width=\"30px\"  bgcolor=\"#ADD8E6\" style=",
		"\"border:1px solid black\">'+k+'</td>';"))
	lineas <- c(lineas,"				}")
	lineas <- c(lineas,"				cadena+='</tr><tr>';")
	lineas <- c(lineas,"				for (k=0; k<ol.length; k++) {")
	lineas <- c(lineas,"					if (ol[k]=='.')")
	lineas <- c(lineas,paste("						cadena+='<td",
		"style=\"border:1px solid black\"></td>';"))
	lineas <- c(lineas,paste0("					else cadena+='<td align",
		"=\"center\" style=\"border:1px solid black\">'+ol[k]+'</td>';"))
	lineas <- c(lineas,"				}")
	lineas <- c(lineas,"				cadena+='</tr>';")
	lineas <- c(lineas,paste0("				cadena+='<tr><td bgcolor",
		"=\"#ADD8E6\">Comentarios</td>';"))
	lineas <- c(lineas,paste0("				cadena+='<td colspan",
		"=\"'+(ol.length)+'\">';"))
	lineas <- c(lineas,paste("				cadena+='0: objetivo no",
		"logrado<br/>1: objetivo logrado<br/>&nbsp; : objetivo no",
		"respondido<br/>?: consultar con el asesor</td></tr>';"))
	lineas <- c(lineas,paste0("				cadena+='<tr><td bgcolor",
		"=\"#ADD8E6\">Feedback </td>';"))
	lineas <- c(lineas,paste0("				cadena+='<td colspan=\"'+",
		"(ol.length)+'\">'+matricula[j][7]+'</td></tr>';"))
	lineas <- c(lineas,"				cadena+='</table>';")
	lineas <- c(lineas,paste0("				document.getElementById",
		"('resultados').innerHTML=cadena;"))
	if (con_googleforms) {
		lineas <- c(lineas,paste("				// envia datos de",
			"consulta al formulario"))
		lineas <- c(lineas,"				$.ajax({")
			for (i in 1:2) {
				lineas <- c(lineas,googleforms[i])
			}
		lineas <- c(lineas,"					type: 'POST',")
		lineas <- c(lineas,"					dataType: 'xml',")
		lineas <- c(lineas,"					statusCode: {")
		lineas <- c(lineas,"						0: function () {},")
		lineas <- c(lineas,"						200: function () {} }")
		lineas <- c(lineas,"				});")
	}
	lineas <- c(lineas,"			}")
	lineas <- c(lineas,"		}")
	lineas <- c(lineas,"  }")
	lineas <- c(lineas,"</script>")
	lineas <- c(lineas,"")
	lineas <- c(lineas,"<form method='POST' name='datospersonales'>")
	lineas <- c(lineas,"<table>")
	lineas <- c(lineas,"<tbody>")
	lineas <- c(lineas,"<tr>")
	lineas <- c(lineas,paste("	<td bgcolor='#ADD8E6'",
		"width='120'>Cédula</td>"))
	lineas <- c(lineas,paste("	<td width='100'><input id='cedula'",
		"maxlength='8' size='12' type='text' /></td>"))
	lineas <- c(lineas,"	<td width='20'></td>")
	lineas <- c(lineas,paste("	<td bgcolor='#ADD8E6'",
		"width='80'>Asignatura</td>"))
	lineas <- c(lineas,paste("	<td width='40'><input id='asignatura'",
		"maxlength='3' size='4' type='text' /></td>"))
	lineas <- c(lineas,"</tr>")
	lineas <- c(lineas,"<tr>")
	lineas <- c(lineas,paste("	<td align='center'",
		"colspan='5'><input name='B1' onclick='Consultar()'",
		"type='button' value='Consultar Objetivos Logrados'",
		"/></td>"))
	lineas <- c(lineas,"</tr>")
	lineas <- c(lineas,"</tbody></table>")
	lineas <- c(lineas,"<div align='center' id='resultados'>")
	lineas <- c(lineas,"</div>")
	lineas <- c(lineas,"</form>")
	#definir asignaturas (JSON)
	lineas <- c(lineas,"<script>")
	lineas <- c(lineas,"	asignaturas=[")
	for (i in 1:nrow(asignaturas)) {
		lineas <- c(lineas,paste0("		[",asignaturas[i,1],",'",
			asignaturas[i,2],"']",ifelse(i==nrow(asignaturas)," ]",",")))
	}
	#definir matricula (JSON)
	lineas <- c(lineas,"	matricula=[")
	for (i in 1:nrow(matricula)) {
		if (!is.na(matricula[i,1])) {
			lineas <- c(lineas,paste0("		[",matricula[i,1],",'",
				matricula[i,2],"','",matricula[i,3],"',",matricula[i,4],
				",",matricula[i,5],",'",matricula[i,6],"','",matricula[i,7],
				"','",matricula[i,8],"']",
				ifelse(i==nrow(matricula)," ]",",")))
		}
	}
	lineas <- c(lineas,"</script>")
	lineas <- c(lineas,"</html>")
	archivo_html <- file.path(dirname(archivo_Excel),
		paste0("OL_",gsub("\\.csv$",".html",basename(base_archivos))) )
	writeLines(lineas,archivo_html,sep="\r\n")
} #crear_html_objetivos_logrados()
