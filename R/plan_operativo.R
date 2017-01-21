#Funciones para generar los reportes de planificación
#Fecha : 24/06/2014
#Autor : José L. Romero
#requiere los siguientes paquetes: rJava, gWidgets, RGtk2
#-----------------------------------------------------------------------

plan_operativo <- list(
	list(
		codigo="020202",
		descrp="T.S.U. en Educación Integral",
		carreras=c(430),
		materias=c(476,477,478,479,480,440,481,483,484,485,486,487,488,
			489,491,492,493,494,495,434,497,457,592,423,490,498,524,571) ),
	list(
		codigo="020203",
		descrp="T.S.U. en Mantenimiento de Sistemas Informáticos",
		carreras=c(237),
		materias=c(327,323,601,324,349,358,370,602,311,330,335,371,372,336,
			342,373,374,375,258,343,344,345,380) ),
	list(
		codigo="020204",
		descrp="T.S.U. en Higiene y Seguridad Industrial",
		carreras=c(281),
		materias=c(200,201,208,259,209,251,252,300,733,202,231,255,326,737,
			207,258,315,349,738,257) ),
	list(
		codigo="020302",
		descrp="Matemática",
		carreras=c(126),
		materias=c(754,323,300,756,757,758,759,760,761,762,763,764,765,
			766,767,768,769,770,771,772,773,774,775,776,778,779,780,781,
			782,783) ),
	list(
		codigo="020303",
		descrp="Ingeniería de Sistemas",
		carreras=c(236),
		materias=c(327,323,300,324,641,733,315,326,330,735,737,311,332,333,
			738,739,305,306,335,348,222,316,336,337,225,235,338,342,339,341,
			205,228,241,310,312,321,334,347) ),
	list(
		codigo="020304",
		descrp="Ingeniería Industrial",
		carreras=c(280),
		materias=c(200,208,323,209,300,641,733,201,231,326,735,737,222,232,
			315,738,739,205,206,233,234,655,202,203,225,235,236,204,207,216,
			223,237,238,228,240,241,305,337,348) ),
	list(
		codigo="020305",
		descrp="Educación Integral",
		carreras=c(440),
		materias=c(410,516,570,408,416,524,571,405,412,414,428,530,420,
			421,440,485,576,411,427,471,488,489,536,431,433,434,472,480,
			437,444,468,473,481,454,457,465,474,578,451,469,475,491) ),
	list(
		codigo="020306",
		descrp="Educación Mención Matemática",
		carreras=c(508),
		materias=c(516,570,530,571,749,754,300,750,752,326,545,751,
			753,542,547,551,747,552,575,748,755,532,576,577,578,579,580) ),
	list(
		codigo="020307",
		descrp="Educación Mención Dificultades del Aprendizaje",
		carreras=c(521),
		materias=c(570,581,516,534,571,582,583,479,530,584,585,586,520,
			587,588,589,578,590,591,592,512,524,536,593,517,576,595,594,597,
			596) ),
	list(
		codigo="020308",
		descrp="Educación Mención Preescolar",
		carreras=c(542),
		materias=c(050,051,516,578,052,053,530,570,054,055,534,571,
			056,057,517,562,058,059,060,559,061,524,536,564,062,064,560,
			576,063) ),
	list(
		codigo="020310",
		descrp="Contaduría Pública",
		carreras=c(610),
		materias=c(601,631,602,632,641,605,633,637,642,734,606,639,650,651,
			745,638,649,653,746,618,665,669,673,663,666,691,634,636,661,
			692,697) ),
	list(
		codigo="020311",
		descrp="Administración de Empresas",
		carreras=c(612),
		materias=c(601,631,602,632,641,605,637,642,734,606,650,671,745,613,
			625,665,672,746,603,604,651,663,666,614,653,661,669,607,608,638,
			681,699) ),
	list(
		codigo="020312",
		descrp="Administración de Empresas - Mención Riesgos y Seguros",
		carreras=c(613),
		materias=c(601,631,602,615,617,605,616,637,734,606,641,644,604,613,
			625,671,745,621,646,665,672,746,619,620,654,661,743,607,614,622,
			645,648,696) ),
	list(
		codigo="020313",
		descrp="Área Interdisciplinaria (Estudios Generales)",
		carreras=c(126,236,237,280,281,430,440,508,521,542,610,612,613),
		materias=c(102,103,104,105,106,107,108,109,111,113,115,116,117,118,
			119,120,121,122,126,175,176,177,178,179) ),
	list(
		codigo="060202",
		descrp="Programa de Asesoría Académica y Desempeño Estudiantil",
		carreras=c(126,236,237,280,281,430,440,508,521,542,610,612,613),
		materias=c(000) ) )

tabla_plan <- data.frame()
for (i in plan_operativo) {
	carr_mat <- as.character(sapply(i$carreras,function(j) 
		paste0(j,sprintf("%03d",i$materias)) ) )
	tabla_plan <- rbind(tabla_plan,
			data.frame(carr_mat=carr_mat,
			codigo=rep(i$codigo,length(carr_mat)),
			stringsAsFactors = FALSE) )
}

id_plan <- function(carrera,asignatura,archivo) {
#indica el id del plan operativo correspondiente a una carrera /
#asignatura
	carr_mat <- paste0(carrera,sprintf("%03d",asignatura))
	if (carr_mat %in% tabla_plan$carr_mat) {
		return(tabla_plan[which(carr_mat==tabla_plan$carr_mat),2])
	} else
		return(paste("ERROR: carrera",substr(carr_mat,1,3),
			"y materia",substr(carr_mat,4,6),"inválidas.<br />Archivo:",
			archivo))
}

descrp_plan <- function(codigo) {
#retorna la descripción del renglón del plan_operativo correspondiente
#al código
	return(plan_operativo[codigo==
		sapply(plan_operativo,function(i) i$codigo)][[1]]$descrp)
}

id_producto <- function(tipo,archivo) {
#retorna el código del producto según el tipo de asesoría
#(aplica sólo a asesorías)
	return(switch(tipo,
		'J' = '37', 'C' = '45', 'E' = '46', 'T' = '50b', 'G' = '59',
		'I' = '60', '@' = '61', 
		paste0("ERROR: tipo de asesoría '",tipo,"' inválido.<br />",
		"Archivo: ",archivo," Hoja: 'Asesorías'") )) 
}

id_servicio <- function(tipo,archivo) {
#retorna en un vector con names el codigo_plan y el producto
#de acuerdo al tipo de servicio de orientación indicado en el
#argumento
	return(switch(tipo,
		EQU=list(codigo_plan="060201",
			subaccion="02",producto="126"),	#Equivalencias
		AVD=list(codigo_plan="060201",
			subaccion="02",producto="126"),	#Actas de v. de d.	
		CCA=list(codigo_plan="060201",
			subaccion="02",producto="126"),	#Cambio de carrera	
		CCL=list(codigo_plan="060201",
			subaccion="02",producto="126"),	#Cambio de CL	
		BEC=list(codigo_plan="060301",
			subaccion="01",producto="129"),	#Becas
		AYU=list(codigo_plan="060301",
			subaccion="02",producto="317"),	#Ayudantías
		PRP=list(codigo_plan="060302",
			subaccion="01",producto="130"),	#Preparadurías
		FAM=list(codigo_plan="060401",
			subaccion="01",producto="89"),	#FAMES
		list(codigo_plan=paste0("ERROR: servicio '",tipo,"' no definido.",
			"<br />Archivo: ",archivo," Hoja: 'Servicios de Orientación'"),
			subaccion="00",producto="00")) )
}

generar_reporte_planificacion <- function() {
#función principal de generación del reporte de planificación

	generar_encabezado_pagina <- function(archivo_html,pagina_nueva) {
	#Esta función genera el HTML para el encabezado de las páginas
	#del informe para la planificación operativa
	#Se supone que logo_html contiene ya el código HTML para la imágen
	#incrustada
		writeLines(c(
			'<DIV ALIGN=CENTER>',
			ifelse(pagina_nueva,
				paste('<TABLE WIDTH=690 CELLSPACING=0 CELLPADDING=0',
					'STYLE="page-break-before: always">'),
				'<TABLE WIDTH=690 CELLSPACING=0 CELLPADDING=0>'),
			'  <COL WIDTH=70>',
			'  <COL WIDTH=600>',
			'  <TR>',
			paste0('    <TD VALIGN=MIDDLE ALIGN=CENTER>',logo_html,'</TD>'),
			'    <TD VALIGN=MIDDLE>',
			paste0('      <P STYLE="margin-bottom: 0cm"><FONT SIZE=2>',
				'UNIVERSIDAD NACIONAL ABIERTA</FONT></P>'),
			paste0('      <P STYLE="margin-bottom: 0cm"><FONT SIZE=2>',
				'VICE-RECTORADO ACADEMICO</FONT></P>'),
			ifelse(centrolocal=="NIVEL CENTRAL",
				paste0('      <P STYLE="margin-bottom: 0cm"><FONT SIZE=2>',
					'NIVEL CENTRAL</FONT></P>'),
				paste0('      <P STYLE="margin-bottom: 0cm"><FONT SIZE=2>',
					'CENTRO LOCAL ',centrolocal,'</FONT></P>')),
			'    </TD>',
			'  </TR>',
			'</TABLE>','</DIV>'),con=archivo_html)
	} #generar_encabezado_pagina

	generar_portada_rpo <- function(archivo_html,ano) {
		writeLines(c(
			'<!DOCTYPE HTML PUBLIC>',
			'<HTML>','<HEAD>',
			paste0('<META HTTP-EQUIV="CONTENT-TYPE" CONTENT=',
				'"text/html; charset=utf-8">'),
			'<TITLE>Reporte de actividades académicas</TITLE>',
			paste0('<META NAME="GENERATOR" CONTENT=',
				'"HEVASU 1.0.0">'),
			'<STYLE TYPE="text/css">','  <!--',
			paste0('  DIV,TABLE,THEAD,TBODY,TFOOT,TR,TH,TD,P ',
				'{ font-family:"sans-serif"; font-size:x-small; padding:2px; ',
				'margin:2px }'),
			'  BODY {width: 21.6cm; height: 25cm}',
			'  H1, H2 {font-family: "sans-serif"; font-size:large}',
			'  @page {margin: 2cm }',
			'  -->','</STYLE>','</HEAD>',
			'<BODY LANG="es-VE" DIR="LTR">'),con=archivo_html)
		generar_encabezado_pagina(archivo_html,FALSE)
		writeLines(c(
			'<DIV></P>',
			rep('<BR>',24),
			paste0('  <P ALIGN=CENTER STYLE="margin-bottom: 0cm"><FONT SIZE=4>',
				'<B>REPORTE DE RENDICION DE LA PLANIFICACIÓN OPERATIVA',
				'</B></FONT></P>'),
			paste0('  <P ALIGN=CENTER STYLE="margin-bottom: 0cm"><FONT SIZE=4>',
				'AÑO ',ano,'</FONT></P>'),
			rep('<BR>',25),
			paste0('  <P ALIGN=CENTER STYLE="margin-bottom: 0cm"><FONT SIZE=2>',
				'Reporte generado por HEVASU: ',
				format(Sys.time(),"%d de %B de %Y %H:%M"),'</FONT></P>'),
			'</DIV>'),con=archivo_html)
	} #generar_portada_rpo

	generar_encabezado_tabla_carreras <- function(archivo_html) {
		writeLines(c(
			rep('<BR>',2),
			'  <DIV ALIGN=CENTER>',
			'  <TABLE WIDTH="500" CELLSPACING="0">',
			'  <COL WIDTH="120">',
			rep('  <COL WIDTH="36">',11),
			'  <TR>',
			paste('    <TD STYLE="BORDER-TOP: 1PX SOLID #000000;',
				'BORDER-BOTTOM: NONE; BORDER-LEFT: 1PX SOLID #000000;',
				'BORDER-RIGHT: 1PX SOLID #000000"><P ALIGN="CENTER">',
				'<FONT COLOR=#000080><FONT SIZE="4"><B>',
				'Subacción</B></FONT></FONT></P></TD>'),
			paste('    <TD COLSPAN=8 BGCOLOR="#000080" STYLE="BORDER-TOP:',
				'1PX SOLID #000000; BORDER-BOTTOM: 1PX SOLID #FFFFFF;',
				'BORDER-LEFT: 1PX SOLID #FFFFFF; BORDER-RIGHT: NONE">',
				'<P ALIGN="CENTER"><FONT COLOR="#FFFFFF"><FONT SIZE="4">',
				'<B>02</B></FONT></FONT></P></TD>'),
			paste('    <TD COLSPAN="3" BGCOLOR="#000080"',
				'STYLE="BORDER-TOP: 1PX SOLID #000000; BORDER-RIGHT:',
				'1PX SOLID #000000"><P ALIGN="CENTER"><FONT',
				'COLOR="#FFFFFF"><FONT SIZE="4"><B>03</B></FONT></FONT>',
				'</P></TD>'),
			'</TR>',
			'<TR>',
			paste('    <TD STYLE="BORDER-TOP: NONE; BORDER-BOTTOM: 1PX SOLID',
				'#000000; BORDER-LEFT: 1PX SOLID #000000; BORDER-RIGHT: 1PX',
				'SOLID #000000; PADDING: 0CM"><P ALIGN="CENTER">',
				'<FONT SIZE="2"><FONT COLOR="#0000FF"><B>Producto</B>',
				'</FONT>/<FONT COLOR="#000033">',
				'<B>Més</B></FONT></FONT></P></TD>'),
			paste0('    <TD BGCOLOR="#0066FF" STYLE="BORDER-TOP: 1PX SOLID',
			'#FFFFFF; BORDER-BOTTOM: 1PX SOLID #000000; BORDER-LEFT: 1PX ',
			'SOLID #FFFFFF; BORDER-RIGHT: 1PX SOLID #FFFFFF; PADDING: 0CM">',
				'<P ALIGN="CENTER"><FONT SIZE="2"><B>',c('37','45','46','50',
				'50b','59','60','61','22','48'),'</B></FONT></P></TD>'),
			paste0('    <TD BGCOLOR="#0066FF" STYLE="BORDER-TOP: 1PX SOLID',
				'#FFFFFF; BORDER-BOTTOM: 1PX SOLID #000000; BORDER-LEFT: 1PX ',
				'SOLID #FFFFFF; BORDER-RIGHT: 1PX SOLID #000000; PADDING: 0CM">',
				'<P ALIGN="CENTER"><FONT SIZE="2"><B>185</B></FONT></P></TD>'),			
			'</TR>'
			),con=archivo_html)
	}#generar_encabezado_tabla_carreras

	generar_fila_tabla <- function(archivo_html,mes,celdas) {
		writeLines(c(
			'<TR>',
			paste('<td bgcolor="#000033" style="border: 1px solid #000000">',
				'<p align="center"><font color="#ffffff"><b>',mes,'</b>',
				'</font></p></td>')),con=archivo_html)
		for (i in 1:length(celdas)) {
			if (i%%2==0) {
				writeLines(
					paste0('<TD STYLE="BORDER: 1PX SOLID #000000">',
						'<P ALIGN="CENTER">',celdas[i],'</P></TD>'),
					con=archivo_html)
			} else writeLines(
					paste0('<TD BGCOLOR="#E6E6FF" STYLE="BORDER: 1PX SOLID',
						'#000000"><P ALIGN="CENTER">',celdas[i],'</P></TD>'),
					con=archivo_html)
			} # for cada celda
		writeLines('</TR>',con=archivo_html)	
	}# generar_fila_tabla

	generar_pie_tabla <- function(archivo_html,celdas) {
		writeLines(c(
			paste0('<TD BGCOLOR="#003366" STYLE="BORDER: 1PX SOLID #000000">',
				'<P ALIGN="CENTER"><FONT COLOR="#FFFFFF"><B>Totales</B>',
				'</FONT></P></TD>'),
			paste0('<TD BGCOLOR="#003366" STYLE="BORDER: 1PX SOLID #000000">',
				'<P ALIGN="CENTER"><FONT COLOR="#FFFFFF"><B>',celdas,'</B>',
				'</FONT></P></TD>')),
			con=archivo_html)
	}# generar_pie_tabla

	tabla_carrera <- function(codigo_plan) {
		tabla_tmp <- tabla_plan[,,"02",codigo_plan]
		productos <- dimnames(tabla_tmp)$producto
		matriz_tmp <- cbind(
			"37"=if ("37" %in% productos) tabla_tmp[,"37"] else rep(0,12),
			"45"=if ("45" %in% productos) tabla_tmp[,"45"] else rep(0,12),
			"46"=if ("46" %in% productos) tabla_tmp[,"46"] else rep(0,12),
			"50"=if ("50" %in% productos) tabla_tmp[,"50"] else rep(0,12),
			"50b"=if ("50b" %in% productos) tabla_tmp[,"50b"] else rep(0,12),
			"59"=if ("59" %in% productos) tabla_tmp[,"59"] else rep(0,12),
			"60"=if ("60" %in% productos) tabla_tmp[,"60"] else rep(0,12),
			"61"=if ("61" %in% productos) tabla_tmp[,"61"] else rep(0,12) )
		tabla_tmp <- tabla_plan[,,"03",codigo_plan]
		matriz_tmp <- cbind(matriz_tmp,
			"22"=if ("185" %in% productos) tabla_tmp[,"185"] else rep(0,12),
			"48"=if ("48" %in% productos) tabla_tmp[,"48"] else rep(0,12),
			"185"=if ("185" %in% productos) tabla_tmp[,"185"] else rep(0,12)
			)
		return(matriz_tmp)
	} #tabla_carrera
	
	generar_pagina_carrera <- function(archivo_html,codigo_plan) {
		#genera la página para la carrera según el código_plan
		#imprime el encabezado en una página nueva
		generar_encabezado_pagina(archivo_html,TRUE)
		writeLines(rep('<BR>',1),con=archivo_html)
		#imprime las líneas de encabezado para esta carrera
		writeLines(c(
		'<DIV ALIGN="CENTER">',
		'<TABLE WIDTH=690 CELLSPACING=0 CELLPADDING=0>',
		'  <COL WIDTH=70>',
		'  <COL WIDTH=600>',
		'  <TR>',
		paste0('    <TD><P ALIGN=CENTER><FONT SIZE=5>',
			substr(codigo_plan,1,4),'</FONT></P></TD>'),
		paste0('    <TD><P ALIGN=LEFT><FONT SIZE=4>',
			ifelse(substr(codigo_plan,3,4)=="02",
				'Formación de estudiantes en carrera: T.S.U.',
				paste('Formación de estudiantes en carrera:',
					'Licenciatura o Equivalente')),
			'</FONT></P></TD>'),
		'  </TR>',
		'  <TR>',
		paste0('    <TD><P ALIGN=CENTER><FONT SIZE=4>',codigo_plan,
			'</FONT></P></TD>'),
		paste0('    <TD><P ALIGN=LEFT><FONT SIZE=3>',
			descrp_plan(codigo_plan),'</FONT></P></TD>'),
		'  </TR>',
		'</TABLE></DIV>'),con=archivo_html)
		#imprime el encabezado de la tabla común a todas las carreras
		generar_encabezado_tabla_carreras(archivo_html)
		#genera la tabla para esa carrera e imprímela
		matriz_carrera <- tabla_carrera(codigo_plan)
		for (mes in rownames(matriz_carrera))
			generar_fila_tabla(archivo_html,mes,matriz_carrera[mes,])
		generar_pie_tabla(archivo_html,colSums(matriz_carrera))
		writeLines('</TABLE></DIV>',con=archivo_html)
		writeLines(rep('<BR>',3),con=archivo_html)
		writeLines(pie_carreras_plan_html,con=archivo_html)
	} #generar_pagina_carrera
	
	generar_pagina_060201 <- function(archivo_html) {
	#genera la página para 060201 (servicios de orientación)
		generar_encabezado_pagina(archivo_html,TRUE)
		writeLines(encabezado_060201_plan_html,con=archivo_html)
		tabla_tmp <- tabla_plan[,,"02","060201"]
		tabla_060201 <- cbind("126"=tabla_tmp[,"126"])
		for (mes in rownames(tabla_060201))
			generar_fila_tabla(archivo_html,mes,tabla_060201[mes,])
		generar_pie_tabla(archivo_html,colSums(tabla_060201))
		writeLines('</TABLE></DIV>',con=archivo_html)
		writeLines(rep('<BR>',3),con=archivo_html)
		writeLines(pie_060201_plan_html,con=archivo_html)
	}# generar_pagina_060201

	generar_pagina_060202 <- function(archivo_html) {
	#genera la página para 060202 (orientación académica)
		generar_encabezado_pagina(archivo_html,TRUE)
		writeLines(encabezado_060202_plan_html,con=archivo_html)
		tabla_tmp <- tabla_plan[,,"02","060202"]
		productos <- dimnames(tabla_tmp)$producto
		tabla_060202 <- cbind(
			"37"=if ("37" %in% productos) tabla_tmp[,"37"] else rep(0,12),
			"45"=if ("45" %in% productos) tabla_tmp[,"45"] else rep(0,12),
			"46"=if ("46" %in% productos) tabla_tmp[,"46"] else rep(0,12),
			"50"=if ("50" %in% productos) tabla_tmp[,"50"] else rep(0,12),
			"50b"=if ("50b" %in% productos) tabla_tmp[,"50b"] else rep(0,12),
			"59"=if ("59" %in% productos) tabla_tmp[,"59"] else rep(0,12),
			
			"60"=if ("60" %in% productos) tabla_tmp[,"60"] else rep(0,12),
			"61"=if ("61" %in% productos) tabla_tmp[,"61"] else rep(0,12) )
		tabla_tmp <- tabla_plan[,,"03","060202"]
		tabla_060202 <- cbind(tabla_060202,
			"48"=if ("48" %in% productos) tabla_tmp[,"48"] else rep(0,12) )
		for (mes in rownames(tabla_060202))
			generar_fila_tabla(archivo_html,mes,tabla_060202[mes,])
		generar_pie_tabla(archivo_html,colSums(tabla_060202))
		writeLines('</TABLE></DIV>',con=archivo_html)
		writeLines(rep('<BR>',3),con=archivo_html)
		writeLines(pie_060202_plan_html,con=archivo_html)
	}# generar_pagina_060202
	
	generar_pagina_06 <- function(archivo_html) {
		#genera la página para 06 
		#(servicios socio-economicos de orientación)
		generar_encabezado_pagina(archivo_html,TRUE)
		writeLines(encabezado_06_plan_html,con=archivo_html)
		if ("060301" %in% codigos_en_plan) {
			tabla_tmp <- tabla_plan[,,"01","060301"]
			productos <- dimnames(tabla_tmp)$producto
			tabla_06 <- cbind(
				"129"=if ("129" %in% productos) tabla_tmp[,"129"]
				else rep(0,12) )
			tabla_tmp <- tabla_plan[,,"02","060301"]
			tabla_06 <- cbind(tabla_06,
				"317"=if ("317" %in% productos) tabla_tmp[,"317"]
				else rep(0,12) )
		} else
			tabla_06 <- cbind("129"=rep(0,12),"317"=rep(0,12))
		if ("060302" %in% codigos_en_plan) {
			tabla_tmp <- tabla_plan[,,"01","060302"]
			productos <- dimnames(tabla_tmp)$producto
			tabla_06 <- cbind(tabla_06,
				"130"=if ("130" %in% productos) tabla_tmp[,"130"]
				else rep(0,12) )
		} else
			tabla_06 <- cbind(tabla_06,"130"=rep(0,12))
		if ("060401" %in% codigos_en_plan) {
			tabla_tmp <- tabla_plan[,,"01","060401"]
			productos <- dimnames(tabla_tmp)$producto
			tabla_06 <- cbind(tabla_06,
				"89"=if ("89" %in% productos) tabla_tmp[,"89"] else rep(0,12) )
		} else
			tabla_06 <- cbind(tabla_06,"89"=rep(0,12))
		#ahora "imprime" la tabla
		for (mes in rownames(tabla_06))
			generar_fila_tabla(archivo_html,mes,tabla_06[mes,])
		generar_pie_tabla(archivo_html,colSums(tabla_06))
		writeLines('</TABLE></DIV>',con=archivo_html)
		writeLines(rep('<BR>',3),con=archivo_html)
		writeLines(pie_06_plan_html,con=archivo_html)
	}# generar_pagina_06
	
	generar_pagina_libros_consultados <- function(archivo_html) {
	#esta función genera la página con la lista de archivos
	#de Excel consultados
		dir_archivo <- "lista_archivos.log"
		archivo <- file(dir_archivo,"r",blocking=FALSE)
		lineas_libros <- readLines(con=archivo)
		close(archivo)
		unlink(dir_archivo)
		generar_encabezado_pagina(archivo_html,TRUE)
		writeLines(encabezado_listado_plan_html,con=archivo_html)
		num_linea <- 1
		for (linea_texto in lineas_libros) {
			writeLines(paste0(linea_texto,'<BR>'),con=archivo_html)
			num_linea <- num_linea+1
			if (num_linea>40 & linea_texto=="") {
				writeLines(pie_listado_plan_html,con=archivo_html)
				generar_encabezado_pagina(archivo_html,TRUE)
				writeLines(encabezado_listado_plan_html,con=archivo_html)
				num_linea <- 1
			}#if se ha comenzado una página nueva				
		}# for cada linea de lista_archivos.log
		if (num_linea>1) writeLines(pie_listado_plan_html,con=archivo_html)
	}# generar_pagina_libros_consultados
	
	generar_pagina_errores <- function(archivo_html) {
	#esta función genera la página con el listado de errores (si los hay)
		dir_archivo <- "errores_plan.log"
		archivo <- file(dir_archivo,"r",blocking=FALSE)
		lineas_errores <- readLines(con=archivo)
		close(archivo)
		unlink(dir_archivo)
		generar_encabezado_pagina(archivo_html,TRUE)
		writeLines(encabezado_errores_plan_html,con=archivo_html)
		num_linea <- 1
		for (linea_texto in lineas_errores) {
			writeLines(paste0(linea_texto,'<BR>'),con=archivo_html)
			num_linea <- num_linea+1
			if (num_linea>40 & linea_texto=="") {
				writeLines(pie_listado_plan_html,con=archivo_html)
				generar_encabezado_pagina(archivo_html,TRUE)
				writeLines(encabezado_errores_plan_html,con=archivo_html)
				num_linea <- 1
			}#if se ha comenzado una página nueva				
		}# for cada linea de lista_archivos.log
		if (num_linea>1) writeLines(pie_listado_plan_html,con=archivo_html)
	}# generar_pagina_errores

	#---------- comienzo de "generar_reporte_planificacion" -------------
	#genera los archivos csv tras procesar los libros Excel
	error <- generar_archivos_plan()
	if (error==1) {
		gmessage(paste("No hay libros excel para consultar.\n",
			"¿Ha seleccionado la carpeta de trabajo\n",
			"donde están esos archivos?"),icon="warning")
		return(0)
	}#if hay archivos
	#mensaje de progreso
	svalue(barra_status) <- paste("Creando el informe (pdf) ...")
	Sys.sleep(0.001)
	#Lee el plan_operativo.csv y genera la tabla
	plantmp <- read.table("plan_operativo.csv",sep="\t",header=TRUE,
		colClasses=rep("character",5))
	tabla_plan <- with(plantmp,table(mes,producto,subaccion,codigo_plan))
	codigos_en_plan <- dimnames(tabla_plan)$codigo_plan
	unlink("plan_operativo.csv")
	#Lee algunos html: encabezados y pies de página carreras
	dir_archivo <- file.path(dir_programa,"R",
		"encabezados_otros_plan.html")
	archivo <- file(dir_archivo,"r",blocking=FALSE)
	lineas_leidas <- readLines(con=archivo)
	encabezado_060201_plan_html <- lineas_leidas[2:27]
	encabezado_060202_plan_html <- lineas_leidas[29:67]
	encabezado_06_plan_html <- lineas_leidas[69:116]
	encabezado_listado_plan_html <- lineas_leidas[118:120]
	encabezado_errores_plan_html <- lineas_leidas[122:124]
	close(archivo)	
	dir_archivo <- file.path(dir_programa,"R","pie_paginas_plan.html")
	archivo <- file(dir_archivo,"r",blocking=FALSE)
	lineas_leidas <- readLines(con=archivo)
	pie_carreras_plan_html <- lineas_leidas[2:70]
	pie_060201_plan_html <- lineas_leidas[72:85]
	pie_060202_plan_html <- lineas_leidas[87:144]
	pie_06_plan_html <- lineas_leidas[146:220]
	pie_listado_plan_html <- lineas_leidas[222]
	close(archivo)
	#crea el archivo html y la portada
	ruta_informe <- "informe_planificacion.html"
	archivo_html <- file(ruta_informe,"w",blocking=FALSE)
	lapso <- svalue(selector_lapso)
	ano <- substr(lapso,1,4)
	generar_portada_rpo(archivo_html,ano)
	#genera la página con la lista de archivos consultados
	generar_pagina_libros_consultados(archivo_html)
	#Genera las páginas para cada carrera en las áreas-acción de
	#formación en carreras
	for (carrera in c(paste0("0202",c("02","03","04")),paste0("0203",
		sprintf("%02d",2:13))) ) {
		if (carrera %in% codigos_en_plan)
			generar_pagina_carrera(archivo_html,carrera)
	}#for carreras
	#genera la página para la 060201, si aplica
	if ("060201" %in% codigos_en_plan)
		generar_pagina_060201(archivo_html)
	#genera la página para la 060202, si aplica
	if ("060202" %in% codigos_en_plan)
		generar_pagina_060202(archivo_html)
	#genera la página para la 06, si aplica
	if (any(c("060301" %in% codigos_en_plan,
		"060302" %in% codigos_en_plan,
		"060401" %in% codigos_en_plan)))
		generar_pagina_06(archivo_html)
	#genera la(s) pagina(s) con los errores, si los hubo
	if (file.access("errores_plan.log")==0)
		generar_pagina_errores(archivo_html)
	#cierre del informe (html)
	writeLines(con=archivo_html,c("</BODY>","</HTML>"))
	close(archivo_html)
	#NOTA: el usuario debe cerrar el libre office cuando se ejecute
  #el comando soffice.
  while (loffice_abierto()) {
		gmessage(paste("Por favor cierre la aplicación LibreOffice\n",
			"para exportar el o los reportes."),icon="warning")
  }# while
	#convierte el informe a formato pdf
	system(paste("soffice --headless --convert-to pdf",ruta_informe))
  #NOTA: el usuario debe cerrar el libre office cuando se ejecute
  #el comando anterior
  #borrar el archivo html
  unlink(ruta_informe)
  svalue(barra_status) <- ""
} #generar_reporte_planificacion

