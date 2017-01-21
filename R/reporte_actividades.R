#Funciones para generar los reportes de actividades
#Fecha : 17/05/2014
#Autor : José L. Romero
#requiere los siguientes paquetes: rJava, gWidgets, RGtk2
#-----------------------------------------------------------------------

#La siguiente variable se usa en el módulo de plan_operativo.R también
logo_html <- ' 
<IMG SRC="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADEAAAAxCAYAAABznEEcAAAACXBIWXMAAA7FAAAOxQFHbOz/AAAHWElEQVR4nNVaeWwXRRSe0nqAZzwxpAh4Q4QCogKiggENihoBFRCiYKio5fRADolBRKGAnOHQBAiK2qCmBIgo0CACagGJIJRSFSlaiFeKSjnsz+/rzDbDdGZ297d/+ZLXtzv73vebtzPz5s3bZqVSKfF/pyz+yQDBmVQgkwDGxbDps40yKk5WoAy7RhCH4nQCqvUhZoGfBP8KHgPTRXEdgHgQt1PBV4LnomlEVIxaJxTtAFhbAByMYT9BSAdIl4AXAqMxMMZHeRnKgc64/EhrHo62TDwaGvWF6k5cCv4AdndBHos4Go9Z2sYBYyPMP43YiaGWtjyYrofpx1Ewsoz7W8H5sHk65IcDOsfRvha/3Rw4eyJgnOFoL1AYpWGOmE6QhsCmCjYjI7wF37M1MG8P8188OqSTnr5tAEa7MAybEyTOxyIYFyaIWFykM2HeF+an0sRppDD6wPRfF4bLiUzwcti0gU1JAkd6g38HP5WGrY5RCh7rUnA5QWoA/hr9vxb9r0jgSC5Md8F0TgKMMTDdA9NlNgyfE6TzhFxgt6fxwzq9AYyt+O3iBI7MhukmmP5oYoQ5QboNvAQ2AxJ0gKP6BcwbwvyPNOxJF4LXga+KOxIB9UcHymD7SgJHzgQXwZxhvCpNjGawXwPZ3TUSR4WcPi7iJvY5bNcncKQl+E2Y5ibAuAecB54VYOhOXA/eD67vMKbuZ7BrCrsDafx4QIOBsQMY8xNgMOxuAQYDT4aexf4M0Qu3K8H1HMbMLrdwSkD/pwSdmAeMCqYVCTA2qM30Wz2LpSOrIV5H0xiP8RXgGeCeRns5eAm4EjwA3FxIp23E9gX4rbVGOxf9UnCVkPtDM08/mPKMBvernU6aI2MhLkZTrgfABC+A3cPa/RRgDBfSWRddJuRiD2gH+E7gVKr70cDIhxzlwahZU67oNBLcSsiE0EbVxn2Nw8ZhZg74FvCjnk7oNJAOGBicEdeB73PY1HUiiBYQ/wCLb3a7kOcELzH2m9FG5Uv9hZxWLSNgfKP3QV2fAAZTlmJwQ5etdSRUhw5CdMTtLuFOl/VO1DliKkfuwG2JkNPHS+aLUPc8bXLD3e91wjxja9f7IJ4QcrG5Ilbwg/WgX61jKOf+RBMXKTepBiF+nAWusvSjTEXOAmEJFs4dW+vEOwBogcuXQjpAZ992YG0ExiBcLg/B4GKeKLQ1pzmyAmIump61OqEUZ6QsB3QFwCwyW9iPowF1h0436D7iwHgPogluJ3sw2oMLoXuv3qg5kqdeaOc6TijiAX0nFBfrczMAwOUz4NbgFo4OMLb3heo2mExxYEwDd6LDDozj4B5QnZ6ynCwVxgPgbeBrbE6Q5qt0t84iUuGvCy4POzoQ0GTobYb+JtMRiJNo6gHJ46ZvoY9QaUWBZX0chegjZMSyOsGFtQ5KN2qbjj6cRyBuFrLW5CIGACaJzc2XoTCqFcZu4d7RSay8tGbotTiyDaKfUPuHbWE3FvKQz7d+wpwSKuny7aIkhuS1Ksc6YnkZByC4vqpCcIjRCfolFox3OXVdTpA6gMeljCKYBrA55MdJTcGLGBqhf9LyNmuSP7VBu4i1sLdUwDhmwag5//sORTw/lEJxqWVuR6X7wUy5B+mNMTG40S0D99Tt9P6EnewWK0e26I6YUSOEBkJ9N9Snx7TT6SGYToPpKBtGmBMca45EDuz+tgBcELETjFjbYVpkZgdCrsEoNAwm61LyuBC7UHA1mHl/R2NtDBGnpyK+yc2Um4eYbJiWaxjMlNtExGAtbFWGLCGdVtqMWijoABvGdm77jBTtwM8bOj9EwGFonAR5SMjMdrzxfKdwp90BbeWujf5XBA26E3+Bz/UYMxWe6Hk+Tsi0obNHhxvcTM/zl4WMjD6Mi4RMSLsFo6E7wW38e+EuFIQS8LoAd5/QUoKY9kz8iMGzRSuPalfoLIB+zWFMT8VZqmRewpQ5M51OKGJY/Ur4yz9W0ub53eCt4CYe9cEqcuabhQJ+GOHXn1fjdkBb9HuBwUx2dQKMw8BgkvhdiEkO/9gKBZMyZKEg1nezgBQG0xZ+qJmXAGNPhvxqtQp8tkPVG514AKKXvgVmR03Vpt0LhVzo/dPFSMlq44vCHwychYLjGfJIyTJKdjqdgOBHkcchbwDflCYGJcuVLDY4S0i+QsFvEAx3zBRDD/kOjGo1JfgyfIUwHwad4cZKaf1YY3VCG85yCFbzWNoMrXiYGEpWqlEtEjEjlpHivCBkHSzH1ItSKPgEneAinxOnAwbWdnWIKUzTnn3hiY4Vcc6MRvrz0LRDjchciMtF3TQhEimMlRAsyc9OgMHQ2xa33FDPD56FOqFFm9eEzO2TRCyeLVhM65UuhnKE5aEVwbNICaAaTha1ukKWCfl5N6BhMTBOgXsDh+lNU+3xlzEwKD8EBmu8fXkfNYsNyiXVAGiCS4ZM1mh3qcUf6bCj6TEvmqAc2StkFT3OP8ZQ932IslhOaFOC18XmszgYEPy09pzZsbh9Samvsf8B8VWeAge7SXEAAAAASUVORK5CYII=" NAME="logo_UNA" ALIGN="LEFT" WIDTH="51" HEIGHT="51" BORDER="0">'

generar_portada_ra <- function(archivo_html,asesor,fecha_inicio,
	fecha_fin) {
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
		'  BODY {width: 23.9cm; height: 17.6cm}',
		'  H1, H2 {font-family: "sans-serif"; font-size:large}',
		'  @page { size: landscape; height: 21.6cm; margin: 2cm }',
		'  -->','</STYLE>','</HEAD>',
		'<BODY LANG="es-VE" DIR="LTR">',
		'<TABLE WIDTH=905 CELLSPACING=0>',
		'  <COL WIDTH=81>',
		'  <COL WIDTH=808>',
		'  <TR>',
		paste0('    <TD VALIGN=MIDDLE>',logo_html,'</TD>'),
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
		'</TABLE>',
	  '<DIV>',
		'<BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR>',
		paste0('  <P ALIGN=CENTER STYLE="margin-bottom: 0cm"><FONT SIZE=4>',
			'<B>INFORME DE ACTIVIDADES DESDE EL ',fecha_inicio,' HASTA EL ',
			fecha_fin,'</B></FONT></P>'),
		paste0('  <P ALIGN=CENTER STYLE="margin-bottom: 0cm"><FONT SIZE=4>',
			asesor$asesor,'</FONT></P>'),
		paste0('  <P ALIGN=CENTER STYLE="margin-bottom: 0cm"><FONT SIZE=3>',
			ifelse(asesor$sexo=="M",'ADSCRITO',
			'ADSCRITA'),' AL CL-UA ',asesor$adscrito,'</FONT></P>'),
		'</DIV>'),con=archivo_html)
#		'<P STYLE="page-break-after: always"></P>'),con=archivo_html)
} #generar_portada_ra

#-----------------------------------------------------------------------
encabezado_pagina_ra <- function(archivo_html,asesor,fecha_inicio,
	fecha_fin,pagina) {
	lapso <- svalue(selector_lapso)
	materias <- paste(rownames(asesor$materias),collapse=", ")
	writeLines(c(
		paste0('<TABLE WIDTH=900 CELLSPACING=0 STYLE="',
			'page-break-before: always">'),
		'  <COL WIDTH=86>',
		'  <COL WIDTH=187>',
		'  <COL WIDTH=186>',
		'  <COL WIDTH=110>',
		'  <COL WIDTH=117>',
		'  <COL WIDTH=110>',
		'  <TR VALIGN=MIDDLE>',
		paste0('    <TD WIDTH=89 ALIGN=CENTER VALIGN=MIDDLE STYLE=',
			'"border-top: 1px solid #000000; border-left: 1px ',
			'solid #000000; border-bottom: 1px solid #000000">',
			logo_html,'</TD>'),
		paste0('    <TD COLSPAN=5 ALIGN=CENTER STYLE=',
			'"border-top: 1px solid #000000; border-right: ',
			'1px solid #000000; border-bottom: 1px solid#000000">',
			'<FONT SIZE=5><B>INFORME DE ACTIVIDADES ACADEMICAS</B>',
			'</FONT></TD>'),
		'  </TR>',
		'  <TR VALIGN=MIDDLE>',
		paste0('    <TD ALIGN=CENTER STYLE="border-left: 1px solid ',
			'#000000"><B>Lapso</B></TD>'),
		paste0('    <TD><P STYLE="background: #eeeeee">',
			lapso,'</P></TD>'),
		'    <TD ALIGN=RIGHT><B>Fecha de Inicio</B></TD>',
		paste0('    <TD ALIGN=CENTER><P STYLE="background: #eeeeee">',
			fecha_inicio,'</P></TD>'),
		'    <TD ALIGN=RIGHT><B>Fecha Final</B></TD>',
		paste0('    <TD ALIGN=CENTER STYLE="border-right: 1px solid ',
			'#000000"><P STYLE="background: #eeeeee">',fecha_fin,'</P>',
			'</TD>'),
		'  </TR>',
		'  <TR VALIGN=MIDDLE>',
		paste0('    <TD ALIGN=CENTER STYLE="border-left: 1px solid', 
			'#000000"><B>',asesor$tipo,ifelse(asesor$sexo=="F","a",""),
			'</B></TD>'),
		paste0('    <TD><P STYLE="background: #eeeeee">',
			asesor$asesor,'</P></TD>'),
		'    <TD ALIGN=RIGHT><B>CL-UA</B></TD>',
		paste0('    <TD ALIGN=CENTER><P STYLE="background: #eeeeee">',
			asesor$adscrito,'</P></TD>'),
		'    <TD ALIGN=RIGHT><B>Área</B></TD>',
		paste0('    <TD ALIGN=CENTER STYLE="border-right: 1px solid ',
			'#000000"><P STYLE="background: #eeeeee">',asesor$area,
			'</P></TD>'),
		'  </TR>',
		'  <TR VALIGN=MIDDLE>',
		paste0('    <TD ALIGN=CENTER STYLE="border-bottom: 1px solid ',
			'#000000; border-left: 1px solid #000000"><B>Materias</B></TD>'),
		paste0('    <TD COLSPAN=3 STYLE="border-bottom: 1px solid ',
			'#000000"><P STYLE="background: #eeeeee">',materias,
			'</P></TD>'),
		paste0('    <TD ALIGN=RIGHT STYLE="border-bottom: 1px solid ', 
			'#000000"><B>Página</B></TD>'),
		paste0('    <TD ALIGN=CENTER STYLE="border-bottom: 1px ',
			'solid #000000; border-right: 1px solid #000000">',
			'<P STYLE="background: #eeeeee">',pagina,'</P></TD>'),
		'  </TR>',
		'</TABLE>',
		'<BR>'),con=archivo_html)
} #encabezado_pagina_ra

#-----------------------------------------------------------------------	
pie_pagina_ra <- function(archivo_html) {
	#termina la tabla del contenido que finaliza
	writeLines(c('</TABLE>','</TD>','</TR>','</TABLE>','<BR>'),
	con=archivo_html)
	#construye la tabla de de página
	writeLines(c(
		paste0('<TABLE WIDTH=900 CELLPADDING=10px CELLSPACING=0 STYLE=',
			'"page-break-before: never">'),
		'  <COL WIDTH=150>',
		'  <COL WIDTH=640>',
		'  <COL WIDTH=110>',
		'  <TR VALIGN=MIDDLE HEIGHT=20>',
		paste0('    <TD STYLE="border-left: 1px solid #000000; ',
			'border-right: 1px solid #000000; border-top: 1px solid ',
			'#000000">Firma</TD>'),
		'    <TD STYLE="border-top: 1px solid #000000"></TD>',
		paste0('    <TD STYLE="border-right: 1px solid #000000; ',
			'border-left: 1px solid #000000; border-top: 1px solid ',
			'#000000">Fecha</TD>'),
		'  </TR>',
		'  <TR HEIGHT=30 VALIGN=BOTTOM>',
		paste0('    <TD STYLE="border-left: 1px solid #000000; ',
			'border-right: 1px solid #000000; border-bottom: 1px solid ',
			'#000000"></TD>'),
		'    <TD STYLE="border-bottom: 1px solid #000000"></TD>',
		paste0('    <TD HEIGHT=30 ALIGN=CENTER STYLE="border-right: ',
			'1px solid #000000; border-left: 1px solid #000000; ',
			'border-bottom: 1px solid #000000">',
			format(Sys.Date(),"%d/%m/%Y"),'</TD>'),
		'  </TR>',
		'</TABLE>','<BR>'),con=archivo_html)
} #pie_pagina_ra

#-----------------------------------------------------------------------
cierre_informe_act <- function(archivo_html,nombre_archivo,formatos) {
  writeLines(c('</BODY>','</HTML>'),con=archivo_html)
  close(archivo_html)
  svalue(barra_status) <- 
		"Exportando reporte a los formatos indicados ..."
  #NOTA: el usuario debe cerrar el libre office cuando se ejecute
  #el comando soffice.
  while (loffice_abierto()) {
		gmessage(paste("Por favor cierre la aplicación LibreOffice\n",
			"para exportar el o los reportes."),icon="warning")
  }# while
  #convierte el informe a formato Word 2003
		if (formatos[1])
			system(paste0("soffice --headless --convert-to doc ",
				nombre_archivo))
  #convierte el informe a formato odt
		if (formatos[2])
			system(paste0("soffice --headless --convert-to odt ",
				nombre_archivo))
  #convierte el informe a formato pdf
		if (formatos[3])
			system(paste0("soffice --headless --convert-to pdf ",
				nombre_archivo))
  #borrar el archivo html
  unlink(nombre_archivo)
  svalue(barra_status) <- ""; Sys.sleep(0.001)
} #cierre informe

#-----------------------------------------------------------------------
#Algunas funciones para dibujar celdas en tablas de HTML
#Estas funciones retornan las líneas de código HTML
bgnegro_encabezado <- function(contenido) return(
	paste0('    <TD ALIGN="CENTER" VALIGN=MIDDLE BGCOLOR="#000000">',
	'<B><FONT COLOR="#FFFFFF">',contenido,'</FONT></B></TD>') )
bgnegro_colspan <- function(columnas,contenido) return(
	paste0('    <TD COLSPAN=',columnas,' ALIGN="CENTER" VALIGN=MIDDLE ',
	'BGCOLOR="#000000"><B><FONT COLOR="#FFFFFF">',contenido,
	'</FONT></B></TD>') )	
bordes_bgnegro <- function(contenido) return(
	paste0('    <TD STYLE="border-top: 1px solid ',
	'#cccccc; border-bottom: 1px solid #cccccc; border-left: 1px solid',
	' #cccccc; border-right: 1px solid #cccccc" ALIGN="CENTER" ',
	'VALIGN=MIDDLE BGCOLOR="#000000"><B><FONT COLOR="#FFFFFF">',
	contenido,'</FONT></B></TD>') )
bordes_bgnegro_rowspan <- function(filas,contenido) return(
	paste0('    <TD ROWSPAN=',filas,' STYLE="border-top: 1px solid ',
	'#cccccc; border-bottom: 1px solid #cccccc; border-left: 1px solid',
	' #cccccc; border-right: 1px solid #cccccc" ALIGN="CENTER"',
	' VALIGN=MIDDLE BGCOLOR="#000000"><B><FONT COLOR="#FFFFFF">',
	contenido,'</B></FONT></TD>') )
	
bordes_bgblanco <- function(contenido) return(
	paste0('    <TD STYLE="border-top: 1px solid #000000; ',
	'border-bottom: 1px solid #000000; border-left: 1px solid',
	' #000000; border-right: 1px solid #000000" ALIGN="CENTER"',
	' VALIGN=MIDDLE><B>',contenido,'</B></TD>') )
bordes_bgblanco_colspan <- function(columnas,contenido) return(
	paste0('    <TD COLSPAN=',columnas,' STYLE="border-top: 1px solid ',
	'#000000; border-bottom: 1px solid #000000; border-left: 1px solid',
	' #000000; border-right: 1px solid #000000" ALIGN="CENTER"',
	' VALIGN=MIDDLE><B>',contenido,'</B></TD>') )
bordes_bgblanco_rowspan <- function(filas,contenido) return(
	paste0('    <TD ROWSPAN=',filas,' STYLE="border-top: 1px solid ',
	'#000000; border-bottom: 1px solid #000000; border-left: 1px solid',
	' #000000; border-right: 1px solid #000000" ALIGN="CENTER"',
	' VALIGN=MIDDLE><B>',contenido,'</B></TD>') )	
bordes_bggris <- function(contenido) return(
	paste0('    <TD STYLE="border-top: 1px solid #000000; ',
	'border-bottom: 1px solid #000000; border-left: 1px solid',
	' #000000; border-right: 1px solid #000000" ALIGN="CENTER" VALIGN',
	'=MIDDLE BGCOLOR="#E6E6E6">',contenido,'</TD>') )
bordes_tbl_bggris <- function(contenido) return(
	paste0('    <TD STYLE="border-top: 1px solid #000000; ',
	'border-bottom: 1px solid #000000; border-left: 1px solid',
	' #000000" ALIGN="CENTER" VALIGN=MIDDLE BGCOLOR="#E6E6E6">',
	contenido,'</TD>') )
bordes_tbr_bggris <- function(contenido) return(
	paste0('    <TD STYLE="border-top: 1px solid #000000; ',
	'border-bottom: 1px solid #000000; border-right: 1px solid',
	' #000000" ALIGN="CENTER" VALIGN=MIDDLE BGCOLOR="#E6E6E6">',
	contenido,'</TD>') )
bordes_tb_bggris <- function(contenido) return(
	paste0('    <TD STYLE="border-top: 1px solid #000000; ',
	'border-bottom: 1px solid #000000" ALIGN="CENTER" VALIGN=',
	'MIDDLE BGCOLOR="#E6E6E6">',contenido,'</TD>') )
bordes_tlr_bggris <- function(contenido) return(
	paste0('    <TD STYLE="border-top: 1px solid #000000; ',
	'border-left: 1px solid #000000; border-right: 1px solid',
	' #000000" ALIGN="CENTER" VALIGN=MIDDLE BGCOLOR="#E6E6E6">',
	contenido,'</TD>') )
bordes_blr_bggris <- function(contenido) return(
	paste0('    <TD STYLE="border-bottom: 1px solid #000000; ',
	'border-left: 1px solid #000000; border-right: 1px solid',
	' #000000" ALIGN="CENTER" VALIGN=MIDDLE BGCOLOR="#E6E6E6">',
	contenido,'</TD>') )
bordes_lr_bggris <- function(contenido) return(
	paste0('    <TD STYLE="border-left: 1px solid #000000; ',
	'border-right: 1px solid #000000" ALIGN="CENTER" VALIGN=',
	'MIDDLE BGCOLOR="#E6E6E6">',contenido,'</TD>') )			
bordes_celda <- function(contenido) return(
	paste0('    <TD STYLE="border-top: 1px solid #000000; ',
	'border-bottom: 1px solid #000000; border-left: 1px solid',
	' #000000" ALIGN="CENTER" VALIGN=MIDDLE BGCOLOR="#E6E6E6">',
	contenido,'</TD>') )

#-----------------------------------------------------------------------
generar_reporte_asesor <- function(asesor,fecha_inicio,
	fecha_fin,formatos) {
	#NOTA: 18/7/14 se remuneraron las tablas porque ahora la tabla 5 es
	#la de los talleres. falta hacerla.
  
	encabezado_tabla <- function(num_tabla,columnas,continua,altura) {
		#la función encabezado_tabla dibuja las filas de encabezado de cada
		#una de las tablas que aparecen en el informe de actividades del
		#asesor.	
		#esto crea una tabla envolvente de altura fija
		writeLines(c('<TABLE>',
			paste0('<TR VALIGN=MIDDLE HEIGHT=',altura,'>'),
			paste0('<TD WIDTH=900  HEIGHT=',altura,' ALIGN=CENTER>')),
			con=archivo_html)
		writeLines(paste0('<H2>Tabla ',num_tabla,' - ',switch(num_tabla,
			'Matricula atendida para las asignaturas de una sola UA-CL',
			'Matricula atendida para las asignaturas de varias UA-CL',
			'Pruebas corregidas',
			'Trabajos corregidos',
			'Talleres dictados',
			'Tipos de asesorías',
			'Asesorías por asignatura, carrera y tipo',
			'Asesorías por tipo y por carrera',
			'Alcance geográfico de asesorías'),
			{if (continua) ' (continuación)' else ''},
			'</H2>'),con=archivo_html )
		if (num_tabla==6) return(0)
		num_col <- length(columnas)
		ancho_t <- switch(num_tabla,165,235,120,110,560,0,235,120,120)+
			num_col*48
		writeLines(paste0('<TABLE WIDTH=',ancho_t,' CELLSPACING=0 COLS="',
			switch(num_tabla,
				num_col+3,
				num_col+4,
				num_col+2,
				num_col+2,
				3,
				0, #this is a place-holder so what i put here is shit anyway
				num_col+4,
				num_col+2,
				num_col+2),
			'" BORDER="0">'),con=archivo_html)
		if (num_tabla %in% c(1,2,7)) {
			writeLines('  <COLGROUP WIDTH="50"></COLGROUP>',con=archivo_html)
			writeLines('  <COLGROUP WIDTH="65"></COLGROUP>',con=archivo_html)
		} else {
			writeLines('  <COLGROUP WIDTH="60"></COLGROUP>',con=archivo_html)
		}
		if (num_tabla!=5)
			for (i in 1:num_col)
				writeLines('  <COLGROUP WIDTH="48"></COLGROUP>',
					con=archivo_html)
		if (num_tabla %in% c(2,7)) {
			writeLines('  <COLGROUP WIDTH="60"></COLGROUP>',con=archivo_html)
			writeLines('  <COLGROUP WIDTH="60"></COLGROUP>',con=archivo_html)
		}
		if (num_tabla %in% c(1,3,4,6,8,9)) {
			writeLines('  <COLGROUP WIDTH="60"></COLGROUP>',con=archivo_html)
		}
		if (num_tabla==5) {
			writeLines('  <COLGROUP WIDTH="100"></COLGROUP>',con=archivo_html)
			writeLines('  <COLGROUP WIDTH="400"></COLGROUP>',con=archivo_html)			
		}
		#escribe fila de encabezado 1
		writeLines('  <TR>',con=archivo_html)
		if (num_tabla %in% c(1:4,6:7))
			writeLines(bordes_bgblanco_rowspan(2,'Asig.'),con=archivo_html)
		if (num_tabla==5)
			writeLines(bordes_bgblanco('Asig.'),con=archivo_html)
		if (num_tabla==8)
			writeLines(bordes_bgblanco_rowspan(2,'Tipo'),con=archivo_html)
		if (num_tabla==9)
				writeLines(bordes_bgblanco_rowspan(2,'CL-UA'),con=archivo_html)
		if (num_tabla %in% c(1,2,7)) 	
			if (num_tabla<7)
				writeLines(bordes_bgblanco_rowspan(2,'CL-UA'),con=archivo_html)
			else {
				writeLines(bordes_bgblanco_rowspan(2,'Tipo'),con=archivo_html)
			}
		if (num_tabla %in% c(1:4,6:8))
			writeLines(bordes_bgblanco_colspan(num_col,'Carreras'),
				con=archivo_html)
		if (num_tabla==5)
			writeLines(bordes_bgblanco('Fecha'),con=archivo_html)				
		if (num_tabla==9)
			writeLines(bordes_bgblanco_colspan(num_col,'Tipo'),
				con=archivo_html)
		if (num_tabla %in% c(2,7))
			writeLines(bordes_bgblanco_colspan(2,'Totales'),con=archivo_html)
		if (num_tabla==5)
			writeLines(bordes_bgblanco('Descripción'),con=archivo_html)
		if (num_tabla %in% c(1,3,4,6,8,9)) 
			writeLines(bordes_bgblanco_rowspan(2,'Totales'),con=archivo_html)
		writeLines('  </TR>',con=archivo_html)
		#escribe fila de encabezado 2 (sólo si num_tabla<>5)
		if (num_tabla!=5) {
			writeLines('  <TR>',con=archivo_html)
			for (item in columnas)
				writeLines(bgnegro_encabezado(item),con=archivo_html)
			if (num_tabla %in% c(2,7)) {
				if (num_tabla==2)
					writeLines(bordes_bgblanco('CL-UA'),con=archivo_html)
				else {
					writeLines(bordes_bgblanco('Tipo'),con=archivo_html)
				}
				writeLines(bordes_bgblanco('Asig.'),con=archivo_html)
			}#if num_tabla %in% c(2,7)
			writeLines('  </TR>',con=archivo_html)
		}#if num_tabla!=5
	} #encabezado_tabla
	
	#--------
	svalue(barra_status) <- paste("Generando el reporte de",
		asesor$asesor,"...")
	#convierte las fechas a formato imprimible:
	fecha_inicio <- format(fecha_inicio,"%d de %b %Y")
	fecha_fin <- format(fecha_fin,"%d de %b %Y")	
	#transforma el nombre de archivo para leer el data frame
	#a partir del archivo OL (Objetivos Logrados).
	lapso <- svalue(selector_lapso)
	#Este archivo contiene la data con la cual se generan las tablas 1-4
	archivo_csv <- paste0("OL_",asesor$archivo,"_",lapso,".csv")
	#lee el archivo generado desde el libro de evaluación
	datos <- read.table(archivo_csv, header=TRUE, sep="\t")
	unlink(archivo_csv)
	#determina el nombre del archivo html y crealo
	narchivo_html <- paste0("actividades_",asesor$archivo,"_",lapso,
		".html")
	archivo_html <- file(narchivo_html,open="w")
	generar_portada_ra(archivo_html,asesor,fecha_inicio,fecha_fin)
	pagina <- 1
	#calcula la altura de las tablas del medio
	linmaterias <- ceiling(nrow(asesor$materias)/15) - 1
	tmp <- gsub(paste("(.{1,",14,"})(\\s|$)",sep=""), "\\1\n", 
			asesor$area)
	linarea <-length(strsplit(tmp,"\n")[[1]])-1
	altura <- 400 - 15*(linarea+linmaterias)
	#escribe el encabezado de la 1era pagina
	encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
		fecha_fin,pagina)
	#determina cuales asignaturas tienen una sola ua-cl
	#unasola es un vector de asignaturas con un solo ua-cl
	#asignatura es el vector de asignatura (cl-ua) para visualizar en
	#la tabla
	t1 <- with(datos,table(asignatura,clua))
	temp1 <- sapply(1:nrow(t1),function(i) sum(t1[i,]>0)==1)
	asignaturas <- rownames(t1)[temp1]
	temp2 <- sapply(1:nrow(t1),function(i) 
		ifelse(temp1[i],colnames(t1)[which(t1[i,]>0)],"") )
	cl_uas <- temp2[temp1]
	#---------------------------------------------------------------------
	#Tabla1: matricula atendida en una sola ua-cl
	datos_1era <- subset(datos,subset=asignatura %in% asignaturas)
	tabla1 <- with(datos_1era,table(asignatura,carrera))
	totales_filas <- 	rowSums(tabla1)
  totales_columnas <- colSums(tabla1)
  nlinea <- 0
	#escribe el encabezado de la tabla (html)
	encabezado_tabla(1,colnames(tabla1),FALSE,altura)
	#escribe el cuerpo de la tabla (html)
	for (i in 1:nrow(tabla1)) {
		writeLines('  <TR>',con=archivo_html)
		writeLines(bgnegro_encabezado(asignaturas[i]),con=archivo_html)
		writeLines(bgnegro_encabezado(cl_uas[i]),con=archivo_html)
		for (j in 1:ncol(tabla1)) 
			writeLines(bordes_bggris(ifelse(tabla1[i,j]>0,tabla1[i,j],' ')),
				con=archivo_html)
		writeLines(bordes_bgblanco(totales_filas[i]),con=archivo_html)
		writeLines('  </TR>',con=archivo_html)
		nlinea <- nlinea+1
		if (nlinea>8 & (nrow(tabla1)-i)>2) {  #divide la tabla
			pie_pagina_ra(archivo_html)
			pagina <- pagina+1
			encabezado_pagina_ra(archivo_html,asesor, fecha_inicio,
				fecha_fin,pagina)
			nlinea <- 0
			encabezado_tabla(1,colnames(tabla1),TRUE,altura)
		}
	}
	#escribe la última fila con los totales
	writeLines('  <TR>',con=archivo_html)
	writeLines(bordes_bgblanco_colspan(2,'Totales'),con=archivo_html)
	for (i in 1:ncol(tabla1))
		writeLines(bordes_bgblanco(totales_columnas[i]),con=archivo_html)
	writeLines(bgnegro_encabezado(sum(totales_columnas)),con=archivo_html)
	pie_pagina_ra(archivo_html)
  pagina <- pagina+1
  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,pagina)
  #---------------------------------------------------------------------
  #Tabla2: matricula atendida en varias ua-cl
  datos_2nda <- subset(datos,subset=!(asignatura %in% asignaturas))
  tabla2 <- with(datos_2nda,table(clua,carrera,asignatura))
  asignaturas <- attributes(tabla2)$dimnames$asignatura
  #escribe el encabezado de la tabla (html)
  encabezado_tabla(2,colnames(tabla2),FALSE,altura)
  nlinea <- 0
  filas <- dim(tabla2)[1]
	#escribe el cuerpo de la tabla (html)
   for (i in asignaturas) {
		writeLines('  <TR>',con=archivo_html)
		writeLines(bordes_bgnegro_rowspan(filas,i),con=archivo_html)
		if (!is.null(dim(tabla2[,,i]))) { #fixed bug here 19/10/2016
		  totales_filas <- rowSums(tabla2[,,i])
		} else totales_filas <- tabla2[,,i]
		for (j in 1:filas) {
			writeLines(bordes_bgnegro(rownames(tabla2)[j]),
					con=archivo_html)
			for (k in 1:ncol(tabla2))
				if (j==1) 
					writeLines(bordes_tlr_bggris(ifelse(tabla2[j,k,i]>0,
						tabla2[j,k,i],' ')),con=archivo_html)
			  else
					if (j<nrow(tabla2))
						writeLines(bordes_lr_bggris(ifelse(tabla2[j,k,i]>0,
						tabla2[j,k,i],' ')),con=archivo_html)
					else
						writeLines(bordes_blr_bggris(ifelse(tabla2[j,k,i]>0,
							tabla2[j,k,i],' ')),con=archivo_html)
			writeLines(bordes_bgblanco(totales_filas[j]),con=archivo_html)
			if (j==1) writeLines(bordes_bgblanco_rowspan(filas,
				sum(totales_filas)),con=archivo_html)
			writeLines('  </TR>',con=archivo_html)
			if (j<filas) writeLines('  <TR>',con=archivo_html)
		} #for j (CL-UA)
		nlinea <- nlinea+filas
		if (nlinea>8 & ((filas*(dim(tabla2)[3]-match(i,asignaturas)))
			>1) ) {	#divide la tabla
			writeLines('  </TR>',con=archivo_html)
			pie_pagina_ra(archivo_html)
			pagina <- pagina+1
			encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
				fecha_fin,pagina)
			nlinea <- 0
			encabezado_tabla(2,colnames(tabla2),TRUE,altura)
		}
  } #for i (asignaturas)
  #escribe última fila de la tabla 2
  totales_columnas <- colSums(t(colSums(tabla2)))
  writeLines('  <TR>',con=archivo_html)
	writeLines(bordes_bgblanco_colspan(2,'Totales'),con=archivo_html)
	for (i in 1:ncol(tabla2))
		writeLines(bordes_bgblanco(totales_columnas[i]),con=archivo_html)
	writeLines(bgnegro_colspan(2,sum(totales_columnas)),con=archivo_html)
	writeLines('  </TR>',con=archivo_html)
	pie_pagina_ra(archivo_html)
  pagina <- pagina+1
  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,pagina)
  #---------------------------------------------------------------------
	#Tabla3:  total de pruebas presentadas, por asignatura y por carrera
	tabla3 <- tapply(datos$pruebas,datos[,c(4,3)],sum)
	tabla3[is.na(tabla3)] <- 0
	totales_filas <- rowSums(tabla3)
  totales_columnas <- colSums(tabla3)
  #imprime el encabezado de la tabla 3 en html
  encabezado_tabla(3,colnames(tabla3),FALSE,altura)
	#escribe el cuerpo de la tabla (html)
	nlinea <- 0
	for (i in 1:nrow(tabla3)) {
		if (any(tabla3[i,]!=0)) {
			#escribe la fila solo si hay un numero distinto de 0
			writeLines('  <TR>',con=archivo_html)
			writeLines(bgnegro_encabezado(rownames(tabla3)[i]),con=archivo_html)
			for (j in 1:ncol(tabla3)) 
				writeLines(bordes_bggris(ifelse(tabla3[i,j]>0,tabla3[i,j],' ')),
					con=archivo_html)
			writeLines(bordes_bgblanco(totales_filas[i]),con=archivo_html)
			writeLines('  </TR>',con=archivo_html)
			nlinea <- nlinea+1
			if (nlinea>8 & (nrow(tabla3)-i)>2) {  #divide la tabla
				pie_pagina_ra(archivo_html)
				pagina <- pagina+1
				encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
					fecha_fin,pagina)
				nlinea <- 0
				encabezado_tabla(3,colnames(tabla3),TRUE,altura)
			}
		}
	}
	#escribe la última fila con los totales
	writeLines('  <TR>',con=archivo_html)
	writeLines(bordes_bgblanco('Totales'),con=archivo_html)
	for (i in 1:ncol(tabla3))
		writeLines(bordes_bgblanco(totales_columnas[i]),con=archivo_html)
	writeLines(bgnegro_encabezado(sum(totales_columnas)),con=archivo_html)
	pie_pagina_ra(archivo_html)
  pagina <- pagina+1
  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,pagina)
  #---------------------------------------------------------------------
	#Tabla4: total de trabajos presentadas, por asignatura y por carrera
	tabla4 <- tapply(datos$trabajos,datos[,c(4,3)],sum)
	tabla4[is.na(tabla4)] <- 0
	totales_filas <- rowSums(tabla4)
  totales_columnas <- colSums(tabla4)
  #imprime el encabezado de la tabla 4 en html
  encabezado_tabla(4,colnames(tabla4),FALSE,altura)
	#escribe el cuerpo de la tabla (html)
	nlinea <- 0
	for (i in 1:nrow(tabla4)) {
		if (any(tabla4[i,]!=0)) {
		#escribe la fila solo si hay un numero distinto de 0
			writeLines('  <TR>',con=archivo_html)
			writeLines(bgnegro_encabezado(rownames(tabla4)[i]),con=archivo_html)
			for (j in 1:ncol(tabla4)) 
				writeLines(bordes_bggris(ifelse(tabla4[i,j]>0,tabla4[i,j],' ')),
					con=archivo_html)
			writeLines(bordes_bgblanco(totales_filas[i]),con=archivo_html)
			writeLines('  </TR>',con=archivo_html)
			nlinea <- nlinea+1
			if (nlinea>8 & (nrow(tabla4)-i)>2) {  #divide la tabla
				pie_pagina_ra(archivo_html)
				pagina <- pagina+1
				encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
					fecha_fin,pagina)
				nlinea <- 0
				encabezado_tabla(4,colnames(tabla4),TRUE,altura)
			}
		}
	}
	#escribe la última fila con los totales
	writeLines('  <TR>',con=archivo_html)
	writeLines(bordes_bgblanco('Totales'),con=archivo_html)
	for (i in 1:ncol(tabla4))
		writeLines(bordes_bgblanco(totales_columnas[i]),con=archivo_html)
	writeLines(bgnegro_encabezado(sum(totales_columnas)),con=archivo_html)
	pie_pagina_ra(archivo_html)
  pagina <- pagina+1
	#---------------------------------------------------------------------
	#Tabla 5: talleres dictados
	archivo_csv <- paste0("talleres_",asesor$archivo,"_",lapso,".csv")
	datos <- read.table(archivo_csv, header=TRUE, sep="\t",
		colClasses=c("Date","numeric","character"))
	unlink(archivo_csv)
	if (nrow(datos)>0) {
	  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,
			pagina)
		encabezado_tabla(5,NULL,FALSE,altura)
		#escribe el cuerpo de la tabla (html)
		nlinea <- 0
		for (i in 1:nrow(datos)) {
			writeLines('  <TR>',con=archivo_html)
			print(datos[i,])
			if (i<nrow(datos)) {
				writeLines(bordes_lr_bggris(datos[i,2]),con=archivo_html)
				writeLines(bordes_lr_bggris(format(datos[i,1],"%d/%m/%Y")),
					con=archivo_html)
				writeLines(bordes_lr_bggris(datos[i,3]),con=archivo_html)
			} else {
				writeLines(bordes_blr_bggris(datos[i,2]),con=archivo_html)
				writeLines(bordes_blr_bggris(format(datos[i,1],"%d/%m/%Y")),
					con=archivo_html)
				writeLines(bordes_blr_bggris(datos[i,3]),con=archivo_html)
			}#if no es la última fila
			writeLines('  </TR>',con=archivo_html)
			nlinea <- nlinea+1
			if (nlinea>8 & (nrow(datos)-i)>2) {  #divide la tabla
				pie_pagina_ra(archivo_html)
				pagina <- pagina+1
				encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
					fecha_fin,pagina)
				nlinea <- 0
				encabezado_tabla(5,NULL,TRUE,altura)
			}#if divide la tabla
		}#for cada linea de datos
		pie_pagina_ra(archivo_html)
		pagina <- pagina+1
	}#if hay filas en "talleres"
	#---------------------------------------------------------------------
  #Lee el libro csv de asesorías para las tablas 6-9
  archivo_csv <- paste0("asesoria_",asesor$archivo,"_",lapso,".csv")
  asesorias <- leer_csv_asesorias(archivo_csv)
  unlink(archivo_csv)
	#verificar si no hubo asesorias
	#en tal caso, no hacer más tablas e imprimir un mensaje en el informe
  if (nrow(asesorias)==0) {
		cierre_informe_act(archivo_html,narchivo_html,formatos)
		return(0)
  }
	#---------------------------------------------------------------------   
	#Tabla 6 : Tipos de asesoría y servicios de orientación
  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,
		pagina)	
	encabezado_tabla(6,NULL,FALSE,altura)
	writeLines(c(
		'<TABLE WIDTH=400 CELLSPACING=0 COLS=3 BORDER=0>',
		'  <COLGROUP WIDTH="50"></COLGROUP>',
		'  <COLGROUP WIDTH="300"></COLGROUP>',
		'  <COLGROUP WIDTH="50"></COLGROUP>',
		'  <TR>',
		bordes_bgblanco_colspan(3,"Tipos de asesoría"),
		'  </TR>',
		'  <TR>',
		bordes_bgnegro("Tipo"),
		bordes_bgnegro("Descripción"),
		bordes_bgnegro("Cód."),
		'  </TR>',
		'  <TR>',
		bordes_tlr_bggris("J"),
		bordes_tlr_bggris("Jornadas"),
		bordes_tlr_bggris("37"),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("C"),
		bordes_lr_bggris("Círculos de estudio"),
		bordes_lr_bggris("45"),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("E"),
		bordes_lr_bggris("Encuentros"),
		bordes_lr_bggris("46"),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("T"),
		bordes_lr_bggris("Taller"),
		bordes_lr_bggris("50"),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("G"),
		bordes_lr_bggris("Asesoría en grupos"),
		bordes_lr_bggris("59"),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("I"),
		bordes_lr_bggris("Asesoría individual"),
		bordes_lr_bggris("60"),
		'  </TR>',
		'  <TR>',
		bordes_blr_bggris("@"),
		bordes_blr_bggris("Asesoría en línea"),
		bordes_blr_bggris("61"),
		'  </TR>'),con=archivo_html)
	pie_pagina_ra(archivo_html)
  pagina <- pagina+1
  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,pagina)
  #---------------------------------------------------------------------
	#Tabla 7 : Asesorias por asignatura, carrera y tipo
	tabla7 <- with(asesorias,table(tipo,carrera,asignatura))
	asignaturas <- attributes(tabla7)$dimnames$asignatura
  #escribe el encabezado de la tabla (html)
  encabezado_tabla(7,colnames(tabla7),FALSE,altura)
  nlinea <- 0
	#escribe el cuerpo de la tabla (html)
   for (i in asignaturas) {
		filas <- sapply(1:nrow(tabla7),function(j)
			ifelse(any(tabla7[j,,i]!=0),1,0) )
		primera <- min(which(filas!=0)) 
		ultima <- max(which(filas!=0))
		if (sum(filas)>0) {
			writeLines('  <TR>',con=archivo_html)
			writeLines(bordes_bgnegro_rowspan(sum(filas),i),con=archivo_html)
			if (nrow(tabla7)>1)
			  if (!is.null(dim(tabla7[,,i]))) { #fixed bug here 19/10/2016
			    totales_filas <- rowSums(tabla7[,,i])
			  } else totales_filas <- tabla7[,,i]
			else {
			  if (!is.null(dim(tabla7[,,i]))) { #fixed bug here 19/10/2016
			    totales_filas <- sum(tabla7[,,i])
			  } else totales_filas <- tabla7[,,i]
			}
			for (j in 1:nrow(tabla7)) {
				if (filas[j]!=0) {
					writeLines(bordes_bgnegro(rownames(tabla7)[j]),
						con=archivo_html)
					for (k in 1:ncol(tabla7))
						if (j==primera) 
							if (sum(filas)>1) writeLines(bordes_tlr_bggris(ifelse(
								tabla7[j,k,i]>0,tabla7[j,k,i],' ')),con=archivo_html)
							else writeLines(bordes_bggris(ifelse(
								tabla7[j,k,i]>0,tabla7[j,k,i],' ')),con=archivo_html)
						else
							if (j<ultima)
								writeLines(bordes_lr_bggris(ifelse(tabla7[j,k,i]>0,
								tabla7[j,k,i],' ')),con=archivo_html)
							else
								writeLines(bordes_blr_bggris(ifelse(tabla7[j,k,i]>0,
									tabla7[j,k,i],' ')),con=archivo_html)
					writeLines(bordes_bgblanco(totales_filas[j]),con=archivo_html)
					if (j==primera) writeLines(bordes_bgblanco_rowspan(sum(filas),
						sum(totales_filas)),con=archivo_html)
					writeLines('  </TR>',con=archivo_html)
					if (j<filas) writeLines('  <TR>',con=archivo_html)
				} #if (filas[j]!=0)
			} #for j (tipo)
		} #if (sum(filas)>0)
		nlinea <- nlinea+sum(filas)
		if (nlinea>8 & ((nrow(tabla7)*(dim(tabla7)[3]-match(i,asignaturas)))
			>1) ) {	#divide la tabla y llevala a la siguiente página
			writeLines('  </TR>',con=archivo_html)
			pie_pagina_ra(archivo_html)
			pagina <- pagina+1
			encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
				fecha_fin,pagina)
			nlinea <- 0
			encabezado_tabla(7,colnames(tabla7),TRUE,altura)
		}
  } #for i (asignaturas)
  #escribe última fila de la tabla 7
  totales_columnas <- colSums(t(colSums(tabla7)))
  writeLines('  <TR>',con=archivo_html)
	writeLines(bordes_bgblanco_colspan(2,'Totales'),con=archivo_html)
	for (i in 1:ncol(tabla7))
		writeLines(bordes_bgblanco(totales_columnas[i]),con=archivo_html)
	writeLines(bgnegro_colspan(2,sum(totales_columnas)),con=archivo_html)
	writeLines('  </TR>',con=archivo_html)
	pie_pagina_ra(archivo_html)
  pagina <- pagina+1
  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,pagina)
	#---------------------------------------------------------------------
	#Tabla 8 : Asesorias por tipo y por carrera
	tabla8 <- with(asesorias,table(tipo,carrera))
	print(asesorias)
	print(str(tabla8))

	print(tabla8)
	totales_filas <- 	rowSums(tabla8)
  totales_columnas <- colSums(tabla8)
  nlinea <- 0
	#escribe el encabezado de la tabla (html)
	encabezado_tabla(8,colnames(tabla8),FALSE,altura)
	#escribe el cuerpo de la tabla (html)
	for (i in 1:nrow(tabla8)) {
		writeLines('  <TR>',con=archivo_html)
		writeLines(bgnegro_encabezado(rownames(tabla8)[i]),con=archivo_html)
		for (j in 1:ncol(tabla8)) 
			writeLines(bordes_bggris(ifelse(tabla8[i,j]>0,tabla8[i,j],' ')),
				con=archivo_html)
		writeLines(bordes_bgblanco(totales_filas[i]),con=archivo_html)
		writeLines('  </TR>',con=archivo_html)
		nlinea <- nlinea+1
		if (nlinea>8 & (nrow(tabla8)-i)>2) {  #divide la tabla
			pie_pagina_ra(archivo_html)
			pagina <- pagina+1
			encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
				fecha_fin,pagina)
			nlinea <- 0
			encabezado_tabla(8,colnames(tabla8),TRUE,altura)
		}
	}
	#escribe la última fila con los totales
	writeLines('  <TR>',con=archivo_html)
	writeLines(bordes_bgblanco('Totales'),con=archivo_html)
	for (i in 1:ncol(tabla8))
		writeLines(bordes_bgblanco(totales_columnas[i]),con=archivo_html)
	writeLines(bgnegro_encabezado(sum(totales_columnas)),con=archivo_html)
	pie_pagina_ra(archivo_html)
  pagina <- pagina+1
  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,pagina)
	#---------------------------------------------------------------------
	#Tabla 9 : Alcance geográfico de asesorias
	tabla9 <- with(asesorias,table(clua,tipo))
	totales_filas <- 	rowSums(tabla9)
  totales_columnas <- colSums(tabla9)
  nlinea <- 0
	#escribe el encabezado de la tabla (html)
	encabezado_tabla(9,colnames(tabla9),FALSE,altura)
	#escribe el cuerpo de la tabla (html)
	for (i in 1:nrow(tabla9)) {
		writeLines('  <TR>',con=archivo_html)
		writeLines(bgnegro_encabezado(rownames(tabla9)[i]),con=archivo_html)
		for (j in 1:ncol(tabla9)) 
			writeLines(bordes_bggris(ifelse(tabla9[i,j]>0,tabla9[i,j],' ')),
				con=archivo_html)
		writeLines(bordes_bgblanco(totales_filas[i]),con=archivo_html)
		writeLines('  </TR>',con=archivo_html)
		nlinea <- nlinea+1
		if (nlinea>8 & (nrow(tabla9)-i)>2) {  #divide la tabla
			pie_pagina_ra(archivo_html)
			pagina <- pagina+1
			encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
				fecha_fin,pagina)
			nlinea <- 0
			encabezado_tabla(9,colnames(tabla9),TRUE,altura)
		}
	}
	#escribe la última fila con los totales
	writeLines('  <TR>',con=archivo_html)
	writeLines(bordes_bgblanco('Totales'),con=archivo_html)
	for (i in 1:ncol(tabla9))
		writeLines(bordes_bgblanco(totales_columnas[i]),con=archivo_html)
	writeLines(bgnegro_encabezado(sum(totales_columnas)),con=archivo_html)
	pie_pagina_ra(archivo_html)	
  #escribe colofon y cierra el archivo html
  cierre_informe_act(archivo_html,narchivo_html,formatos)
} #generar_reporte_asesor

#-----------------------------------------------------------------------

generar_reporte_orientador <- function(asesor,fecha_inicio,
	fecha_fin,formatos) {

	encabezado_tabla <- function(num_tabla,columnas,continua) {
	  #la función encabezado_tabla dibuja las filas de encabezado de cada
		#una de las tablas que aparecen en el informe de actividades del
		#orientador.
		#esto crea una tabla envolvente de altura fija
		writeLines(c('<TABLE>',
			'<TR VALIGN=MIDDLE HEIGHT=400>',
			'<TD WIDTH=900  HEIGHT=400 ALIGN=CENTER>'),con=archivo_html)
		writeLines(paste0('<H2>Tabla ',num_tabla,' - ',switch(num_tabla,
			'Matricula atendida por carrera',
			'Trabajos corregidos',
			'Talleres dictados',
			'Tipos de asesoría y servicios de orientación',
			'Asesorías por tipo y por carrera',
			'Servicios de Orientación por carrera'),
			{if (continua) ' (continuación)' else ''},
			'</H2>'),con=archivo_html )
		if (num_tabla==4) return(0)
		num_col <- length(columnas)
		ancho_t <- switch(num_tabla,60,60,560,60,120,120)+
			num_col*48
		writeLines(paste0('<TABLE WIDTH=',ancho_t,' CELLSPACING=0 COLS="',
			switch(num_tabla,
				num_col+1,
				num_col+1,
				3,
				num_col+1, #la tabla 4 es para relleno
				num_col+2,
				num_col+2),
			'" BORDER="0">'),con=archivo_html)
		if (num_tabla %in% c(3,5,6)) {
			writeLines('  <COLGROUP WIDTH="60"></COLGROUP>',con=archivo_html)
		}
		if (num_tabla==3) {
			writeLines('  <COLGROUP WIDTH="100"></COLGROUP>',con=archivo_html)
			writeLines('  <COLGROUP WIDTH="400"></COLGROUP>',con=archivo_html)
		} else {
			for (i in 1:num_col)
				writeLines('  <COLGROUP WIDTH="48"></COLGROUP>',
					con=archivo_html)
			writeLines('  <COLGROUP WIDTH="60"></COLGROUP>',con=archivo_html)
		}
		#escribe fila de encabezado 1
		writeLines('  <TR>',con=archivo_html)
		if (num_tabla==3) {
			writeLines(bordes_bgblanco('Asig.'),con=archivo_html)
			writeLines(bordes_bgblanco('Fecha'),con=archivo_html)
			writeLines(bordes_bgblanco('Descripción'),con=archivo_html)
		} else {
			if (num_tabla %in% c(5,6))
				writeLines(bordes_bgblanco_rowspan(2,'Tipo'),con=archivo_html)
			writeLines(bordes_bgblanco_colspan(num_col,'Carreras'),
				con=archivo_html)
			writeLines(bordes_bgblanco_rowspan(2,'Totales'),con=archivo_html)
			writeLines('  </TR>',con=archivo_html)
			#escribe fila de encabezado 2
			writeLines('  <TR>',con=archivo_html)
			for (item in columnas)
				writeLines(bgnegro_encabezado(item),con=archivo_html)
			writeLines('  </TR>',con=archivo_html)
		}#if num_tabla==3
	} #encabezado_tabla
	#-------------
	svalue(barra_status) <- paste("Generando el reporte de",
		asesor$asesor,"...")
	#convierte las fechas a formato imprimible:
	fi <- fecha_inicio
	ff <- fecha_fin
	fecha_inicio <- format(fecha_inicio,"%d de %b %Y")
	fecha_fin <- format(fecha_fin,"%d de %b %Y")	
	#Este archivo contiene la data con la cual se generan las tablas 1 y 2
	lapso <- svalue(selector_lapso)
	archivo_csv <- paste0("OL_",asesor$archivo,"_",lapso,".csv")
	#lee el archivo generado desde el libro de evaluación
	datos <- read.table(archivo_csv, header=TRUE, skip=4, sep="\t")
	unlink(archivo_csv)
	#determina el nombre del archivo html y crealo
	narchivo_html <- paste0("actividades_",asesor$archivo,"_",lapso,
		".html")
	archivo_html <- file(narchivo_html,open="w")
	generar_portada_ra(archivo_html,asesor,fecha_inicio,fecha_fin)
	pagina <- 1
	#escribe el encabezado de la 1era pagina
	encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,pagina)
	#---------------------------------------------------------------------
	#Tabla1: matricula atendida por carrera
	tabla1 <- with(datos,tapply(carrera,carrera,length))
	total_mat <- 	sum(tabla1)
	#escribe el encabezado de la tabla (html)
	encabezado_tabla(1,names(tabla1),FALSE)
	#escribe el cuerpo de la tabla (html)
	writeLines('  <TR>',con=archivo_html)
	for (i in 1:length(tabla1)) 
		writeLines(bordes_bggris(ifelse(tabla1[i]>0,tabla1[i],' ')),
			con=archivo_html)
	writeLines(bordes_bgblanco(total_mat),con=archivo_html)
	writeLines('  </TR>',con=archivo_html)
	pie_pagina_ra(archivo_html)
  pagina <- pagina+1
  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,pagina)
  #---------------------------------------------------------------------
  #Tabla2: trabajos corregidos por carrera
  fechas <- as.Date(datos$fecha)
	trabajos <- ifelse(fechas>=fi & fechas<=ff,1,0)
	trabajos[is.na(trabajos)] <- 0
  tabla2 <- tapply(trabajos,datos$carrera,sum)
  #escribe el encabezado de la tabla (html)
  encabezado_tabla(2,names(tabla2),FALSE)
 	#escribe el cuerpo de la tabla (html)
	writeLines('  <TR>',con=archivo_html)
	for (i in 1:length(tabla2)) 
		writeLines(bordes_bggris(ifelse(tabla2[i]>0,tabla2[i],' ')),
			con=archivo_html)
	writeLines(bordes_bgblanco(sum(tabla2)),con=archivo_html)
	writeLines('  </TR>',con=archivo_html)
	pie_pagina_ra(archivo_html)
  pagina <- pagina+1
  #---------------------------------------------------------------------
  #Tabla 3 - Talleres
  archivo_csv <- paste0("talleres_",asesor$archivo,"_",lapso,".csv")
  datos <- read.table(archivo_csv, header=TRUE, sep="\t",
		colClasses=c("Date","numeric","character"))
	unlink(archivo_csv)
	if (nrow(datos)>0) {
	  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,
		pagina)
		encabezado_tabla(3,NULL,FALSE)
		#escribe el cuerpo de la tabla (html)
		nlinea <- 0
		for (i in 1:nrow(datos)) {
			writeLines('  <TR>',con=archivo_html)
			datos[i,3] <- ifelse(is.na(datos[i,3]),"",datos[i,3])
			if (i<nrow(datos)) {
				writeLines(bordes_lr_bggris(sprintf("%03d",datos[i,2])),
					con=archivo_html)
				writeLines(bordes_lr_bggris(format(datos[i,1],"%d/%m/%Y")),
					con=archivo_html)
				writeLines(bordes_lr_bggris(datos[i,3]),con=archivo_html)
			} else {
				writeLines(bordes_blr_bggris(sprintf("%03d",datos[i,2])),
					con=archivo_html)
				writeLines(bordes_blr_bggris(format(datos[i,1],"%d/%m/%Y")),
					con=archivo_html)
				writeLines(bordes_blr_bggris(datos[i,3]),con=archivo_html)
			}#if no es la última fila
			writeLines('  </TR>',con=archivo_html)
			nlinea <- nlinea+1
			if (nlinea>8 & (nrow(datos)-i)>2) {  #divide la tabla
				pie_pagina_ra(archivo_html)
				pagina <- pagina+1
				encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
					fecha_fin,pagina)
				nlinea <- 0
				encabezado_tabla(3,NULL,TRUE)
			}#if divide la tabla
		}#for cada linea de datos
		pie_pagina_ra(archivo_html)
		pagina <- pagina+1
	}#if hay filas en "talleres"
	#---------------------------------------------------------------------
  #Lee el libro csv de asesorías para la tabla 5 y el libro de 
  #servicios de orientación para la tabla 6
  archivo_csv <- paste0("asesoria_",asesor$archivo,"_",lapso,".csv")
  asesorias <- leer_csv_asesorias(archivo_csv)
  unlink(archivo_csv)
  archivo_csv <- paste0("orientacion_",asesor$archivo,"_",lapso,".csv")
  orientacion <- read.table(archivo_csv,header=TRUE,sep="\t")
  unlink(archivo_csv)
	#verificar si no hubo asesorias o servicios de orientación
	#en tal caso, no hacer más tablas y cerrar el informe
  if (nrow(asesorias)==0 & nrow(orientacion)==0) {
		cierre_informe_act(archivo_html,narchivo_html,formatos)
		return(0)
  }#if no hubo asesorías o servicios de orientación
 	#---------------------------------------------------------------------
	#Tabla 4 : Tipos de asesoría y servicios de orientación
	encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,
		pagina)
	encabezado_tabla(4,NULL,FALSE)
	writeLines(c(
		'<TABLE WIDTH=820 CELLSPACING=0 COLS=7 BORDER=0>',
		'  <COLGROUP WIDTH="50"></COLGROUP>',
		'  <COLGROUP WIDTH="300"></COLGROUP>',
		'  <COLGROUP WIDTH="50"></COLGROUP>',
		'  <COLGROUP WIDTH="20"></COLGROUP>',
		'  <COLGROUP WIDTH="50"></COLGROUP>',
		'  <COLGROUP WIDTH="300"></COLGROUP>',
		'  <COLGROUP WIDTH="50"></COLGROUP>',		
		'  <TR>',
		bordes_bgblanco_colspan(3,"Tipos de asesoría"),
		'  <TD></TD>',
		bordes_bgblanco_colspan(3,"Tipos de orientación"),
		'  </TR>',
		'  <TR>',
		bordes_bgnegro("Tipo"),
		bordes_bgnegro("Descripción"),
		bordes_bgnegro("Cód."),
		'  <TD></TD>',
		bordes_bgnegro("Tipo"),
		bordes_bgnegro("Descripción"),
		bordes_bgnegro("Cód."),
		'  </TR>',
		'  <TR>',
		bordes_tlr_bggris("J"),
		bordes_tlr_bggris("Jornadas"),
		bordes_tlr_bggris("37"),
		'  <TD></TD>',
		bordes_tlr_bggris("AVD"),
		bordes_tlr_bggris("Actas de verificación de datos"),
		bordes_tlr_bggris(""),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("C"),
		bordes_lr_bggris("Círculos de estudio"),
		bordes_lr_bggris("45"),
		'  <TD></TD>',
		bordes_lr_bggris("AYU"),
		bordes_lr_bggris("Ayudantias"),
		bordes_lr_bggris("317"),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("E"),
		bordes_lr_bggris("Encuentros"),
		bordes_lr_bggris("46"),
		'  <TD></TD>',
		bordes_lr_bggris("BEC"),
		bordes_lr_bggris("Becas"),
		bordes_lr_bggris("129"),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("T"),
		bordes_lr_bggris("Taller"),
		bordes_lr_bggris("50"),
		'  <TD></TD>',
		bordes_lr_bggris("CCA"),
		bordes_lr_bggris("Cambio de Carrera"),
		bordes_lr_bggris(""),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("G"),
		bordes_lr_bggris("Asesoría en grupos"),
		bordes_lr_bggris("59"),
		'  <TD></TD>',
		bordes_lr_bggris("CCL"),
		bordes_lr_bggris("Cambio de Centro Local"),
		bordes_lr_bggris(""),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("I"),
		bordes_lr_bggris("Asesoría individual"),
		bordes_lr_bggris("60"),
		'  <TD></TD>',
		bordes_lr_bggris("EQU"),
		bordes_lr_bggris("Equivalencias"),
		bordes_lr_bggris(""),
		'  </TR>',
		'  <TR>',
		bordes_blr_bggris("@"),
		bordes_blr_bggris("Asesoría en línea"),
		bordes_blr_bggris("61"),
		'  <TD></TD>',
		bordes_lr_bggris("FAM"),
		bordes_lr_bggris("FAMES"),
		bordes_lr_bggris("89"),
		'  </TR>',
		'  <TR>',
		'  <TD></TD>',
		'  <TD></TD>',
		'  <TD></TD>',
		'  <TD></TD>',
		bordes_blr_bggris("PRP"),
		bordes_blr_bggris("Preparadurías"),
		bordes_blr_bggris("130"),
		'  </TR>'),con=archivo_html)
	pie_pagina_ra(archivo_html)
  pagina <- pagina+1
	#---------------------------------------------------------------------
	#Tabla 5 : Asesorias por tipo y por carrera
	if (nrow(asesorias)>0) {
		encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
			fecha_fin,pagina)	
		tabla5 <- with(asesorias,table(tipo,carrera))
		totales_filas <- 	rowSums(tabla5)
		totales_columnas <- colSums(tabla5)
		nlinea <- 0
		#escribe el encabezado de la tabla (html)
		encabezado_tabla(5,colnames(tabla5),FALSE)
		#escribe el cuerpo de la tabla (html)
		for (i in 1:nrow(tabla5)) {
			writeLines('  <TR>',con=archivo_html)
			writeLines(bgnegro_encabezado(rownames(tabla5)[i]),
				con=archivo_html)
			for (j in 1:ncol(tabla5)) 
				writeLines(bordes_bggris(ifelse(tabla5[i,j]>0,tabla5[i,j],
					' ')),con=archivo_html)
			writeLines(bordes_bgblanco(totales_filas[i]),con=archivo_html)
			writeLines('  </TR>',con=archivo_html)
			nlinea <- nlinea+1
			if (nlinea>8 & (nrow(tabla5)-i)>2) {  #divide la tabla
				pie_pagina_ra(archivo_html)
				pagina <- pagina+1
				encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
					fecha_fin,pagina)
				nlinea <- 0
				encabezado_tabla(5,colnames(tabla4),TRUE)
			}#if divide la tabla
		}#for cada fila de la tabla 5
		#escribe la última fila con los totales
		writeLines('  <TR>',con=archivo_html)
		writeLines(bordes_bgblanco('Totales'),con=archivo_html)
		for (i in 1:ncol(tabla5))
			writeLines(bordes_bgblanco(totales_columnas[i]),con=archivo_html)
		writeLines(bgnegro_encabezado(sum(totales_columnas)),
			con=archivo_html)
		pie_pagina_ra(archivo_html)
		pagina <- pagina+1
	}#if hubo asesorías
	#---------------------------------------------------------------------
	#Tabla 6 : Servicio de orientación por tipo y por carrera
	#primero verificar si hubo orientación
  if (nrow(orientacion)>0) {
		encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
			fecha_fin,pagina)	
		tabla6 <- with(orientacion,table(tipo,carrera))
		totales_filas <- 	rowSums(tabla6)
		totales_columnas <- colSums(tabla6)
		nlinea <- 0
		#escribe el encabezado de la tabla (html)
		encabezado_tabla(6,colnames(tabla6),FALSE)
		#escribe el cuerpo de la tabla (html)
		for (i in 1:nrow(tabla6)) {
			writeLines('  <TR>',con=archivo_html)
			writeLines(bgnegro_encabezado(rownames(tabla6)[i]),
				con=archivo_html)
			for (j in 1:ncol(tabla6)) 
				writeLines(bordes_bggris(ifelse(tabla6[i,j]>0,tabla6[i,j],
					' ')),con=archivo_html)
			writeLines(bordes_bgblanco(totales_filas[i]),con=archivo_html)
			writeLines('  </TR>',con=archivo_html)
			nlinea <- nlinea+1
			if (nlinea>8 & (nrow(tabla6)-i)>2) {  #divide la tabla
				pie_pagina_ra(archivo_html)
				pagina <- pagina+1
				encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
					fecha_fin,pagina)
				nlinea <- 0
				encabezado_tabla(6,colnames(tabla6),TRUE)
			}#if divide la tabla
		}#for
		#escribe la última fila con los totales
		writeLines('  <TR>',con=archivo_html)
		writeLines(bordes_bgblanco('Totales'),con=archivo_html)
		for (i in 1:ncol(tabla6))
			writeLines(bordes_bgblanco(totales_columnas[i]),con=archivo_html)
		writeLines(bgnegro_encabezado(sum(totales_columnas)),con=archivo_html)
		pie_pagina_ra(archivo_html)
	}#if hubo servicios de orientación
  #escribe colofon y cierra el archivo html
  cierre_informe_act(archivo_html,narchivo_html,formatos)
} #generar_reporte_orientador

#-----------------------------------------------------------------------

generar_reporte_preparador <- function(asesor,fecha_inicio,
	fecha_fin,formatos) {
  
	encabezado_tabla <- function(num_tabla,columnas,continua,altura) {
		#la función encabezado_tabla dibuja las filas de encabezado de cada
		#una de las tablas que aparecen en el informe de actividades del
		#asesor. Primero crea una tabla envolvente de altura fija
		writeLines(c('<TABLE>',
			paste0('<TR VALIGN=MIDDLE HEIGHT=',altura,'>'),
			paste0('<TD WIDTH=900  HEIGHT=',altura,' ALIGN=CENTER>')),
			con=archivo_html)
		writeLines(paste0('<H2>Tabla ',num_tabla,' - ',switch(num_tabla,
			'Talleres dictados',	#1
			'Tipos de asesorías',	#2
			'Asesorías por asignatura, carrera y tipo',	#3
			'Asesorías por tipo y por carrera'),	#4
			{if (continua) ' (continuación)' else ''},
			'</H2>'),con=archivo_html )
		if (num_tabla==2) return(0)
		num_col <- length(columnas)
		ancho_t <- switch(num_tabla,0,560,230,120)+
			num_col*48
		writeLines(paste0('<TABLE WIDTH=',ancho_t,' CELLSPACING=0 COLS="',
			switch(num_tabla,
				3,
				0, #this is a place-holder so what i put here is shit anyway
				num_col+4,
				num_col+2),
			'" BORDER="0">'),con=archivo_html)
		if (num_tabla==3) {
			writeLines('  <COLGROUP WIDTH="50"></COLGROUP>',con=archivo_html)
			writeLines('  <COLGROUP WIDTH="60"></COLGROUP>',con=archivo_html)
		} else {
			writeLines('  <COLGROUP WIDTH="60"></COLGROUP>',con=archivo_html)
		}
		if (num_tabla!=1) {
			for (i in 1:num_col)
				writeLines('  <COLGROUP WIDTH="48"></COLGROUP>',
					con=archivo_html)
			if (num_tabla==3) {
				writeLines('  <COLGROUP WIDTH="60"></COLGROUP>',
					con=archivo_html)
				writeLines('  <COLGROUP WIDTH="60"></COLGROUP>',
					con=archivo_html)
			} else {
				writeLines('  <COLGROUP WIDTH="60"></COLGROUP>',
					con=archivo_html)
			}#if num_tabla==3
		} else {
			writeLines('  <COLGROUP WIDTH="100"></COLGROUP>',
				con=archivo_html)
			writeLines('  <COLGROUP WIDTH="400"></COLGROUP>',
				con=archivo_html)		
		}#if num_tabla!=1		
		#escribe fila de encabezado 1
		writeLines('  <TR>',con=archivo_html)
		switch(num_tabla,
			{#Tabla 1
				writeLines(bordes_bgblanco('Asig.'),con=archivo_html)
				writeLines(bordes_bgblanco('Fecha'),con=archivo_html)
				writeLines(bordes_bgblanco('Descripción'),con=archivo_html)
			},
			{}, #Tabla 2 (nunca entra aqui)
			{#Tabla 3
				writeLines(bordes_bgblanco_rowspan(2,'Asig.'),con=archivo_html)
				writeLines(bordes_bgblanco_rowspan(2,'Tipo'),con=archivo_html)
				writeLines(bordes_bgblanco_colspan(num_col,'Carreras'),
					con=archivo_html)
				writeLines(bordes_bgblanco_colspan(2,'Totales'),
					con=archivo_html)
			},
			{#Tabla 4
				writeLines(bordes_bgblanco_rowspan(2,'Tipo'),con=archivo_html)
				writeLines(bordes_bgblanco_colspan(num_col,'Carreras'),
					con=archivo_html)
				writeLines(bordes_bgblanco_rowspan(2,'Totales'),
					con=archivo_html)
			}
		)
		#escribe fila de encabezado 2
		if (num_tabla!=1) {
			writeLines('  <TR>',con=archivo_html)
			for (item in columnas)
				writeLines(bgnegro_encabezado(item),con=archivo_html)
			if (num_tabla==3) {
				writeLines(bordes_bgblanco('Tipo'),con=archivo_html)
				writeLines(bordes_bgblanco('Asig.'),con=archivo_html)
			}	
			writeLines('  </TR>',con=archivo_html)
		}#if num_tabla!=1
	} #encabezado_tabla
	#----------------
	svalue(barra_status) <- paste("Generando el reporte de",
		asesor$asesor,"...")
	#convierte las fechas a formato imprimible:
	fecha_inicio <- format(fecha_inicio,"%d de %b %Y")
	fecha_fin <- format(fecha_fin,"%d de %b %Y")
	lapso <- svalue(selector_lapso)
	#determina el nombre del archivo html y crealo
	narchivo_html <- paste0("actividades_",asesor$archivo,"_",lapso,
		".html")
	archivo_html <- file(narchivo_html,open="w")
	generar_portada_ra(archivo_html,asesor,fecha_inicio,fecha_fin)
	pagina <- 1
	#calcula la altura de las tablas del medio
	linmaterias <- ceiling(nrow(asesor$materias)/15) - 1
	tmp <- gsub(paste("(.{1,",14,"})(\\s|$)",sep=""), "\\1\n", 
			asesor$area)
	linarea <-length(strsplit(tmp,"\n")[[1]])-1
	altura <- 400 - 15*(linarea+linmaterias)
	 #---------------------------------------------------------------------
  #Tabla 1 - Talleres
  archivo_csv <- paste0("talleres_",asesor$archivo,"_",lapso,".csv")
  datos <- read.table(archivo_csv, header=TRUE, sep="\t",
		colClasses=c("Date","numeric","character"))
	unlink(archivo_csv)
	if (nrow(datos)>0) {
	  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,
		pagina)
		encabezado_tabla(1,NULL,FALSE)
		#escribe el cuerpo de la tabla (html)
		nlinea <- 0
		for (i in 1:nrow(datos)) {
			writeLines('  <TR>',con=archivo_html)
			datos[i,3] <- ifelse(is.na(datos[i,3]),"",datos[i,3])
			if (i<nrow(datos)) {
				writeLines(bordes_lr_bggris(sprintf("%03d",datos[i,2])),
					con=archivo_html)
				writeLines(bordes_lr_bggris(format(datos[i,1],"%d/%m/%Y")),
					con=archivo_html)
				writeLines(bordes_lr_bggris(datos[i,3]),con=archivo_html)
			} else {
				writeLines(bordes_blr_bggris(sprintf("%03d",datos[i,2])),
					con=archivo_html)
				writeLines(bordes_blr_bggris(format(datos[i,1],"%d/%m/%Y")),
					con=archivo_html)
				writeLines(bordes_blr_bggris(datos[i,3]),con=archivo_html)
			}#if no es la última fila
			writeLines('  </TR>',con=archivo_html)
			nlinea <- nlinea+1
			if (nlinea>8 & (nrow(datos)-i)>2) {  #divide la tabla
				pie_pagina_ra(archivo_html)
				pagina <- pagina+1
				encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
					fecha_fin,pagina)
				nlinea <- 0
				encabezado_tabla(1,NULL,TRUE)
			}#if divide la tabla
		}#for cada linea de datos
		pie_pagina_ra(archivo_html)
		pagina <- pagina+1
	}#if hay filas en "talleres"
	#---------------------------------------------------------------------
  #Lee el libro csv de asesorías para las tablas 3-5
  archivo_csv <- paste0("asesoria_",asesor$archivo,"_",lapso,".csv")
  asesorias <- leer_csv_asesorias(archivo_csv)
  unlink(archivo_csv)
	#verificar si no hubo asesorias
	#en tal caso, no hacer más tablas e imprimir un mensaje en el informe
  if (nrow(asesorias)==0) {
		cierre_informe_act(archivo_html,narchivo_html,formatos)
		return(0)
  }
  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,pagina)
	#---------------------------------------------------------------------
	#Tabla 2 : Tipos de asesoría
	encabezado_tabla(2,NULL,FALSE,altura)
	writeLines(c(
		'<TABLE WIDTH=400 CELLSPACING=0 COLS=3 BORDER=0>',
		'  <COLGROUP WIDTH="50"></COLGROUP>',
		'  <COLGROUP WIDTH="300"></COLGROUP>',
		'  <COLGROUP WIDTH="50"></COLGROUP>',
		'  <TR>',
		bordes_bgblanco_colspan(3,"Tipos de asesoría"),
		'  </TR>',
		'  <TR>',
		bordes_bgnegro("Tipo"),
		bordes_bgnegro("Descripción"),
		bordes_bgnegro("Cód."),
		'  </TR>',
		'  <TR>',
		bordes_tlr_bggris("J"),
		bordes_tlr_bggris("Jornadas"),
		bordes_tlr_bggris("37"),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("C"),
		bordes_lr_bggris("Círculos de estudio"),
		bordes_lr_bggris("45"),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("E"),
		bordes_lr_bggris("Encuentros"),
		bordes_lr_bggris("46"),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("T"),
		bordes_lr_bggris("Taller"),
		bordes_lr_bggris("50"),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("G"),
		bordes_lr_bggris("Asesoría en grupos"),
		bordes_lr_bggris("59"),
		'  </TR>',
		'  <TR>',
		bordes_lr_bggris("I"),
		bordes_lr_bggris("Asesoría individual"),
		bordes_lr_bggris("60"),
		'  </TR>',
		'  <TR>',
		bordes_blr_bggris("@"),
		bordes_blr_bggris("Asesoría en línea"),
		bordes_blr_bggris("61"),
		'  </TR>'),con=archivo_html)
	pie_pagina_ra(archivo_html)
  pagina <- pagina+1
  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,fecha_fin,pagina)
  #---------------------------------------------------------------------
	#Tabla 3 : Asesorias por asignatura, carrera y tipo
	tabla3 <- with(asesorias,table(tipo,carrera,asignatura))
	asignaturas <- attributes(tabla3)$dimnames$asignatura
  #escribe el encabezado de la tabla (html)
  encabezado_tabla(3,colnames(tabla3),FALSE,altura)
  nlinea <- 0
  filas <- dim(tabla3)[1]
	#escribe el cuerpo de la tabla (html)
   for (i in asignaturas) {
		writeLines('  <TR>',con=archivo_html)
		writeLines(bordes_bgnegro_rowspan(filas,i),con=archivo_html)
		if (filas>1)
			totales_filas <- rowSums(tabla3[,,i])
		else totales_filas <- sum(tabla3[,,i])
		for (j in 1:filas) {
			writeLines(bordes_bgnegro(rownames(tabla3)[j]),
					con=archivo_html)
			for (k in 1:ncol(tabla3))
				if (j==1) 
					if (filas>1) writeLines(bordes_tlr_bggris(ifelse(
						tabla3[j,k,i]>0,tabla3[j,k,i],' ')),con=archivo_html)
					else writeLines(bordes_bggris(ifelse(
						tabla3[j,k,i]>0,tabla3[j,k,i],' ')),con=archivo_html)
			  else
					if (j<nrow(tabla3))
						writeLines(bordes_lr_bggris(ifelse(tabla3[j,k,i]>0,
						tabla3[j,k,i],' ')),con=archivo_html)
					else
						writeLines(bordes_blr_bggris(ifelse(tabla3[j,k,i]>0,
							tabla3[j,k,i],' ')),con=archivo_html)
			writeLines(bordes_bgblanco(totales_filas[j]),con=archivo_html)
			if (j==1) writeLines(bordes_bgblanco_rowspan(filas,
				sum(totales_filas)),con=archivo_html)
			writeLines('  </TR>',con=archivo_html)
			if (j<filas) writeLines('  <TR>',con=archivo_html)
		} #for j (tipo)
		nlinea <- nlinea+filas
		if (nlinea>8 & ((filas*(dim(tabla3)[3]-match(i,asignaturas)))
			>2) ) {	#divide la tabla y llevala a la siguiente página
			writeLines('  </TR>',con=archivo_html)
			pie_pagina_ra(archivo_html)
			pagina <- pagina+1
			encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
				fecha_fin,pagina)
			nlinea <- 0
			encabezado_tabla(3,colnames(tabla3),TRUE,altura)
		}
  } #for i (asignaturas)
  #escribe última fila de la tabla 3
  totales_columnas <- colSums(t(colSums(tabla3)))
  writeLines('  <TR>',con=archivo_html)
	writeLines(bordes_bgblanco_colspan(3,'Totales'),con=archivo_html)
	for (i in 1:ncol(tabla3))
		writeLines(bordes_bgblanco(totales_columnas[i]),con=archivo_html)
	writeLines(bgnegro_colspan(2,sum(totales_columnas)),con=archivo_html)
	writeLines('  </TR>',con=archivo_html)
	pie_pagina_ra(archivo_html)
  pagina <- pagina+1
  encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
		fecha_fin,pagina)
	#---------------------------------------------------------------------
	#Tabla 4 : Asesorias por tipo y por carrera
	tabla4 <- with(asesorias,table(tipo,carrera))
	totales_filas <- 	rowSums(tabla4)
  totales_columnas <- colSums(tabla4)
  nlinea <- 0
	#escribe el encabezado de la tabla (html)
	encabezado_tabla(4,colnames(tabla4),FALSE,altura)
	#escribe el cuerpo de la tabla (html)
	for (i in 1:nrow(tabla4)) {
		writeLines('  <TR>',con=archivo_html)
		writeLines(bgnegro_encabezado(rownames(tabla4)[i]),con=archivo_html)
		for (j in 1:ncol(tabla4)) 
			writeLines(bordes_bggris(ifelse(tabla4[i,j]>0,tabla4[i,j],' ')),
				con=archivo_html)
		writeLines(bordes_bgblanco(totales_filas[i]),con=archivo_html)
		writeLines('  </TR>',con=archivo_html)
		nlinea <- nlinea+1
		if (nlinea>8 & (nrow(tabla4)-i)>2) {  #divide la tabla
			pie_pagina_ra(archivo_html)
			pagina <- pagina+1
			encabezado_pagina_ra(archivo_html,asesor,fecha_inicio,
				fecha_fin,pagina)
			nlinea <- 0
			encabezado_tabla(4,colnames(tabla4),TRUE,altura)
		}
	}
	#escribe la última fila con los totales
	writeLines('  <TR>',con=archivo_html)
	writeLines(bordes_bgblanco('Totales'),con=archivo_html)
	for (i in 1:ncol(tabla4))
		writeLines(bordes_bgblanco(totales_columnas[i]),con=archivo_html)
	writeLines(bgnegro_encabezado(sum(totales_columnas)),con=archivo_html)
	pie_pagina_ra(archivo_html)
  #escribe colofon y cierra el archivo html
  cierre_informe_act(archivo_html,narchivo_html,formatos)
} #generar_reporte_preparador

#-----------------------------------------------------------------------
generar_reportes_actividad <- function() {
	#Funciones internas
	tienen_los_archivos <- function(sel_asesores) {
		#si se invoca como funcion de dibujo de iconos "yes" o "no",
		#sel_asesores un data frame de una sola columna
		#con los nombres seleccionados
		#de lo contrario es un vector de cadenas
		if (is.data.frame(sel_asesores)) sel_asesores <- sel_asesores[,1]
		sub_lista <- sub_lista_asesores(sel_asesores)
		archivos <- sapply(1:length(sub_lista),function(i) 
			sub_lista[[i]]$archivo)
		archivos_OL <- paste0("OL_",archivos,"_",svalue(selector_lapso),
			".xls")
		archivos_ase <- paste0("asesoria_",archivos,"_",
			svalue(selector_lapso),".xls")
		#se verifica si existen los archivos requeridos
		#(los preparadores no tienen archivos de objetivos logrados)
		existen <- sapply(1:length(sel_asesores), function(i) {
			ifelse(sub_lista[[i]]$tipo=="Preparador",TRUE,
				file.access(archivos_OL[i])==0) &
			file.access(archivos_ase[i])==0 } )
		names(existen) <- sel_asesores
		return(existen)
	}

	ejecutar <- function(h,...) {
		fi <- convertir_fecha(svalue(fecha_inicio))
		ff <- convertir_fecha(svalue(fecha_fin))
		error <- 0
		#códigos de error
		#0 no hay error
		#1 fecha de inicio es invalida
		#2 fecha final es invalida
		#3 ambas fechas son invalidas
		#4 fecha de inicio es posterior a fecha final
		#5 no existen los archivos de asesores
		#6 no existen los archivos de asesores y 
		#  fecha de inicio es invalida
		#7 no existen los archivos de asesores y
		#  fecha final es invalida
		#8 no existen los archivos de asesores y 
		#  ambas fechas son invalidas
		#9 fecha de inicio es posterior a la fecha final
		#  y no existen los archivos de asesores
		cuales <- tienen_los_archivos(asesores_seleccionados[])
		if (is.na(fi)) error <- 1 
		if (is.na(ff)) 
			if (error==1) error <- 3 else error <- 2
		if (!(is.na(fi)|is.na(ff)) & fi>ff) error <- 4
		if (!any(cuales)) error <- error +5
		if (error>0) {
			gmessage(switch(error,
				"Fecha de inicio inválida.",
				"Fecha final inválida.",
				"Fecha de inicio y fecha final inválidas.",
				"Fecha de inicio es posterior a la fecha final.",
				"No existen los archivos de asesores.",
				paste0("Fecha de inicio inválida\n",
					"y no existen los archivos de asesores."),
				paste0("Fecha final inválida y no\n",
					"existen los archivos de asesores."),
				paste0("Fecha de inicio y fecha final inválidas.\n",
					"No existen los archivos de asesores."),
				paste0("Fecha de inicio es posterior a la fecha final.\n",
					"No existen los archivos de asesores.") ),
				icon="warning")
			return() #sale sin continuar con la elaboración de los reportes
		}# if hay errores
		quienes <- sub_lista_asesores(names(cuales)[cuales])
		formatos <- c(svalue(formato_doc),svalue(formato_odt),
			svalue(formato_pdf))
		for (asesor in quienes)  {
			recorrer_hoja_asesoria(asesor,fi,ff)
			switch(asesor$tipo,
				"Asesor"= {
					recorrer_libro_evaluacion(asesor,fi,ff)
					generar_reporte_asesor(asesor,fi,ff,formatos) },
				"Orientador" = {
					leer_curso_introductorio(paste0("OL_",asesor$archivo,"_",
						svalue(selector_lapso),".xls"))
					generar_reporte_orientador(asesor,fi,ff,formatos) },
				"Preparador" = {
					generar_reporte_preparador(asesor,fi,ff,formatos) }
			)
		} # for
		dispose(wp)
	}# ejecutar
	
	#crea el cuadro de dialogo
	wp <- gbasicdialog(title="Generar reporte de actividades",
		parent=ventana,do.buttons=FALSE,visible=FALSE,horizontal=FALSE)
	size(wp) <- c(430,520)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#menu bar superior
	lista_menu <- list(
		Volver = list(
			salir=gaction(label="Volver al menú principal",icon="gtk-quit",
				handler = function(h,...) dispose(wp),
				tooltip = "Salir al menú principal")
			)
	)
	gmenu(lista_menu, cont = wp_marco)
	addSpace(wp_marco,value=5)
	fechas_frame <- gframe(text=paste("Rango de fechas para el",
		"reporte de actividades"),horizontal=FALSE,container=wp_marco)
	addSpace(fechas_frame,value=10)
	finicio_marco <- ggroup(horizontal=TRUE,cont=fechas_frame)
	etiq_fecha_inicio <- glabel("Fecha de inicio", cont=finicio_marco)
	size(etiq_fecha_inicio) <- c(100,20)
	fecha_inicio <- gtext("",width=120,height=20,container=finicio_marco)
	addHandlerKeystroke(fecha_inicio, handler=function(h,...) {
		  #elimina cualquier caracter que no sea de fecha
		  svalue(h$obj) <- gsub("[^0-9/]","",svalue(h$obj))
		  #máxima longitud de texto es 10: xx/xx/xxxx
		  svalue(h$obj) <- strtrim(svalue(h$obj),10)		  
			}
		)
	addSpace(fechas_frame,value=5)
	ffin_marco <- ggroup(horizontal=TRUE,cont=fechas_frame)
	etiq_fecha_final <- glabel("Fecha final", cont=ffin_marco)
	size(etiq_fecha_final) <- c(100,20)
	fecha_fin <- gtext("",width=120,height=20,container=ffin_marco)
	addHandlerKeystroke(fecha_fin, handler=function(h,...) {
		  #elimina cualquier caracter que no sea de fecha
		  svalue(h$obj) <- gsub("[^0-9/]","",svalue(h$obj))
		  #máxima longitud de texto es 10: xx/xx/xxxx
		  svalue(h$obj) <- strtrim(svalue(h$obj),10)		  
			}
		)
	addSpace(fechas_frame,value=10)
	asesores_frame <- gframe(text=paste("Generar reportes para los",
		"siguientes asesores"),horizontal=FALSE,container=wp_marco)
	asesores_seleccionados <- gtable(svalue(selector_asesores),
		icon.FUN=function(x) ifelse(tienen_los_archivos(x),"gtk-yes",
		"gtk-no"), cont=asesores_frame)
	#elimina el encabezado de "Value" por defecto
	gtk_asesel <- getToolkitWidget(asesores_seleccionados)
	col1 <- gtkTreeViewGetColumn(gtk_asesel, 1)
	gtkTreeViewColumnSetClickable(col1, FALSE)
	col1_header <- gtkLabelNew("", show = FALSE)
	gtkTreeViewColumnSetWidget(col1, widget = col1_header)
	size(asesores_seleccionados) <- c(360,200)
	formato_frame <- gframe("Seleccione los formatos de reporte",
		horizontal=TRUE, cont=wp_marco)
	addSpring(formato_frame,horizontal=TRUE)
	logo_doc <- gimage(file.path(dir_programa,"word.png"),
		cont=formato_frame)
	formato_doc <- gcheckbox("MS Word\n2003",cont=formato_frame)
	logo_odt <- gimage(file.path(dir_programa,"LOwriter.png"),
		cont=formato_frame)
	formato_odt <- gcheckbox("LibreOffice\nWriter",cont=formato_frame)
	logo_pdf <- gimage(file.path(dir_programa,"application-pdf.png"),
		cont=formato_frame)
	formato_pdf <- gcheckbox("PDF",cont=formato_frame)
	addSpring(formato_frame,horizontal=TRUE)
	size(formato_frame) <- c(360,80)
	size(formato_doc) <- c(84,48)
	size(formato_odt) <- c(96,48)
	size(formato_pdf) <- c(50,48)
	go_marco <- ggroup(horizontal=TRUE,cont=wp_marco)
	size(go_marco) <- c(430,48)
	addSpring(go_marco,horizontal=TRUE)
	go_button <- gbutton("gtk-execute",border=TRUE,
		handler=ejecutar, cont=go_marco)
	addSpring(go_marco,horizontal=TRUE)
	#agregar barra de estatus
	barra_status_reportes <- gstatusbar("",wp_marco)
	font(barra_status_reportes) <- list(color="darkgreen",size=11)
	visible(wp) <- TRUE
} #generar_reportes_actividad
