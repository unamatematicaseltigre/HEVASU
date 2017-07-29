function identificadorCarpeta() {
	// esta función obtiene el Id de la carpeta donde está este libro de cálculo
	var ss = SpreadsheetApp.getActiveSpreadsheet();
	var estearchivo = DriveApp.getFileById(ss.getId());
	var estacarpeta=estearchivo.getParents();
	return estacarpeta.next().getId();
}

function listaArchivosEnCarpeta() {
	// esta función obtiene una tabla con los enlaces (públicos) para cada
	// ficha/certificado según el número de cédula de cada estudiante
    var hoja = SpreadsheetApp.getActiveSheet();
    hoja.appendRow(["cedula", "url"]);
    //cambie el ID en .getFolderById("...") abajo para incluir el ID de esta carpeta
    var carpeta = DriveApp.getFolderById(identificadorCarpeta());
    var directorio = carpeta.getFiles();
    var archivo;
    var cedulaRegEx=/^([0-9]+)\..+$/;
    while (directorio.hasNext()) {
		// obtiene el nombre del siguiente archivo en directorio
        var archivo = directorio.next();
		var nombrearchivo = archivo.getName();	
		// verifica si el nombre de archivo sin la extensión .pdf es un número de cédula
		var coincidencia=cedulaRegEx.exec(nombrearchivo); //verifica si el nombre
		// si es un número de cédula, agregalo como renglón a la hoja
		if (coincidencia!=null) {
        	data = [
            	coincidencia[1],
            	"https://drive.google.com/uc?export=download&id=" + archivo.getId(),
        	];
			hoja.appendRow(data);} // if
    }; // while
}; // function

