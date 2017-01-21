#!/bin/bash
#CREAR EL ARCHIVO DESKTOP A PARTIR DE LA PLANTILLA,
#INCLUYENDO LA RUTA AL EJECUTABLE EN EL DIRECTORIO RAIZ
cp hevasu.plantilla hevasu.desktop
echo "Exec="$HOME"/HEVASU/hevasu.sh" >> hevasu.desktop
echo "Icon="$HOME"/HEVASU/logo.png" >> hevasu.desktop
echo "Path="$HOME"/HEVASU" >> hevasu.desktop
desktop-file-install --dir=$HOME/.local/share/applications $HOME/HEVASU/hevasu.desktop
rm hevasu.desktop
#CAMBIA EL ATRIBUTO (A EJECUTABLE) DEL SCRIPT PRINCIPAL DE LA APLICACION
chmod a+x hevasu.sh
