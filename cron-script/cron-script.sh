# Script que se ejecuta diariamente en una máquina virtual para actualizar los datos

Rscript /home/andresfernandez32/gasolineras.R
gsutil cp /home/andresfernandez32/temp.csv gs://lpe-gasolineras/limpio"`date +"%d%m"`".csv
gsutil cp /home/andresfernandez32/temp.csv gs://lpe-gasolineras/ultimo.csv