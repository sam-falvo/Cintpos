

pdf:
	cintsys -c c b lokifft
	rastsys -c "raster; lokifft 9; raster close"
	cintsys -c "rast2ps fh 25200000 mh 1000000"
	ps2pdf RASTER.ps loki9.pdf
	rastsys -c "raster; lokifft 6; raster close"
	cintsys -c "rast2ps fh 1500000 fg 100000 mh 500000 mg 100000"
	ps2pdf RASTER.ps loki6.pdf

pdfv2:
	cintsys -c c b lokifftv2
	rastsys -c "raster; lokifftv2 9; raster close"
	cintsys -c "rast2ps fh 20200000 mh 1020000"
	ps2pdf RASTER.ps lokiv2-9.pdf
	rastsys -c "raster; lokifftv2 6; raster close"
	cintsys -c "rast2ps fh 1320000 fg 100000 mh 402000 mg 100000"
	ps2pdf RASTER.ps lokiv2-6.pdf
