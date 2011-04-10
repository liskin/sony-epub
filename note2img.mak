%.svg: %.note
	note2svg $< >$@

%.eps: %.svg
	inkscape -z -f $< -D --export-area-snap -E $@

%.pdf: %.eps
	epstopdf --outfile=$@ $<

%.png: %.pdf
	pstoimg -depth 8 -density 120 -antialias -aaliastext -out $@ $<
