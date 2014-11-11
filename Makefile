SOURCES = \
Data.ml \
Gen.ml \
Fit.ml \
Pop.ml \
Main.ml


all: $(SOURCES)
	corebuild Main.native

check: $(SOURCES)
	@chmod u+x check_width
	@./check_width Data.ml; \
        ./check_width Gen.ml; \
	./check_width Fit.ml; \
	./check_width Pop.ml; \
	./check_width Main.ml

clean:
	rm -rf _build Main.native


