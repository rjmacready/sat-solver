SRC=uf20-91 UUF50.218.1000 uf50-218
OUT=bin

FILES0=$(foreach dir,$(SRC),$(wildcard $(dir)/*.cnf))

#all0:
#	echo $(FILES0)

FILES1=$(wildcard $(SRC)/*.cnf)
FILES2=$(patsubst %.cnf,%.out.it,$(FILES0)) $(patsubst %.cnf,%.out.rec,$(FILES0))
FILES3=$(addprefix $(OUT)/,$(FILES2))

.PHONY: all clean

$(OUT)/%.out.it: %.cnf
	mkdir -p $(@D)
	time -o $(OUT)/it.time -a --format="%e,%U,%s" _build/bin/sat it $< $@

$(OUT)/%.out.rec: %.cnf
	mkdir -p $(@D)
	time -o $(OUT)/rec.time -a --format="%e,%U,%s" _build/bin/sat rec $< $@

all: $(FILES3)

clean:
	rm -rf $(OUT)
