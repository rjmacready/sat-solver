SRC=uf20-91 UUF50.218.1000 uf50-218
OUT=bin

FILES0=$(foreach dir,$(SRC),$(wildcard $(dir)/*.cnf))

#all0:
#	echo $(FILES0)

FILES1=$(wildcard $(SRC)/*.cnf)
FILES2=$(patsubst %.cnf,%.out,$(FILES0))
FILES3=$(addprefix $(OUT)/,$(FILES2))

.PHONY: all clean

$(OUT)/%.out: %.cnf
	mkdir -p $(@D)
	_build/bin/sat $< > $@

all: $(FILES3)

clean:
	rm -rf $(OUT)
