APPS=test stats
LEHMER=mod_lehmer

FC=f95

all: $(APPS)

%: %.f95 $(LEHMER).o
	$(FC) -o $@ $^

%.o: %.f95
	$(FC) -c $<

.PHONY: clean
clean:
	$(RM) $(LEHMER).o $(APPS)
